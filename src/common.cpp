#include "common.h"
#define NOMINMAX
#include <Windows.h>
CompilerContext context;

SourceFileInfo &get_source_info(utf8 *location);


u32 get_line_number(utf8 *from) {
	auto lines = get_source_info(from).lines;

	// lines will be empty at lexing time.
	// So if an error occurs at lexing time,
	// slower algorithm is executed.
	if (lines.count) {
#if 1
		// binary search
		auto begin = lines.data;
		auto end = lines.data + lines.count;
		while (1) {
			auto line = begin + (end - begin) / 2;
			if (line->data <= from && from < line->data + line->count) {
				return line - lines.data + 1;
			}
			if (from < line->data) {
				end = line;
			} else {
				begin = line + 1;
			}
		}
		invalid_code_path();
#else
		for (auto &line : lines) {
			if (line.begin() <= from && from < line.end()) {
				return &line - lines.data;
			}
		}
		invalid_code_path();
#endif
	} else {
		u32 result = 1;
		while (*--from != 0)
			result += (*from == '\n');
		return result;
	}
}

u32 get_column_number(utf8 *from) {
	u32 result = 0;
	while (1) {
		if (*from == '\n' || *from == '\0')
			break;

		if (*from == '\t')
			result += 4;
		else
			result += 1;

		from -= 1;
	}
	return result;
}

void print_replacing_tabs_with_4_spaces(PrintKind kind, Span<utf8> string) {
	for (auto c : string) {
		if (c == '\t') {
			print(kind, "    ");
		} else {
			print(kind, c);
		}
	}
}
PrintKind get_print_kind(ReportKind kind) {
	switch (kind) {
		using enum ReportKind;
		case info: return Print_info;
		case warning: return Print_warning;
		case error: return Print_error;
	}
	invalid_code_path();
}

void print_source_line(ReportKind kind, Span<utf8> location) {

	if (location.data == nullptr) {
		// print("(null location)\n\n");
		return;
	}


	auto error_line_begin = location.begin();
	if (*error_line_begin != 0) {
		while (1) {
			error_line_begin--;
			if (*error_line_begin == 0 || *error_line_begin == '\n') {
				error_line_begin++;
				break;
			}
		}
	}

	auto error_line_end = location.end();
	while (1) {
		if (*error_line_end == 0 || *error_line_end == '\n') {
			break;
		}
		error_line_end++;
	}


	auto error_line = Span(error_line_begin, error_line_end);
	auto error_line_number = get_line_number(error_line_begin);

	auto print_line = [&](auto line) {
		return print("{} | ", Format{line, align_right(5, ' ')});
	};

	// I don't know if previous line is really useful
#if 0
	if (error_line.data[-1] != 0) {
		auto prev_line_end = error_line.data - 1;
		auto prev_line_begin = prev_line_end - 1;

		while (1) {
			if (*prev_line_begin == 0) {
				prev_line_begin += 1;
				break;
			}

			if (*prev_line_begin == '\n') {
				++prev_line_begin;
				break;
			}

			--prev_line_begin;
		}
		auto prev_line = Span(prev_line_begin, prev_line_end);
		auto prev_line_number = get_line_number(prev_line_begin);

		print_line(prev_line_number);
		print_replacing_tabs_with_4_spaces(Print_info, prev_line);
		print('\n');
	}
#endif

	auto line_start = Span(error_line.begin(), location.begin());
	auto line_end   = Span(location.end(), error_line.end());
	auto offset = print_line(error_line_number);
	print_replacing_tabs_with_4_spaces(Print_info,  line_start);
	print_replacing_tabs_with_4_spaces(get_print_kind(kind), location);
	print_replacing_tabs_with_4_spaces(Print_info,  line_end);
	print('\n');

	for (u32 i = 0; i < offset; ++i) {
		print(' ');
	}
	for (auto c : line_start) {
		if (c == '\t') {
			print("    ");
		} else {
			print(' ');
		}
	}
	for (auto c : location) {
		if (c == '\t') {
			print("^^^^");
		} else {
			print('^');
		}
	}
	print("\n");
}

SourceFileInfo &get_source_info(utf8 *location) {
	for (auto &source : context.sources) {
		if (source.source.begin() <= location && location < source.source.end()) {
			return source;
		}
	}
	invalid_code_path();
}

HeapString where(utf8 *location) {
	if (location) {
		return format<MyAllocator>(u8"{}:{}:{}", parse_path(get_source_info(location).path).name_and_extension(), get_line_number(location), get_column_number(location));
	} else {
		return {};
	}
}

void print_report(Report r) {
	if (r.where.count)
		print("{}: ", r.where);
	switch (r.kind) {
		case ReportKind::info:    print(Print_info,    strings.info);  break;
		case ReportKind::warning: print(Print_warning, strings.warning); break;
		case ReportKind::error:	  print(Print_error,   strings.error);	  break;
		default: invalid_code_path();
	}
	print(": {}\n", r.message);
	print_source_line(r.kind, r.location);
}

struct AllocationBlock {
	u8 *base   = 0;
	u8 *cursor = 0;
	umm size   = 0;
};

List<AllocationBlock> ast_allocation_blocks;
u32 last_allocation_block_index;
Mutex allocation_mutex;

umm ast_allocation_block_size;

u32 debug_allocation_blocks = 0;

void new_my_block() {
	AllocationBlock block;
	block.size = ast_allocation_block_size;
	block.cursor = block.base = os_allocator.allocate<u8>(ast_allocation_block_size);
	assert(block.cursor);
	ast_allocation_blocks.add(block);
	last_allocation_block_index += 1;
	atomic_increment(&debug_allocation_blocks);
}

void init_my_allocator() {
	ast_allocation_blocks.allocator = os_allocator;
	ast_allocation_block_size = 1024*1024;
	last_allocation_block_index = (u32)-1;
	new_my_block();
	ast_allocation_block_size *= 2;
}

AllocationResult MyAllocator::allocate_impl(umm size, umm align, std::source_location location) {
	scoped_lock(allocation_mutex);

retry:
	auto block = &ast_allocation_blocks[last_allocation_block_index];

	u8 *target = (u8 *)(((umm)block->cursor + align - 1) & ~(align - 1));
	if ((umm)((u8 *)block->base + block->size - target) < size) {
		ast_allocation_block_size *= 2;
		while (ast_allocation_block_size < size + align - 1) {
			ast_allocation_block_size *= 2;
		}
		new_my_block();
		goto retry;
	}

	block->cursor = target + size;

	return {
		.data = target,
		.count = size,
		.is_zeroed = false,
	};
}
AllocationResult MyAllocator::reallocate_impl(void *data, umm old_size, umm new_size, umm align, std::source_location location) {
	auto result = allocate_impl(new_size, align);
	memcpy(result.data, data, old_size);
	return result;
}

void MyAllocator::deallocate_impl(void *data, umm size, umm align, std::source_location location) {
	(void)data;
	(void)size;
}

HeapString escape_string(String string) {
	if (!string.count)
		return {};

	HeapString new_string;
	new_string.reserve(string.count);

	auto p = string.data;
	utf32 c = 0;
	utf32 prev = 0;

	while (1) {
		if (p >= string.end())
			break;
		auto got_char = get_char_and_advance_utf8(&p);
		if (!got_char) {
			return {};
		}

		prev = c;
		c = got_char.value_unchecked();

		switch (c) {
			case '"':  { new_string.add({'\\', '"'}); break; }
			case '\n': { new_string.add({'\\', 'n'}); break; }
			case '\r': { new_string.add({'\\', 'r'}); break; }
			case '\t': { new_string.add({'\\', 't'}); break; }
			case '\0': { new_string.add({'\\', '0'}); break; }
			case '\\': { new_string.add({'\\', '\\'}); break; }
			default: { new_string.add(c); break; }
		}
	}
	return new_string;
}

HeapString unescape_string(String string) {

	if (!string.count)
		return {};

	if (string.front() == '"') {
		assert(string.back() == '"');
		string.data  += 1;
		string.count -= 2;
	} else if (string.front() == '\'') {
		assert(string.back() == '\'');
		string.data  += 1;
		string.count -= 2;
	}

	if (!string.count)
		return {};

	HeapString new_string;
	new_string.reserve(string.count);

	auto p = string.data;
	utf32 c = 0;
	utf32 prev = 0;

	while (1) {
		if (p >= string.end())
			break;
		auto got_char = get_char_and_advance_utf8(&p);
		if (!got_char) {
			return {};
		}

		prev = c;
		c = got_char.value_unchecked();

		if (prev == '\\') {
			switch (c) {
				case 'n': { new_string.back() = '\n'; break; }
				case 'r': { new_string.back() = '\r'; break; }
				case 't': { new_string.back() = '\t'; break; }
				case '0': { new_string.back() = '\0'; break; }
				case '\\': { new_string.back() = '\\'; c = 0; break; }
				default: { new_string.back() = c; break; }
			}
		} else {
			new_string.add(c);
		}
	}
	return new_string;
}

Strings strings = {};
const Strings strings_en = {
	.usage =
		u8"Usage:\n"
        "    {} <path> [options]\n"
        "Option               Description\n"
        "--print-ast <when>   Print the abstract syntax tree of the program\n"
        "    when variants:\n"
		"        parse        After parsing\n"
		"        type         After typechecking\n"
        "--keep-temp          Keep temporary files (build.bat, *.asm, etc)\n"
        "--output <path>      Specify pathname of resulting executable.\n"
        "--target <toolchain> Generate the executable using specified toolchain.\n"
        "    toolchain variants:\n"
        "        fasm_x86_64_windows (default)\n"
        "        nasm_x86_64_windows\n",
	.no_source_path_received = u8"No source path received.",
	.error = u8"Error",
	.warning = u8"Warning",
	.info = u8"Info",
};
const Strings strings_ru = {
	.usage =
		u8"Использование:\n"
        "    {} <путь> [опции]\n"
        "Опция                Описание\n"
        "--print-ast          Вывести дерево программы\n"
        "--keep-temp          Сохранить временные файлы (build.bat, *.asm, etc)\n"
        "--output <path>      Путь к выходному исполняемому файлу.\n"
        "--target <toolchain> Генератор исполняемого файла.\n"
        "    варианты:\n"
        "        fasm_x86_64_windows (по умолчанию)\n"
        "        nasm_x86_64_windows\n",
	.no_source_path_received = u8"Не указан путь к исходному файлу.",
	.error = u8"Ошибка",
	.warning = u8"Предупреждение",
	.info = u8"Информация",
};
