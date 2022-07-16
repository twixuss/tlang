#include "common.h"
#define NOMINMAX
#include <Windows.h>
Compiler compiler;

SourceFileInfo *get_source_info(utf8 *location);


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
	block.cursor = block.base = page_allocator.allocate_uninitialized<u8>(ast_allocation_block_size);
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

Optional<HeapString> unescape_string(String string) {

	if (!string.count)
		return HeapString{};

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
		return HeapString{};

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

const Strings strings_en = {
	.usage = u8R"(Usage:
    {} <path> [options]
Option                             Description
--print-ast <when>                 Print the abstract syntax tree of the program
    parse                          After parsing
    type                           After typechecking
--keep-temp                        Keep temporary files (build.bat, *.asm, etc)
--output <path>                    Specify pathname of resulting executable
--target <toolchain>               Generate the executable using specified toolchain
    none
    fasm_x86_64_windows (default)
    nasm_x86_64_windows
--debug-path                       Print paths
--debug-poly                       Show polymorphic functions instantiations
)",
	.no_source_path_received = u8"No source path received.",
	.error = u8"Error",
	.warning = u8"Warning",

	.info = u8"Info",
};
const Strings strings_ru = {
	.usage =u8R"(Использование:
    {} <путь> [опции]
Опция                                  Описание
--print-ast <когда>                    Вывести дерево программы
    parse                              После парсинга
    type                               После проверки типов
--keep-temp                            Сохранить временные файлы (build.bat, *.asm, etc)
--output <путь>                        Путь к выходному исполняемому файлу
--target <генератор>                   Генератор исполняемого файла
    none
    fasm_x86_64_windows (по умолчанию)
    nasm_x86_64_windows
--debug-path                           Вывести пути
--debug-poly                           Показать создание полиморфных функций
)",
	.no_source_path_received = u8"Не указан путь к исходному файлу.",
	.error = u8"Ошибка",
	.warning = u8"Предупреждение",
	.info = u8"Информация",
};

void init_strings() {
	compiler.strings = strings_en;
	{
		utf16 buffer[256];
		GetUserDefaultLocaleName((wchar *)buffer, sizeof(buffer));
		if (as_span(buffer) == u"ru-RU"s) {
			compiler.strings = strings_ru;
		}
		for (auto i = &compiler.strings._start_marker + 1; i != &compiler.strings._end_marker; ++i) {
			if (*i == 0) {
				*i = *(&strings_en._start_marker + (i - &compiler.strings._start_marker));
			}
		}
	}
}
