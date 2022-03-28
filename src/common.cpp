#include "common.h"
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

List<utf8> where(utf8 *location) {
	if (location) {
		return format(u8"{}:{}:{}", get_source_info(location).path, get_line_number(location), get_column_number(location));
	} else {
		return {};
	}
}

void print_report(Report r) {
	print("{}: ", r.where);
	switch (r.kind) {
		case ReportKind::info:    print(Print_info,    u8"Info"s);  break;
		case ReportKind::warning: print(Print_warning, u8"Warning"s); break;
		case ReportKind::error:	  print(Print_error,   u8"Error"s);	  break;
		default: invalid_code_path();
	}
	print(": {}.\n", r.message);
	print_source_line(r.kind, r.location);
}
