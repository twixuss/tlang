#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>
#include <conio.h>

using namespace tl;

List<utf8> normalize_path(Span<utf8> path) {
	// F:\tlang\bin\..\tests\binop.tl
	//         ^    ^
	//         X  dotdot

	auto dotdot = find(path, u8".."s);
	if (!dotdot || dotdot == path.begin())
		return to_list(path);

	auto X = find_last(Span(path.begin(), dotdot-1), u8'\\');

	List<utf8> result;
	result.add(Span<utf8>{(utf8 *)path.begin(), X});
	result.add(Span<utf8>{dotdot + 2, (utf8 *)path.end()});
	return result;
}

void update_output(Span<utf8> path) {
	auto command = format(u8"..\\bin\\tlang.exe {} --target nasm_x86_64_windows"s, path);
	print("{}\n", command);
	auto process = start_process(to_utf16(command));
	assert(is_valid(process));

	u8 buf[256];

	StringBuilder builder;
	while (1) {
		auto bytes_read = process.standard_out->read(array_as_span(buf));
		if (!bytes_read)
			break;
		append(builder, Span((utf8 *)buf, bytes_read));
	}

	wait(process);
	auto file = open_file(format("{}.compiler_output", parse_path(path).name), {.write = true});
	defer { close(file); };

	bool has_executable = file_exists(format("{}.exe", parse_path(path).name));

	debug_break();

	write(file, value_as_bytes(get_exit_code(process)));
	write(file, value_as_bytes(has_executable));
	write(file, to_string(builder));

	if (has_executable) {
		command = format(u8"{}.exe"s, parse_path(path).name);
		print("{}\n", command);
		process = start_process(to_utf16(command));
		assert(is_valid(process));

		u8 buf[256];

		builder.clear();
		while (1) {
			auto bytes_read = process.standard_out->read(array_as_span(buf));
			if (!bytes_read)
				break;
			append(builder, Span((utf8 *)buf, bytes_read));
		}

		wait(process);
		auto file = open_file(format("{}.program_output", parse_path(path).name), {.write = true});
		defer { close(file); };

		write(file, value_as_bytes(get_exit_code(process)));
		write(file, to_string(builder));
	}
}

s32 tl_main(Span<Span<utf8>> arguments) {
	Span<utf8> update_name;
	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] == u8"update"s) {
			++i;
			if (i >= arguments.count) {
				print("Expected an argument after '{}'\n", arguments[i-1]);
				return 1;
			}
			update_name = arguments[i];
		}
	}

	auto executable_path = get_executable_path();
	auto executable_directory = parse_path(executable_path).directory;

	set_current_directory(format(u8"{}\\..\\tests", executable_directory));

	if (update_name.count) {
		if (update_name == u8"all"s) {
			print("Are you sure?\n");
			system("pause");
			for (auto item : get_items_in_directory(u8"..\\tests\\"s)) {
				if (item.kind == FileItem_file && ends_with(item.name, u8".tl"s)) {
					update_output(item.name);
				}
			}
		} else {
			update_output(update_name);
		}
	} else {
		List<Span<utf8>> failed_tests;
		for (auto item : get_items_in_directory(u8"..\\tests\\"s)) {
			if (item.kind == FileItem_file && ends_with(item.name, u8".tl"s)) {
				auto command = format(u8"tlang.exe {} --output {}.exe --target nasm_x86_64_windows"s, item.name, parse_path(item.name).name);
				print("{}\n", command);
				auto process = start_process(to_utf16(command));
				assert(is_valid(process));

				u8 buf[256];

				StringBuilder output_builder;
				while (1) {
					auto bytes_read = process.standard_out->read(array_as_span(buf));
					if (!bytes_read)
						break;
					append(output_builder, Span((utf8 *)buf, bytes_read));
				}

				wait(process);
				auto actual_exit_code = get_exit_code(process);

				bool has_executable = false;

				auto compiler_output = read_entire_file(format("{}.compiler_output", parse_path(item.name).name));
				if (compiler_output.count) {
					bool failed = false;
					auto expected_exit_code = *(u32 *)compiler_output.data;
					has_executable = *(bool *)(compiler_output.data + 4);

					StringBuilder message_builder;
					if (actual_exit_code != expected_exit_code) {
						append_format(message_builder, "Expected exit code: {}. Got: {}\n", expected_exit_code, actual_exit_code);
						failed = true;
					}

					auto expected_output = as_utf8(compiler_output.subspan(5, compiler_output.count - 5));
					auto actual_output = as_utf8(to_string(output_builder));
					if (actual_output != expected_output) {
						// append_format(message_builder, "Expected output:\nvvvvvvvvvv\n{}\n^^^^^^^^^^\nActual output:\nvvvvvvvvvv\n{}\n^^^^^^^^^^\n", expected_output, actual_output);
						auto a = expected_output.begin();
						auto b = actual_output.begin();
						umm difference_start = -1;
						while (1) {
							if (a == expected_output.end() || b == actual_output.end())
								break;
							if (*a != *b) {
								difference_start = a - expected_output.begin();
								break;
							}
							++a;
							++b;
						}

						if (difference_start != -1) {
							auto expected_diff_start = expected_output.begin() + difference_start;
							auto found_expected_end = find(Span(expected_diff_start, expected_output.end()), u8'\n');
							auto actual_diff_start = actual_output.begin() + difference_start;
							auto found_actual_end = find(Span(actual_diff_start, actual_output.end()), u8'\n');
							append_format(message_builder, "First difference here:\nExpected:\n{}\nActual:\n{}\n", Span(expected_diff_start, found_expected_end ? found_expected_end : expected_output.end()), Span(actual_diff_start, found_actual_end ? found_actual_end : actual_output.end()));
						}

						failed = true;
					}
					if (failed) {
						failed_tests.add(format(u8"{}\n{}\n", item.name, to_string(message_builder)));
					}
				} else {
					if (actual_exit_code != 0) {
						failed_tests.add(format(u8"========== {}.tl ==========\n{}\n", item.name, to_string(output_builder)));
					}
				}

				if (has_executable) {
					auto command = format(u8"{}.exe"s, parse_path(item.name).name);
					print("{}\n", command);
					process = start_process(to_utf16(command));
					assert(is_valid(process));

					output_builder.clear();
					while (1) {
						auto bytes_read = process.standard_out->read(array_as_span(buf));
						if (!bytes_read)
							break;
						append(output_builder, Span((utf8 *)buf, bytes_read));
					}

					wait(process);
					auto actual_exit_code = get_exit_code(process);

					auto program_output = read_entire_file(format("{}.program_output", parse_path(item.name).name));
					if (program_output.count) {
						bool failed = false;
						auto expected_exit_code = *(u32 *)program_output.data;

						StringBuilder message_builder;
						if (actual_exit_code != expected_exit_code) {
							append_format(message_builder, "Expected exit code: {}. Got: {}\n", expected_exit_code, actual_exit_code);
							failed = true;
						}

						auto expected_output = as_utf8(program_output.subspan(4, program_output.count - 4));
						auto actual_output = as_utf8(to_string(output_builder));
						if (actual_output != expected_output) {
							// append_format(message_builder, "Expected output:\nvvvvvvvvvv\n{}\n^^^^^^^^^^\nActual output:\nvvvvvvvvvv\n{}\n^^^^^^^^^^\n", expected_output, actual_output);
							auto a = expected_output.begin();
							auto b = actual_output.begin();
							umm difference_start = -1;
							while (1) {
								if (a == expected_output.end() || b == actual_output.end())
									break;
								if (*a != *b) {
									difference_start = a - expected_output.begin();
									break;
								}
								++a;
								++b;
							}

							if (difference_start != -1) {
								auto expected_diff_start = expected_output.begin() + difference_start;
								auto found_expected_end = find(Span(expected_diff_start, expected_output.end()), u8'\n');
								auto actual_diff_start = actual_output.begin() + difference_start;
								auto found_actual_end = find(Span(actual_diff_start, actual_output.end()), u8'\n');
								append_format(message_builder, "First difference here:\nExpected:\n{}\nActual:\n{}\n", Span(expected_diff_start, found_expected_end ? found_expected_end : expected_output.end()), Span(actual_diff_start, found_actual_end ? found_actual_end : actual_output.end()));
							}

							failed = true;
						}
						if (failed) {
							failed_tests.add(format(u8"{}\n{}\n", item.name, to_string(message_builder)));
						}
					} else {
						if (actual_exit_code != 0) {
							failed_tests.add(format(u8"========== {}.exe ==========\n{}\n", item.name, to_string(output_builder)));
						}
					}
				}
			}
		}

		if (failed_tests.count) {
			print("Tests failed: {}\n", failed_tests.count);
			for (auto t : failed_tests) {
				print(t);
			}
		}
	}

//retry:
//	print("Remove temporary files? (y/n)\n");
//	switch (_getch()) {
//		case 'y': break;
//		case 'n': return 0;
//		default: goto retry;
//	}

	StringBuilder command_builder;
	append_format(command_builder, "del *.pdb && del *.exe && del *.ilk && del *.obj && del *.asm"s);
	auto command = as_utf8(to_string(command_builder));
	print("{}\n", command);
	_wsystem((wchar *)to_utf16(command, true).data);

	return 0;
}

