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

struct RanProcess {
	u32 exit_code = {};
	Span<utf8> output = {};
};

RanProcess run_process(Span<utf8> command) {
	print("{} ", command);

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

	RanProcess result {
		.exit_code = get_exit_code(process),
		.output = as_utf8(to_string(output_builder)),
	};

	with(ConsoleColor::cyan, print("{}\n", result.exit_code));

	return result;
}

s32 tl_main(Span<Span<utf8>> arguments) {
	auto executable_path = get_executable_path();
	auto executable_directory = parse_path(executable_path).directory;

	set_current_directory(format(u8"{}\\..\\tests", executable_directory));


	List<Span<utf8>> tests;
	bool update = false;
	bool all = true;
	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] == u8"update"s) {
			update = true;
			continue;
		}
		if (arguments[i] != u8"all"s) {
			all = false;
			tests.add(arguments[i]);
		}
	}

	if (all) {
		for (auto item : get_items_in_directory(u8"."s)) {
			if (item.kind == FileItem_file && ends_with(item.name, u8".tl"s))
				tests.add(item.name);
		}
	}

	if (update && all) {
		print("Really update all tests? (y to proceed)");
		switch (_getch()) {
			case 'y': break;
			default: return 1;
		}
	}

	u32 n_failed = 0;
	u32 n_succeeded = 0;

	for (auto test : tests) {
		struct ExpectedOutput {
			RanProcess compiler;
			Optional<RanProcess> test;
		};

		auto separator = u8"\0\0\0\0"s;

		auto expected_output_path = format("expected/{}", test);

		ExpectedOutput expected = {};
		if (auto expected_output_data = read_entire_file(expected_output_path); expected_output_data.data) {
			auto remaining = (Span<utf8>)expected_output_data;

			expected.compiler.exit_code = *(u32 *)remaining.data;
			remaining.set_begin(remaining.data + sizeof u32);

			auto found_separator = find(remaining, separator);
			if (found_separator) {
				expected.compiler.output = {
					remaining.data,
					found_separator
				};

				remaining.set_begin(expected.compiler.output.end() + separator.count);

				assert(remaining.count);

				RanProcess test;
				test.exit_code = *(u32 *)remaining.data;
				remaining.set_begin(remaining.data + sizeof u32);

				test.output = remaining;
				expected.test = test;
			} else {
				expected.compiler.output = remaining;
			}
		}

		auto actual_compiler = run_process(format(u8"tlang.exe {}"s, test));
		auto test_exe_path = format(u8"{}.exe"s, parse_path(test).name);
		Optional<RanProcess> actual_test;
		if (file_exists(test_exe_path))
			actual_test = run_process(test_exe_path);

		if (update) {
			StringBuilder builder;
			append_bytes(builder, actual_compiler.exit_code);
			append(builder, actual_compiler.output);
			if (actual_test) {
				append(builder, separator);
				append_bytes(builder, actual_test.value().exit_code);
				append(builder, actual_test.value().output);
			}
			write_entire_file(expected_output_path, as_bytes(to_string(builder)));
		} else {
			bool fail = false;

			auto check = [&] (RanProcess actual, RanProcess expected) {
				if (actual.exit_code != expected.exit_code) {
					fail = true;
					with(ConsoleColor::red, print("Exit code mismatch: "));
					print("expected {}, got {}\n", expected.exit_code, actual.exit_code);
				}
				if (actual.output != expected.output) {
					fail = true;
					with(ConsoleColor::red, print("Output mismatch:\n"));
					with(ConsoleColor::cyan, print("Expected:\n"));
					print("{}\n", expected.output);
					with(ConsoleColor::cyan, print("Actual:\n"));
					print("{}\n", actual.output);
				}
			};

			check(actual_compiler, expected.compiler);

			if (!fail) {
				if (actual_test) {
					if (expected.test) {
						check(actual_test.value(), expected.test.value());
					} else {
						fail = true;
						with(ConsoleColor::red, print("Test executable was generated, but no expected output was found.\n"));
					}
				} else {
					if (expected.test) {
						fail = true;
						with(ConsoleColor::red, print("Compiler was expected to generate an executable, but it was not found.\n"));
					}
				}
			}

			n_failed += fail;
			n_succeeded += !fail;
		}
	}

	if (update) {
		with(ConsoleColor::cyan, print("{} tests updated.\n", tests.count));
	} else {
		if (n_failed) with(ConsoleColor::red,   print("{}/{} tests failed.\n", n_failed, tests.count));
		else          with(ConsoleColor::green, print("All tests succeeded.\n"));
	}

//retry:
//	print("Remove temporary files? (y/n)\n");
//	switch (_getch()) {
//		case 'y': break;
//		case 'n': return 0;
//		default: goto retry;
//	}

	for (auto item : get_items_in_directory(u8"."s)) {
		if (item.kind == FileItem_file) {
			if (ends_with(item.name, u8".pdb"s) ||
				ends_with(item.name, u8".ilk"s) ||
				ends_with(item.name, u8".obj"s) ||
				ends_with(item.name, u8".asm"s) ||
				ends_with(item.name, u8".exe"s) ||
				item.name == u8"compile_log.txt"s
			) {
				delete_file(item.name);
			}
		}
	}

	return 0;
}

