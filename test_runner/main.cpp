#define TL_IMPL
#include <tl/main.h>
#include <tl/file.h>
#include <tl/process.h>
#include <tl/thread.h>
#include <tl/cpu.h>
#include <conio.h>

using namespace tl;

using String = Span<utf8>;

List<utf8> normalize_path(String path) {
	// F:\tlang\bin\..\tests\binop.tl
	//         ^    ^
	//         X  dotdot

	auto dotdot = find(path, u8".."s);
	if (!dotdot || dotdot == path.begin())
		return to_list(path);

	auto X = find_last(Span(path.begin(), dotdot-1), u8'\\');

	List<utf8> result;
	result.add(String{(utf8 *)path.begin(), X});
	result.add(String{dotdot + 2, (utf8 *)path.end()});
	return result;
}

struct RanProcess {
	u32 exit_code = {};
	String output = {};
	bool timed_out = {};
};

Mutex stdout_mutex;

RanProcess run_process(String command) {
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

	bool timed_out = !wait(process, 5000);

	if (timed_out) {
		terminate(process);
	}

	RanProcess result {
		.exit_code = get_exit_code(process),
		.output = as_utf8(to_string(output_builder)),
		.timed_out = timed_out,
	};

	return result;
}

s32 tl_main(Span<String> arguments) {
	auto executable_path = get_executable_path();
	auto executable_directory = parse_path(executable_path).directory;

	set_current_directory(format(u8"{}\\..\\tests", executable_directory));


	List<String> test_filenames;
	bool all = true;
	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] != u8"all"s) {
			all = false;
			test_filenames.add(arguments[i]);
		}
	}

	if (all) {
		for (auto item : get_items_in_directory(u8"."s)) {
			if (item.kind == FileItem_file && ends_with(item.name, u8".tl"s))
				test_filenames.add(item.name);
		}
	}

	u32 n_failed = 0;
	u32 n_succeeded = 0;

	ThreadPool pool;
	init_thread_pool(pool, get_cpu_info().logical_processor_count - 1);

	auto queue = make_work_queue(pool);

	for (auto test_filename : test_filenames) {
		queue += [test_filename, &n_failed, &n_succeeded] {
			bool fail = false;
			defer {
				atomic_add(&n_failed, fail);
				atomic_add(&n_succeeded, !fail);
			};

			with(stdout_mutex, print("{}\n", test_filename));

			auto do_fail = [&](auto fn) {
				fail = true;
				withs(stdout_mutex) {
					with(ConsoleColor::yellow, print("    Test {} failed:\n", test_filename));
					fn();
				};
			};

			auto test_source_buffer = read_entire_file(test_filename);
			defer { free(test_source_buffer); };

			auto test_source = (String)test_source_buffer;


			bool compiler_should_error = find((String)test_source, u8"// COMPILER ERROR"s);

			auto find_param = [&](String param_prefix) -> String {
				if (auto found = find(test_source, param_prefix)) {
					auto param_start = found + param_prefix.count;
					return {param_start, find_any(String{param_start, test_source.end()}, {u8'\r', u8'\n'})};
				}
				return {};
			};

			String expected_compiler_output = find_param(u8"// COMPILER OUTPUT "s);
			String expected_program_output = find_param(u8"// PROGRAM OUTPUT "s);
			auto expected_program_exit_code = parse_u64(find_param(u8"// PROGRAM CODE "s)).value_or(0);

			auto actual_compiler = run_process(format(u8"tlang.exe {}"s, test_filename));

			if (actual_compiler.timed_out) {
				do_fail([&] {
					with(ConsoleColor::red, print("Compiler timed out\n"));
				});
				return;
			}

			if (expected_compiler_output.count && !find(actual_compiler.output, expected_compiler_output)) {
				do_fail([&] {
					with(ConsoleColor::red, print("Compiler output mismatch:\n"));
					with(ConsoleColor::cyan, print("Expected:\n"));
					print("{}\n", expected_compiler_output);
					with(ConsoleColor::cyan, print("Actual:\n"));
					print("{}\n", actual_compiler.output);
				});

				return;
			}

			if (compiler_should_error) {
				if (actual_compiler.exit_code == 0) {
					do_fail([&]{
						with(ConsoleColor::red, println("Compiler should have returned non-zero exit code (failed)"));
					});
				}

				return;
			} else {
				if (actual_compiler.exit_code != 0) {
					do_fail([&]{
						with(ConsoleColor::red, println("Compiler should have returned zero exit code (succeeded). Output:"));
						println(actual_compiler.output);
					});

					return;
				}
			}

			auto program_path = format(u8"{}.exe"s, parse_path(test_filename).name);
			if (!file_exists(program_path)) {
				do_fail([&]{
					with(ConsoleColor::red, println("Compiler was expected to generate an executable, but it didn't"));
				});

				return;
			}

			auto actual_program = run_process(program_path);

			if (actual_program.timed_out) {
				do_fail([&] {
					with(ConsoleColor::red, print("Program timed out\n"));
				});
				return;
			}

			if (expected_program_output.count && !find(actual_program.output, expected_program_output)) {
				do_fail([&] {
					with(ConsoleColor::red, print("Program output mismatch:\n"));
					with(ConsoleColor::cyan, print("Expected:\n"));
					print("{}\n", expected_program_output);
					with(ConsoleColor::cyan, print("Actual:\n"));
					print("{}\n", actual_program.output);
				});

				return;
			}

			if (expected_program_exit_code != actual_program.exit_code) {
				do_fail([&]{
					with(ConsoleColor::red, println("Program should have returned {} exit code. But actual is {}", expected_program_exit_code, actual_program.exit_code));
				});

				return;
			}
		};
	}

	queue.wait_for_completion();

	if (n_failed) with(ConsoleColor::red,   print("{}/{} tests failed.\n", n_failed, test_filenames.count));
	else          with(ConsoleColor::green, print("All {} tests succeeded.\n", test_filenames.count));

//retry:
//	print("Remove temporary files? (y/n)\n");
//	switch (_getch()) {
//		case 'y': break;
//		case 'n': return 0;
//		default: goto retry;
//	}

	//for (auto item : get_items_in_directory(u8"."s)) {
	//	if (item.kind == FileItem_file) {
	//		if (ends_with(item.name, u8".pdb"s) ||
	//			ends_with(item.name, u8".ilk"s) ||
	//			ends_with(item.name, u8".obj"s) ||
	//			ends_with(item.name, u8".asm"s) ||
	//			ends_with(item.name, u8".exe"s) ||
	//			item.name == u8"compile_log.txt"s
	//		) {
	//			delete_file(item.name);
	//		}
	//	}
	//}

	return 0;
}

