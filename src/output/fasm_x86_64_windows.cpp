#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86_64_asm.h"
#include <tl/ram.h>

using namespace x86_64;

static void append_instructions(CompilerContext &context, StringBuilder &builder, InstructionList instructions) {
	timed_function(context.profiler);

	append_format(builder,
		"section '.text' code readable executable\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"cld\n"
		"call .{}\n"
		"mov rcx, [rsp]\n"
		"call [ExitProcess]\n"
		"ret\n", instruction_address(context.main_lambda->location_in_bytecode));

	s64 idx = 0;
	for (auto i : instructions) {
		if (i.kind == InstructionKind::jmp_label)
			append_format(builder, ".{}: ", instruction_address(idx));

		// Override some of default instruction printing
		switch (i.kind) {
			using enum InstructionKind;
			case mov_re: append_format(builder, "mov {}, [{}]", i.mov_re.d, i.mov_re.s); break;
			default: append_instruction(builder, idx, i); break;
		}
#if BYTECODE_DEBUG
		append_format(builder, "; bytecode.cpp:{}\n", i.line);
		if (i.comment.data) {
			append_format(builder, "; {}\n", i.comment);
		}
#else
		append(builder, '\n');
#endif
		++idx;
	}
}

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	init_printer();

	timed_function(context.profiler);


	StringBuilder builder;

	/*

section '.idata' import data readable writeable

  library kernel,'KERNEL32.DLL',\
	  errormsg,'ERRORMSG.DLL'

  import kernel,\
	 SetLastError,'SetLastError',\
	 ExitProcess,'ExitProcess'

	*/


	{
		scoped_phase("Writing fasm");


		append(builder, "format PE64 console\nentry main\ninclude 'win64a.inc'\n");

		append_instructions(context, builder, bytecode.instructions);

		not_implemented("data sections");
#if 0
		if (bytecode.constant_data.count) {
			append(builder, "section '.rodata' data readable\nconstants db ");
			append_format(builder, "{}", bytecode.constant_data[0]);
			bytecode.constant_data.data += 1;
			bytecode.constant_data.count -= 1;
			defer {
				bytecode.constant_data.data -= 1;
				bytecode.constant_data.count += 1;
			};
			for (auto byte : bytecode.constant_data) {
				append_format(builder, ",{}", byte);
			}
			append(builder, '\n');
		}
		if (bytecode.data.count) {
			append(builder, "section '.data' data readable writeable\nrwdata db ");
			append_format(builder, "{}", bytecode.data[0]);
			bytecode.data.data += 1;
			bytecode.data.count -= 1;
			defer {
				bytecode.data.data -= 1;
				bytecode.data.count += 1;
			};
			for (auto byte : bytecode.data) {
				append_format(builder, ",{}", byte);
			}
			append(builder, '\n');
		}
#endif
		if (context.zero_section_size) {
			append_format(builder, "section '.bss' data readable writeable\nzeros rb {}\n", context.zero_section_size);
		}

		{
			append(builder, "section '.idata' import data readable writeable\n");


			// TODO: import kernel32 even when windows.tl is not included
			//if (!find_if(bytecode.extern_libraries, [](auto lib, auto) { return equals_case_insensitive(lib, u8"kernel32"s); })) {
			//	append(builder, "library kernel32,'kernel32.dll'\nimport kernel32,ExitProcess,'ExitProcess'\n");
			//}

			if (is_empty(bytecode.extern_libraries)) {
				append(builder, "library kernel32,'kernel32.dll'\nimport kernel32,ExitProcess,'ExitProcess'\n");
			} else {
				u32 library_index = 0;
				append(builder, "library ");
				for_each(bytecode.extern_libraries, [&](auto library, auto functions) {
					if (library_index != 0)
						append(builder, ",\\\n\t");
					append_format(builder, "{},'{}.dll'", library, library);
					library_index += 1;
				});

				append(builder, '\n');

				for_each(bytecode.extern_libraries, [&](auto library, auto functions) {
					append_format(builder, "import {}", library);
					for (auto function : functions) {
						append_format(builder, ",\\\n\t{},'{}'", function, function);
					}
					if (library == u8"kernel32"s) {
						if (!find(functions, u8"ExitProcess"s)) {
							append(builder, ",\\\n\tExitProcess,'ExitProcess'");
						}
					}

					append(builder, '\n');
				});
			}

		}
	}

	auto output_path_base = format("{}\\{}", context.current_directory, parse_path(context.source_path).name);

	auto asm_path = format(u8"{}.asm", output_path_base);

	write_entire_file(asm_path, as_bytes(to_string(builder)));
	defer { if (!context.keep_temp) delete_file(asm_path); };


	builder.clear();
	auto &bat_builder = builder;

	// For some reason fasm refuses to allocate more than 1371000 KiB
	// Also it divides it by 2 at random times??? wtf
	u32 fasm_max_kilobytes = 1024 * 1024; // min(1371000, get_ram_size() / 2 / 1024);

	append_format(bat_builder,
		u8"@echo off\r\n"
		"{}\\fasm\\fasm.exe -m {} \"{}\" \"{}\"\r\n",
		context.compiler_directory, fasm_max_kilobytes, asm_path, context.output_path);

	auto bat_path = to_pathchars(concatenate(context.compiler_directory, u8"\\fasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
	defer { if (!context.keep_temp) delete_file(bat_path); };

	_wputenv((wchar *)to_utf16(tformat(u8"Include={}\\fasm\\include{}", context.compiler_directory, '\0'), true).data);

	timed_block(context.profiler, "fasm"s);

	auto process = start_process(bat_path);
	if (!process.handle) {
		with(ConsoleColor::red, print("Cannot execute file '{}'\n", bat_path));
		return;
	}

	defer { free(process); };

	builder.clear();
	auto &compile_log = builder;

	while (1) {
		u8 buf[256];
		auto bytes_read = process.standard_out->read(array_as_span(buf));

		if (bytes_read == 0)
			break;

		auto string = Span((utf8 *)buf, bytes_read);
		print(string);
		append(compile_log, string);
	}

	write_entire_file("compile_log.txt"s, as_bytes(to_string(compile_log)));

	wait(process);
	auto exit_code = get_exit_code(process);
	if (exit_code != 0) {
		with(ConsoleColor::red, print("Build command failed\n"));
		return;
	}

	print("Build succeeded\n");
}

DECLARE_TARGET_INFORMATION_GETTER {
	context.stack_word_size = 8;
	context.register_size = 8;
	context.general_purpose_register_count = 16;
}
