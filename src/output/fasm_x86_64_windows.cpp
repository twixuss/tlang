#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86_64_asm.h"
#include <tl/ram.h>

using namespace x86_64;

namespace tl {

inline umm append(StringBuilder &builder, ::Address a) {
	using namespace x86_64;
	using enum Register;
	umm result = 0;
	result += append(builder, '[');
	switch (a.base) {
		case locals:
			a.base = rb;
			a.c += locals_offset;
			break;
		case temporary:
			a.base = rb;
			a.c += temporary_offset;
			break;
		case parameters:
			a.base = rb;
			a.c = parameters_size - a.c + 8;
			break;
		case return_parameters:
			a.base = rb;
			a.c += parameters_size + 16;
			break;
	}
	switch (a.base) {
		case constants   : result += append(builder, "constants"); break;
		case rwdata      : result += append(builder, "rwdata"); break;
		case zeros       : result += append(builder, "zeros"); break;
		case instructions: return append_format(builder, "i{}]", a.c);
		default: result += append(builder, to_x86_register(a.base)); break;
	}
	if (a.r1_scale_index) {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			result += append(builder, '+');
			result += append(builder, to_x86_register(a.r1));
			result += append(builder, '*');
			result += append(builder, lea_scales[a.r1_scale_index]);
			if (a.c) {
				result += append(builder, '+');
				result += append(builder, a.c);
			}
		}
	} else {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			if (a.c) {
				result += append(builder, '+');
				result += append(builder, a.c);
			}
		}
	}
	result += append(builder, ']');
	return result;
}

}

static void append_instructions(Compiler &compiler, StringBuilder &builder, List<Instruction> instructions) {
	timed_function(compiler.profiler);

	append_format(builder,
		"section '.text' code readable executable\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"cld\n"
		"call i{}\n"
		"mov rcx, [rsp]\n"
		"call [ExitProcess]\n"
		"ret\n", compiler.main_lambda->location_in_bytecode);

	s64 idx = 0;
	for (auto i : instructions) {
#if BYTECODE_DEBUG
		if (i.comment.data) {
			split(i.comment, u8'\n', [&](auto part) {
				append_format(builder, "; {}\n", part);
			});
		}
#endif
		if (i.kind == InstructionKind::jmp_label)
			append_format(builder, "i{}: ", idx);

		// Override some of default instruction printing
		switch (i.kind) {
			using enum InstructionKind;
			case mov_re: append_format(builder, "mov {}, [{}]", i.mov_re.d, i.mov_re.s); break;
			default: append_instruction(builder, idx, i); break;
		}
#if BYTECODE_DEBUG
		append_format(builder, "; #{} @ bytecode.cpp:{}\n", idx, i.line);
#else
		append_format(builder, "; #{}\n", idx);
#endif
		++idx;
	}
}

DECLARE_OUTPUT_BUILDER {
	init_allocator();
	init_printer();

	timed_function(compiler.profiler);


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

		append_instructions(compiler, builder, bytecode.instructions);

		auto append_section = [&](auto name, auto label, auto &section) {
			auto it = section.buffer.begin();
			umm i = 0;
			bool last_is_byte = true;
			append_format(builder, "section {}\n{} db ", name, label);
			while (it != section.buffer.end()) {
				auto relocation = binary_search(section.relocations, i);
				if (relocation) {
					u64 offset = 0;
					for (umm j = 0; j < 8; ++j)
						offset = (offset >> 8) | ((u64)*it++ << 56);

					if (last_is_byte) {
						append(builder, "\ndq ");
						last_is_byte = false;
					}
					append_format(builder, "{}+{},", label, offset);

					i += 8;
				} else {
					if (!last_is_byte) {
						append(builder, "\ndb ");
						last_is_byte = true;
					}
					if (it + 1 == section.buffer.end())
						append_format(builder, "{}", *it++);
					else
						append_format(builder, "{},", *it++);
					i += 1;
				}
			}
			append(builder, '\n');
		};
		append_section("'.rodata' data readable",         "constants", compiler.constant_section);
		append_section("'.data' data readable writeable", "rwdata", compiler.data_section);

		if (compiler.zero_section_size) {
			append_format(builder, "section '.bss' data readable writeable\nzeros rb {}\n", compiler.zero_section_size);
		}

		{
			append(builder, "section '.idata' import data readable writeable\n");


			// TODO: import kernel32 even when windows.tl is not included
			//if (!find_if(compiler.extern_libraries, [](auto lib, auto) { return equals_case_insensitive(lib, u8"kernel32"s); })) {
			//	append(builder, "library kernel32,'kernel32.dll'\nimport kernel32,ExitProcess,'ExitProcess'\n");
			//}

			if (is_empty(compiler.extern_libraries)) {
				append(builder, "library kernel32,'kernel32.dll'\nimport kernel32,ExitProcess,'ExitProcess'\n");
			} else {
				u32 library_index = 0;
				append(builder, "library ");
				for_each(compiler.extern_libraries, [&](auto library, auto functions) {
					if (library_index != 0)
						append(builder, ",\\\n\t");
					append_format(builder, "{},'{}.dll'", library, library);
					library_index += 1;
				});

				append(builder, '\n');

				for_each(compiler.extern_libraries, [&](auto library, auto functions) {
					append_format(builder, "import {}", library);
					for (auto function : functions) {
						append_format(builder, ",\\\n\t{},'{}'", function, function);
					}
					if (library == u8"kernel32"s) {
						if (!find(functions, "ExitProcess"str)) {
							append(builder, ",\\\n\tExitProcess,'ExitProcess'");
						}
					}

					append(builder, '\n');
				});
			}

		}
	}

	auto output_path_base = format("{}\\{}", compiler.current_directory, parse_path(compiler.source_path).name);

	auto asm_path = format(u8"{}.asm", output_path_base);

	write_entire_file(asm_path, as_bytes(to_string(builder)));
	defer { if (!compiler.keep_temp) delete_file(asm_path); };


	builder.clear();
	auto &bat_builder = builder;

	// For some reason fasm refuses to allocate more than 1371000 KiB
	// Also it divides it by 2 at random times??? wtf
	u32 fasm_max_kilobytes = 1024 * 1024; // min(1371000, get_ram_size() / 2 / 1024);

	append_format(bat_builder,
		u8"@echo off\r\n"
		"{}\\fasm\\fasm.exe -m {} \"{}\" \"{}\"\r\n",
		compiler.compiler_directory, fasm_max_kilobytes, asm_path, compiler.output_path);

	auto bat_path = to_pathchars(concatenate(compiler.compiler_directory, u8"\\fasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
	defer { if (!compiler.keep_temp) delete_file(bat_path); };

	_wputenv((wchar *)to_utf16(tformat(u8"Include={}\\fasm\\include{}", compiler.compiler_directory, '\0'), true).data);

	timed_block(compiler.profiler, "fasm"s);

	auto process = start_process(bat_path);
	if (!process.handle) {
		with(ConsoleColor::red, print("Cannot execute file '{}'\n", bat_path));
		return false;
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
		return false;
	}
	return true;
}

DECLARE_TARGET_INFORMATION_GETTER {
	compiler.stack_word_size = 8;
	compiler.register_size = 8;
	compiler.general_purpose_register_count = 16;
}
