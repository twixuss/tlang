#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86_64.h"

using namespace x86_64;

static Span<utf8> locate_msvc() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Microsoft Visual Studio\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"2022"s,
			u8"2019"s,
			u8"2017"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = to_utf8(found_version.name);
					if (name == supported_version) {
						return format(u8"{}{}", path, name);
					}
				}
			}
		}
		return {};
	};
	auto find_edition = [&]() -> Span<utf8> {
		auto found_editions = get_items_in_directory(path);
		Span<utf8> supported_editions[] = {
			u8"Community"s,
			u8"Professional"s,
		};
		for (auto supported_edition : supported_editions) {
			for (auto found_edition : found_editions) {
				if (found_edition.kind == FileItem_directory) {
					auto name = to_utf8(found_edition.name);
					if (name == supported_edition) {
						return format(u8"{}\\{}\\VC\\Tools\\MSVC\\", path, name);
					}
				}
			}
		}
		return {};
	};


	path = find_version();
	if (!path.data) {
		return {};
	}
	path = find_edition();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	path = format(u8"{}{}\\bin\\Hostx64\\x64\\", path, to_utf8(found_builds.back().name));

	return path;
}

static Span<utf8> locate_wkits() {
	Span<utf8> path = u8"C:\\Program Files (x86)\\Windows Kits\\"s;

	auto find_version = [&]() -> Span<utf8> {
		Span<utf8> supported_versions[] = {
			u8"10"s,
			u8"8.1"s,
			u8"7"s,
		};
		auto found_versions = get_items_in_directory(path);
		for (auto supported_version : supported_versions) {
			for (auto found_version : found_versions) {
				if (found_version.kind == FileItem_directory) {
					auto name = to_utf8(found_version.name);
					if (name == supported_version) {
						return format(u8"{}{}\\Lib\\", path, name);
					}
				}
			}
		}
		return {};
	};

	path = find_version();
	if (!path.data) {
		return {};
	}

	auto found_builds = get_items_in_directory(path);
	if (!found_builds.count) {
		return {};
	}
	path = format(u8"{}{}\\um\\x64", path, to_utf8(found_builds.back().name)); // NO SLASH AT THE END BECAUSE LINK.EXE IS ...

	return path;
}

static void append_instructions(CompilerContext &context, StringBuilder &builder, InstructionList instructions) {
	timed_function(context.profiler);

	append_format(builder,
		"section .text\n"
		"global main\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"call .{}\n"
		"mov rcx, [rsp]\n"
		"call ExitProcess\n"
		"ret\n",
		instruction_address(context.main_lambda->location_in_bytecode)
	);

	s64 idx = 0;
	for (auto i : instructions) {
		if (i.labeled)
			append_format(builder, ".{}: ", instruction_address(idx));
		switch (i.kind) {
			using enum InstructionKind;
			case mov_re: append_format(builder, "mov {}, {}", i.mov_re.d, Span(i.mov_re.s_data, i.mov_re.s_count)); break;
			default: append_instruction(builder, idx, i); break;
		}
#if BYTECODE_DEBUG
		append_format(builder, "; bytecode.cpp:{}\n", i.line);
		if (i.comment) {
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
	current_printer = console_printer;

	timed_function(context.profiler);

	auto output_path_base = format("{}\\{}", context.current_directory, parse_path(context.source_path).name);
	auto asm_path = to_pathchars(format(u8"{}.asm", output_path_base));

	auto msvc_directory = locate_msvc();
	if (!msvc_directory.data) {
		print(Print_error, "Couldn't locate msvc");
		return;
	}

	auto wkits_directory = locate_wkits();
	if (!wkits_directory.data) {
		print(Print_error, "Couldn't locate windows kits");
		return;
	}

	StringBuilder builder;

	{
		scoped_phase("Writing nasm");

		append(builder, "bits 64\nextern ExitProcess\n");

		for_each(bytecode.extern_libraries, [&](auto lib, auto fns) {
			for (auto f : fns) {
				append_format(builder, "extern {}\n", f);
			}
		});

		append(builder, "section .rodata\nconstants: db ");
		for (auto byte : bytecode.constant_data) {
			append_format(builder, "{},", byte);
		}
		append(builder, '\n');
		append(builder, "section .data\nrwdata: db ");
		for (auto byte : bytecode.data) {
			append_format(builder, "{},", byte);
		}
		append_format(builder, "\nsection .bss\nzeros: resb {}\n", bytecode.zero_data_size);

		append_instructions(context, builder, bytecode.instructions);

		{
			auto file = open_file(asm_path, {.write = true});
			defer { close(file); };

			write(file, as_bytes(to_string(builder)));
		}
	}

	{
		scoped_phase("Assembling and linking");

		builder.clear();
		auto &bat_builder = builder;

		append_format(bat_builder, u8R"(@echo off
{}\nasm -f win64 -gcv8 "{}.asm" -o "{}.obj" -w-number-overflow -w-db-empty
	)", context.executable_directory, output_path_base, output_path_base);

		append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
		append_format(bat_builder,
			R"("{}link" /nologo "{}.obj" /out:"{}.exe" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
			msvc_directory,
			output_path_base,
			output_path_base,
			wkits_directory
		);
		for_each(bytecode.extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = to_pathchars(concatenate(context.executable_directory, u8"\\nasm_build.bat"s));
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(context.profiler, "nasm + link"s);

		auto process = start_process(bat_path);
		if (!process.handle) {
			print(Print_error, "Cannot execute file '{}'\n", bat_path);
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
			print(Print_error, "Build command failed\n");
			return;
		}
#endif
		print("Build succeeded\n");
	}

}

DECLARE_TARGET_INFORMATION_GETTER {
	context.stack_word_size = 8;
	context.register_size = 8;
	context.general_purpose_register_count = 16;
}
