#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86_64_asm.h"
#include "msvc.h"
#include <tl/bits.h>

using namespace x86_64;

static void append_instructions(CompilerContext &context, StringBuilder &builder, InstructionList instructions) {
	timed_function(context.profiler);

	append_format(builder,
		"section .text\n"
		"global main\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"cld\n"
		"call .{}\n"
		"mov rcx, [rsp]\n"
		"call ExitProcess\n"
		"ret\n",
		instruction_address(context.main_lambda->location_in_bytecode)
	);

	// prepare stack routine.
	// touches every 4096th byte to mark the pages.
	// expects size in rax
	// in debug mode fills the memory with known value
	if (/*debug*/ true) {
		append(builder,
			"._ps:"
			"push rcx\n"
			"push rdi\n"
			"lea rdi, [rsp-1]\n"
			"mov rcx, rax\n"
			"std\n"
			"rep stosb\n"
			"cld\n"
			"pop rdi\n"
			"pop rcx\n"
			"ret\n"
		);
	} else {
		append(builder,
			"._ps:"
			"push rbx\n"
			"mov rbx, rsp\n"
			"sub rbx, rax\n"
			"lea rax, [rsp-1]\n"
			"._psl:\n"
			"mov byte [rax], 0\n"
			"sub rax, 4096\n"
			"cmp rax, rbx\n"
			"jg ._psl\n"
			"pop rbx\n"
			"ret\n"
		);
	}

	s64 idx = 0;
	for (auto i : instructions) {
#if BYTECODE_DEBUG
		if (i.comment.data) {
			split(i.comment, u8'\n', [&](auto part) {
				append_format(builder, "; {}\n", part);
			});
		}
#endif
		umm n = 0;
		if (i.kind == InstructionKind::jmp_label)
			n += append_format(builder, ".{}: ", instruction_address(idx));
		switch (i.kind) {
			using enum InstructionKind;
			case mov_re: n += append_format(builder, "mov {}, {}", i.mov_re.d, i.mov_re.s); break;
			case prepare_stack:
				n += append_format(builder, "mov rax, {}\ncall ._ps", i.prepare_stack.byte_count);

				// HACK: bytecode does not change instructions that point to code after inserting `unguard_stack` instruction.
				// This accounts for adding `jmp_label` and `unguard_stack` instructions.
				// idx -= 2;

				break;
			default: n += append_instruction(builder, idx, i); break;
		}
		for (umm i = n; i < 30; ++i)
			append(builder, ' ');
		append_format(builder, "; #{} @ bytecode.cpp:{}\n", idx, i.line);
		++idx;
	}
}

template <class T>
T *find_binary(Span<T> span, T value) {
	auto begin = span.begin();
	auto end   = span.end();
	while (1) {
		if (begin == end)
			return 0;

		auto mid = begin + (end - begin) / 2;
		if (value == *mid)
			return mid;

		if (value < *mid) {
			end = mid;
		} else {
			begin = mid + 1;
		}
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

		auto append_section = [&](auto name, auto label, auto &section) {
			auto it = section.buffer.begin();
			umm i = 0;
			bool last_is_byte = true;
			append_format(builder, "section {}\n{}:db ", name, label);
			while (it != section.buffer.end()) {
				auto relocation = find_binary(section.relocations, i);
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
					append_format(builder, "{},", *it++);
					i += 1;
				}
			}
			append(builder, '\n');
		};
		append_section(".rodata", "constants", context.constant_section);
		append_section(".data", "rwdata", context.data_section);

		append_format(builder, "section .bss\nzeros: resb {}\n", context.zero_section_size);

		append_instructions(context, builder, bytecode.instructions);

		write_entire_file(asm_path, as_bytes(to_string(builder)));
	}

	{
		scoped_phase("Assembling and linking");

		builder.clear();
		auto &bat_builder = builder;

		append_format(bat_builder, u8R"(@echo off
{}\nasm -f win64 -gcv8 "{}.asm" -o "{}.obj" -w-number-overflow -w-db-empty
)", context.compiler_directory, output_path_base, output_path_base);

		append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
		append_format(bat_builder,
			R"("{}link" /nologo "{}.obj" /out:"{}" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
			msvc_directory,
			output_path_base,
			context.output_path,
			wkits_directory
		);
		for_each(bytecode.extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = u8"nasm_build.bat"s;
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
		if (!context.keep_temp)
			delete_file(bat_path);
		print("Build succeeded\n");
	}

	if (!context.keep_temp)
		delete_file(asm_path);
}

DECLARE_TARGET_INFORMATION_GETTER {
	context.stack_word_size = 8;
	context.register_size = 8;
	context.general_purpose_register_count = 16;
}
