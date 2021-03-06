#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86_64_asm.h"
#include "msvc.h"
#include <tl/bits.h>

using namespace x86_64;

static void append_instructions(Compiler &compiler, StringBuilder &builder, List<Instruction> instructions) {
	timed_function(compiler.profiler);

	append_format(builder,
		"section .text\n"
		"global main\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"cld\n"
		"call i{}\n"
		"mov rcx, [rsp]\n"
		"call ExitProcess\n"
		"ret\n",
		compiler.main_lambda->location_in_bytecode
	);

	// prepare stack routine.
	// touches every 4096th byte to mark the pages.
	// expects size in rax
	// in debug mode fills the memory with known value
	if (/*debug*/ true) {
		append(builder, R"(
_ps:
	push rcx
	push rdi
	lea rdi, [rsp-1]
	mov rcx, rax
	std
	rep stosb
	cld
	pop rdi
	pop rcx
	ret
)"
		);
	} else {
		append(builder, R"(
_ps:
	push rbx
	mov rbx, rsp
	sub rbx, rax
	lea rax, [rsp-1]
._psl:
	mov byte [rax], 0
	sub rax, 4096
	cmp rax, rbx
	jg ._psl
	pop rbx
	ret
)"
		);
	}

	// stdcall routine
	// Input:
	//     rax - lambda
	//     rbx - args size
	// Does:
	//     1. convert tlangcall arguments to stdcall arguments
	//     2. call rax
	//     3. put rax into return value
	append(builder, R"(
_stdcall:
	xor rcx, rcx
._stdcall_l0:
	cmp rcx, rbx
	je ._stdcall_l1

	lea	r8, [rsp + 8 + rcx]
	lea	r9, [rsp + rbx]
	sub r9, rcx

	mov r10, [r8]
	xchg [r9], r10
	mov [r8], r10

	add rcx, 8
	jmp ._stdcall_l0
._stdcall_l1:
	lea r9, [rsp + 8 + rbx]
	mov rcx, [r9 - 8];
	mov rdx, [r9 - 16];
	mov r8,  [r9 - 24];
	mov r9,  [r9 - 32];
	sub rsp, 32
	call rax
	add rsp, 32
	mov [rsp + 8 + rbx], rax
	ret
)"
	);

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
			n += append_format(builder, "i{}: ", idx);
		switch (i.kind) {
			using enum InstructionKind;
			case mov_re: n += append_format(builder, "mov {}, {}", i.mov_re.d, i.mov_re.s); break;
			default: n += append_instruction(builder, idx, i); break;
		}
		for (umm i = n; i < 30; ++i)
			append(builder, ' ');
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

	auto output_path_base = format("{}\\{}", compiler.current_directory, parse_path(compiler.source_path).name);
	auto asm_path = to_pathchars(format(u8"{}.asm", output_path_base));

	auto msvc_directory = locate_msvc();
	if (!msvc_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate msvc"));
		return;
	}

	auto wkits_directory = locate_wkits();
	if (!wkits_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate windows kits"));
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
					append_format(builder, "{},", *it++);
					i += 1;
				}
			}
			append(builder, '\n');
		};
		append_section(".rodata", "constants", compiler.constant_section);
		append_section(".data", "rwdata", compiler.data_section);

		append_format(builder, "section .bss\nzeros: resb {}\n", compiler.zero_section_size);

		append_instructions(compiler, builder, bytecode.instructions);

		write_entire_file(asm_path, as_bytes(to_string(builder)));
	}

	{
		scoped_phase("Assembling and linking");

		builder.clear();
		auto &bat_builder = builder;

		append_format(bat_builder, u8R"(@echo off
{}\nasm -f win64 -gcv8 "{}.asm" -o "{}.obj" -w-number-overflow -w-db-empty
)", compiler.compiler_directory, output_path_base, output_path_base);

		append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
		append_format(bat_builder,
			R"("{}link" /nologo "{}.obj" /out:"{}" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}" kernel32.lib)",
			msvc_directory,
			output_path_base,
			compiler.output_path,
			wkits_directory
		);
		for_each(bytecode.extern_libraries, [&](auto library, auto) {
			append_format(bat_builder, " {}.lib", library);
		});

		auto bat_path = u8"nasm_build.bat"s;
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(compiler.profiler, "nasm + link"s);

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
#endif
		if (!compiler.keep_temp)
			delete_file(bat_path);
		print("Build succeeded\n");
	}

	if (!compiler.keep_temp)
		delete_file(asm_path);
}

DECLARE_TARGET_INFORMATION_GETTER {
	compiler.stack_word_size = 8;
	compiler.register_size = 8;
	compiler.general_purpose_register_count = 16;
}
