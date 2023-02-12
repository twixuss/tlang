#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <compiler.h>
#include "../x86_64_asm.h"
#include "msvc.h"
#include <tl/bits.h>

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
			switch (calling_convention) {
				case CallingConvention::tlang: a.c += 16; break;
				case CallingConvention::stdcall: a.c = parameters_size - a.c + 8; break;
				default:invalid_code_path();
			};
			break;
		case return_parameters:
			a.base = rb;
			a.c += parameters_size + 16;
			break;
	}
	switch (a.base) {
		case constants   : result += append(builder, "rel constants"); break;
		case rwdata      : result += append(builder, "rel rwdata"); break;
		case zeros       : result += append(builder, "rel zeros"); break;
		case instructions: return append_format(builder, "rel i{}]", a.c);
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

static void append_instructions(Compiler *compiler, StringBuilder &builder, List<Instruction> instructions) {
	timed_function(compiler->profiler);

	append_format(builder,
		"section .text\n"
		"global main\n"
		"main:\n"
		"and rsp, -16\n"
		"push 0\n"
		"push 0\n"
		"cld\n"
		"call i{}\n"
		"call i{}\n"
		"mov rcx, [rsp]\n"
		"call ExitProcess\n"
		"ret\n",
		compiler->init_runtime_lambda->location_in_bytecode,
		compiler->main_lambda->location_in_bytecode
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
	xor rax, rax
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
	// NOTE: to keep stack aligned to 16 bytes there are two versions, for even and odd number of arguments.
	append(builder, R"(
_stdcall_even:
	push rcx
	push rdx
	push r8
	push r9
	push rsi
	sub rsp, 32
	mov rsi, rbx
	xor rcx, rcx
._stdcall_l0:
	cmp rcx, rsi
	je ._stdcall_l1

	push qword [rsp + 80 + rcx*2]

	add rcx, 8
	jmp ._stdcall_l0
._stdcall_l1:

	mov rcx, rsp
	and rcx, 15
	test rcx, rcx
	jz .ok
	int3
.ok:

	mov rcx, [rsp + 0]
	mov rdx, [rsp + 8]
	mov r8,  [rsp + 16]
	mov r9,  [rsp + 24]
	movq xmm0, rcx
	movq xmm1, rdx
	movq xmm2, r8
	movq xmm3, r9
	call rax
	add rsp, rsi
	mov [rsp + 80 + rsi], rax
	add rsp, 32
	pop rsi
	pop r9
	pop r8
	pop rdx
	pop rcx
	ret
)"
	);
	append(builder, R"(
_stdcall_odd:
	push rcx
	push rdx
	push r8
	push r9
	push rsi
	push rsi
	sub rsp, 32
	mov rsi, rbx
	xor rcx, rcx
._stdcall_l0:
	cmp rcx, rsi
	je ._stdcall_l1

	push qword [rsp + 88 + rcx*2]

	add rcx, 8
	jmp ._stdcall_l0
._stdcall_l1:

	mov rcx, rsp
	and rcx, 15
	test rcx, rcx
	jz .ok
	int3
.ok:

	mov rcx, [rsp + 0]
	mov rdx, [rsp + 8]
	mov r8,  [rsp + 16]
	mov r9,  [rsp + 24]
	movq xmm0, rcx
	movq xmm1, rdx
	movq xmm2, r8
	movq xmm3, r9
	call rax
	add rsp, rsi
	mov [rsp + 88 + rsi], rax
	add rsp, 32
	pop rsi
	pop rsi
	pop r9
	pop r8
	pop rdx
	pop rcx
	ret
)"
	);
	append(builder, R"(
global memcpy
memcpy:
	push rsi
	push rdi
	mov rdi, rcx
	mov rsi, rdx
	mov rcx, r8
	rep movsb
	pop rdi
	pop rsi
	ret
global memset
memset:
	push rsi
	push rdi
	mov rdi, rcx
	mov rax, rdx
	mov rcx, r8
	rep stosb
	pop rdi
	pop rsi
	ret
)"
	);

	// Debug error message
	// Inputs:
	//     rdx - pointer to message string
	//
	append(builder, R"(
_debug_error:
	push rcx
	push r8
	push r9

	xor rcx, rcx
	xor r8, r8
	xor r9, r9
	call MessageBoxA

	pop r9
	pop r8
	pop rcx
	ret
)"
	);

	for (auto &l : compiler->lambdas_with_body) {
		if (l->location_in_bytecode == -1) {
			// Skipped due to DCE
			continue;
		}
		append_format(builder, "global i{}\n", l->location_in_bytecode);
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

	timed_function(compiler->profiler);

	x86_64::init();

	auto output_path_base = format("{}\\{}", compiler->current_directory, parse_path(compiler->source_path).name);
	auto asm_path = to_pathchars(format(u8"{}.asm", output_path_base));

	auto msvc_directory = locate_msvc();
	if (!msvc_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate msvc"));
		return false;
	}

	auto wkits_directory = locate_wkits();
	if (!wkits_directory.data) {
		with(ConsoleColor::red, print("Couldn't locate windows kits"));
		return false;
	}

	StringBuilder builder;

	{
		scoped_phase("Writing nasm");

		append(builder, "bits 64\nextern ExitProcess\nextern MessageBoxA\n");

		for_each(compiler->extern_libraries, [&](auto &kv) {
			auto &[lib, fns] = kv;
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
				auto relocation = binary_search(section.relocations, i, [](Relocation r) { return r.offset; });
				if (relocation) {
					u64 offset = 0;
					for (umm j = 0; j < 8; ++j)
						offset = (offset >> 8) | ((u64)*it++ << 56);

					if (last_is_byte) {
						append(builder, "\ndq ");
						last_is_byte = false;
					}

					switch (relocation->section) {
						case SectionKind::data_readonly: append_format(builder, "constants+{}"s, offset); break;
						case SectionKind::data_readwrite: append_format(builder, "rwdata+{}"s, offset); break;
						case SectionKind::data_zero: append_format(builder, "zeros+{}"s, offset); break;
						case SectionKind::code: append_format(builder, "i{}"s, relocation->lambda->location_in_bytecode + offset); break;
					}

					i += 8;
				} else {
					if (!last_is_byte) {
						append(builder, "\ndb ");
						last_is_byte = true;
					}
					append(builder, *it++);
					i += 1;
				}
				append(builder, ",");
			}
			append(builder, '\n');
		};

		append_section(".rodata", "constants", compiler->constant_section);

		append_section(".data", "rwdata", compiler->data_section);

		append_format(builder, "section .bss\nzeros: resb {}\n", compiler->zero_section_size);

		append_instructions(compiler, builder, bytecode.instructions);

		append(builder, "section .rodata\n_debug_messages: db ");
		debug_message_builder.for_each_block([&](StringBuilder::Block *block) {
			for (auto c : *block) {
				append_format(builder, "0x{},", FormatInt{.value = c, .radix = 16});
			}
		});
		append(builder, "\n");

		write_entire_file(asm_path, as_bytes(to_string(builder)));
	}

	{
		scoped_phase("Assembling and linking");

		builder.clear();
		auto &bat_builder = builder;

		append_format(bat_builder, u8R"(@echo off
{}\nasm -f win64 -gcv8 "{}.asm" -o "{}.obj" -w-number-overflow -w-db-empty
)", compiler->compiler_directory, output_path_base, output_path_base);

		append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
		append_format(bat_builder,
			R"("{}\bin\Hostx64\x64\link" /LARGEADDRESSAWARE:NO /nologo "{}.obj" /out:"{}" /nodefaultlib /entry:"main" /subsystem:console /DEBUG:FULL /LIBPATH:"{}\um\x64" /LIBPATH:"{}\libs" /LIBPATH:"{}\lib\x64" kernel32.lib)",
			msvc_directory,
			output_path_base,
			compiler->output_path,
			wkits_directory,
			compiler->compiler_directory,
			msvc_directory
		);
		for_each(compiler->extern_libraries, [&](auto &kv) {
			append_format(bat_builder, " {}.lib", kv.key);
		});

		auto bat_path = format(u8"{}.build.bat"s, output_path_base);
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));
#if 1
		timed_block(compiler->profiler, "nasm + link"s);

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
#endif
		if (!compiler->keep_temp)
			delete_file(bat_path);
	}

	if (!compiler->keep_temp)
		delete_file(asm_path);

	return true;
}

DECLARE_TARGET_INFORMATION_GETTER {
	::compiler = compiler;
	compiler->stack_word_size = 8;
	compiler->register_size = 8;
	compiler->general_purpose_register_count = 10;
}
