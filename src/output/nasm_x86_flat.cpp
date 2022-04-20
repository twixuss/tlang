#define TL_IMPL
#pragma warning(disable: 4702) // unreachable
#include <bytecode.h>
#include <ast.h>
#include "../x86.h"

using namespace x86;

static void append_instructions(CompilerContext &context, StringBuilder &builder, InstructionList instructions) {
	timed_function(context.profiler);

	append_format(builder,
		"global main\n"
		"main:\n"
		"push 0\n"
		"xchg bx, bx\n" // bochs debug break
		"call .{}\n"
		"pop eax\n"
		"ret\n",
		instruction_address(context.main_lambda->location_in_bytecode)
	);

	s64 idx = 0;
	for (auto i : instructions) {
		if (i.kind == InstructionKind::jmp_label)
			append_format(builder, ".{}: ", instruction_address(idx));
		switch (i.kind) {
			using enum Register32;
			using enum InstructionKind;
			case mov_re: append_format(builder, "mov {}, {}", i.mov_re.d, Span(i.mov_re.s_data, i.mov_re.s_count)); break;
			case mov_rr: append_format(builder, "mov {}, {}", i.mov_rr.d, i.mov_rr.s); break;
			case mov_rc: append_format(builder, "mov {}, {}", i.mov_rc.d, i.mov_rc.s); break;

			case mov1_mc: append_format(builder, "mov byte {}, {}", i.mov1_mc.d, i.mov1_mc.s); break;
			case mov2_mc: append_format(builder, "mov word {}, {}", i.mov2_mc.d, i.mov2_mc.s); break;
			case mov4_mc: append_format(builder, "mov dword {}, {}", i.mov4_mc.d, i.mov4_mc.s); break;

			case mov1_rm: append_format(builder, "mov {}, byte {}", part1b(i.mov1_rm.d), i.mov1_rm.s); break;
			case mov2_rm: append_format(builder, "mov {}, word {}", part2b(i.mov2_rm.d), i.mov2_rm.s); break;
			case mov4_rm: append_format(builder, "mov {}, dword {}", part4b(i.mov4_rm.d), i.mov4_rm.s); break;

			case mov1_mr: append_format(builder, "mov byte {}, {}", i.mov1_mr.d, part1b(i.mov1_mr.s)); break;
			case mov2_mr: append_format(builder, "mov word {}, {}", i.mov2_mr.d, part2b(i.mov2_mr.s)); break;
			case mov4_mr: append_format(builder, "mov dword {}, {}", i.mov4_mr.d, part4b(i.mov4_mr.s)); break;

			case push_c: append_format(builder, "push dword {}", (s32)i.push_c.s); break;
			case push_r: append_format(builder, "push {}", i.push_r.s); break;
			case push_f: append_format(builder, "movq ebx, {}\npush ebx", i.push_f.s); break;
			case push_m: append_format(builder, "push dword {}", i.push_m.s); break;

			case push_a: append_format(builder, "push constants + {}", i.push_a.s); break;
			case push_d: append_format(builder, "push data + {}"     , i.push_d.s); break;
			case push_u: append_format(builder, "push zeros + {}"    , i.push_u.s); break;
			case push_t: append_format(builder, "push .{}"           , instruction_address(i.push_t.s)); break;

			case mov_ra: append_format(builder, "mov {}, constants + {}", i.mov_ra.d, i.mov_ra.s); break;
			case mov_rd: append_format(builder, "mov {}, rwdata + {}"   , i.mov_rd.d, i.mov_rd.s); break;
			case mov_ru: append_format(builder, "mov {}, zeros + {}"    , i.mov_ru.d, i.mov_ru.s); break;
			case mov_rt: append_format(builder, "mov {}, .{}"           , i.mov_rt.d, instruction_address(i.mov_rt.s)); break;

			case pop_r: append_format(builder, "pop {}", i.pop_r.d); break;
			case pop_f: append_format(builder, "pop rbx\nmovq {}, rbx", i.pop_f.d); break;

			case ret: append_format(builder, "ret"); break;

			case shl_rc: append_format(builder, "shl {}, {}", i.shl_rc.d, i.shl_rc.s); break;
			case shl_mr: append_format(builder, "mov cl, {}\nshl dword {}, cl", part1b(i.shl_mr.s), i.shl_mr.d); break;

			case shr_rc: append_format(builder, "shr {}, {}", i.shr_rc.d, i.shr_rc.s); break;
			case shr_mr: append_format(builder, "mov cl, {}\nshr dword {}, cl", part1b(i.shr_mr.s), i.shr_mr.d); break;

			case add_rc: append_format(builder, "add {}, {}"        , i.add_rc.d, i.add_rc.s); break;
			case add_rr: append_format(builder, "add {}, {}"        , i.add_rr.d, i.add_rr.s); break;
			case add_mc: append_format(builder, "add dword {}, {}", i.add_mc.d, i.add_mc.s); break;
			case add_mr: append_format(builder, "add dword {}, {}", i.add_mr.d, i.add_mr.s); break;

			case sub_rc: append_format(builder, "sub {}, {}"        , i.sub_rc.d, i.sub_rc.s); break;
			case sub_rr: append_format(builder, "sub {}, {}"        , i.sub_rr.d, i.sub_rr.s); break;
			case sub_mc: append_format(builder, "sub dword {}, {}", i.sub_mc.d, i.sub_mc.s); break;
			case sub_mr: append_format(builder, "sub dword {}, {}", i.sub_mr.d, i.sub_mr.s); break;

			case mul_rc: append_format(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s); break;
			case mul_mr: append_format(builder, "imul {}, dword {}\nmov dword {}, {}", i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s); break;

				//  DIV - Unsigned divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
				// IDIV -   Signed divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
			case div_mr:
				if (to_x86_register(i.div_mr.s) == edx) {
					append_format(builder,
	"mov rbx, rdx\n"
	"xor rdx, rdx\n"
	"mov rax, qword {}\n"
	"div rbx\n"
	"mov qword {}, rax\n"
	"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
				} else {
					append_format(builder,
	"mov rbx, rdx\n"
	"xor rdx, rdx\n"
	"mov rax, qword {}\n"
	"div {}\n"
	"mov qword {}, rax\n"
	"mov rdx, rbx", i.div_mr.d, i.div_mr.s, i.div_mr.d);
				}
				break;
			case mod_mr:
				if (to_x86_register(i.div_mr.s) == edx) {
					append_format(builder,
	"mov rbx, rdx\n"
	"xor rdx, rdx\n"
	"mov rax, qword {}\n"
	"div rbx\n"
	"mov qword {}, rdx\n"
	"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
				} else {
					append_format(builder,
	"mov rbx, rdx\n"
	"xor rdx, rdx\n"
	"mov rax, qword {}\n"
	"div {}\n"
	"mov qword {}, rdx\n"
	"mov rdx, rbx", i.div_mr.d, i.div_mr.s, i.div_mr.d);
				}
				break;

			case negi_r: append_format(builder, "neg {}", i.negi_r.d); break;
			case negi8_m:  append_format(builder, "neg byte {}",  i.negi8_m.d); break;
			case negi16_m: append_format(builder, "neg word {}",  i.negi16_m.d); break;
			case negi32_m: append_format(builder, "neg dword {}", i.negi32_m.d); break;
			case negi64_m: append_format(builder, "neg qword {}", i.negi64_m.d); break;

			case or_mr: append_format(builder, "or qword {}, {}", i. or_mr.d, i. or_mr.s); break;

			case and_rc: append_format(builder, "and {}, {}"        , i.and_rc.d, i.and_rc.s); break;
			case and_mc: {
				if (~i.and_mc.s & 0xffffffff00000000) {
					auto l = (s32)i.and_mc.s;
					auto h = (s32)((u64)i.and_mc.s >> 32);
					append_format(builder, "and dword {}, {}\n"  , i.and_mc.d, l);
					auto addr = i.and_mc.d;
					addr.c += 4;
					append_format(builder, "and dword {}, {}", addr, h);
				} else {
					append_format(builder, "and qword {}, {}", i.and_mc.d, (s32)i.and_mc.s);
				}
				break;
			}
			case and_mr: append_format(builder, "and qword {}, {}", i.and_mr.d, i.and_mr.s); break;

			case xor_rr: append_format(builder, "xor {}, {}"        , i.xor_rr.d, i.xor_rr.s); break;
			case xor_mr: append_format(builder, "xor qword {}, {}", i.xor_mr.d, i.xor_mr.s); break;

			case cmps1: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps1.d, i.cmps1.d, part1b(i.cmps1.a), part1b(i.cmps1.b), cmps_string(i.cmps1.c), part1b(i.cmps1.d)); break;
			case cmps2: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps2.d, i.cmps2.d, part2b(i.cmps2.a), part2b(i.cmps2.b), cmps_string(i.cmps2.c), part1b(i.cmps2.d)); break;
			case cmps4: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps4.d, i.cmps4.d, part4b(i.cmps4.a), part4b(i.cmps4.b), cmps_string(i.cmps4.c), part1b(i.cmps4.d)); break;
			case cmpu1: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d)); break;
			case cmpu2: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d)); break;
			case cmpu4: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d)); break;

			case jmp: append_format(builder, "jmp .{}", instruction_address(idx + i.jmp.offset)); break;

			case jz_cr: { auto reg = part1b(i.jz_cr.reg); append_format(builder, "test {}, {}\njz .{}", reg, reg, instruction_address(idx + i.jz_cr.offset)); break; }

				// Here move into rcx must be last, because it can be source for rdi or rsi
			case copyf_mmc:
				append_format(builder,
					"mov esi, {}\n"
					"mov edi, {}\n"
					"mov ecx, {}\n"
					"cld\n"
					"rep movsb",
					i.copyf_mmc.s, i.copyf_mmc.d, i.copyf_mmc.size
				);
				break;
			case copyb_mmc:
				append_format(builder,
					"lea esi, [{} + {}]\n"
					"lea edi, [{} + {}]\n"
					"mov ecx, {}\n"
					"std\n"
					"rep movsb",
					i.copyb_mmc.s, i.copyb_mmc.size - 1, i.copyb_mmc.d, i.copyb_mmc.size - 1, i.copyb_mmc.size
				);
				break;
			case copyf_ssc: append_format(builder, "pop rsi\npop rdi\nmov rcx, {}\ncld\nrep movsb", i.copyf_ssc.size); break;
			case copyb_ssc: append_format(builder, "pop rsi\npop rdi\nadd rsi, {}\nadd rdi, {}\nmov rcx, {}\nstd\nrep movsb", i.copyb_ssc.size - 1, i.copyb_ssc.size - 1, i.copyb_ssc.size); break;
			case set_mcc: append_format(builder, "mov rdi, {}\nmov al, {}\nmov rcx, {}\ncld\nrep stosb", i.set_mcc.d, i.set_mcc.s, i.set_mcc.size); break;

			case call_c: {
				append_format(builder, "call .{}", instruction_address(i.call_c.constant));
				break;
			}
			case call_r:
				append_format(builder, "call {}", i.call_r.s);
				break;
			case call_m:
				append_format(builder, "call qword {}", i.call_m.s);
				break;
	#if 0
				if (i.call_m.lambda->convention == CallingConvention::stdcall)
					prepare_stdcall(i.call_m.lambda);

				append_format(builder, "call qword {}", i.call_m.s);

				if (i.call_m.lambda->convention == CallingConvention::stdcall)
					append(builder, "\npush rax");

				break;
	#endif

			case lea:
				append_format(builder, "lea {}, {}", i.lea.d, i.lea.s);
				break;

			case cvt_f64_s64:
				append(builder, "cvtsd2si rax, [rsp]\nmov [rsp], rax");
				break;
			case cvt_s64_f64:
				append(builder, "cvtsi2sd xmm7, [rsp]\nmovq [rsp], xmm7");
				break;

			case mov_fr: append_format(builder, "movq {}, {}", i.mov_fr.d, i.mov_fr.s); break;
			case mov_rf: append_format(builder, "movq {}, {}", i.mov_rf.d, i.mov_rf.s); break;

			case mov1_xm: append_format(builder, "movb {}, byte {}", i.mov1_xm.d, i.mov1_xm.s); break;
			case mov2_xm: append_format(builder, "movw {}, word {}", i.mov2_xm.d, i.mov2_xm.s); break;
			case mov4_xm: append_format(builder, "movd {}, dword {}", i.mov4_xm.d, i.mov4_xm.s); break;
			case mov8_xm: append_format(builder, "movq {}, qword {}", i.mov8_xm.d, i.mov8_xm.s); break;

			case add_f32_f32: append_format(builder, "addss {}, {}", i.add_f32_f32.d, i.add_f32_f32.s); break;
			case sub_f32_f32: append_format(builder, "subss {}, {}", i.sub_f32_f32.d, i.sub_f32_f32.s); break;
			case mul_f32_f32: append_format(builder, "mulss {}, {}", i.mul_f32_f32.d, i.mul_f32_f32.s); break;
			case div_f32_f32: append_format(builder, "divss {}, {}", i.div_f32_f32.d, i.div_f32_f32.s); break;

			case add_f64_f64: append_format(builder, "addsd {}, {}", i.add_f64_f64.d, i.add_f64_f64.s); break;
			case sub_f64_f64: append_format(builder, "subsd {}, {}", i.sub_f64_f64.d, i.sub_f64_f64.s); break;
			case mul_f64_f64: append_format(builder, "mulsd {}, {}", i.mul_f64_f64.d, i.mul_f64_f64.s); break;
			case div_f64_f64: append_format(builder, "divsd {}, {}", i.div_f64_f64.d, i.div_f64_f64.s); break;

			case xor_ff: append_format(builder, "xorps {}, {}", i.xor_ff.d, i.xor_ff.s); break;

			case tobool_r:    { auto d = part1b(i.tobool_r.d);    append_format(builder, "test {}, {}\nsetnz {}", d, d, d); break; }
			case toboolnot_r: { auto d = part1b(i.toboolnot_r.d); append_format(builder, "test {}, {}\nsetz {}" , d, d, d); break; }
			case noop: break;

			case debug_break: append(builder, "xchg bx, bx"s); break;

			case movsx21_rm: append_format(builder, "movsx {}, byte {}"s,  part2b(i.movsx21_rm.d), i.movsx21_rm.s); break;
			case movsx41_rm: append_format(builder, "movsx {}, byte {}"s,  part4b(i.movsx41_rm.d), i.movsx41_rm.s); break;
			case movsx42_rm: append_format(builder, "movsx {}, word {}"s,  part4b(i.movsx42_rm.d), i.movsx42_rm.s); break;

			case movzx21_rm: append_format(builder, "movzx {}, byte {}"s,  part2b(i.movsx21_rm.d), i.movsx21_rm.s); break;
			case movzx41_rm: append_format(builder, "movzx {}, byte {}"s,  part4b(i.movsx41_rm.d), i.movsx41_rm.s); break;
			case movzx42_rm: append_format(builder, "movzx {}, word {}"s,  part4b(i.movsx42_rm.d), i.movsx42_rm.s); break;

			default:invalid_code_path();
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

	StringBuilder builder;

	{
		scoped_phase("Writing nasm");

		append(builder, "bits 32\n");

		assert(count_of(bytecode.extern_libraries) == 0);

		if (bytecode.constant_data.count) {
			append(builder, "constants: db ");
			for (auto byte : bytecode.constant_data) {
				append_format(builder, "{},", byte);
			}
			append(builder, '\n');
		}

		if (bytecode.data.count) {
			append(builder, "rwdata: db ");
			for (auto byte : bytecode.data) {
				append_format(builder, "{},", byte);
			}
			append(builder, '\n');
		}

		if (bytecode.zero_data_size) {
			append_format(builder, "zeros: resb {}\n", bytecode.zero_data_size);
		}

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
{}\nasm -f bin "{}.asm" -o "{}.bin" -w-number-overflow -w-db-empty
	)", context.compiler_directory, output_path_base, output_path_base);

		append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");

		auto bat_path = to_pathchars(concatenate(context.compiler_directory, u8"\\nasm_build.bat"s));
		write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

		timed_block(context.profiler, "nasm"s);

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
		print("Build succeeded\n");
	}

}

DECLARE_TARGET_INFORMATION_GETTER {
	context.stack_word_size = 4;
	context.register_size = 4;
	context.general_purpose_register_count = 6;
}
