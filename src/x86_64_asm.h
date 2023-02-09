#pragma once
#include "x86_64.h"

namespace x86_64 {

s64 saved_registers_size;
s64 temporary_offset;
s64 locals_offset;
s64 parameters_size;

static Span<utf8> cmov_string(Comparison c) {
	using enum Comparison;
	switch (c) {
		case e : return u8"e"s;
		case ne: return u8"ne"s;
		case l : return u8"l"s;
		case le: return u8"le"s;
		case g : return u8"g"s;
		case ge: return u8"ge"s;
	}
	invalid_code_path();
	return {};
}

static Span<utf8> cmps_string(Comparison c) {
	using enum Comparison;
	switch (c) {
		case e : return u8"e"s;
		case ne: return u8"ne"s;
		case l : return u8"l"s;
		case le: return u8"le"s;
		case g : return u8"g"s;
		case ge: return u8"ge"s;
	}
	invalid_code_path();
	return {};
}

static Span<utf8> cmpu_string(Comparison c) {
	using enum Comparison;
	switch (c) {
		case e : return u8"e"s;
		case ne: return u8"ne"s;
		case l : return u8"b"s;
		case le: return u8"be"s;
		case g : return u8"a"s;
		case ge: return u8"ae"s;
	}
	invalid_code_path();
	return {};
}

#define C(x) case x: return u8#x##s

static Span<utf8> as_string(Register64 r){using enum Register64;switch(r){C(rax);C(rbx);C(rcx);C(rdx);C(rsi );C(rdi );C(rsp );C(rbp );C(r8 );C(r9 );C(r10 );C(r11 );C(r12 );C(r13 );C(r14 );C(r15 );}invalid_code_path();return{};}
static Span<utf8> as_string(Register32 r){using enum Register32;switch(r){C(eax);C(ebx);C(ecx);C(edx);C(esi );C(edi );C(esp );C(ebp );C(r8d);C(r9d);C(r10d);C(r11d);C(r12d);C(r13d);C(r14d);C(r15d);}invalid_code_path();return{};}
static Span<utf8> as_string(Register16 r){using enum Register16;switch(r){C( ax);C( bx);C( cx);C( dx);C( si );C( di );C( sp );C( bp );C(r8w);C(r9w);C(r10w);C(r11w);C(r12w);C(r13w);C(r14w);C(r15w);}invalid_code_path();return{};}
static Span<utf8> as_string(Register8  r){using enum Register8 ;switch(r){C( al);C( bl);C( cl);C( dl);C( sil);C( dil);C( spl);C( bpl);C(r8b);C(r9b);C(r10b);C(r11b);C(r12b);C(r13b);C(r14b);C(r15b);}invalid_code_path();return{};}

#undef C

}

namespace tl {

inline umm append(StringBuilder&builder,x86_64::Register64 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register32 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register16 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register8  r){return append(builder,as_string(r));}

inline umm append(StringBuilder &builder, Register r) {
	using namespace x86_64;
	return append(builder, to_x86_register(r));
}
}

namespace x86_64 {

inline StringBuilder debug_message_builder;

inline void init() {
	construct(debug_message_builder);
}

inline umm append_instruction(StringBuilder &builder, s64 idx, Instruction i) {
	using enum Register64;

	current_instruction = i;

	switch (i.kind) {
		using enum InstructionKind;
		case mov_rr: return append_format(builder, "mov {},{}", i.mov_rr.d, i.mov_rr.s);
		case mov_rc: return append_format(builder, "mov {},{}", i.mov_rc.d, i.mov_rc.s);

		case mov1_mc: return append_format(builder, "mov byte{},{}", i.mov1_mc.d, i.mov1_mc.s);
		case mov2_mc: return append_format(builder, "mov word{},{}", i.mov2_mc.d, i.mov2_mc.s);
		case mov4_mc: return append_format(builder, "mov dword{},{}", i.mov4_mc.d, i.mov4_mc.s);
		case mov8_mc: {
			REDECLARE_REF(i, i.mov8_mc);
			auto mask = 0xFFFF'FFFF'8000'0000;
			auto s = i.s & mask;
			if (s == 0 || s == mask)
				return append_format(builder, "mov qword{},{}", i.d, i.s);

			return append_format(builder, "mov dword{},{}\nmov dword{},{}", i.d, (u32)i.s, i.d + 4, (u32)(i.s >> 32));
		}

		case mov1_rm: return append_format(builder, "mov {},byte {}", part1b(i.mov1_rm.d), i.mov1_rm.s);
		case mov2_rm: return append_format(builder, "mov {},word {}", part2b(i.mov2_rm.d), i.mov2_rm.s);
		case mov4_rm: return append_format(builder, "mov {},dword{}", part4b(i.mov4_rm.d), i.mov4_rm.s);
		case mov8_rm: return append_format(builder, "mov {},qword{}", part8b(i.mov8_rm.d), i.mov8_rm.s);

		case mov1_mr: return append_format(builder, "mov byte {},{}", i.mov1_mr.d, part1b(i.mov1_mr.s));
		case mov2_mr: return append_format(builder, "mov word {},{}", i.mov2_mr.d, part2b(i.mov2_mr.s));
		case mov4_mr: return append_format(builder, "mov dword{},{}", i.mov4_mr.d, part4b(i.mov4_mr.s));
		case mov8_mr: return append_format(builder, "mov qword{},{}", i.mov8_mr.d, part8b(i.mov8_mr.s));

		case push_c: {
			if (min_value<s32> <= i.push_c.s && i.push_c.s <= max_value<s32>) {
				return append_format(builder, "push {}", (s32)i.push_c.s);
			} else {
				//if (i.push_c.s & 0x80000000) {
					return append_format(builder,
						"sub rsp,8\nmov dword[rsp],{}\nmov dword[rsp+4],{}",
						(s32)i.push_c.s       ,
						(s32)(i.push_c.s >> 32)
					);
				//} else {
				//	return append_format(builder,
				//		"push dword {}\nmov dword [rsp+4], {}",
				//		(s32)i.push_c.s       ,
				//		(s32)(i.push_c.s >> 32)
				//	);
				//}
			}
			break;
		}
		case push_r: return append_format(builder, "push {}", i.push_r.s);
		case push_m: return append_format(builder, "push qword{}", i.push_m.s);

		case pop_r: return append_format(builder, "pop {}", i.pop_r.d);

		case ret: return append_format(builder, "ret");

		case shl_rc: return append_format(builder, "shl {},{}", i.shl_rc.d, i.shl_rc.s);
		case shl_rr: {
			if (to_x86_register(i.shl_rr.d) == Register64::rcx) {
				if (to_x86_register(i.shl_rr.s) == Register64::rdx) {
					return append(builder,
						"mov r8,rcx\n"
						"mov cl,dl\n"
						"shl r8,cl\n"
						"mov rcx,r8"
					);
				} else {
					return append_format(builder,
						"mov rdx,rcx\n"
						"mov cl,{}\n"
						"shl rdx,cl\n"
						"mov rcx,rdx",
						part1b(i.shl_rr.s)
					);
				}
			} else {
				if (to_x86_register(i.shl_rr.s) == Register64::rcx) {
					return append_format(builder,
						"shl {},cl",
						i.shl_rr.d
					);
				} else {
					return append_format(builder,
						"mov cl,{}\n"
						"shl {},cl",
						part1b(i.shl_rr.s),
						i.shl_rr.d
					);
				}
			}
			break;
		}
		case slr_rc: return append_format(builder, "shr {}, {}", i.slr_rc.d, i.slr_rc.s);
		case slr_rr: {
			if (to_x86_register(i.slr_rr.d) == Register64::rcx) {
				if (to_x86_register(i.slr_rr.s) == Register64::rdx) {
					return append(builder,
						"mov r8,rcx\n"
						"mov cl,dl\n"
						"shr r8,cl\n"
						"mov rcx,r8"
					);
				} else {
					return append_format(builder,
						"mov rdx,rcx\n"
						"mov cl,{}\n"
						"shr rdx,cl\n"
						"mov rcx,rdx",
						part1b(i.slr_rr.s)
					);
				}
			} else {
				if (to_x86_register(i.slr_rr.s) == Register64::rcx) {
					return append_format(builder,
						"shr {},cl",
						i.slr_rr.d
					);
				} else {
					return append_format(builder,
						"mov cl,{}\n"
						"shr {},cl",
						part1b(i.slr_rr.s),
						i.slr_rr.d
					);
				}
			}
			break;
		}

		case sar_rc: return append_format(builder, "sar {}, {}", i.sar_rc.d, i.sar_rc.s);
		case sar_rr: {
			if (to_x86_register(i.sar_rr.d) == Register64::rcx) {
				if (to_x86_register(i.sar_rr.s) == Register64::rdx) {
					return append(builder,
						"mov r8,rcx\n"
						"mov cl,dl\n"
						"sar r8,cl\n"
						"mov rcx,r8"
					);
				} else {
					return append_format(builder,
						"mov rdx,rcx\n"
						"mov cl,{}\n"
						"sar rdx,cl\n"
						"mov rcx,rdx",
						part1b(i.sar_rr.s)
					);
				}
			} else {
				if (to_x86_register(i.sar_rr.s) == Register64::rcx) {
					return append_format(builder,
						"sar {},cl",
						i.sar_rr.d
					);
				} else {
					return append_format(builder,
						"mov cl,{}\n"
						"sar {},cl",
						part1b(i.sar_rr.s),
						i.sar_rr.d
					);
				}
			}
			break;
		}

		case add_rc: return append_format(builder, "add {},{}"      , i.add_rc.d, i.add_rc.s);
		case add_rr: return append_format(builder, "add {},{}"      , i.add_rr.d, i.add_rr.s);

		case sub_rc: return append_format(builder, "sub {},{}"      , i.sub_rc.d, i.sub_rc.s);
		case sub_rr: return append_format(builder, "sub {},{}"      , i.sub_rr.d, i.sub_rr.s);

		case mul_rc: return append_format(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s);
		case mul_rr: return append_format(builder, "imul {}, {}", i.mul_rr.d, i.mul_rr.s);

			//  DIV - Unsigned divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
			// IDIV -   Signed divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
		case divu_rr:
			return append_format(builder,
"push rdx\n"
"xor rdx,rdx\n"
"mov rax,{}\n"
"div {}\n"
"mov {},rax\n"
"pop rdx", i.divu_rr.d, i.divu_rr.s, i.divu_rr.d);
			break;

		case divs_rr:
			return append_format(builder,
"push rdx\n"
"mov rax,{}\n"
"cqo\n"
"div {}\n"
"mov {},rax\n"
"pop rdx", i.divs_rr.d, i.divs_rr.s, i.divs_rr.d);
			break;

		case modu_rr:
			return append_format(builder,
"push rdx\n"
"xor rdx,rdx\n"
"mov rax,{}\n"
"div {}\n"
"mov {},rdx\n"
"pop rdx", i.modu_rr.d, i.modu_rr.s, i.modu_rr.d);
			break;

		case mods_rr:
			return append_format(builder,
"push rdx\n"
"mov rax,{}\n"
"cqo\n"
"div {}\n"
"mov {},rdx\n"
"pop rdx", i.mods_rr.d, i.mods_rr.s, i.mods_rr.d);
			break;


		case or_rc: return append_format(builder, "or {},{}", i.or_rc.d, i.or_rc.s);
		case or_rr: return append_format(builder, "or {},{}", i. or_rr.d, i. or_rr.s);

		case and_rc: return append_format(builder, "and {},{}", i.and_rc.d, i.and_rc.s);
		case and_rr: return append_format(builder, "and {},{}", i.and_rr.d, i.and_rr.s);

		case xor_rc: return append_format(builder, "xor {},{}", i.xor_rc.d, i.xor_rc.s);
		case xor_rr: return append_format(builder, "xor {},{}", i.xor_rr.d, i.xor_rr.s);

		case sbxor_rc: return append_format(builder, "btc {},{}", i.sbxor_rc.d, i.sbxor_rc.s);
		case sbxor_rr: return append_format(builder, "btc {},{}", i.sbxor_rr.d, i.sbxor_rr.s);

		case not_r: return append_format(builder, "not {}", i.not_r.d);
		case negi_r: return append_format(builder, "neg {}", i.negi_r.d);

		case cmps1: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmps1.d, i.cmps1.d, part1b(i.cmps1.a), part1b(i.cmps1.b), cmps_string(i.cmps1.c), part1b(i.cmps1.d));
		case cmps2: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmps2.d, i.cmps2.d, part2b(i.cmps2.a), part2b(i.cmps2.b), cmps_string(i.cmps2.c), part1b(i.cmps2.d));
		case cmps4: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmps4.d, i.cmps4.d, part4b(i.cmps4.a), part4b(i.cmps4.b), cmps_string(i.cmps4.c), part1b(i.cmps4.d));
		case cmps8: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmps8.d, i.cmps8.d, part8b(i.cmps8.a), part8b(i.cmps8.b), cmps_string(i.cmps8.c), part1b(i.cmps8.d));
		case cmpu1: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d));
		case cmpu2: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d));
		case cmpu4: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d));
		case cmpu8: return append_format(builder, "xor {},{}\ncmp {},{}\nset{} {}", i.cmpu8.d, i.cmpu8.d, part8b(i.cmpu8.a), part8b(i.cmpu8.b), cmpu_string(i.cmpu8.c), part1b(i.cmpu8.d));
		case cmpf4: return append_format(builder, "xor {},{}\nmovd xmm6,{}\nmovd xmm7,{}\ncomiss xmm6,xmm7\nset{} {}", i.cmpf4.d, i.cmpf4.d, part4b(i.cmpf4.a), part4b(i.cmpf4.b), cmpu_string(i.cmpf4.c), part1b(i.cmpf4.d));
		case cmpf8: return append_format(builder, "xor {},{}\nmovq xmm6,{}\nmovq xmm7,{}\ncomisd xmm6,xmm7\nset{} {}", i.cmpf8.d, i.cmpf8.d, part8b(i.cmpf8.a), part8b(i.cmpf8.b), cmpu_string(i.cmpf8.c), part1b(i.cmpf8.d));

		case jz_cr:  { auto reg = part1b(i.jz_cr.reg); return append_format(builder, "test {}, {}\njz i{}", reg, reg, idx + i.jz_cr.offset); }
		case jnz_cr: { auto reg = part1b(i.jnz_cr.reg); return append_format(builder, "test {}, {}\njnz i{}", reg, reg, idx + i.jnz_cr.offset); }

		case cmpflag1: return append_format(builder, "cmp {}, {}", part1b(i.cmpflag1.a), part1b(i.cmpflag1.b));
		case cmpflag2: return append_format(builder, "cmp {}, {}", part2b(i.cmpflag2.a), part2b(i.cmpflag2.b));
		case cmpflag4: return append_format(builder, "cmp {}, {}", part4b(i.cmpflag4.a), part4b(i.cmpflag4.b));
		case cmpflag8: return append_format(builder, "cmp {}, {}", part8b(i.cmpflag8.a), part8b(i.cmpflag8.b));

		case jef_c:  { return append_format(builder, "je i{}",  idx + i.jef_c .offset); }
		case jnef_c: { return append_format(builder, "jne i{}", idx + i.jnef_c.offset); }
		case jlf_c:  { return append_format(builder, "jl i{}",  idx + i.jlf_c .offset); }
		case jgf_c:  { return append_format(builder, "jg i{}",  idx + i.jgf_c .offset); }
		case jlef_c: { return append_format(builder, "jle i{}", idx + i.jlef_c.offset); }
		case jgef_c: { return append_format(builder, "jge i{}", idx + i.jgef_c.offset); }

		case jmp: return append_format(builder, "jmp i{}", idx + i.jmp.offset);

			// Here move into rcx must be last, because it can be source for rdi or rsi
		case copyf_mmc:
			return append_format(builder,
				"lea rsi, {}\n"
				"lea rdi, {}\n"
				"mov rcx, {}\n"
				"rep movsb",
				i.copyf_mmc.s,
				i.copyf_mmc.d,
				i.copyf_mmc.size
			);
		case copyb_mmc:
			return append_format(builder,
				"lea rsi, {}\n"
				"lea rdi, {}\n"
				"mov rcx, {}\n"
				"std\n"
				"rep movsb\n"
				"cld",
				i.copyb_mmc.s + (i.copyb_mmc.size - 1),
				i.copyb_mmc.d + (i.copyb_mmc.size - 1),
				i.copyb_mmc.size
			);
		case copyf_mmr:
			return append_format(builder,
				"lea rsi, {}\n"
				"lea rdi, {}\n"
				"mov rcx, {}\n"
				"rep movsb",
				i.copyf_mmr.s,
				i.copyf_mmr.d,
				i.copyf_mmr.size
			);
		case copyb_mmr:
			return append_format(builder,
				"lea rsi, {}\n"
				"lea rdi, {}\n"
				"mov rcx, {}\n"
				"add rsi, rcx\n"
				"add rdi, rcx\n"
				"dec rsi\n"
				"dec rdi\n"
				"std\n"
				"rep movsb\n"
				"cld",
				i.copyb_mmr.s,
				i.copyb_mmr.d,
				i.copyb_mmr.size
			);
		case set_mcc: return append_format(builder, "lea rdi, {}\nmov al, {}\nmov rcx, {}\nrep stosb", i.set_mcc.d, i.set_mcc.s, i.set_mcc.size);

		case cmpstr:
			return append_format(builder,
				"lea rsi, {}\n"
				"lea rdi, {}\n"
				"mov rcx, {}\n"
				"repe cmpsb\n"
				"sete {}",
				i.cmpstr.a,
				i.cmpstr.b,
				i.cmpstr.d,
				part1b(i.cmpstr.d)
			);

#if 0
		case begin_lambda: {
			if (i.begin_lambda.convention == CallingConvention::stdcall) {
				auto lambda = i.begin_lambda.lambda;

				// what we have right now is:
				//
				// rcx: arg0
				// rdx: arg1
				// r8:  arg2
				// r9:  arg3
				//
				// arg5
				// arg4
				// shadow
				// shadow
				// shadow
				// shadow <- rsp aligned to 16
				//
				// we need to get this:
				//
				// ret
				// arg0
				// arg1
				// arg2
				// arg3
				// arg4
				// arg5 <- rsp aligned to 16
				//

				for (auto parameter : lambda->parameters)
					assert(get_size(parameter->type) <= 8);

				auto return_value_size = ceil(get_size(lambda->return_parameter->type), 8ll);

				if (return_value_size % 16 != 0) {
					append(builder,
						";keep the stack aligned\n"
						"sub rsp, 8\n"
					);
				}

				append_format(builder,
					";reserve space for return value\n"
					"sub rsp, {}\n",
					return_value_size
				);

				// push parameters
				if (lambda->parameters.count >= 1) append(builder, "push rcx\n");
				if (lambda->parameters.count >= 2) append(builder, "push rdx\n");
				if (lambda->parameters.count >= 3) append(builder, "push r8\n");
				if (lambda->parameters.count >= 4) append(builder, "push r9\n");
				int offset = 72; // 4 first args + ret + shadow space
				for (int i = 4; i < lambda->parameters.count; ++i) {
					append_format(builder, "push [rsp + {}]\n", offset);
					offset += 16; // offset is 2*8 because rsp goes down by 8, and next argument goes up by 8.
				}

				append(builder,
					";dummy return address\n"
					"sub rsp, 8\n"
					"push rbp\n"
					"mov rbp, rsp"
				);
			}
			break;
		}
		case end_lambda: {
			if (i.end_lambda.convention == CallingConvention::stdcall) {
				auto lambda = i.end_lambda.lambda;
				append_format(builder,
					";pop dummy return address and parameters\n"
					"add rsp, {}\n"
					"pop rax\n"
					"mov rsp, rbp\n"
					"pop rbp",
					8 + lambda->parameters_size
				);
			}
			break;
		}
#endif

		// case align_stack_before_call: {
		// 	invalid_code_path("not implemented");
		//
		// 	append(builder, "mov rax, rsp\nand rsp, -16\n");
		// 	if (((i.align_stack_before_call.arguments_size_on_stack + 8) % 16) == 0) { // 8: include call address
		// 		append(builder, "sub rsp, 8\n"));
		// 	}
		// 	append(builder, "push rax");
		//
		// 	break;
		// }

		case call_c: {
			REDECLARE_REF(i, i.call_c);
			assert(i.lambda->convention == CallingConvention::tlang);
			return append_format(builder,
					"mov rax, rsp\n"
					"and rax, 15\n"
					"test rax, rax\n"
					"jz .s{}\n"
					"int3; STACK WAS NOT ALIGNED TO 16 BYTES BEFORE CALLING THIS PROCEDURE\n"
					".s{}:\n"
					"call i{}", FormatInt{.value=(u64)idx, .radix=62}, FormatInt{.value=(u64)idx, .radix=62}, i.constant);
			break;
#if 0
			auto lambda = i.call_c.lambda;
			if (lambda->convention == CallingConvention::stdcall) {
				// prepare_stdcall(i.call_c.lambda);

				// what we have right now is:
				//
				// arg0
				// arg1
				// arg2
				// arg3
				// arg4
				// arg5 <- rs aligned to 16
				//
				// we need to get this:
				//
				// rcx: arg0
				// rdx: arg1
				// r8:  arg2
				// r9:  arg3
				//
				// arg5
				// arg4
				// shadow
				// shadow
				// shadow
				// shadow <- rs aligned to 16
				//


				if (lambda->parameters.count >= 4) append_format(builder, "mov r9,  [rsp+{}]\n", lambda->parameters_size-32);
				if (lambda->parameters.count >= 3) append_format(builder, "mov r8,  [rsp+{}]\n", lambda->parameters_size-24);
				if (lambda->parameters.count >= 2) append_format(builder, "mov rdx, [rsp+{}]\n", lambda->parameters_size-16);
				if (lambda->parameters.count >= 1) append_format(builder, "mov rcx, [rsp+{}]\n", lambda->parameters_size- 8);


				for (s64 i = 0; i < lambda->parameters.count / 2; ++i) {
					append_format(builder, "mov rax, [rsp+{}]\nxchg rax, [rsp+{}]\n", i*8, lambda->parameters_size-i*8-8);
				}

#if 0

				// remember the stack pointer and align the stack to 16 byte boundary
				append(builder, "mov rax, rsp\nand rsp, -16\nsub rsp, 8\npush rax\n");

				int offset = 0;
				for (auto parameter : lambda->parameters) {
					assert(get_size(parameter->type) <= 8);
					append_format(builder, "push [rsp + {}]\n", offset);
					offset += 16;
				}


				if (lambda->parameters.count >= 4) append(builder, "mov r9,  [rsp+24]\n");
				if (lambda->parameters.count >= 3) append(builder, "mov r8,  [rsp+16]\n");
				if (lambda->parameters.count >= 2) append(builder, "mov rdx, [rsp+ 8]\n");
				if (lambda->parameters.count >= 1) append(builder, "mov rcx, [rsp+ 0]\n");

				if (lambda->parameters.count == 0);


				// I don't know if shadow space is always needed
				append(builder, "sub rsp, 32\n");
#endif
			} else {
				assert(lambda->convention == CallingConvention::tlang);
			}

			append_format(builder, "call .{}", instruction_address(i.call_c.constant));

			if (lambda->convention == CallingConvention::stdcall) {
				append_format(builder, "\nadd rsp, {}", lambda->parameters_size);
				//append(builder, "\nadd rsp, 32\npop rsp\nmov [rsp], rax");
			} else {
				assert(lambda->convention == CallingConvention::tlang);
				append_format(builder, "\nadd rsp, {}", lambda->parameters_size);
			}

			break;
#endif
		}

		case call_r: {
			REDECLARE_REF(i, i.call_r);
			switch (i.lambda->convention) {
				case CallingConvention::tlang: return append_format(builder, "call {}", i.s);
				case CallingConvention::stdcall:
					return append_format(builder, "mov rax, {}\nmov rbx, {}\ncall _stdcall_{}", i.s, i.lambda->parameters_size, i.lambda->parameters_size % 16 == 0 ? "even" : "odd");
				default: invalid_code_path();
			}
			break;
		}
		case call_m: {
			REDECLARE_REF(i, i.call_m);
			switch (i.lambda->convention) {
				case CallingConvention::tlang:  return append_format(builder, "call qword {}", i.s);
				case CallingConvention::stdcall: return append_format(builder, "mov rax, {}\nmov rbx, {}\ncall _stdcall_{}", i.s, i.lambda->parameters_size, i.lambda->parameters_size % 16 == 0 ? "even" : "odd");
				default: invalid_code_path();
			}
			break;
		}
#if 0
			if (i.call_m.lambda->convention == CallingConvention::stdcall)
				prepare_stdcall(i.call_m.lambda);

			append_format(builder, "call qword {}", i.call_m.s);

			if (i.call_m.lambda->convention == CallingConvention::stdcall)
				append(builder, "\npush rax");

			break;
#endif

		case lea: return append_format(builder, "lea {}, {}", i.lea.d, i.lea.s);

		case cvt_f32_s32: return append_format(builder, "movd xmm7, {}\ncvtss2si {}, xmm7", part4b(i.cvt_f32_s32.d), part4b(i.cvt_f32_s32.d));
		case cvt_s32_f32: return append_format(builder, "cvtsi2ss xmm7, {}\nmovd {}, xmm7", part4b(i.cvt_s32_f32.d), part4b(i.cvt_s32_f32.d));

		case cvt_f64_s64: return append_format(builder, "movq xmm7, {}\ncvttsd2si {}, xmm7", i.cvt_f64_s64.d, i.cvt_f64_s64.d);
		case cvt_s64_f64: return append_format(builder, "cvtsi2sd xmm7, {}\nmovq {}, xmm7", i.cvt_s64_f64.d, i.cvt_s64_f64.d);

		case cvt_f64_f32: return append_format(builder, "movq xmm7,{}\ncvtsd2ss xmm7,xmm7\nmovd {}, xmm7", i.cvt_f64_f32.d, part4b(i.cvt_f64_f32.d));
		case cvt_f32_f64: return append_format(builder, "movd xmm7,{}\ncvtss2sd xmm7,xmm7\nmovq {}, xmm7", part4b(i.cvt_f32_f64.d), i.cvt_f32_f64.d);

	    case add4_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\naddss xmm6, xmm7\nmovd {}, xmm6", part4b(i.add4_ff.d), part4b(i.add4_ff.s), part4b(i.add4_ff.d));
	    case sub4_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\nsubss xmm6, xmm7\nmovd {}, xmm6", part4b(i.sub4_ff.d), part4b(i.sub4_ff.s), part4b(i.sub4_ff.d));
	    case mul4_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\nmulss xmm6, xmm7\nmovd {}, xmm6", part4b(i.mul4_ff.d), part4b(i.mul4_ff.s), part4b(i.mul4_ff.d));
	    case div4_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\ndivss xmm6, xmm7\nmovd {}, xmm6", part4b(i.div4_ff.d), part4b(i.div4_ff.s), part4b(i.div4_ff.d));

	    case add8_ff: return append_format(builder, "movq xmm6, {}\nmovq xmm7, {}\naddsd xmm6, xmm7\nmovq {}, xmm6", i.add8_ff.d, i.add8_ff.s, i.add8_ff.d);
	    case sub8_ff: return append_format(builder, "movq xmm6, {}\nmovq xmm7, {}\nsubsd xmm6, xmm7\nmovq {}, xmm6", i.sub8_ff.d, i.sub8_ff.s, i.sub8_ff.d);
	    case mul8_ff: return append_format(builder, "movq xmm6, {}\nmovq xmm7, {}\nmulsd xmm6, xmm7\nmovq {}, xmm6", i.mul8_ff.d, i.mul8_ff.s, i.mul8_ff.d);
	    case div8_ff: return append_format(builder, "movq xmm6, {}\nmovq xmm7, {}\ndivsd xmm6, xmm7\nmovq {}, xmm6", i.div8_ff.d, i.div8_ff.s, i.div8_ff.d);

		case sqrt4_f: return append_format(builder, "movd xmm7, {}\nsqrtss xmm7,xmm7\nmovd {}, xmm7", part4b(i.sqrt4_f.d), part4b(i.sqrt4_f.d));
		case sqrt8_f: return append_format(builder, "movq xmm7, {}\nsqrtsd xmm7,xmm7\nmovq {}, xmm7", i.sqrt8_f.d, i.sqrt8_f.d);

		case round4_f: return append_format(builder, "movd xmm7, {}\nroundss xmm7,xmm7,0\nmovd {}, xmm7", part4b(i.round4_f.d), part4b(i.round4_f.d));
		case round8_f: return append_format(builder, "movq xmm7, {}\nroundsd xmm7,xmm7,0\nmovq {}, xmm7", i.round8_f.d, i.round8_f.d);

		case floor4_f: return append_format(builder, "movd xmm7, {}\nroundss xmm7,xmm7,1\nmovd {}, xmm7", part4b(i.floor4_f.d), part4b(i.floor4_f.d));
		case floor8_f: return append_format(builder, "movq xmm7, {}\nroundsd xmm7,xmm7,1\nmovq {}, xmm7", i.floor8_f.d, i.floor8_f.d);

		case ceil4_f: return append_format(builder, "movd xmm7, {}\nroundss xmm7,xmm7,2\nmovd {}, xmm7", part4b(i.ceil4_f.d), part4b(i.ceil4_f.d));
		case ceil8_f: return append_format(builder, "movq xmm7, {}\nroundsd xmm7,xmm7,2\nmovq {}, xmm7", i.ceil8_f.d, i.ceil8_f.d);

		case tobool_r:    { auto d = i.tobool_r.d;    return append_format(builder, "test {}, {}\nsetnz {}", d, d, part1b(d)); }
		case toboolnot_r: { auto d = i.toboolnot_r.d; return append_format(builder, "test {}, {}\nsetz {}" , d, d, part1b(d)); }
		case jmp_label:
		case noop:
			return 0;

		case debug_break: return append(builder, "int3"s);
		case debug_error: {
			auto result = append_format(builder, "mov rdx, _debug_messages + {}\ncall _debug_error\nint3; {}"s, debug_message_builder.count(), split(i.debug_error.message, u8'\n'));

			append(debug_message_builder, i.debug_error.message);
			append(debug_message_builder, (ascii)0);

			return result;
		}

		case movsx21_rr: return append_format(builder, "movsx {}, {}", part2b(i.movsx21_rr.d), part1b(i.movsx21_rr.s));
		case movsx41_rr: return append_format(builder, "movsx {}, {}", part4b(i.movsx41_rr.d), part1b(i.movsx41_rr.s));
		case movsx81_rr: return append_format(builder, "movsx {}, {}", part8b(i.movsx81_rr.d), part1b(i.movsx81_rr.s));
		case movsx42_rr: return append_format(builder, "movsx {}, {}", part4b(i.movsx42_rr.d), part2b(i.movsx42_rr.s));
		case movsx82_rr: return append_format(builder, "movsx {}, {}", part8b(i.movsx82_rr.d), part2b(i.movsx82_rr.s));
		case movsx84_rr: return append_format(builder, "movsxd {}, {}", part8b(i.movsx84_rr.d), part4b(i.movsx84_rr.s));

		case movzx21_rr: return append_format(builder, "movzx {}, {}", part2b(i.movsx21_rr.d), part1b(i.movsx21_rr.s));
		case movzx41_rr: return append_format(builder, "movzx {}, {}", part4b(i.movsx41_rr.d), part1b(i.movsx41_rr.s));
		case movzx81_rr: return append_format(builder, "movzx {}, {}", part8b(i.movsx81_rr.d), part1b(i.movsx81_rr.s));
		case movzx42_rr: return append_format(builder, "movzx {}, {}", part4b(i.movsx42_rr.d), part2b(i.movsx42_rr.s));
		case movzx82_rr: return append_format(builder, "movzx {}, {}", part8b(i.movsx82_rr.d), part2b(i.movsx82_rr.s));
		case movzx84_rr: return append_format(builder, "mov {}, {}", part4b(i.movsx84_rr.d), part4b(i.movsx84_rr.s));

		case movsx21_rm: return append_format(builder, "movsx {}, byte {}",  part2b(i.movsx21_rm.d), i.movsx21_rm.s);
		case movsx41_rm: return append_format(builder, "movsx {}, byte {}",  part4b(i.movsx41_rm.d), i.movsx41_rm.s);
		case movsx81_rm: return append_format(builder, "movsx {}, byte {}",  part8b(i.movsx81_rm.d), i.movsx81_rm.s);
		case movsx42_rm: return append_format(builder, "movsx {}, word {}",  part4b(i.movsx42_rm.d), i.movsx42_rm.s);
		case movsx82_rm: return append_format(builder, "movsx {}, word {}",  part8b(i.movsx82_rm.d), i.movsx82_rm.s);
		case movsx84_rm: return append_format(builder, "movsxd {}, dword {}", part8b(i.movsx84_rm.d), i.movsx84_rm.s);

		case movzx21_rm: return append_format(builder, "movzx {}, byte {}",  part2b(i.movsx21_rm.d), i.movsx21_rm.s);
		case movzx41_rm: return append_format(builder, "movzx {}, byte {}",  part4b(i.movsx41_rm.d), i.movsx41_rm.s);
		case movzx81_rm: return append_format(builder, "movzx {}, byte {}",  part8b(i.movsx81_rm.d), i.movsx81_rm.s);
		case movzx42_rm: return append_format(builder, "movzx {}, word {}",  part4b(i.movsx42_rm.d), i.movsx42_rm.s);
		case movzx82_rm: return append_format(builder, "movzx {}, word {}",  part8b(i.movsx82_rm.d), i.movsx82_rm.s);
		case movzx84_rm: return append_format(builder, "mov {}, dword {}", part4b(i.movsx84_rm.d), i.movsx84_rm.s);
#if 0
		case push_used_registers: {
			umm ch = 0;
			if (i.push_used_registers.mask == -1) {
				ch += append(builder,
					//"push rax\n"
					"push rbx\n"
					//"push rcx\n"
					//"push rdx\n"
					"push rsi\n"
					"push rdi\n"
					//"push rsp\n"
					//"push rbp\n"
					//"push r8\n"
					//"push r9\n"
					//"push r10\n"
					//"push r11\n"
					"push r12\n"
					"push r13\n"
					"push r14\n"
					"push r15\n"
					"sub rsp, 8\n" // keep alignment
				);
				return ch;
			}

			for (u64 bit = 0; bit < sizeof(i.push_used_registers.mask) * 8; ++bit) {
				if ((i.push_used_registers.mask >> bit) & 1) {
					ch += append_format(builder, "push {}\n", (Register)bit);
				}
			}

			// keep the stack 16-byte aligned
			if (count_bits(i.push_used_registers.mask) & 1)
				ch += append_format(builder, "sub rsp, 8\n");
			return ch;
		}
		case pop_used_registers: {
			umm ch = 0;
			if (i.pop_used_registers.mask == -1) {
				ch += append(builder,
					"add rsp, 8\n" // keep alignment
					"pop r15\n"
					"pop r14\n"
					"pop r13\n"
					"pop r12\n"
					//"pop r11\n"
					//"pop r10\n"
					//"pop r9\n"
					//"pop r8\n"
					//"pop rbp\n"
					//"pop rsp\n"
					"pop rdi\n"
					"pop rsi\n"
					//"pop rdx\n"
					//"pop rcx\n"
					"pop rbx\n"
					//"pop rax\n"
				);
				return ch;
			}

			// keep the stack 16-byte aligned
			if (count_bits(i.push_used_registers.mask) & 1)
				ch += append_format(builder, "add rsp,8\n");

			for (u64 bit = sizeof(i.push_used_registers.mask) * 8 - 1; bit != ~0; --bit) {
				if ((i.push_used_registers.mask >> bit) & 1) {
					ch += append_format(builder, "pop {}\n", (Register)bit);
				}
			}
			return ch;
		}
#endif
		case xchg_rr: return append_format(builder, "xchg {},{}", i.xchg_rr.a, i.xchg_rr.b);
		case xchg1_mr: return append_format(builder, "xchg {},{}", i.xchg1_mr.a, part1b(i.xchg1_mr.b));
		case xchg2_mr: return append_format(builder, "xchg {},{}", i.xchg2_mr.a, part2b(i.xchg2_mr.b));
		case xchg4_mr: return append_format(builder, "xchg {},{}", i.xchg4_mr.a, part4b(i.xchg4_mr.b));
		case xchg8_mr: return append_format(builder, "xchg {},{}", i.xchg8_mr.a, part8b(i.xchg8_mr.b));
		case debug_start_lambda:
		case debug_line:
			return 0;

		case begin_lambda: {
			REDECLARE_REF(i, i.begin_lambda);

			auto lambda = i.lambda;

			auto begin_tlang = [&] {
				append_format(builder,
					"push rbp\n"
					"mov rbp, rsp\n"
					"mov rax, rsp\n"
					"and rax, 15\n"
					"test rax, rax\n"
					"jz .s{}\n"
					"int3; STACK WAS NOT ALIGNED TO 16 BYTES WHEN ENTERING THIS PROCEDURE\n"
					".s{}:\n", FormatInt{.value=(u64)i.lambda, .radix=62}, FormatInt{.value=(u64)i.lambda, .radix=62}
				);

				u32 total_bytes_pushed = 0;

				saved_registers_size = 0;

				for_each(lambda->used_registers, [&](umm bit) {
					append_format(builder, "push {}\n", (Register)bit);
					total_bytes_pushed += 8;
					saved_registers_size += 8;
				});


				auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;
				if (used_bytes >= 4096) {
					append_format(builder, "mov rax, {}\ncall _ps\n", used_bytes);
				}

				if (used_bytes) {
					append_format(builder, "sub rsp, {}; reserve {} bytes for locals, {} for temporary storage and {} for call arguments\n", used_bytes, lambda->locals_size, lambda->temporary_size, lambda->max_stack_space_used_for_call);
					total_bytes_pushed += used_bytes;
				}

				// keep the stack 16-byte aligned
				if (total_bytes_pushed % 16 != 0) {
					append_format(builder, "sub rsp, 8\n");
					total_bytes_pushed += 8;
				}

				assert(total_bytes_pushed % 16 == 0);

				append_format(builder,
					"mov rax, rsp\n"
					"and rax, 15\n"
					"test rax, rax\n"
					"jz .s{}\n"
					"int3; STACK WAS NOT ALIGNED TO 16 BYTES AFTER RESERVING STACK SPACE\n"
					".s{}:\n", FormatInt{.value=(u64)i.lambda + 1, .radix=62}, FormatInt{.value=(u64)i.lambda + 1, .radix=62}
				);

				temporary_offset = -(saved_registers_size + lambda->temporary_size);
				locals_offset    = -(saved_registers_size + lambda->temporary_size + lambda->locals_size);
				parameters_size  = lambda->parameters_size;
			};

			switch (lambda->convention) {
				case CallingConvention::tlang: {
					begin_tlang();
					break;
				}
				case CallingConvention::stdcall: {
					auto param_size = (s64)lambda->parameters.count * 8;
					auto return_size = 8;

					s64 total_bytes_will_be_pushed = param_size + return_size;

					auto param_offset = 16; // return address and return value

					if (total_bytes_will_be_pushed % 16 == 0) {
						append(builder, "sub rsp, 8\n");  // keep alignment
						total_bytes_will_be_pushed += 8;
						param_offset += 8;
					}

					append(builder, "sub rsp, 8; reseve return parameter space\n");

					if (lambda->parameters.count > 0) append(builder, "push rcx\n");
					if (lambda->parameters.count > 1) append(builder, "push rdx\n");
					if (lambda->parameters.count > 2) append(builder, "push r8\n");
					if (lambda->parameters.count > 3) append(builder, "push r9\n");

					for (umm i = 4; i < lambda->parameters.count; ++i) {
						append_format(builder, "push qword[rsp + {}]\n", (i * 2) * 8 + param_offset);
					}

					append(builder, "push 0xDEADC0D; dummy return address\n");

					begin_tlang();

					break;
				}
			}
			return 0;
		}
		case end_lambda: {
			REDECLARE_REF(i, i.end_lambda);
			auto lambda = i.lambda;

			auto end_tlang = [&] {
				auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;

				u32 total_bytes_popped = 0;

				// keep the stack 16-byte aligned
				if ((lambda->used_registers.count() * 8 + used_bytes) % 16 != 0) {
					used_bytes += 8;
				}
				if (used_bytes) {
					append_format(builder, "add rsp, {}; remove space for locals, temporary storage and call arguments\n", used_bytes);
					total_bytes_popped += used_bytes;
				}

				for_each<ForEach_reverse>(lambda->used_registers, [&](umm bit) {
					append_format(builder, "pop {}\n", (Register)bit);
					total_bytes_popped += 8;
				});

				assert(total_bytes_popped % 16 == 0);

				append(builder, "mov rsp, rbp\npop rbp\n");
			};

			switch (lambda->convention) {
				case CallingConvention::tlang: {
					end_tlang();
					append(builder, "ret");
					break;
				}
				case CallingConvention::stdcall: {
					end_tlang();

					auto param_size = (s64)lambda->parameters.count * 8;
					auto return_size = 8;
					s64 total_bytes_were_pushed = param_size + return_size;


					append(builder, "add rsp,8; pop dummy return address\n");


					append_format(builder, "add rsp,{}\n", param_size);

					append(builder, "pop rax\n");

					if (total_bytes_were_pushed % 16 == 0) {
						append(builder, "add rsp, 8\n");  // keep alignment
					}
					append(builder, "ret");
					break;
				}
			}
			return 0;
		}

		default:invalid_code_path();
	}
	invalid_code_path();
	return 0;
}

}
