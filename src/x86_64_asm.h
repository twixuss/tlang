#pragma once
#include "x86_64.h"

namespace x86_64 {

inline List<AstLambda *> lambda_stack;
inline AstLambda *current_lambda() { return lambda_stack.back(); }
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

inline auto instruction_address(s64 val) { return FormatInt<s64>{.value=val, .radix=62}; }

}

namespace tl {

inline umm append(StringBuilder&builder,x86_64::Register64 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register32 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register16 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86_64::Register8  r){return append(builder,as_string(r));}

inline umm append(StringBuilder &builder, Address a) {
	using namespace x86_64;
	umm result = 0;
	result += append(builder, '[');
	switch (a.base.v) {
		case Registers::locals.v:
			a.base = Registers::rb;
			a.c += locals_offset;
			break;
		case Registers::temporary.v:
			a.base = Registers::rb;
			a.c += temporary_offset;
			break;
		case Registers::parameters.v:
			a.base = Registers::rb;
			a.c = parameters_size - a.c + 8;
			break;
		case Registers::return_parameters.v:
			a.base = Registers::rb;
			a.c += parameters_size + 16;
			break;
	}
	switch (a.base.v) {
		case Registers::constants   .v: result += append(builder, "rel constants"); break;
		case Registers::rwdata      .v: result += append(builder, "rel rwdata"); break;
		case Registers::zeros       .v: result += append(builder, "rel zeros"); break;
		case Registers::instructions.v: return append_format(builder, "rel .{}]", instruction_address(a.c));
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

inline umm append(StringBuilder &builder, Register r) {
	using namespace x86_64;
	return append(builder, to_x86_register(r));
}
inline umm append(StringBuilder &builder, XRegister r) {
	using enum XRegister;
	switch (r) {
		case x0: return append(builder, "xmm0");
		case x1: return append(builder, "xmm1");
		case x2: return append(builder, "xmm2");
		case x3: return append(builder, "xmm3");
	}
	invalid_code_path();
	return {};
}
}

namespace x86_64 {

inline umm append_instruction(StringBuilder &builder, s64 idx, Instruction i) {
	using enum Register64;

	switch (i.kind) {
		using enum InstructionKind;
		case mov_rr: return append_format(builder, "mov {},{}", i.mov_rr.d, i.mov_rr.s);
		case mov_rc: return append_format(builder, "mov {},{}", i.mov_rc.d, i.mov_rc.s);

		case mov1_mc: return append_format(builder, "mov byte{},{}", i.mov1_mc.d, i.mov1_mc.s);
		case mov2_mc: return append_format(builder, "mov word{},{}", i.mov2_mc.d, i.mov2_mc.s);
		case mov4_mc: return append_format(builder, "mov dword{},{}", i.mov4_mc.d, i.mov4_mc.s);
		case mov8_mc: return append_format(builder, "mov qword{},{}", i.mov8_mc.d, i.mov8_mc.s);

		case mov1_rm: return append_format(builder, "mov {},{}", part1b(i.mov1_rm.d), i.mov1_rm.s);
		case mov2_rm: return append_format(builder, "mov {},{}", part2b(i.mov2_rm.d), i.mov2_rm.s);
		case mov4_rm: return append_format(builder, "mov {},{}", part4b(i.mov4_rm.d), i.mov4_rm.s);
		case mov8_rm: return append_format(builder, "mov {},{}", part8b(i.mov8_rm.d), i.mov8_rm.s);

		case mov1_mr: return append_format(builder, "mov {},{}", i.mov1_mr.d, part1b(i.mov1_mr.s));
		case mov2_mr: return append_format(builder, "mov {},{}", i.mov2_mr.d, part2b(i.mov2_mr.s));
		case mov4_mr: return append_format(builder, "mov {},{}", i.mov4_mr.d, part4b(i.mov4_mr.s));
		case mov8_mr: return append_format(builder, "mov {},{}", i.mov8_mr.d, part8b(i.mov8_mr.s));

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
		case push_f: return append_format(builder, "movq rbx,{}\npush rbx", i.push_f.s);
		case push_m: return append_format(builder, "push qword{}", i.push_m.s);

		case pop_r: return append_format(builder, "pop {}", i.pop_r.d);
		case pop_f: return append_format(builder, "pop rbx\nmovq {}, rbx", i.pop_f.d);

		case ret: return append_format(builder, "ret");

		case shl_rc: return append_format(builder, "shl {},{}", i.shl_rc.d, i.shl_rc.s);
		case shl_rr: return append_format(builder, "mov cl,{}\nshl {},cl", part1b(i.shl_rr.s), i.shl_rr.d);
		case shl_mr: return append_format(builder, "mov cl,{}\nshl qword{},cl", part1b(i.shl_mr.s), i.shl_mr.d);

		case shr_rc: return append_format(builder, "shr {}, {}", i.shr_rc.d, i.shr_rc.s);
		case shr_rr: return append_format(builder, "mov cl, {}\nshr {}, cl", part1b(i.shr_rr.s), i.shr_rr.d);
		case shr_mr: return append_format(builder, "mov cl, {}\nshr qword {}, cl", part1b(i.shr_mr.s), i.shr_mr.d);

		case add_rc: return append_format(builder, "add {},{}"      , i.add_rc.d, i.add_rc.s);
		case add_rr: return append_format(builder, "add {},{}"      , i.add_rr.d, i.add_rr.s);
		case add_mc: return append_format(builder, "add qword {},{}", i.add_mc.d, i.add_mc.s);
		case add_mr: return append_format(builder, "add qword {},{}", i.add_mr.d, i.add_mr.s);

		case sub_rc: return append_format(builder, "sub {},{}"      , i.sub_rc.d, i.sub_rc.s);
		case sub_rr: return append_format(builder, "sub {},{}"      , i.sub_rr.d, i.sub_rr.s);
		case sub_mc: return append_format(builder, "sub qword {},{}", i.sub_mc.d, i.sub_mc.s);
		case sub_mr: return append_format(builder, "sub qword {},{}", i.sub_mr.d, i.sub_mr.s);

		case mul_rc: return append_format(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s);
		case mul_rr: return append_format(builder, "imul {}, {}", i.mul_rr.d, i.mul_rr.s);
		case mul_mr: return append_format(builder, "imul {}, qword {}\nmov qword {}, {}", i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s);

			//  DIV - Unsigned divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
			// IDIV -   Signed divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
		case div_mr:
			if (to_x86_register(i.div_mr.s) == rdx) {
				return append_format(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div rbx\n"
"mov qword {}, rax\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
			} else {
				return append_format(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div {}\n"
"mov qword {}, rax\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.s, i.div_mr.d);
			}
			break;
		case mod_mr:
			if (to_x86_register(i.div_mr.s) == rdx) {
				return append_format(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div rbx\n"
"mov qword {}, rdx\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
			} else {
				return append_format(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div {}\n"
"mov qword {}, rdx\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.s, i.div_mr.d);
			}
			break;


		case div_rr:
			return append_format(builder,
"push rdx\n"
"xor rdx,rdx\n"
"mov rax,{}\n"
"div {}\n"
"mov {},rax\n"
"pop rdx", i.div_rr.d, i.div_rr.s, i.div_rr.d);
			break;

		case mod_rr:
			return append_format(builder,
"push rdx\n"
"xor rdx,rdx\n"
"mov rax,{}\n"
"div {}\n"
"mov {},rdx\n"
"pop rdx", i.mod_rr.d, i.mod_rr.s, i.mod_rr.d);
			break;

		case negi_r: return append_format(builder, "neg {}", i.negi_r.d);
		case negi8_m:  return append_format(builder, "neg byte {}",  i.negi8_m.d);
		case negi16_m: return append_format(builder, "neg word {}",  i.negi16_m.d);
		case negi32_m: return append_format(builder, "neg dword {}", i.negi32_m.d);
		case negi64_m: return append_format(builder, "neg qword {}", i.negi64_m.d);

		case or_rr: return append_format(builder, "or {},{}", i. or_rr.d, i. or_rr.s);
		case or_mr: return append_format(builder, "or qword{},{}", i. or_mr.d, i. or_mr.s);

		case and_rc: return append_format(builder, "and {},{}", i.and_rc.d, i.and_rc.s);
		case and_rr: return append_format(builder, "and {},{}", i.and_rr.d, i.and_rr.s);
		case and_mc: {
			if (~i.and_mc.s & 0xffffffff00000000) {
				umm ch = 0;
				auto l = (s32)i.and_mc.s;
				auto h = (s32)((u64)i.and_mc.s >> 32);
				ch += append_format(builder, "and dword {}, {}\n"  , i.and_mc.d, l);
				auto addr = i.and_mc.d;
				addr.c += 4;
				ch += append_format(builder, "and dword {}, {}", addr, h);
				return ch;
			} else {
				return append_format(builder, "and qword {}, {}", i.and_mc.d, (s32)i.and_mc.s);
			}
			break;
		}
		case and_mr: return append_format(builder, "and qword {}, {}", i.and_mr.d, i.and_mr.s);

		case xor_rr: return append_format(builder, "xor {}, {}"        , i.xor_rr.d, i.xor_rr.s);
		case xor_mr: return append_format(builder, "xor qword {}, {}", i.xor_mr.d, i.xor_mr.s);

		case cmps1: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps1.d, i.cmps1.d, part1b(i.cmps1.a), part1b(i.cmps1.b), cmps_string(i.cmps1.c), part1b(i.cmps1.d));
		case cmps2: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps2.d, i.cmps2.d, part2b(i.cmps2.a), part2b(i.cmps2.b), cmps_string(i.cmps2.c), part1b(i.cmps2.d));
		case cmps4: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps4.d, i.cmps4.d, part4b(i.cmps4.a), part4b(i.cmps4.b), cmps_string(i.cmps4.c), part1b(i.cmps4.d));
		case cmps8: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps8.d, i.cmps8.d, part8b(i.cmps8.a), part8b(i.cmps8.b), cmps_string(i.cmps8.c), part1b(i.cmps8.d));
		case cmpu1: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d));
		case cmpu2: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d));
		case cmpu4: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d));
		case cmpu8: return append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu8.d, i.cmpu8.d, part8b(i.cmpu8.a), part8b(i.cmpu8.b), cmpu_string(i.cmpu8.c), part1b(i.cmpu8.d));

		case jz_cr:  { auto reg = part1b(i.jz_cr.reg); return append_format(builder, "test {}, {}\njz .{}", reg, reg, instruction_address(idx + i.jz_cr.offset)); }
		case jnz_cr: { auto reg = part1b(i.jnz_cr.reg); return append_format(builder, "test {}, {}\njnz .{}", reg, reg, instruction_address(idx + i.jnz_cr.offset)); }

		case cmpf1: return append_format(builder, "cmp {}, {}", part1b(i.cmpf1.a), part1b(i.cmpf1.b));
		case cmpf2: return append_format(builder, "cmp {}, {}", part2b(i.cmpf2.a), part2b(i.cmpf2.b));
		case cmpf4: return append_format(builder, "cmp {}, {}", part4b(i.cmpf4.a), part4b(i.cmpf4.b));
		case cmpf8: return append_format(builder, "cmp {}, {}", part8b(i.cmpf8.a), part8b(i.cmpf8.b));

		case jef_c:  { return append_format(builder, "je .{}",  instruction_address(idx + i.jef_c .offset)); }
		case jnef_c: { return append_format(builder, "jne .{}", instruction_address(idx + i.jnef_c.offset)); }
		case jlf_c:  { return append_format(builder, "jl .{}",  instruction_address(idx + i.jlf_c .offset)); }
		case jgf_c:  { return append_format(builder, "jg .{}",  instruction_address(idx + i.jgf_c .offset)); }
		case jlef_c: { return append_format(builder, "jle .{}", instruction_address(idx + i.jlef_c.offset)); }
		case jgef_c: { return append_format(builder, "jge .{}", instruction_address(idx + i.jgef_c.offset)); }

		case jmp: return append_format(builder, "jmp .{}", instruction_address(idx + i.jmp.offset));

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
		case setf_mcc: return append_format(builder, "lea rdi, {}\nmov al, {}\nmov rcx, {}\nrep stosb", i.setf_mcc.d, i.setf_mcc.s, i.setf_mcc.size);
		case setb_mcc: return append_format(builder, "lea rdi, {}\nmov al, {}\nmov rcx, {}\nadd rdi, {}\nstd\nrep stosb\ncld", i.setb_mcc.d, i.setb_mcc.s, i.setb_mcc.size, i.setb_mcc.size-1);

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
			return append_format(builder, "call .{}", instruction_address(i.constant));
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
				case CallingConvention::stdcall: return append_format(builder, "mov rax, {}\nmov rbx, {}\ncall ._stdcall", i.s, i.lambda->parameters_size);
				default: invalid_code_path();
			}
			break;
		}
		case call_m: {
			REDECLARE_REF(i, i.call_m);
			switch (i.lambda->convention) {
				case CallingConvention::tlang:  return append_format(builder, "call qword {}", i.s);
				case CallingConvention::stdcall: return append_format(builder, "mov rax, {}\nmov rbx, {}\ncall ._stdcall", i.s, i.lambda->parameters_size);
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

		case mov_fr: return append_format(builder, "movq {}, {}", i.mov_fr.d, i.mov_fr.s);
	    case mov_rf: return append_format(builder, "movq {}, {}", i.mov_rf.d, i.mov_rf.s);

		case mov1_xm: return append_format(builder, "movb {}, byte {}", i.mov1_xm.d, i.mov1_xm.s);
		case mov2_xm: return append_format(builder, "movw {}, word {}", i.mov2_xm.d, i.mov2_xm.s);
		case mov4_xm: return append_format(builder, "movd {}, dword {}", i.mov4_xm.d, i.mov4_xm.s);
		case mov8_xm: return append_format(builder, "movq {}, qword {}", i.mov8_xm.d, i.mov8_xm.s);

	    case add_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\naddss xmm6, xmm7\nmovd {}, xmm6", part4b(i.add_ff.d), part4b(i.add_ff.s), part4b(i.add_ff.d));
	    case sub_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\nsubss xmm6, xmm7\nmovd {}, xmm6", part4b(i.sub_ff.d), part4b(i.sub_ff.s), part4b(i.sub_ff.d));
	    case mul_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\nmulss xmm6, xmm7\nmovd {}, xmm6", part4b(i.mul_ff.d), part4b(i.mul_ff.s), part4b(i.mul_ff.d));
	    case div_ff: return append_format(builder, "movd xmm6, {}\nmovd xmm7, {}\ndivss xmm6, xmm7\nmovd {}, xmm6", part4b(i.div_ff.d), part4b(i.div_ff.s), part4b(i.div_ff.d));

	    case xor_ff: return append_format(builder, "xorps {}, {}", i.xor_ff.d, i.xor_ff.s);

		case tobool_r:    { auto d = part1b(i.tobool_r.d);    return append_format(builder, "test {}, {}\nsetnz {}", d, d, d); }
		case toboolnot_r: { auto d = part1b(i.toboolnot_r.d); return append_format(builder, "test {}, {}\nsetz {}" , d, d, d); }
		case not_r: return append_format(builder, "not {}", i.not_r.d);
		case not_m: return append_format(builder, "not {}", i.not_m.d);
		case jmp_label:
		case noop:
			return 0;

		case debug_break: return append(builder, "int3"s);

		case movsx21_rr: return append_format(builder, "movsx {}, {}", part2b(i.movsx21_rr.d), part1b(i.movsx21_rr.s));
		case movsx41_rr: return append_format(builder, "movsx {}, {}", part4b(i.movsx41_rr.d), part1b(i.movsx41_rr.s));
		case movsx81_rr: return append_format(builder, "movsx {}, {}", part8b(i.movsx81_rr.d), part1b(i.movsx81_rr.s));
		case movsx42_rr: return append_format(builder, "movsx {}, {}", part4b(i.movsx42_rr.d), part2b(i.movsx42_rr.s));
		case movsx82_rr: return append_format(builder, "movsx {}, {}", part8b(i.movsx82_rr.d), part2b(i.movsx82_rr.s));
		case movsx84_rr: return append_format(builder, "movsx {}, {}", part8b(i.movsx84_rr.d), part4b(i.movsx84_rr.s));

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
		case movsx84_rm: return append_format(builder, "movsx {}, dword {}", part8b(i.movsx84_rm.d), i.movsx84_rm.s);

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

			// TODO:
			//
			// [x ] push rbp
			// [x ] push used registers
			// [x ] touch stack pages (if necessary)
			// [  ] reserve temporary space
			// [  ] set rsp away from rbp
			// [ ] all of the above inverted in reverse for end_lambda
			//
			// patch:
			// [ ] locals
			// [ ] parameters
			// [ ] return parameters

			switch (lambda->convention) {
				case CallingConvention::tlang: {
					append_format(builder, "push rbp\nmov rbp, rsp\n");

					saved_registers_size = 0;

					for_each(lambda->used_registers, [&](umm bit) {
						append_format(builder, "push {}\n", (Register)bit);
						saved_registers_size += 8;
					});

					// keep the stack 16-byte aligned
					if (lambda->used_registers.count() & 1) {
						append_format(builder, "sub rsp, 8\n");
						saved_registers_size += 8;
					}


					auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;
					if (used_bytes >= 4096) {
						append_format(builder, "mov rax, {}\ncall ._ps\n", used_bytes);
					}

					if (used_bytes)
						append_format(builder, "sub rsp, {}; reserve space for locals, temporary storage and call arguments\n", used_bytes);

					temporary_offset = -(saved_registers_size + lambda->temporary_size);
					locals_offset    = -(saved_registers_size + lambda->temporary_size + lambda->locals_size);
					parameters_size  = lambda->parameters_size;

					//not_implemented();
					break;
				}
				case CallingConvention::stdcall: not_implemented();
			}
			return 0;
		}
		case end_lambda: {
			REDECLARE_REF(i, i.end_lambda);
			auto lambda = i.lambda;
			switch (lambda->convention) {
				case CallingConvention::tlang: {

					auto used_bytes = lambda->locals_size + lambda->temporary_size + lambda->max_stack_space_used_for_call;

					// keep the stack 16-byte aligned
					if (lambda->used_registers.count() & 1) {
						used_bytes += 8;
					}
					if (used_bytes)
						append_format(builder, "add rsp, {}; remove space for locals, temporary storage and call arguments\n", used_bytes);

					for_each(lambda->used_registers, [&](umm bit) {
						append_format(builder, "pop {}\n", (Register)bit);
					});

					append_format(builder, "mov rsp, rbp\npop rbp\nret");
					break;
				}
				case CallingConvention::stdcall: not_implemented();
			}
			return 0;
		}

		default:invalid_code_path();
	}
	invalid_code_path();
	return 0;
}

}
