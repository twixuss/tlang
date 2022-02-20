#pragma once
#include <ast.h>
#include <bytecode.h>

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

enum class Register64{rax,rbx,rcx,rdx,rsi,rdi,rsp,rbp, r8, r9, r10, r11, r12, r13, r14, r15 };
enum class Register32{eax,ebx,ecx,edx,esi,edi,esp,ebp, r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d};
enum class Register16{ ax, bx, cx, dx, si, di, sp, bp, r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w};
enum class Register8 { al, bl, cl, dl, sil,dil,spl,bpl,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b};

#define C(x) case x: return u8#x##s

static Span<utf8> as_string(Register64 r){using enum Register64;switch(r){C(rax);C(rbx);C(rcx);C(rdx);C(rsi );C(rdi );C(rsp );C(rbp );C(r8 );C(r9 );C(r10 );C(r11 );C(r12 );C(r13 );C(r14 );C(r15 );}invalid_code_path();return{};}
static Span<utf8> as_string(Register32 r){using enum Register32;switch(r){C(eax);C(ebx);C(ecx);C(edx);C(esi );C(edi );C(esp );C(ebp );C(r8d);C(r9d);C(r10d);C(r11d);C(r12d);C(r13d);C(r14d);C(r15d);}invalid_code_path();return{};}
static Span<utf8> as_string(Register16 r){using enum Register16;switch(r){C( ax);C( bx);C( cx);C( dx);C( si );C( di );C( sp );C( bp );C(r8w);C(r9w);C(r10w);C(r11w);C(r12w);C(r13w);C(r14w);C(r15w);}invalid_code_path();return{};}
static Span<utf8> as_string(Register8  r){using enum Register8 ;switch(r){C( al);C( bl);C( cl);C( dl);C( sil);C( dil);C( spl);C( bpl);C(r8b);C(r9b);C(r10b);C(r11b);C(r12b);C(r13b);C(r14b);C(r15b);}invalid_code_path();return{};}

#undef C

static umm append(StringBuilder&builder,Register64 r){return append(builder,as_string(r));}
static umm append(StringBuilder&builder,Register32 r){return append(builder,as_string(r));}
static umm append(StringBuilder&builder,Register16 r){return append(builder,as_string(r));}
static umm append(StringBuilder&builder,Register8  r){return append(builder,as_string(r));}


// Microsoft 64 bit calling convention - saved registers
// -------------------------------------
// | reg | volatile | used in bytecode |
// -------------------------------------
// | rax |    +     |                  |
// | rbx |          |                  | used to save rdx in div/idiv
// | rcx |    +     | +                |
// | rdx |    +     | +                |
// | rsi |          |                  |
// | rdi |          |                  |
// | rsp |          | +                |
// | rbp |          | +                |
// | r8  |    +     | +                |
// | r9  |    +     | +                |
// | r10 |    +     |                  |
// | r11 |    +     |                  |
// | r12 |          | +                |
// | r13 |          | +                |
// | r14 |          | +                |
// | r15 |          | +                |
// -------------------------------------

#define C(a, b) case Register::a: return Register64::b

inline static constexpr Register64 convert(Register r) {
	switch (r) {    // volatile //
		C(r0, rcx); //    +     //
		C(r1, rdx); //    +     //
		C(r2, r8);  //    +     //
		C(r3, r9);  //    +     //
		C(r4, r12); //          //
		C(r5, r13); //          //
		C(r6, r14); //          //
		C(r7, r15); //          //
		C(rs, rsp); //          //
		C(rb, rbp); //          //
	}
	invalid_code_path();
	return {};
}

#undef C

static umm append(StringBuilder &builder, Register r) {
	return append(builder, convert(r));
}

inline static constexpr Register64 part8b(Register64 r) { return r; }
inline static constexpr Register32 part4b(Register64 r) { return (Register32)r; }
inline static constexpr Register16 part2b(Register64 r) { return (Register16)r; }
inline static constexpr Register8  part1b(Register64 r) { return (Register8 )r; }

inline static constexpr Register64 part8b(Register r) { return part8b(convert(r)); }
inline static constexpr Register32 part4b(Register r) { return part4b(convert(r)); }
inline static constexpr Register16 part2b(Register r) { return part2b(convert(r)); }
inline static constexpr Register8  part1b(Register r) { return part1b(convert(r)); }


static umm append(StringBuilder &builder, Address a) {
	umm result = 0;
	result += append(builder, '[');
	result += append(builder, a.base);
	if (a.r1_scale) {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			result += append(builder, '+');
			result += append(builder, a.r1);
			result += append(builder, '*');
			result += append(builder, a.r1_scale);
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

static umm append(StringBuilder &builder, XRegister r) {
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

inline auto instruction_address(s64 val) { return FormatInt<s64>{.value=val, .radix=62}; }
inline void move_stdcall_registers() {
	// There is no need in this now because values go into right registers immediately
	// append(builder, "mov rcx, r8\nmov rdx, r9\nmov r8, r10\nmov r9, r11\n");
}

inline void append_instruction(StringBuilder &builder, s64 idx, Instruction i) {
	using enum Register64;
	switch (i.kind) {
		using enum InstructionKind;
		case mov_rr: append_format(builder, "mov {}, {}", i.mov_rr.d, i.mov_rr.s); break;
		case mov_rc: append_format(builder, "mov {}, {}", i.mov_rc.d, i.mov_rc.s); break;

		case mov1_mc: append_format(builder, "mov byte {}, {}", i.mov1_mc.d, i.mov1_mc.s); break;
		case mov2_mc: append_format(builder, "mov word {}, {}", i.mov2_mc.d, i.mov2_mc.s); break;
		case mov4_mc: append_format(builder, "mov dword {}, {}", i.mov4_mc.d, i.mov4_mc.s); break;
		case mov8_mc: append_format(builder, "mov qword {}, {}", i.mov8_mc.d, i.mov8_mc.s); break;

		case mov1_rm: append_format(builder, "mov {}, byte {}", part1b(i.mov1_rm.d), i.mov1_rm.s); break;
		case mov2_rm: append_format(builder, "mov {}, word {}", part2b(i.mov2_rm.d), i.mov2_rm.s); break;
		case mov4_rm: append_format(builder, "mov {}, dword {}", part4b(i.mov4_rm.d), i.mov4_rm.s); break;
		case mov8_rm: append_format(builder, "mov {}, qword {}", part8b(i.mov8_rm.d), i.mov8_rm.s); break;

		case mov1_mr: append_format(builder, "mov byte {}, {}", i.mov1_mr.d, part1b(i.mov1_mr.s)); break;
		case mov2_mr: append_format(builder, "mov word {}, {}", i.mov2_mr.d, part2b(i.mov2_mr.s)); break;
		case mov4_mr: append_format(builder, "mov dword {}, {}", i.mov4_mr.d, part4b(i.mov4_mr.s)); break;
		case mov8_mr: append_format(builder, "mov qword {}, {}", i.mov8_mr.d, part8b(i.mov8_mr.s)); break;

		case push_r: append_format(builder, "push {}", i.push_r.s); break;
		case push_c: {
			if (min_value<s32> <= i.push_c.s && i.push_c.s <= max_value<s32>) {
				append_format(builder, "push qword {}", (s32)i.push_c.s);
			} else {
				//if (i.push_c.s & 0x80000000) {
					append_format(builder,
						"sub rsp, 8\nmov dword [rsp], {}\nmov dword [rsp+4], {}",
						(s32)i.push_c.s       ,
						(s32)(i.push_c.s >> 32)
					);
				//} else {
				//	append_format(builder,
				//		"push dword {}\nmov dword [rsp+4], {}",
				//		(s32)i.push_c.s       ,
				//		(s32)(i.push_c.s >> 32)
				//	);
				//}
			}
			break;
		}
		case push_m: append_format(builder, "push        qword {}"           , i.push_m.s); break;

		case push_a: append_format(builder, "mov rax, constants + {}\npush rax", i.push_a.s); break;
		case push_d: append_format(builder, "mov rax, data + {}\npush rax"     , i.push_d.s); break;
		case push_u: append_format(builder, "mov rax, zeros + {}\npush rax"    , i.push_u.s); break;
		case push_t: append_format(builder, "mov rax, .{}\npush rax"           , instruction_address(i.push_t.s)); break;

		case mov_ra: append_format(builder, "mov {}, constants + {}", i.mov_ra.d, i.mov_ra.s); break;
		case mov_rd: append_format(builder, "mov {}, rwdata + {}"   , i.mov_rd.d, i.mov_rd.s); break;
		case mov_ru: append_format(builder, "mov {}, zeros + {}"    , i.mov_ru.d, i.mov_ru.s); break;
		case mov_rt: append_format(builder, "mov {}, .{}"           , i.mov_rt.d, instruction_address(i.mov_rt.s)); break;

		case pop_r: append_format(builder, "pop {}", i.pop_r.d); break;

		case ret: append_format(builder, "ret"); break;

		case shl_rc: append_format(builder, "shl {}, {}", i.shl_rc.d, i.shl_rc.s); break;
		case shl_mr: append_format(builder, "mov cl, {}\nshl qword {}, cl", part1b(i.shl_mr.s), i.shl_mr.d); break;

		case shr_rc: append_format(builder, "shr {}, {}", i.shr_rc.d, i.shr_rc.s); break;
		case shr_mr: append_format(builder, "mov cl, {}\nshr qword {}, cl", part1b(i.shr_mr.s), i.shr_mr.d); break;

		case add_rc: append_format(builder, "add {}, {}"        , i.add_rc.d, i.add_rc.s); break;
		case add_rr: append_format(builder, "add {}, {}"        , i.add_rr.d, i.add_rr.s); break;
		case add_mc: append_format(builder, "add qword {}, {}", i.add_mc.d, i.add_mc.s); break;
		case add_mr: append_format(builder, "add qword {}, {}", i.add_mr.d, i.add_mr.s); break;

		case sub_rc: append_format(builder, "sub {}, {}"        , i.sub_rc.d, i.sub_rc.s); break;
		case sub_rr: append_format(builder, "sub {}, {}"        , i.sub_rr.d, i.sub_rr.s); break;
		case sub_mc: append_format(builder, "sub qword {}, {}", i.sub_mc.d, i.sub_mc.s); break;
		case sub_mr: append_format(builder, "sub qword {}, {}", i.sub_mr.d, i.sub_mr.s); break;

		case mul_rc: append_format(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s); break;
		case mul_mr: append_format(builder, "imul {}, qword {}\nmov qword {}, {}", i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s); break;

			//  DIV - Unsigned divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
			// IDIV -   Signed divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
		case div_mr:
			if (convert(i.div_mr.s) == rdx) {
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
			if (convert(i.div_mr.s) == rdx) {
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
		case cmps8: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps8.d, i.cmps8.d, part8b(i.cmps8.a), part8b(i.cmps8.b), cmps_string(i.cmps8.c), part1b(i.cmps8.d)); break;
		case cmpu1: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d)); break;
		case cmpu2: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d)); break;
		case cmpu4: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d)); break;
		case cmpu8: append_format(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu8.d, i.cmpu8.d, part8b(i.cmpu8.a), part8b(i.cmpu8.b), cmpu_string(i.cmpu8.c), part1b(i.cmpu8.d)); break;

		case popcall: append(builder, "pop rax\ncall rax"); break;
		case call_constant: append_format(builder, "call .{}", instruction_address(i.call_constant.constant)); break;
		case call_string:   append_format(builder, "call {}", i.call_string.string); break;

		case jmp: append_format(builder, "jmp .{}", instruction_address(idx + i.jmp.offset)); break;

		case jz: append_format(builder, "test {}, {}\njz .{}", i.jz.reg, i.jz.reg, instruction_address(idx + i.jz.offset)); break;

			// Here move into rcx must be last, because it can be source for rdi or rsi
		case copyf_mmc: append_format(builder, "mov rsi, {}\nmov rdi, {}\nmov rcx, {}\ncld\nrep movsb", i.copyf_mmc.s, i.copyf_mmc.d, i.copyf_mmc.size); break;
		case copyb_mmc: append_format(builder, "lea rsi, [{} + {}]\nlea rdi, [{} + {}]\nmov rcx, {}\nstd\nrep movsb", i.copyb_mmc.s, i.copyb_mmc.size - 1, i.copyb_mmc.d, i.copyb_mmc.size - 1, i.copyb_mmc.size); break;
		case copyf_ssc: append_format(builder, "pop rsi\npop rdi\nmov rcx, {}\ncld\nrep movsb", i.copyf_ssc.size); break;
		case copyb_ssc: append_format(builder, "pop rsi\npop rdi\nadd rsi, {}\nadd rdi, {}\nmov rcx, {}\nstd\nrep movsb", i.copyb_ssc.size - 1, i.copyb_ssc.size - 1, i.copyb_ssc.size); break;
		case set_mcc: append_format(builder, "mov rdi, {}\nmov al, {}\nmov rcx, {}\ncld\nrep stosb", i.set_mcc.d, i.set_mcc.s, i.set_mcc.size); break;

		case stdcall_begin_lambda: {
			auto lambda = i.stdcall_begin_lambda.lambda;
			append_format(builder, "sub rsp, {}\n", ceil(get_size(lambda->return_parameter->type), 8ll));

			// push parameters
			if (lambda->parameters.count >= 4) append(builder, "push r9\n");
			if (lambda->parameters.count >= 3) append(builder, "push r8\n");
			if (lambda->parameters.count >= 2) append(builder, "push rdx\n");
			if (lambda->parameters.count >= 1) append(builder, "push rcx\n");

			assert(lambda->parameters.count <= 4, "not implemented");

			// Dummy return address
			append(builder, "push 0x12345678\npush rbp\nmov rbp, rsp");
			break;
		}
		case stdcall_end_lambda: {
			auto lambda = i.stdcall_end_lambda.lambda;
			// pop fake return address and parameters
			append_format(builder, "add rsp, {}\npop rax\nmov rsp, rbp\npop rbp", 8 + lambda->parameters_size);
			break;
		}

		case popstdcall:
			move_stdcall_registers();
			append(builder, "pop rax\ncall rax");
			break;
		case stdcall_constant: {
			move_stdcall_registers();
			append_format(builder, "call .{}", instruction_address(i.stdcall_constant.constant));
			break;
		}
		case stdcall_string: {
			move_stdcall_registers();
			append_format(builder, "call {}", i.stdcall_string.string);
			break;
		}

		case push_stdcall_result: append(builder, "push rax"); break;

		case stdcall_m:
			move_stdcall_registers();
			append_format(builder, "call qword {}", i.stdcall_m.s);
			break;

		case call_m:
			append_format(builder, "call qword {}", i.call_m.s);
			break;

		case lea:
			append_format(builder, "lea {}, {}", i.lea.d, i.lea.s);
			break;

		case cvtf64s64:
			append(builder, "cvtsd2si rax, [rsp]\nmov [rsp], rax");
			break;

		case mov_f64r: append_format(builder, "movq {}, {}", i.mov_f64r.d, i.mov_f64r.s); break;
	    case mov_rf64: append_format(builder, "movq {}, {}", i.mov_rf64.d, i.mov_rf64.s); break;

	    case add_f64: append_format(builder, "addsd {}, {}", i.add_f64.d, i.add_f64.s); break;
	    case sub_f64: append_format(builder, "subsd {}, {}", i.sub_f64.d, i.sub_f64.s); break;
	    case mul_f64: append_format(builder, "mulsd {}, {}", i.mul_f64.d, i.mul_f64.s); break;
	    case div_f64: append_format(builder, "divsd {}, {}", i.div_f64.d, i.div_f64.s); break;
		case tobool_r:    { auto d = part1b(i.tobool_r.d);    append_format(builder, "test {}, {}\nsetnz {}", d, d, d); break; }
		case toboolnot_r: { auto d = part1b(i.toboolnot_r.d); append_format(builder, "test {}, {}\nsetz {}" , d, d, d); break; }
		case noop: break;

		case dbgbrk: append(builder, "int3"s); break;

		default:invalid_code_path();
	}
}
