#pragma once
#include <ast.h>
#include <bytecode.h>

namespace x86_64 {

enum class Register64{rax,rbx,rcx,rdx,rsi,rdi,rsp,rbp, r8, r9, r10, r11, r12, r13, r14, r15 };
enum class Register32{eax,ebx,ecx,edx,esi,edi,esp,ebp, r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d};
enum class Register16{ ax, bx, cx, dx, si, di, sp, bp, r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w};
enum class Register8 { al, bl, cl, dl, sil,dil,spl,bpl,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b};

// Microsoft 64 bit calling convention - saved registers
// -------------------------------------
// | reg | volatile | used in bytecode |
// -------------------------------------
// | rax |    +     |                  |
// | rbx |          |                  | used for bunch of bytecode instructions, for example to save rdx in div/idiv, or for pushing/popping floats
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

inline static constexpr Register64 stdcall_int_registers[] {
	Register64::rcx,
	Register64::rdx,
	Register64::r8,
	Register64::r9,
};

inline static constexpr XRegister stdcall_float_registers[] {
	XRegister::x0,
	XRegister::x1,
	XRegister::x2,
	XRegister::x3,
};

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

#define REGISTER_MAP \
	C(r0, rcx) \
	C(r1, rdx) \
	C(r2, r8)  \
	C(r3, r9)  \
	C(r4, r12) \
	C(r5, r13) \
	C(r6, r14) \
	C(r7, r15) \
	C(r8, rax) \
	C(rs, rsp) \
	C(rb, rbp)

#define C(a, b) case Register::a: return Register64::b;

inline static constexpr Register64 to_x86_register(Register r) {
	switch (r) {
		REGISTER_MAP;
	}
	invalid_code_path();
	return {};
}

#undef C

#define C(a, b) case Register64::b: return Register::a;

inline static constexpr Register to_bc_register(Register64 r) {
	switch (r) {
		REGISTER_MAP;
	}
	invalid_code_path();
	return {};
}

#undef C
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
	result += append(builder, to_x86_register(a.base));
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

inline static constexpr Register64 part8b(Register64 r) { return r; }
inline static constexpr Register32 part4b(Register64 r) { return (Register32)r; }
inline static constexpr Register16 part2b(Register64 r) { return (Register16)r; }
inline static constexpr Register8  part1b(Register64 r) { return (Register8 )r; }

inline static constexpr Register64 part8b(Register r) { return part8b(to_x86_register(r)); }
inline static constexpr Register32 part4b(Register r) { return part4b(to_x86_register(r)); }
inline static constexpr Register16 part2b(Register r) { return part2b(to_x86_register(r)); }
inline static constexpr Register8  part1b(Register r) { return part1b(to_x86_register(r)); }


inline auto instruction_address(s64 val) { return FormatInt<s64>{.value=val, .radix=62}; }
inline void prepare_stdcall(AstLambda *lambda) {

}
template<size_t N>
struct StringLiteral {
    constexpr StringLiteral(const char (&str)[N]) {
        std::copy_n(str, N, value);
    }
	constexpr char const *begin() const { return value; }
	constexpr char const *end() const { return value + N; }
	operator Span<char>() const { return {begin(), end()}; }
    char value[N];
};


template <StringLiteral format_string>
umm append_cformat(StringBuilder &b) {
	return append(b, (Span<char>)format_string);
}
template <StringLiteral format_string, class Arg, class ...Args>
umm append_cformat(StringBuilder &b, Arg const &arg, Args const &...args) {
	char previous = {};
	auto start = format_string.begin();
	auto c = start;
	auto end = format_string.end();
	umm appended_char_count = 0;
	for (;c != end; ++c) {
		auto next_prev = *c;
		defer { previous = next_prev; };
		if (previous == '{') {
			switch (*c) {
				case '{':
					appended_char_count += append(b, Span(start, c));
					start = c + 1;
					next_prev = 0;
					break;
				case '}':
					appended_char_count += append(b, Span(start, c - 1));
					static_assert(is_same<decltype(append(b, arg)), tl::umm>, "`append` must return `umm`");
					appended_char_count += append(b, arg);
					appended_char_count += append_format(b, Span(c + 1, end), args...);
					start = end;
					c = end;
					return appended_char_count;
				default:
					invalid_code_path("bad format string: only '{' or '}' can follow '{'");
					break;
			}
		} else if (previous == '}') {
			if (*c == '}') {
				appended_char_count += append(b, Span(start, c));
				start = c + 1;
				next_prev = 0;
			} else {
				invalid_code_path("bad format string: only '}' can follow '}'");
			}
		}
	}
	invalid_code_path("Too many arguments provided for this format string");
	return {};
}

//#define append_cformat(builder, format, ...) append_cformat<format>(builder, __VA_ARGS__)
#define append_cformat(builder, format, ...) append_format(builder, format, __VA_ARGS__)

inline void append_instruction(StringBuilder &builder, s64 idx, Instruction i) {
	using enum Register64;
	switch (i.kind) {
		using enum InstructionKind;
		case mov_rr: append_cformat(builder, "mov {},{}", i.mov_rr.d, i.mov_rr.s); break;
		case mov_rc: append_cformat(builder, "mov {},{}", i.mov_rc.d, i.mov_rc.s); break;

		case mov1_mc: append_cformat(builder, "mov byte{},{}", i.mov1_mc.d, i.mov1_mc.s); break;
		case mov2_mc: append_cformat(builder, "mov word{},{}", i.mov2_mc.d, i.mov2_mc.s); break;
		case mov4_mc: append_cformat(builder, "mov dword{},{}", i.mov4_mc.d, i.mov4_mc.s); break;
		case mov8_mc: append_cformat(builder, "mov qword{},{}", i.mov8_mc.d, i.mov8_mc.s); break;

		case mov1_rm: append_cformat(builder, "mov {},{}", part1b(i.mov1_rm.d), i.mov1_rm.s); break;
		case mov2_rm: append_cformat(builder, "mov {},{}", part2b(i.mov2_rm.d), i.mov2_rm.s); break;
		case mov4_rm: append_cformat(builder, "mov {},{}", part4b(i.mov4_rm.d), i.mov4_rm.s); break;
		case mov8_rm: append_cformat(builder, "mov {},{}", part8b(i.mov8_rm.d), i.mov8_rm.s); break;

		case mov1_mr: append_cformat(builder, "mov {},{}", i.mov1_mr.d, part1b(i.mov1_mr.s)); break;
		case mov2_mr: append_cformat(builder, "mov {},{}", i.mov2_mr.d, part2b(i.mov2_mr.s)); break;
		case mov4_mr: append_cformat(builder, "mov {},{}", i.mov4_mr.d, part4b(i.mov4_mr.s)); break;
		case mov8_mr: append_cformat(builder, "mov {},{}", i.mov8_mr.d, part8b(i.mov8_mr.s)); break;

		case push_c: {
			if (min_value<s32> <= i.push_c.s && i.push_c.s <= max_value<s32>) {
				append_cformat(builder, "push {}", (s32)i.push_c.s);
			} else {
				//if (i.push_c.s & 0x80000000) {
					append_cformat(builder,
						"sub rsp,8\nmov dword[rsp],{}\nmov dword[rsp+4],{}",
						(s32)i.push_c.s       ,
						(s32)(i.push_c.s >> 32)
					);
				//} else {
				//	append_cformat(builder,
				//		"push dword {}\nmov dword [rsp+4], {}",
				//		(s32)i.push_c.s       ,
				//		(s32)(i.push_c.s >> 32)
				//	);
				//}
			}
			break;
		}
		case push_r: append_cformat(builder, "push {}", i.push_r.s); break;
		case push_f: append_cformat(builder, "movq rbx,{}\npush rbx", i.push_f.s); break;
		case push_m: append_cformat(builder, "push qword{}", i.push_m.s); break;

		case push_a: append_cformat(builder, "mov rax, constants + {}\npush rax", i.push_a.s); break;
		case push_d: append_cformat(builder, "mov rax, data + {}\npush rax"     , i.push_d.s); break;
		case push_u: append_cformat(builder, "mov rax, zeros + {}\npush rax"    , i.push_u.s); break;
		case push_t: append_cformat(builder, "mov rax, .{}\npush rax"           , instruction_address(i.push_t.s)); break;

		case mov_ra: append_cformat(builder, "mov {}, constants + {}", i.mov_ra.d, i.mov_ra.s); break;
		case mov_rd: append_cformat(builder, "mov {}, rwdata + {}"   , i.mov_rd.d, i.mov_rd.s); break;
		case mov_ru: append_cformat(builder, "mov {}, zeros + {}"    , i.mov_ru.d, i.mov_ru.s); break;
		case mov_rt: append_cformat(builder, "mov {}, .{}"           , i.mov_rt.d, instruction_address(i.mov_rt.s)); break;

		case pop_r: append_cformat(builder, "pop {}", i.pop_r.d); break;
		case pop_f: append_cformat(builder, "pop rbx\nmovq {}, rbx", i.pop_f.d); break;

		case ret: append_cformat(builder, "ret"); break;

		case shl_rc: append_cformat(builder, "shl {}, {}", i.shl_rc.d, i.shl_rc.s); break;
		case shl_mr: append_cformat(builder, "mov cl, {}\nshl qword {}, cl", part1b(i.shl_mr.s), i.shl_mr.d); break;

		case shr_rc: append_cformat(builder, "shr {}, {}", i.shr_rc.d, i.shr_rc.s); break;
		case shr_mr: append_cformat(builder, "mov cl, {}\nshr qword {}, cl", part1b(i.shr_mr.s), i.shr_mr.d); break;

		case add_rc: append_cformat(builder, "add {}, {}"        , i.add_rc.d, i.add_rc.s); break;
		case add_rr: append_cformat(builder, "add {}, {}"        , i.add_rr.d, i.add_rr.s); break;
		case add_mc: append_cformat(builder, "add qword {}, {}", i.add_mc.d, i.add_mc.s); break;
		case add_mr: append_cformat(builder, "add qword {}, {}", i.add_mr.d, i.add_mr.s); break;

		case sub_rc: append_cformat(builder, "sub {}, {}"        , i.sub_rc.d, i.sub_rc.s); break;
		case sub_rr: append_cformat(builder, "sub {}, {}"        , i.sub_rr.d, i.sub_rr.s); break;
		case sub_mc: append_cformat(builder, "sub qword {}, {}", i.sub_mc.d, i.sub_mc.s); break;
		case sub_mr: append_cformat(builder, "sub qword {}, {}", i.sub_mr.d, i.sub_mr.s); break;

		case mul_rc: append_cformat(builder, "imul {}, {}", i.mul_rc.d, i.mul_rc.s); break;
		case mul_mr: append_cformat(builder, "imul {}, qword {}\nmov qword {}, {}", i.mul_mr.s, i.mul_mr.d, i.mul_mr.d, i.mul_mr.s); break;

			//  DIV - Unsigned divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
			// IDIV -   Signed divide RDX:RAX by r/m64, with result stored in RAX - Quotient, RDX - Remainder.
		case div_mr:
			if (to_x86_register(i.div_mr.s) == rdx) {
				append_cformat(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div rbx\n"
"mov qword {}, rax\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
			} else {
				append_cformat(builder,
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
				append_cformat(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div rbx\n"
"mov qword {}, rdx\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.d);
			} else {
				append_cformat(builder,
"mov rbx, rdx\n"
"xor rdx, rdx\n"
"mov rax, qword {}\n"
"div {}\n"
"mov qword {}, rdx\n"
"mov rdx, rbx", i.div_mr.d, i.div_mr.s, i.div_mr.d);
			}
			break;

		case negi_r: append_cformat(builder, "neg {}", i.negi_r.d); break;
		case negi8_m:  append_cformat(builder, "neg byte {}",  i.negi8_m.d); break;
		case negi16_m: append_cformat(builder, "neg word {}",  i.negi16_m.d); break;
		case negi32_m: append_cformat(builder, "neg dword {}", i.negi32_m.d); break;
		case negi64_m: append_cformat(builder, "neg qword {}", i.negi64_m.d); break;

		case or_mr: append_cformat(builder, "or qword {}, {}", i. or_mr.d, i. or_mr.s); break;

		case and_rc: append_cformat(builder, "and {}, {}"        , i.and_rc.d, i.and_rc.s); break;
		case and_mc: {
			if (~i.and_mc.s & 0xffffffff00000000) {
				auto l = (s32)i.and_mc.s;
				auto h = (s32)((u64)i.and_mc.s >> 32);
				append_cformat(builder, "and dword {}, {}\n"  , i.and_mc.d, l);
				auto addr = i.and_mc.d;
				addr.c += 4;
				append_cformat(builder, "and dword {}, {}", addr, h);
			} else {
				append_cformat(builder, "and qword {}, {}", i.and_mc.d, (s32)i.and_mc.s);
			}
			break;
		}
		case and_mr: append_cformat(builder, "and qword {}, {}", i.and_mr.d, i.and_mr.s); break;

		case xor_rr: append_cformat(builder, "xor {}, {}"        , i.xor_rr.d, i.xor_rr.s); break;
		case xor_mr: append_cformat(builder, "xor qword {}, {}", i.xor_mr.d, i.xor_mr.s); break;

		case cmps1: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps1.d, i.cmps1.d, part1b(i.cmps1.a), part1b(i.cmps1.b), cmps_string(i.cmps1.c), part1b(i.cmps1.d)); break;
		case cmps2: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps2.d, i.cmps2.d, part2b(i.cmps2.a), part2b(i.cmps2.b), cmps_string(i.cmps2.c), part1b(i.cmps2.d)); break;
		case cmps4: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps4.d, i.cmps4.d, part4b(i.cmps4.a), part4b(i.cmps4.b), cmps_string(i.cmps4.c), part1b(i.cmps4.d)); break;
		case cmps8: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmps8.d, i.cmps8.d, part8b(i.cmps8.a), part8b(i.cmps8.b), cmps_string(i.cmps8.c), part1b(i.cmps8.d)); break;
		case cmpu1: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu1.d, i.cmpu1.d, part1b(i.cmpu1.a), part1b(i.cmpu1.b), cmpu_string(i.cmpu1.c), part1b(i.cmpu1.d)); break;
		case cmpu2: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu2.d, i.cmpu2.d, part2b(i.cmpu2.a), part2b(i.cmpu2.b), cmpu_string(i.cmpu2.c), part1b(i.cmpu2.d)); break;
		case cmpu4: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu4.d, i.cmpu4.d, part4b(i.cmpu4.a), part4b(i.cmpu4.b), cmpu_string(i.cmpu4.c), part1b(i.cmpu4.d)); break;
		case cmpu8: append_cformat(builder, "xor {}, {}\ncmp {}, {}\nset{} {}", i.cmpu8.d, i.cmpu8.d, part8b(i.cmpu8.a), part8b(i.cmpu8.b), cmpu_string(i.cmpu8.c), part1b(i.cmpu8.d)); break;

		case jmp: append_cformat(builder, "jmp .{}", instruction_address(idx + i.jmp.offset)); break;

		case jz_cr:  { auto reg = part1b(i.jz_cr.reg); append_cformat(builder, "test {}, {}\njz .{}", reg, reg, instruction_address(idx + i.jz_cr.offset)); break; }
		case jnz_cr: { auto reg = part1b(i.jnz_cr.reg); append_cformat(builder, "test {}, {}\njnz .{}", reg, reg, instruction_address(idx + i.jnz_cr.offset)); break; }

			// Here move into rcx must be last, because it can be source for rdi or rsi
		case copyf_mmc: append_cformat(builder, "mov rsi, {}\nmov rdi, {}\nmov rcx, {}\ncld\nrep movsb", i.copyf_mmc.s, i.copyf_mmc.d, i.copyf_mmc.size); break;
		case copyb_mmc: append_cformat(builder, "lea rsi, [{} + {}]\nlea rdi, [{} + {}]\nmov rcx, {}\nstd\nrep movsb", i.copyb_mmc.s, i.copyb_mmc.size - 1, i.copyb_mmc.d, i.copyb_mmc.size - 1, i.copyb_mmc.size); break;
		case copyf_ssc: append_cformat(builder, "pop rsi\npop rdi\nmov rcx, {}\ncld\nrep movsb", i.copyf_ssc.size); break;
		case copyb_ssc: append_cformat(builder, "pop rsi\npop rdi\nadd rsi, {}\nadd rdi, {}\nmov rcx, {}\nstd\nrep movsb", i.copyb_ssc.size - 1, i.copyb_ssc.size - 1, i.copyb_ssc.size); break;
		case set_mcc: append_cformat(builder, "mov rdi, {}\nmov al, {}\nmov rcx, {}\ncld\nrep stosb", i.set_mcc.d, i.set_mcc.s, i.set_mcc.size); break;

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

				append_cformat(builder,
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
					append_cformat(builder, "push [rsp + {}]\n", offset);
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
				append_cformat(builder,
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
			append_cformat(builder, "call .{}", instruction_address(i.call_c.constant));
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


				if (lambda->parameters.count >= 4) append_cformat(builder, "mov r9,  [rsp+{}]\n", lambda->parameters_size-32);
				if (lambda->parameters.count >= 3) append_cformat(builder, "mov r8,  [rsp+{}]\n", lambda->parameters_size-24);
				if (lambda->parameters.count >= 2) append_cformat(builder, "mov rdx, [rsp+{}]\n", lambda->parameters_size-16);
				if (lambda->parameters.count >= 1) append_cformat(builder, "mov rcx, [rsp+{}]\n", lambda->parameters_size- 8);


				for (s64 i = 0; i < lambda->parameters.count / 2; ++i) {
					append_cformat(builder, "mov rax, [rsp+{}]\nxchg rax, [rsp+{}]\n", i*8, lambda->parameters_size-i*8-8);
				}

#if 0

				// remember the stack pointer and align the stack to 16 byte boundary
				append(builder, "mov rax, rsp\nand rsp, -16\nsub rsp, 8\npush rax\n");

				int offset = 0;
				for (auto parameter : lambda->parameters) {
					assert(get_size(parameter->type) <= 8);
					append_cformat(builder, "push [rsp + {}]\n", offset);
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

			append_cformat(builder, "call .{}", instruction_address(i.call_c.constant));

			if (lambda->convention == CallingConvention::stdcall) {
				append_cformat(builder, "\nadd rsp, {}", lambda->parameters_size);
				//append(builder, "\nadd rsp, 32\npop rsp\nmov [rsp], rax");
			} else {
				assert(lambda->convention == CallingConvention::tlang);
				append_cformat(builder, "\nadd rsp, {}", lambda->parameters_size);
			}

			break;
#endif
		}

		case call_r:
			append_cformat(builder, "call {}", i.call_r.s);
			break;
		case call_m:
			append_cformat(builder, "call qword {}", i.call_m.s);
			break;
#if 0
			if (i.call_m.lambda->convention == CallingConvention::stdcall)
				prepare_stdcall(i.call_m.lambda);

			append_cformat(builder, "call qword {}", i.call_m.s);

			if (i.call_m.lambda->convention == CallingConvention::stdcall)
				append(builder, "\npush rax");

			break;
#endif

		case lea:
			append_cformat(builder, "lea {}, {}", i.lea.d, i.lea.s);
			break;

		case cvt_f32_s32: append(builder, "cvtss2si eax, [rsp]\nmov [rsp], eax"); break;
		case cvt_s32_f32: append(builder, "cvtsi2ss xmm7, [rsp]\nmovd [rsp], xmm7"); break;

		case cvt_f64_s64: append(builder, "cvtsd2si rax, [rsp]\nmov [rsp], rax"); break;
		case cvt_s64_f64: append(builder, "cvtsi2sd xmm7, [rsp]\nmovq [rsp], xmm7"); break;

		case mov_fr: append_cformat(builder, "movq {}, {}", i.mov_fr.d, i.mov_fr.s); break;
	    case mov_rf: append_cformat(builder, "movq {}, {}", i.mov_rf.d, i.mov_rf.s); break;

		case mov1_xm: append_cformat(builder, "movb {}, byte {}", i.mov1_xm.d, i.mov1_xm.s); break;
		case mov2_xm: append_cformat(builder, "movw {}, word {}", i.mov2_xm.d, i.mov2_xm.s); break;
		case mov4_xm: append_cformat(builder, "movd {}, dword {}", i.mov4_xm.d, i.mov4_xm.s); break;
		case mov8_xm: append_cformat(builder, "movq {}, qword {}", i.mov8_xm.d, i.mov8_xm.s); break;

	    case add_f32_f32: append_cformat(builder, "addss {}, {}", i.add_f32_f32.d, i.add_f32_f32.s); break;
	    case sub_f32_f32: append_cformat(builder, "subss {}, {}", i.sub_f32_f32.d, i.sub_f32_f32.s); break;
	    case mul_f32_f32: append_cformat(builder, "mulss {}, {}", i.mul_f32_f32.d, i.mul_f32_f32.s); break;
	    case div_f32_f32: append_cformat(builder, "divss {}, {}", i.div_f32_f32.d, i.div_f32_f32.s); break;

	    case add_f64_f64: append_cformat(builder, "addsd {}, {}", i.add_f64_f64.d, i.add_f64_f64.s); break;
	    case sub_f64_f64: append_cformat(builder, "subsd {}, {}", i.sub_f64_f64.d, i.sub_f64_f64.s); break;
	    case mul_f64_f64: append_cformat(builder, "mulsd {}, {}", i.mul_f64_f64.d, i.mul_f64_f64.s); break;
	    case div_f64_f64: append_cformat(builder, "divsd {}, {}", i.div_f64_f64.d, i.div_f64_f64.s); break;

	    case xor_ff: append_cformat(builder, "xorps {}, {}", i.xor_ff.d, i.xor_ff.s); break;

		case tobool_r:    { auto d = part1b(i.tobool_r.d);    append_cformat(builder, "test {}, {}\nsetnz {}", d, d, d); break; }
		case toboolnot_r: { auto d = part1b(i.toboolnot_r.d); append_cformat(builder, "test {}, {}\nsetz {}" , d, d, d); break; }
		case not_r: append_cformat(builder, "not {}", i.not_r.d); break;
		case not_m: append_cformat(builder, "not {}", i.not_m.d); break;
		case jmp_label:
		case noop: break;

		case debug_break: append(builder, "int3"s); break;

		case movsx21_rm: append_cformat(builder, "movsx {}, byte {}",  part2b(i.movsx21_rm.d), i.movsx21_rm.s); break;
		case movsx41_rm: append_cformat(builder, "movsx {}, byte {}",  part4b(i.movsx41_rm.d), i.movsx41_rm.s); break;
		case movsx81_rm: append_cformat(builder, "movsx {}, byte {}",  part8b(i.movsx81_rm.d), i.movsx81_rm.s); break;
		case movsx42_rm: append_cformat(builder, "movsx {}, word {}",  part4b(i.movsx42_rm.d), i.movsx42_rm.s); break;
		case movsx82_rm: append_cformat(builder, "movsx {}, word {}",  part8b(i.movsx82_rm.d), i.movsx82_rm.s); break;
		case movsx84_rm: append_cformat(builder, "movsx {}, dword {}", part8b(i.movsx84_rm.d), i.movsx84_rm.s); break;

		case movzx21_rm: append_cformat(builder, "movzx {}, byte {}",  part2b(i.movsx21_rm.d), i.movsx21_rm.s); break;
		case movzx41_rm: append_cformat(builder, "movzx {}, byte {}",  part4b(i.movsx41_rm.d), i.movsx41_rm.s); break;
		case movzx81_rm: append_cformat(builder, "movzx {}, byte {}",  part8b(i.movsx81_rm.d), i.movsx81_rm.s); break;
		case movzx42_rm: append_cformat(builder, "movzx {}, word {}",  part4b(i.movsx42_rm.d), i.movsx42_rm.s); break;
		case movzx82_rm: append_cformat(builder, "movzx {}, word {}",  part8b(i.movsx82_rm.d), i.movsx82_rm.s); break;
		case movzx84_rm: append_cformat(builder, "movzx {}, dword {}", part8b(i.movsx84_rm.d), i.movsx84_rm.s); break;

		case push_used_registers:
			for (u64 bit = 0; bit < sizeof(i.push_used_registers.mask) * 8; ++bit) {
				if ((i.push_used_registers.mask >> bit) & 1) {
					append_cformat(builder, "push {}\n", (Register)bit);
				}
			}

			// keep the stack 16-byte aligned
			if (count_bits(i.push_used_registers.mask) & 1)
				append_cformat(builder, "sub rsp, 8\n");
			break;
		case pop_used_registers:
			// keep the stack 16-byte aligned
			if (count_bits(i.push_used_registers.mask) & 1)
				append_cformat(builder, "add rsp, 8\n");

			for (u64 bit = sizeof(i.push_used_registers.mask) * 8 - 1; bit != ~0; --bit) {
				if ((i.push_used_registers.mask >> bit) & 1) {
					append_cformat(builder, "pop {}\n", (Register)bit);
				}
			}
			break;

		default:invalid_code_path();
	}
}

}
