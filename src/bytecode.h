#pragma once
#include "ast.h"

#define BYTECODE_DEBUG 1

/*

Naming convention:

{instruction}_{param0, param1, ...}
where param is any of:
	c: constant,
	r: register,
	m: memory,

Destination of an instruction is first

*/

enum class InstructionKind : u8 {
	mov_rc,
	mov_rr,

	mov1_rm,
	mov1_mc,
	mov1_mr,

	mov2_rm,
	mov2_mc,
	mov2_mr,

	mov4_rm,
	mov4_mc,
	mov4_mr,

	mov8_rm,
	mov8_mc,
	mov8_mr,

	movsx21_rm,
	movsx41_rm,
	movsx81_rm,
	movsx42_rm,
	movsx82_rm,
	movsx84_rm,

	movzx21_rm,
	movzx41_rm,
	movzx81_rm,
	movzx42_rm,
	movzx82_rm,
	movzx84_rm,

	lea,

	push_c,
	push_r,
	push_f,
	push_m,

	push_a, // constant data address
	push_d, // data address
	push_u, // uninitialized data address
	push_t, // text address
	push_e, // extern symbol address

	mov_ra,
	mov_rd,
	mov_ru,
	mov_rt,
	mov_re,

	pop_r,
	pop_f,
	pop_m,

	ret,

	shr_rc,
	shr_rr,
	shr_rm,
	shr_mc,
	shr_mr,

	shl_rc,
	shl_rr,
	shl_rm,
	shl_mc,
	shl_mr,

	add_rc,
	add_rr,
	add_rm,
	add_mc,
	add_mr,

	sub_rc,
	sub_rr,
	sub_rm,
	sub_mc,
	sub_mr,

	mul_rc,
	mul_rr,
	mul_rm,
	mul_mc,
	mul_mr,

	div_rc,
	div_rr,
	div_rm,
	div_mc,
	div_mr,

	mod_rc,
	mod_rr,
	mod_rm,
	mod_mc,
	mod_mr,

	or_rc,
	or_rr,
	or_rm,
	or_mc,
	or_mr,

	and_rc,
	and_rr,
	and_rm,
	and_mc,
	and_mr,

	xor_rc,
	xor_rr,
	xor_rm,
	xor_mc,
	xor_mr,

	negi_r,
	negi8_m,
	negi16_m,
	negi32_m,
	negi64_m,

	cmpu1,
	cmpu2,
	cmpu4,
	cmpu8,

	cmps1,
	cmps2,
	cmps4,
	cmps8,

	call_r,
	call_m,

	stdcall_r,
	stdcall_m,

	popcall,
	popstdcall,
	call_constant,
	call_string,

	jmp,
	jz_cr, // jump to constant offset if boolean in register is zero

	copyf_mmc,
	copyb_mmc,
	copyf_ssc,
	copyb_ssc,

	set_mcc,

	stdcall_begin_lambda,
	stdcall_end_lambda,

	stdcall_constant,
	stdcall_string,

	push_stdcall_result,

	cvt_f64_s64,

	mov_fr,
	mov_rf,

	add_f32_f32,
	add_f64_f64,

	mul_f32_f32,
	mul_f64_f64,

	sub_f32_f32,
	sub_f64_f64,

	div_f32_f32,
	div_f64_f64,

	xor_ff,

	tobool_r,
	toboolnot_r,

	dbgbrk,

	noop,

	count,
};

// Make sure instruction count does not go over 256
static_assert((int)InstructionKind::count >= 127);

// NOTE:
// registers r0-r4 are scratch and are used for expression evaluation
// registers r5-r7 are allocatable
// Also registers r0-r3 are used as stdcall arguments
enum class Register : u8 {
	r0,
	r1,
	r2,
	r3,
	r4,
	r5,
	r6,
	r7,
	rs,
	rb,
};

enum class FRegister : u8 {
	f0,
	f1,
	f2,
	f3,
};

enum InstructionFlags : u8 {
	labeled = 0x1,
};

struct Address {
	Register base = {};
	Register r1 = {};
	s64      r1_scale = {};
	Register r2 = {};
	bool     r2_scale = {};
	s64      c = {};

	bool is(Register r) {
		return base == r && !r1_scale && !r2_scale && !c;
	}

	Address() = default;
	Address(Register base) : base(base) {}
};

inline Address operator+(Register r, s64 c) {
	Address a;
	a.base = r;
	a.c = c;
	return a;
}
inline Address operator+(Address a, s64 c) {
	a.c += c;
	return a;
}

struct Instruction {
	union {
		struct { Register d; s64      s; } mov_rc;
		struct { Register d; Register s; } mov_rr;

		struct { Register d; Address s; } mov1_rm;
		struct { Register d; Address s; } mov2_rm;
		struct { Register d; Address s; } mov4_rm;
		struct { Register d; Address s; } mov8_rm;

		struct { Address d; s64 s; } mov1_mc;
		struct { Address d; s64 s; } mov2_mc;
		struct { Address d; s64 s; } mov4_mc;
		struct { Address d; s64 s; } mov8_mc;

		struct { Address d; Register s; } mov1_mr;
		struct { Address d; Register s; } mov2_mr;
		struct { Address d; Register s; } mov4_mr;
		struct { Address d; Register s; } mov8_mr;

		struct { Register d; Address s; } movsx21_rm;
		struct { Register d; Address s; } movsx41_rm;
		struct { Register d; Address s; } movsx81_rm;
		struct { Register d; Address s; } movsx42_rm;
		struct { Register d; Address s; } movsx82_rm;
		struct { Register d; Address s; } movsx84_rm;

		struct { Register d; Address s; } movzx21_rm;
		struct { Register d; Address s; } movzx41_rm;
		struct { Register d; Address s; } movzx81_rm;
		struct { Register d; Address s; } movzx42_rm;
		struct { Register d; Address s; } movzx82_rm;
		struct { Register d; Address s; } movzx84_rm;

		struct { Register d; Address s; } lea;

		struct { s64       s; } push_c;
		struct { Register  s; } push_r;
		struct { FRegister s; } push_f;
		struct { Address   s; } push_m;

		struct { s64 s; } push_a;
		struct { s64 s; } push_d;
		struct { s64 s; } push_u;
		struct { s64 s; } push_t;
		struct { Span<utf8> s; } push_e;

		struct { Register d; s64 s; } mov_ra;
		struct { Register d; s64 s; } mov_rd;
		struct { Register d; s64 s; } mov_ru;
		struct { Register d; s64 s; } mov_rt;
		struct { Register d; Span<utf8> s; } mov_re;


		struct { Register  d; } pop_r;
		struct { FRegister d; } pop_f;
		struct { Address   d; } pop_m;


		struct {} ret;

		struct { Register d; s64      s; } shr_rc;
		struct { Register d; Register s; } shr_rr;
		struct { Register d; Address  s; } shr_rm;
		struct { Address  d; s64      s; } shr_mc;
		struct { Address  d; Register s; } shr_mr;

		struct { Register d; s64      s; } shl_rc;
		struct { Register d; Register s; } shl_rr;
		struct { Register d; Address  s; } shl_rm;
		struct { Address  d; s64      s; } shl_mc;
		struct { Address  d; Register s; } shl_mr;

		struct { Register d; s64      s; } add_rc;
		struct { Register d; Register s; } add_rr;
		struct { Register d; Address  s; } add_rm;
		struct { Address  d; s64      s; } add_mc;
		struct { Address  d; Register s; } add_mr;

		struct { Register d; s64      s; } sub_rc;
		struct { Register d; Register s; } sub_rr;
		struct { Register d; Address  s; } sub_rm;
		struct { Address  d; s64      s; } sub_mc;
		struct { Address  d; Register s; } sub_mr;

		struct { Register d; s64      s; } mul_rc;
		struct { Register d; Register s; } mul_rr;
		struct { Register d; Address  s; } mul_rm;
		struct { Address  d; s64      s; } mul_mc;
		struct { Address  d; Register s; } mul_mr;

		struct { Register d; s64      s; } div_rc;
		struct { Register d; Register s; } div_rr;
		struct { Register d; Address  s; } div_rm;
		struct { Address  d; s64      s; } div_mc;
		struct { Address  d; Register s; } div_mr;

		struct { Register d; s64      s; } mod_rc;
		struct { Register d; Register s; } mod_rr;
		struct { Register d; Address  s; } mod_rm;
		struct { Address  d; s64      s; } mod_mc;
		struct { Address  d; Register s; } mod_mr;

		struct { Register d; s64      s; } or_rc;
		struct { Register d; Register s; } or_rr;
		struct { Register d; Address  s; } or_rm;
		struct { Address  d; s64      s; } or_mc;
		struct { Address  d; Register s; } or_mr;

		struct { Register d; s64      s; } and_rc;
		struct { Register d; Register s; } and_rr;
		struct { Register d; Address  s; } and_rm;
		struct { Address  d; s64      s; } and_mc;
		struct { Address  d; Register s; } and_mr;

		struct { Register d; s64      s; } xor_rc;
		struct { Register d; Register s; } xor_rr;
		struct { Register d; Address  s; } xor_rm;
		struct { Address  d; s64      s; } xor_mc;
		struct { Address  d; Register s; } xor_mr;

		struct { Register d; } negi_r;
		struct { Address d; } negi8_m;
		struct { Address d; } negi16_m;
		struct { Address d; } negi32_m;
		struct { Address d; } negi64_m;

		struct { Register d, a, b; Comparison c; } cmpu1;
		struct { Register d, a, b; Comparison c; } cmpu2;
		struct { Register d, a, b; Comparison c; } cmpu4;
		struct { Register d, a, b; Comparison c; } cmpu8;
		struct { Register d, a, b; Comparison c; } cmps1;
		struct { Register d, a, b; Comparison c; } cmps2;
		struct { Register d, a, b; Comparison c; } cmps4;
		struct { Register d, a, b; Comparison c; } cmps8;

		struct { Register s; } call_r;
		struct { Address  s; } call_m;

		struct { Register s; } stdcall_r;
		struct { Address  s; } stdcall_m;

		struct {} popcall;
		struct {} popstdcall;
		struct { s64 constant; } call_constant;
		struct { Span<utf8> string; } call_string;

		struct { s64 offset; } jmp;
		struct { s64 offset; Register reg; } jz_cr;

		struct { Register d, s; s64 size; } copyf_mmc;
		struct { Register d, s; s64 size; } copyb_mmc;
		struct { s64 size; } copyf_ssc;
		struct { s64 size; } copyb_ssc;

		struct { Register d; s64 s, size; } set_mcc;

		struct { AstLambda *lambda; } stdcall_begin_lambda;
		struct { AstLambda *lambda; } stdcall_end_lambda;

		struct { s64 constant; } stdcall_constant;
		struct { Span<utf8> string; } stdcall_string;

		struct {} push_stdcall_result;

		struct {} cvt_f64_s64;

		struct { FRegister d; Register s; } mov_fr;
		struct { Register d; FRegister s; } mov_rf;

		struct { FRegister d; FRegister s; } add_f32_f32;
		struct { FRegister d; FRegister s; } add_f64_f64;

		struct { FRegister d; FRegister s; } mul_f32_f32;
		struct { FRegister d; FRegister s; } mul_f64_f64;

		struct { FRegister d; FRegister s; } sub_f32_f32;
		struct { FRegister d; FRegister s; } sub_f64_f64;

		struct { FRegister d; FRegister s; } div_f32_f32;
		struct { FRegister d; FRegister s; } div_f64_f64;

		struct { FRegister d; FRegister s; } xor_ff;

		struct { Register d; } tobool_r;
		struct { Register d; } toboolnot_r;

		struct {} noop;

		struct {} dbgbrk;
	};
	InstructionKind kind;
	std::underlying_type_t<InstructionFlags> flags;
#if BYTECODE_DEBUG
	utf8 *comment;
	u64 line;
#endif
};

using ExternLibraries = HashMap<Span<utf8>, List<Span<utf8>>>;

struct Bytecode {
	List<Instruction> instructions;
	List<u8> constant_data;
	List<u8> data;
	umm zero_data_size;
	ExternLibraries extern_libraries;
};

Bytecode build_bytecode();

inline umm append(StringBuilder &builder, Comparison c) {
	switch (c) {
		case Comparison::e:	 return append(builder, "==");
		case Comparison::ne: return append(builder, "!=");
		case Comparison::l:	 return append(builder, "<");
		case Comparison::le: return append(builder, "<=");
		case Comparison::g:	 return append(builder, ">");
		case Comparison::ge: return append(builder, ">=");
		default:
			invalid_code_path();
	}
}

#define DECLARE_OUTPUT_BUILDER extern "C" __declspec(dllexport) void tlang_build_output(CompilerContext &context, Bytecode &bytecode)
using OutputBuilder = void (*)(CompilerContext &context, Bytecode &bytecode);
