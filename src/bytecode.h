#pragma once
#include "ast.h"

#define BYTECODE_DEBUG 0

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

	push_r,
	push_c,
	push_m,

	pushcda, // constant data address
	pushda,  // data address
	pushuda, // uninitialized data address
	pushta,  // text address
	pushextern, // extern symbol address

	pop_r,
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

	cmpu1,
	cmpu2,
	cmpu4,
	cmpu8,

	cmps1,
	cmps2,
	cmps4,
	cmps8,

	call_constant,
	call_string,

	jmp,
	jz,

	copyf_mmc,
	copyb_mmc,
	copyf_ssc,
	copyb_ssc,

	stdcall_begin_lambda,
	stdcall_end_lambda,

	stdcall_constant,
	stdcall_string,

	push_stdcall_result,

	count,
};

// Make sure instruction count does not go over 256
static_assert((int)InstructionKind::count >= 100);

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

struct Instruction {
	InstructionKind kind;
#if BYTECODE_DEBUG
	utf8 *comment;
	u64 line;
#endif
	union {
		struct { Register d; s64      s; } mov_rc;
		struct { Register d; Register s; } mov_rr;

		struct { Register d; Register s; } mov1_rm;
		struct { Register d; s64      s; } mov1_mc;
		struct { Register d; Register s; } mov1_mr;

		struct { Register d; Register s; } mov2_rm;
		struct { Register d; s64      s; } mov2_mc;
		struct { Register d; Register s; } mov2_mr;

		struct { Register d; Register s; } mov4_rm;
		struct { Register d; s64      s; } mov4_mc;
		struct { Register d; Register s; } mov4_mr;

		struct { Register d; Register s; } mov8_rm;
		struct { Register d; s64      s; } mov8_mc;
		struct { Register d; Register s; } mov8_mr;

		struct { Register d, s; } movsx21_rm;
		struct { Register d, s; } movsx41_rm;
		struct { Register d, s; } movsx81_rm;
		struct { Register d, s; } movsx42_rm;
		struct { Register d, s; } movsx82_rm;
		struct { Register d, s; } movsx84_rm;

		struct { s64      s; } push_c;
		struct { Register s; } push_r;
		struct { Register s; } push_m;

		struct { s64 s; } pushcda;
		struct { s64 s; } pushda;
		struct { s64 s; } pushuda;
		struct { s64 s; } pushta;
		struct { Span<utf8> s; } pushextern;


		struct { Register d; } pop_r;
		struct { Register d; } pop_m;


		struct {} ret;

		struct { Register d; s64      s; } shr_rc;
		struct { Register d; Register s; } shr_rr;
		struct { Register d; Register s; } shr_rm;
		struct { Register d; s64      s; } shr_mc;
		struct { Register d; Register s; } shr_mr;

		struct { Register d; s64      s; } shl_rc;
		struct { Register d; Register s; } shl_rr;
		struct { Register d; Register s; } shl_rm;
		struct { Register d; s64      s; } shl_mc;
		struct { Register d; Register s; } shl_mr;

		struct { Register d; s64      s; } add_rc;
		struct { Register d; Register s; } add_rr;
		struct { Register d; Register s; } add_rm;
		struct { Register d; s64      s; } add_mc;
		struct { Register d; Register s; } add_mr;

		struct { Register d; s64      s; } sub_rc;
		struct { Register d; Register s; } sub_rr;
		struct { Register d; Register s; } sub_rm;
		struct { Register d; s64      s; } sub_mc;
		struct { Register d; Register s; } sub_mr;

		struct { Register d; s64      s; } mul_rc;
		struct { Register d; Register s; } mul_rr;
		struct { Register d; Register s; } mul_rm;
		struct { Register d; s64      s; } mul_mc;
		struct { Register d; Register s; } mul_mr;

		struct { Register d; s64      s; } div_rc;
		struct { Register d; Register s; } div_rr;
		struct { Register d; Register s; } div_rm;
		struct { Register d; s64      s; } div_mc;
		struct { Register d; Register s; } div_mr;

		struct { Register d; s64      s; } mod_rc;
		struct { Register d; Register s; } mod_rr;
		struct { Register d; Register s; } mod_rm;
		struct { Register d; s64      s; } mod_mc;
		struct { Register d; Register s; } mod_mr;

		struct { Register d; s64      s; } or_rc;
		struct { Register d; Register s; } or_rr;
		struct { Register d; Register s; } or_rm;
		struct { Register d; s64      s; } or_mc;
		struct { Register d; Register s; } or_mr;

		struct { Register d; s64      s; } and_rc;
		struct { Register d; Register s; } and_rr;
		struct { Register d; Register s; } and_rm;
		struct { Register d; s64      s; } and_mc;
		struct { Register d; Register s; } and_mr;

		struct { Register d; s64      s; } xor_rc;
		struct { Register d; Register s; } xor_rr;
		struct { Register d; Register s; } xor_rm;
		struct { Register d; s64      s; } xor_mc;
		struct { Register d; Register s; } xor_mr;

		struct { Register d, a, b; Comparison c; } cmpu1;
		struct { Register d, a, b; Comparison c; } cmpu2;
		struct { Register d, a, b; Comparison c; } cmpu4;
		struct { Register d, a, b; Comparison c; } cmpu8;
		struct { Register d, a, b; Comparison c; } cmps1;
		struct { Register d, a, b; Comparison c; } cmps2;
		struct { Register d, a, b; Comparison c; } cmps4;
		struct { Register d, a, b; Comparison c; } cmps8;

		struct { s64 constant; } call_constant;
		struct { Span<utf8> string; } call_string;

		struct { s64 offset; } jmp;
		struct { Register reg; s64 offset; } jz;

		struct { Register d, s; s64 size; } copyf_mmc;
		struct { Register d, s; s64 size; } copyb_mmc;
		struct { s64 size; } copyf_ssc;
		struct { s64 size; } copyb_ssc;

		struct { AstLambda *lambda; } stdcall_begin_lambda;
		struct { AstLambda *lambda; } stdcall_end_lambda;

		struct { s64 constant; } stdcall_constant;
		struct { Span<utf8> string; } stdcall_string;

		struct {} push_stdcall_result;
	};
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
