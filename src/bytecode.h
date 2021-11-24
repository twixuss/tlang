#pragma once
#include "ast.h"

/*

Naming convention:

{instruction}_{param0, param1, ...}
where param is any of:
	c: constant,
	r: register,
	m: memory,

Destination of an instruction is first

*/

enum class InstructionKind : u64 {
	mov_rc,
	mov_rr,
	mov_rm,
	mov_mc,
	mov_mr,

	push_r,
	push_c,
	push_m,

	pushcda, // constant data address
	pushda,  // data address
	pushuda, // uninitialized data address

	pop_r,
	pop_m,

	ret,

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

	cmp_rax_rbx,

	call_constant,
	call_string,

	jmp,
	jz,
};

enum class Register : u8 {
	rax,
	rbx,
	rcx,
	rdx,
	rsi,
	rdi,
	rbp,
	rsp,
	r8,
	r9,
	r10,
	r11,
	r12,
	r13,
	r14,
	r15,
};

struct Instruction {
	InstructionKind kind;
	utf8 *comment;
	union {
		struct { Register d; s64      s; } mov_rc;
		struct { Register d; Register s; } mov_rr;
		struct { Register d; Register s; } mov_rm;
		struct { Register d; s64      s; } mov_mc;
		struct { Register d; Register s; } mov_mr;

		struct { s64      s; } push_c;
		struct { Register s; } push_r;
		struct { Register s; } push_m;

		struct { s64 s; } pushcda;
		struct { s64 s; } pushda;
		struct { s64 s; } pushuda;


		struct { Register d; } pop_r;
		struct { Register d; } pop_m;


		struct {} ret;

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

		struct {
			Register dst_reg;
			Comparison comparison;
		} cmp_rax_rbx;


		struct {
			s64 constant;
		} call_constant;


		struct {
			Span<utf8> string;
		} call_string;

		struct {
			s64 offset;
		} jmp;


		struct {
			Register reg;
			s64 offset;
		} jz;
	};
};

struct Bytecode {
	List<Instruction> instructions;
	List<u8> constant_data;
	List<u8> data;
	List<u8> zero_data;
	List<Span<utf8>> extern_functions;
};

Bytecode build_bytecode();

inline umm append(StringBuilder &builder, Register r) {
	switch (r) {
		case Register::rax: return append(builder, "rax");
		case Register::rbx: return append(builder, "rbx");
		case Register::rcx: return append(builder, "rcx");
		case Register::rdx: return append(builder, "rdx");
		case Register::rsi: return append(builder, "rsi");
		case Register::rdi: return append(builder, "rdi");
		case Register::rbp: return append(builder, "rbp");
		case Register::rsp: return append(builder, "rsp");
		case Register::r8:  return append(builder, "r8");
		case Register::r9:  return append(builder, "r9");
		case Register::r10: return append(builder, "r10");
		case Register::r11: return append(builder, "r11");
		case Register::r12: return append(builder, "r12");
		case Register::r13: return append(builder, "r13");
		case Register::r14: return append(builder, "r14");
		case Register::r15: return append(builder, "r15");
		default:
			invalid_code_path();
	}
	return append(builder, (u8)r);
}

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
