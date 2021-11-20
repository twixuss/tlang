#pragma once
#include "ast.h"

enum class InstructionKind : u8 {
	move_reg_to_reg,
	move_constant_to_reg,
	move_mem_to_reg,
	move_reg_to_mem,

	push_reg,
	push_constant,
	push_mem,
	push_constant_data_address,
	push_data_address,
	push_uninitialized_data_address,

	pop_reg,

	ret,

	add_constant_to_reg,
	add_constant_to_mem,
	add_reg_to_mem,
	add_reg_to_reg,

	sub_constant_to_reg,
	sub_reg_to_reg,
	sub_reg_to_mem,

	mul_reg_to_mem,
	div_reg_to_mem,
	mod_reg_to_mem,
	or_reg_to_mem,

	and_constant_to_reg,
	and_reg_to_mem,

	xor_reg_to_reg,
	xor_reg_to_mem,

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
	union {
		struct {
			Register dst_reg;
			Register src_reg;
		} move_reg_to_reg;

		struct {
			Register reg;
			s64 constant;
		} move_constant_to_reg;

		struct {
			Register dst_reg;
			Register src_reg;
		} move_mem_to_reg;

		struct {
			Register dst_reg;
			Register src_reg;
		} move_reg_to_mem;


		struct {
			Register reg;
		} push_reg;

		struct {
			s64 constant;
		} push_constant;

		struct {
			Register reg;
		} push_mem;

		struct {
			s64 address;
		} push_constant_data_address;
		struct {
			s64 address;
		} push_data_address;
		struct {
			s64 address;
		} push_uninitialized_data_address;


		struct {
			Register reg;
		} pop_reg;


		struct {
		} ret;


		struct {
			Register reg;
			s64 constant;
		} add_constant_to_reg;

		struct {
			Register reg;
			s64 constant;
		} add_constant_to_mem;

		struct {
			Register dst_reg;
			Register src_reg;
		} add_reg_to_mem;

		struct {
			Register dst_reg;
			Register src_reg;
		} add_reg_to_reg;


		struct {
			Register dst_reg;
			Register src_reg;
		} sub_reg_to_reg;

		struct {
			Register reg;
			s64 constant;
		} sub_constant_to_reg;

		struct {
			Register dst_reg;
			Register src_reg;
		} sub_reg_to_mem;


		struct {
			Register dst_reg;
			Register src_reg;
		} mul_reg_to_mem;


		struct {
			Register dst_reg;
			Register src_reg;
		} div_reg_to_mem;


		struct {
			Register dst_reg;
			Register src_reg;
		} mod_reg_to_mem;


		struct {
			Register dst_reg;
			Register src_reg;
		} or_reg_to_mem;


		struct {
			Register reg;
			s64 constant;
		} and_constant_to_reg;

		struct {
			Register dst_reg;
			Register src_reg;
		} and_reg_to_mem;


		struct {
			Register dst_reg;
			Register src_reg;
		} xor_reg_to_reg;

		struct {
			Register dst_reg;
			Register src_reg;
		} xor_reg_to_mem;


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
