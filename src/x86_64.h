#pragma once
#include <compiler.h>
#include <bytecode.h>

namespace x86_64 {

enum class Register64:u8{rax,rcx,rdx,rbx,rsp,rbp,rsi,rdi,r8, r9, r10, r11, r12, r13, r14, r15 };
enum class Register32:u8{eax,ecx,edx,ebx,esp,ebp,esi,edi,r8d,r9d,r10d,r11d,r12d,r13d,r14d,r15d};
enum class Register16:u8{ ax, cx, dx, bx, sp, bp, si, di,r8w,r9w,r10w,r11w,r12w,r13w,r14w,r15w};
enum class Register8 :u8{ al, cl, dl, bl,spl,bpl,sil,dil,r8b,r9b,r10b,r11b,r12b,r13b,r14b,r15b};

// Microsoft 64 bit calling convention - saved registers
// +-----+-----+----------+
// | reg | idx | volatile |
// +-----+-----+----------+
// | rax |     |    +     |
// | rbx | 10  |          |
// | rcx | 0   |    +     |
// | rdx | 1   |    +     |
// | rsi | 8   |          |
// | rdi | 9   |          |
// | rsp |     |          |
// | rbp |     |          |
// | r8  | 2   |    +     |
// | r9  | 3   |    +     |
// | r10 |     |    +     |
// | r11 |     |    +     |
// | r12 | 4   |          |
// | r13 | 5   |          |
// | r14 | 6   |          |
// | r15 | 7   |          |
// +-----+-----+----------+

inline static constexpr Register64 stdcall_int_registers[] {
	Register64::rcx,
	Register64::rdx,
	Register64::r8,
	Register64::r9,
};

inline Instruction current_instruction = {};

inline static Register64 to_x86_register(Register r) {
	using enum Register;
	using enum Register64;
	switch (r) {
		case (Register)0:  return rcx;
		case (Register)1:  return rdx;
		case (Register)2:  return r8;
		case (Register)3:  return r9;
		case (Register)4:  return r12;
		case (Register)5:  return r13;
		case (Register)6:  return r14;
		case (Register)7:  return r15;
		case (Register)8:  return rsi;
		case (Register)9:  return rdi;
		case (Register)10: return rbx;
		case rs: return rsp;
		case rb: return rbp;
	}
	invalid_code_path(current_instruction.node ? current_instruction.node->location : String{}, "Register {} does not map to x86_64 one.", (u32)r);
}

inline static constexpr Register64 part8b(Register64 r) { return r; }
inline static constexpr Register32 part4b(Register64 r) { return (Register32)r; }
inline static constexpr Register16 part2b(Register64 r) { return (Register16)r; }
inline static constexpr Register8  part1b(Register64 r) { return (Register8 )r; }

inline static constexpr Register64 part8b(Register r) { return part8b(to_x86_register(r)); }
inline static constexpr Register32 part4b(Register r) { return part4b(to_x86_register(r)); }
inline static constexpr Register16 part2b(Register r) { return part2b(to_x86_register(r)); }
inline static constexpr Register8  part1b(Register r) { return part1b(to_x86_register(r)); }

struct Address {
	Register64 base = {};
	Register64 r1 = {};
	Register64 r2 = {};
	// stored value | represented scale
	// 0              0
	// 1              1
	// 2              2
	// 3              4
	// 4              8
	u8 r1_scale_index : 3 = {};
	u8 r2_scale : 1 = {}; // either 0 or 1
	s32 c = {};

	bool is(Register64 r) {
		return base == r && !r1_scale_index && !r2_scale && !c;
	}

	Address() = default;
	Address(Register64 base) : base(base) {}
	Address(::Address a):
		base(to_x86_register(a.base)),
		r1(to_x86_register(a.r1)),
		r2(to_x86_register(a.r2)),
		r1_scale_index(a.r1_scale_index),
		r2_scale(a.r2_scale),
		c(a.c)
	{}
};

inline Address operator+(Register64 r, s64 c) {
	assert(c == (s64)(s32)c);

	Address a;
	a.base = r;
	a.c = (s32)c;
	return a;
}
inline Address operator+(Address a, s64 c) {
	assert((s64)(a.c + (s32)c) == ((s64)a.c + c));
	a.c += c;
	return a;
}
inline Address operator-(Register64 r, s64 c) { return r + (-c); }
inline Address operator-(Address a, s64 c) { return a + (-c); }
}
