#pragma once
#include <ast.h>
#include <bytecode.h>

namespace x86 {

enum class Register32{eax,ebx,ecx,edx,esi,edi,esp,ebp, };
enum class Register16{ ax, bx, cx, dx, si, di, sp, bp, };
enum class Register8 { al, bl, cl, dl, sil,dil,spl,bpl,};

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

static Span<utf8> as_string(Register32 r){using enum Register32;switch(r){C(eax);C(ebx);C(ecx);C(edx);C(esi );C(edi );C(esp );C(ebp );}invalid_code_path();return{};}
static Span<utf8> as_string(Register16 r){using enum Register16;switch(r){C( ax);C( bx);C( cx);C( dx);C( si );C( di );C( sp );C( bp );}invalid_code_path();return{};}
static Span<utf8> as_string(Register8  r){using enum Register8 ;switch(r){C( al);C( bl);C( cl);C( dl);C( sil);C( dil);C( spl);C( bpl);}invalid_code_path();return{};}

#undef C

inline auto instruction_address(s64 val) { return FormatInt<s64>{.value=val, .radix=62}; }

#define REGISTER_MAP \
	C(r0, eax) \
	C(r1, ebx) \
	C(r2, ecx) \
	C(r3, edx) \
	C(r4, esi) \
	C(r5, edi) \
	C(rs, esp) \
	C(rb, ebp)

#define C(a, b) case Register::a: return Register32::b;

inline static constexpr Register32 to_x86_register(Register r) {
	switch (r) {
		REGISTER_MAP;
	}
	invalid_code_path();
	return {};
}

#undef C

#define C(a, b) case Register32::b: return Register::a;

inline static constexpr Register to_bc_register(Register32 r) {
	switch (r) {
		REGISTER_MAP;
	}
	invalid_code_path();
	return {};
}

#undef C
}

namespace tl {

inline umm append(StringBuilder&builder,x86::Register32 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86::Register16 r){return append(builder,as_string(r));}
inline umm append(StringBuilder&builder,x86::Register8  r){return append(builder,as_string(r));}

inline umm append(StringBuilder &builder, Address a) {
	using namespace x86;
	umm result = 0;
	result += append(builder, '[');
	switch (a.base) {
		using enum Register;
		case constants: result += append(builder, "rel constants"); break;
		case rwdata: result += append(builder, "rel rwdata"); break;
		case zeros: result += append(builder, "rel zeros"); break;
		case instructions: return append_format(builder, "rel .{}]", instruction_address(a.c));
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
	using namespace x86;
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

namespace x86 {

inline static constexpr Register32 part4b(Register32 r) { return (Register32)r; }
inline static constexpr Register16 part2b(Register32 r) { return (Register16)r; }
inline static constexpr Register8  part1b(Register32 r) { return (Register8 )r; }

inline static constexpr Register32 part4b(Register r) { return part4b(to_x86_register(r)); }
inline static constexpr Register16 part2b(Register r) { return part2b(to_x86_register(r)); }
inline static constexpr Register8  part1b(Register r) { return part1b(to_x86_register(r)); }


inline void append_instruction(StringBuilder &builder, s64 idx, Instruction i) {
	using enum Register32;
	switch (i.kind) {
	}
}

}
