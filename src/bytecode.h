#pragma once
#include "ast.h"

#define BYTECODE_DEBUG TL_DEBUG

// XMM registers
enum class XRegister : u8 {
	x0,
	x1,
	x2,
	x3,
};

inline static constexpr Array<u8, 5> lea_scales {
	0, 1, 2, 4, 8
};

struct Address {
	Register base = {};
	Register r1 = {};
	Register r2 = {};
	// stored value | represented scale
	// 0              0
	// 1              1
	// 2              2
	// 3              4
	// 4              8
	u8 r1_scale_index : 3 = {};
	u8 r2_scale : 1 = {}; // either 0 or 1
	s32 c = {};

	forceinline bool is(Register r) {
		return base == r && !r1_scale_index && !r2_scale && !c;
	}

	forceinline Address() = default;
	forceinline explicit Address(Register base) : base(base) {}
};

forceinline Address operator+(Register r, s64 c) {
	assert(c == (s64)(s32)c);

	Address a;
	a.base = r;
	a.c = (s32)c;
	return a;
}
forceinline Address operator+(Address a, s64 c) {
	assert((s64)(a.c + (s32)c) == ((s64)a.c + c));
	a.c += c;
	return a;
}
forceinline Address operator-(Register r, s64 c) { return r + (-c); }
forceinline Address operator-(Address a, s64 c) { return a + (-c); }

// enum class ValueLocation {
// 	Register,       // value is in a register
// 	address,        // value is in memory
// 	address_itself, // value is an address itself
// };

struct RegisterOrAddress {
	bool is_in_register : 1;
	bool value_is_address : 1;
	union {
		Register reg;
		Address address;
	};

	RegisterOrAddress(Register reg) : is_in_register(true), reg(reg) {}
	RegisterOrAddress(Address address) : is_in_register(false), address(address) {}

	Address ensure_address() {
		assert(!is_in_register);
		return address;
	}
};

/*

Naming convention:

{instruction}_{param0, param1, ...}
where param is any of:
	c: constant,
	r: general purpose register,
	f: floating point register,
	m: memory,

Destination of an instruction is first

*/

// #define e(name, ...)
// #define m(type, name)
// ENUMERATE_INSTRUCTIONS
// #undef m
// #undef e

#define ENUMERATE_INSTRUCTIONS \
e(mov, rc             , m(Register, d) m(s64, s)) \
e(mov, rr             , m(Register, d) m(Register, s)) \
e(mov1, rm            , m(Register, d) m(Address, s)) \
e(mov2, rm            , m(Register, d) m(Address, s)) \
e(mov4, rm            , m(Register, d) m(Address, s)) \
e(mov8, rm            , m(Register, d) m(Address, s)) \
e(mov1, mc            , m(Address, d) m(s64, s)) \
e(mov2, mc            , m(Address, d) m(s64, s)) \
e(mov4, mc            , m(Address, d) m(s64, s)) \
e(mov8, mc            , m(Address, d) m(s64, s)) \
e(mov1, mr            , m(Address, d) m(Register, s)) \
e(mov2, mr            , m(Address, d) m(Register, s)) \
e(mov4, mr            , m(Address, d) m(Register, s)) \
e(mov8, mr            , m(Address, d) m(Register, s)) \
e(xchg, rr            , m(Register, a) m(Register, b)) \
e(xchg1, mr           , m(Address, a) m(Register, b)) \
e(xchg2, mr           , m(Address, a) m(Register, b)) \
e(xchg4, mr           , m(Address, a) m(Register, b)) \
e(xchg8, mr           , m(Address, a) m(Register, b)) \
e(movsx21, rm         , m(Register, d) m(Address, s)) \
e(movsx41, rm         , m(Register, d) m(Address, s)) \
e(movsx81, rm         , m(Register, d) m(Address, s)) \
e(movsx42, rm         , m(Register, d) m(Address, s)) \
e(movsx82, rm         , m(Register, d) m(Address, s)) \
e(movsx84, rm         , m(Register, d) m(Address, s)) \
e(movzx21, rm         , m(Register, d) m(Address, s)) \
e(movzx41, rm         , m(Register, d) m(Address, s)) \
e(movzx81, rm         , m(Register, d) m(Address, s)) \
e(movzx42, rm         , m(Register, d) m(Address, s)) \
e(movzx82, rm         , m(Register, d) m(Address, s)) \
e(movzx84, rm         , m(Register, d) m(Address, s)) \
e(movsx21, rr         , m(Register, d) m(Register, s)) \
e(movsx41, rr         , m(Register, d) m(Register, s)) \
e(movsx81, rr         , m(Register, d) m(Register, s)) \
e(movsx42, rr         , m(Register, d) m(Register, s)) \
e(movsx82, rr         , m(Register, d) m(Register, s)) \
e(movsx84, rr         , m(Register, d) m(Register, s)) \
e(movzx21, rr         , m(Register, d) m(Register, s)) \
e(movzx41, rr         , m(Register, d) m(Register, s)) \
e(movzx81, rr         , m(Register, d) m(Register, s)) \
e(movzx42, rr         , m(Register, d) m(Register, s)) \
e(movzx82, rr         , m(Register, d) m(Register, s)) \
e(movzx84, rr         , m(Register, d) m(Register, s)) \
w(lea                 , m(Register, d) m(Address, s)) \
e(push, c             , m(s64, s)) \
e(push, r             , m(Register, s)) \
e(push, f             , m(XRegister, s)) \
e(push, m             , m(Address, s)) \
e(mov, re             , m(Register, d) m(String, s)) \
e(pop, r              , m(Register, d)) \
e(pop, f              , m(XRegister, d)) \
e(pop, m              , m(Address, d)) \
w(ret                 , ) \
e(shr, rc             , m(Register, d) m(s64, s)) \
e(shr, rr             , m(Register, d) m(Register, s)) \
e(shr, rm             , m(Register, d) m(Address, s)) \
e(shr, mc             , m(Address, d) m(s64, s)) \
e(shr, mr             , m(Address, d) m(Register, s)) \
e(shl, rc             , m(Register, d) m(s64, s)) \
e(shl, rr             , m(Register, d) m(Register, s)) \
e(shl, rm             , m(Register, d) m(Address, s)) \
e(shl, mc             , m(Address, d) m(s64, s)) \
e(shl, mr             , m(Address, d) m(Register, s)) \
e(add, rc             , m(Register, d) m(s64, s)) \
e(add, rr             , m(Register, d) m(Register, s)) \
e(add, rm             , m(Register, d) m(Address, s)) \
e(add, mc             , m(Address, d) m(s64, s)) \
e(add, mr             , m(Address, d) m(Register, s)) \
e(sub, rc             , m(Register, d) m(s64, s)) \
e(sub, rr             , m(Register, d) m(Register, s)) \
e(sub, rm             , m(Register, d) m(Address, s)) \
e(sub, mc             , m(Address, d) m(s64, s)) \
e(sub, mr             , m(Address, d) m(Register, s)) \
e(mul, rc             , m(Register, d) m(s64, s)) \
e(mul, rr             , m(Register, d) m(Register, s)) \
e(mul, rm             , m(Register, d) m(Address, s)) \
e(mul, mc             , m(Address, d) m(s64, s)) \
e(mul, mr             , m(Address, d) m(Register, s)) \
e(div, rc             , m(Register, d) m(s64, s)) \
e(div, rr             , m(Register, d) m(Register, s)) \
e(div, rm             , m(Register, d) m(Address, s)) \
e(div, mc             , m(Address, d) m(s64, s)) \
e(div, mr             , m(Address, d) m(Register, s)) \
e(mod, rc             , m(Register, d) m(s64, s)) \
e(mod, rr             , m(Register, d) m(Register, s)) \
e(mod, rm             , m(Register, d) m(Address, s)) \
e(mod, mc             , m(Address, d) m(s64, s)) \
e(mod, mr             , m(Address, d) m(Register, s)) \
e(rotl, rc            , m(Register, d) m(s64, s)) \
e(rotr, rc            , m(Register, d) m(s64, s)) \
e(not, r              , m(Register, d)) \
e(not, m              , m(Address, d)) \
e(or, rc              , m(Register, d) m(s64, s)) \
e(or, rr              , m(Register, d) m(Register, s)) \
e(or1, rm             , m(Register, d) m(Address, s)) \
e(or2, rm             , m(Register, d) m(Address, s)) \
e(or4, rm             , m(Register, d) m(Address, s)) \
e(or8, rm             , m(Register, d) m(Address, s)) \
e(or, mc              , m(Address, d) m(s64, s)) \
e(or, mr              , m(Address, d) m(Register, s)) \
e(and, rc             , m(Register, d) m(s64, s)) \
e(and, rr             , m(Register, d) m(Register, s)) \
e(and, rm             , m(Register, d) m(Address, s)) \
e(and, mc             , m(Address, d) m(s64, s)) \
e(and, mr             , m(Address, d) m(Register, s)) \
e(xor, rc             , m(Register, d) m(s64, s)) \
e(xor, rr             , m(Register, d) m(Register, s)) \
e(xor, rm             , m(Register, d) m(Address, s)) \
e(xor, mc             , m(Address, d) m(s64, s)) \
e(xor, mr             , m(Address, d) m(Register, s)) \
e(negi, r             , m(Register, d)) \
e(negi8, m            , m(Address, d)) \
e(negi16, m           , m(Address, d)) \
e(negi32, m           , m(Address, d)) \
e(negi64, m           , m(Address, d)) \
/* Comparison with destination */ \
w(cmpu1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpstr             , m(Register, d) m(Address, a) m(Address, b)) /* d is count */ \
e(jz, cr              , m(s64, offset) m(Register, reg)) \
e(jnz, cr             , m(s64, offset) m(Register, reg)) \
/* Comparison without destination (uses flags) */ \
w(cmpf1               , m(Register, a) m(Register, b)) \
w(cmpf2               , m(Register, a) m(Register, b)) \
w(cmpf4               , m(Register, a) m(Register, b)) \
w(cmpf8               , m(Register, a) m(Register, b)) \
e(jef, c              , m(s64, offset)) \
e(jnef, c             , m(s64, offset)) \
e(jlf, c              , m(s64, offset)) \
e(jgf, c              , m(s64, offset)) \
e(jlef, c             , m(s64, offset)) \
e(jgef, c             , m(s64, offset)) \
w(jmp                 , m(s64, offset)) \
e(call, c             , m(s64, constant) m(AstLambda *, lambda)) \
e(call, r             , m(Register, s)   m(AstLambda *, lambda)) \
e(call, m             , m(Address, s)    m(AstLambda *, lambda)) \
e(copyf, mmc          , m(Address, d) m(Address, s) m(s64, size)) \
e(copyb, mmc          , m(Address, d) m(Address, s) m(s64, size)) \
e(copyf, mmr          , m(Address, d) m(Address, s) m(Register, size)) \
e(copyb, mmr          , m(Address, d) m(Address, s) m(Register, size)) \
e(set, mcc            , m(Address, d) m(s8, s) m(s32, size)) \
w(begin_lambda        , m(AstLambda *, lambda)) \
w(end_lambda          , m(AstLambda *, lambda)) \
w(cvt_f32_s32         , m(Register, d)) \
w(cvt_s32_f32         , m(Register, d)) \
w(cvt_f64_s64         , m(Register, d)) \
w(cvt_s64_f64         , m(Register, d)) \
e(mov, fr             , m(XRegister, d) m(Register, s)) \
e(mov, rf             , m(Register, d) m(XRegister, s)) \
e(mov1, xm            , m(XRegister, d) m(Address, s)) \
e(mov2, xm            , m(XRegister, d) m(Address, s)) \
e(mov4, xm            , m(XRegister, d) m(Address, s)) \
e(mov8, xm            , m(XRegister, d) m(Address, s)) \
e(add, ff             , m(Register, d) m(Register, s)) \
e(mul, ff             , m(Register, d) m(Register, s)) \
e(sub, ff             , m(Register, d) m(Register, s)) \
e(div, ff             , m(Register, d) m(Register, s)) \
e(xor, ff             , m(XRegister, d) m(XRegister, s)) \
e(tobool, r           , m(Register, d)) \
e(toboolnot, r        , m(Register, d)) \
w(jmp_label           , ) \
w(noop                , ) \
w(prepare_stack       , m(s64, byte_count)) \
w(debug_break         , ) \
w(debug_line          , m(u32, line)) \
w(debug_start_lambda  , m(AstLambda *, lambda)) \



enum class InstructionKind : u8 {

#define e(name, suffix, ...) name##_##suffix,
#define w(name, ...) name,
ENUMERATE_INSTRUCTIONS
#undef w
#undef e


	count,
};

// Make sure instruction count does not go over 256
static_assert((int)InstructionKind::count >= 127);

inline umm append(StringBuilder &builder, InstructionKind kind) {
	switch (kind) {
		using enum InstructionKind;
#define e(name, suffix, ...) case name##_##suffix: return append(builder, #name);
#define w(name, ...)         case name           : return append(builder, #name);
ENUMERATE_INSTRUCTIONS
#undef w
#undef e
		default: return append(builder, "unknown");
	}
}

#pragma pack(push, 1)

#define __COUNTER__ 0

inline static constexpr umm instruction_counter_base = __COUNTER__ + 1;

struct Instruction {
	// put all the variants first to allow type punning from a variant back to `Instruction`
	union {
#define e(name, suffix, ...) struct { __VA_ARGS__ } name##_##suffix;
#define w(name, ...)         struct { __VA_ARGS__ } name;
#define m(type, name) type name;
ENUMERATE_INSTRUCTIONS
#undef m
#undef w
#undef e
	};
	InstructionKind kind;
#if BYTECODE_DEBUG
	String comment;
	u32 line;
	AstNode *node;
#endif
};

#pragma pack(pop)

inline auto get_instruction_variant() {

}

inline static constexpr auto QWERTY1 = sizeof RegisterOrAddress;
inline static constexpr auto QWERTY2 = sizeof Instruction;

using ExternLibraries = Map<String, List<String>>;

using InstructionList = BlockList<Instruction>;

struct SectionBuilder {
	struct Part {
		BlockList<u8> builder;
		s64 reference = -1;
		bool is_constant = false;
	};

	List<Part> parts;
	s64 count = 0;

	SectionBuilder() {
		parts.resize(1);
	}

	s64 append(Span<u8> string) {
		auto result = count;
		for (auto c : string)
			parts.back().builder.add(c);
		count += string.count;
		return result;
	}
	s64 allocate(s64 count) {
		auto result = this->count;
		this->count += count;
		while (count--)
			parts.back().builder.add(0);
		return result;
	}
};

struct Bytecode {
	List<Instruction> instructions;
	ExternLibraries extern_libraries;
};

Bytecode build_bytecode();

void print_bytecode(InstructionList &instructions);

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

#define DECLARE_OUTPUT_BUILDER extern "C" __declspec(dllexport) void tlang_build_output(Compiler &compiler, Bytecode &bytecode)
using OutputBuilder = void (*)(Compiler &compiler, Bytecode &bytecode);

#define DECLARE_TARGET_INFORMATION_GETTER extern "C" __declspec(dllexport) void tlang_get_target_information(Compiler &compiler)
using TargetInformationGetter = void (*)(Compiler &compiler);

template <class T = void>
inline umm print_instruction(Instruction i) {
	switch (i.kind) {
		using enum InstructionKind;

#define e(name, suffix, ...) case name##_##suffix: { auto &sub = i.name##_##suffix; return print(#name " ") __VA_ARGS__ + 0; }
#define w(name, ...)         case name: { auto &sub = i.name; return print(#name " ") __VA_ARGS__ + 0; }
#define m(type, name) + print("{} ", sub.name)
ENUMERATE_INSTRUCTIONS
#undef m
#undef w
#undef e

		default: return print("unknown {}", (u64)i.kind);
	}
}

inline const Array<StaticList<u8, 2>, (u16)InstructionKind::count> address_members_offsets = [](){
	Array<StaticList<u8, 2>, (u16)InstructionKind::count> result = {};

#define m(type, name) \
	if (is_same<type, Address>) \
		result[index].add(offsetof(InstName, name));

#define e(name, suffix, ...) \
{ \
	using InstName = decltype(Instruction::name##_##suffix); \
	constexpr u8 index = (u8)InstructionKind::name##_##suffix; \
	__VA_ARGS__ \
}

#define w(name, ...) \
{ \
	using InstName = decltype(Instruction::name); \
	constexpr u8 index = (u8)InstructionKind::name; \
	__VA_ARGS__ \
}

	ENUMERATE_INSTRUCTIONS
#undef e
#undef m
#undef w

#undef OFFSET

	return result;
}();
