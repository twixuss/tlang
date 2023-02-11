#pragma once
#include "compiler.h"

#define BYTECODE_DEBUG TL_DEBUG

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

struct ScaledRegister {
	Register r = {};
	u8 scale_index = {};
};

forceinline Address operator+(Register r, s64 c) {
	assert(c == (s64)(s32)c);

	Address a;
	a.base = r;
	a.c = (s32)c;
	return a;
}
forceinline Address operator+(Register r, ScaledRegister b) {
	Address a;
	a.base = r;
	a.r1 = b.r;
	a.r1_scale_index = b.scale_index;
	return a;
}
forceinline Address operator+(Address a, s64 c) {
	assert((s64)(a.c + (s32)c) == ((s64)a.c + c));
	a.c += c;
	return a;
}
forceinline Address operator+(Address a, ScaledRegister b) {
	assert(a.r1_scale_index == 0);
	a.r1 = b.r;
	a.r1_scale_index = b.scale_index;
	return a;
}
forceinline Address operator-(Register r, s64 c) { return r + (-c); }
forceinline Address operator-(Address a, s64 c) { return a + (-c); }
forceinline ScaledRegister operator*(Register r, s64 c) {
	ScaledRegister result;
	result.r = r;
	switch (c) {
		case 0: result.scale_index = 0; break;
		case 1: result.scale_index = 1; break;
		case 2: result.scale_index = 2; break;
		case 4: result.scale_index = 3; break;
		case 8: result.scale_index = 4; break;
		default: invalid_code_path();
	}
	return result;
}

// enum class ValueLocation {
// 	Register,       // value is in a register
// 	address,        // value is in memory
// 	address_itself, // value is an address itself
// };

struct RegisterOrAddress {
	bool is_in_register : 1;
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

#define DEFINE_ENUM_MEMBER(name) name,
#define APPEND_ENUM_MEMBER(name) case name: return append(builder, #name);

#define MAKE_ENUM(iterator, name) \
	enum class name { \
		iterator(DEFINE_ENUM_MEMBER) \
	}; \
	inline umm append(StringBuilder &builder, name v) { \
		switch (v) { \
			using enum name; \
			iterator(APPEND_ENUM_MEMBER) \
			default: return append(builder, "(unknown " #name ")"); \
		} \
	}


#define ROUNDING_MODE_ENUM(e) \
e(to_negative_infinity) \
e(to_positive_infinity) \
e(to_zero) \
e(to_closest_integer) \

MAKE_ENUM(ROUNDING_MODE_ENUM, RoundingMode);

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
e(push, m             , m(Address, s)) \
e(mov, re             , m(Register, d) m(String, s)) \
e(pop, r              , m(Register, d)) \
e(pop, m              , m(Address, d)) \
w(ret                 , ) \
e(sar, rc             , m(Register, d) m(s64, s)) \
e(sar, rr             , m(Register, d) m(Register, s)) \
e(slr, rc             , m(Register, d) m(s64, s)) \
e(slr, rr             , m(Register, d) m(Register, s)) \
e(shl, rc             , m(Register, d) m(s64, s)) \
e(shl, rr             , m(Register, d) m(Register, s)) \
e(add, rc             , m(Register, d) m(s64, s)) \
e(add, rr             , m(Register, d) m(Register, s)) \
e(sub, rc             , m(Register, d) m(s64, s)) \
e(sub, rr             , m(Register, d) m(Register, s)) \
e(mul, rc             , m(Register, d) m(s64, s)) \
e(mul, rr             , m(Register, d) m(Register, s)) \
e(divs, rc             , m(Register, d) m(s64, s)) \
e(divs, rr             , m(Register, d) m(Register, s)) \
e(mods, rc             , m(Register, d) m(s64, s)) \
e(mods, rr             , m(Register, d) m(Register, s)) \
e(divu, rc             , m(Register, d) m(s64, s)) \
e(divu, rr             , m(Register, d) m(Register, s)) \
e(modu, rc             , m(Register, d) m(s64, s)) \
e(modu, rr             , m(Register, d) m(Register, s)) \
e(rotl, rc            , m(Register, d) m(s64, s)) \
e(rotr, rc            , m(Register, d) m(s64, s)) \
e(not, r              , m(Register, d)) \
e(or, rc              , m(Register, d) m(s64, s)) \
e(or, rr              , m(Register, d) m(Register, s)) \
e(and, rc             , m(Register, d) m(s64, s)) \
e(and, rr             , m(Register, d) m(Register, s)) \
e(xor, rc             , m(Register, d) m(s64, s)) \
e(xor, rr             , m(Register, d) m(Register, s)) \
e(sbxor, rc           , m(Register, d) m(s64, s)) \
e(sbxor, rr           , m(Register, d) m(Register, s)) \
e(negi, r             , m(Register, d)) \
/* Comparison with destination */ \
/* Unsigned integers */ \
w(cmpu1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpu8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
/* Signed integers */ \
w(cmps1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmps8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
/* Floats */ \
w(cmpf4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpf8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
w(cmpstr             , m(Register, d) m(Address, a) m(Address, b)) /* d is count */ \
e(jz, cr              , m(s64, offset) m(Register, reg)) \
e(jnz, cr             , m(s64, offset) m(Register, reg)) \
/* Comparison without destination (uses flags) */ \
w(cmpflag1              , m(Register, a) m(Register, b)) \
w(cmpflag2              , m(Register, a) m(Register, b)) \
w(cmpflag4              , m(Register, a) m(Register, b)) \
w(cmpflag8              , m(Register, a) m(Register, b)) \
e(jef, c              , m(s64, offset)) \
e(jnef, c             , m(s64, offset)) \
e(jlf, c              , m(s64, offset)) \
e(jgf, c              , m(s64, offset)) \
e(jlef, c             , m(s64, offset)) \
e(jgef, c             , m(s64, offset)) \
e(jmp, c              , m(s64, offset)) \
e(jmp, r              , m(Register, d)) \
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
w(cvt_f32_f64         , m(Register, d)) \
w(cvt_f64_f32         , m(Register, d)) \
e(add4, ff            , m(Register, d) m(Register, s)) \
e(mul4, ff            , m(Register, d) m(Register, s)) \
e(sub4, ff            , m(Register, d) m(Register, s)) \
e(div4, ff            , m(Register, d) m(Register, s)) \
e(add8, ff            , m(Register, d) m(Register, s)) \
e(mul8, ff            , m(Register, d) m(Register, s)) \
e(sub8, ff            , m(Register, d) m(Register, s)) \
e(div8, ff            , m(Register, d) m(Register, s)) \
e(tobool, r           , m(Register, d)) \
e(toboolnot, r        , m(Register, d)) \
e(sqrt4, f            , m(Register, d)) \
e(sqrt8, f            , m(Register, d)) \
e(floor4, f           , m(Register, d)) \
e(floor8, f           , m(Register, d)) \
e(ceil4, f            , m(Register, d)) \
e(ceil8, f            , m(Register, d)) \
e(round4, f           , m(Register, d)) \
e(round8, f           , m(Register, d)) \
w(jmp_label           , ) \
w(noop                , ) \
w(prepare_stack       , m(s64, byte_count)) \
w(debug_break         , ) \
w(debug_error         , m(String, message)) \
w(debug_line          , m(u32, line)) \
w(debug_start_lambda  , m(AstLambda *, lambda)) \
w(debug_print_int     , m(Register, r)) \
e(adds1, rr           , m(Register, d) m(Register, s)) \
e(adds2, rr           , m(Register, d) m(Register, s)) \
e(adds4, rr           , m(Register, d) m(Register, s)) \
e(adds8, rr           , m(Register, d) m(Register, s)) \
e(subs1, rr           , m(Register, d) m(Register, s)) \
e(subs2, rr           , m(Register, d) m(Register, s)) \
e(subs4, rr           , m(Register, d) m(Register, s)) \
e(subs8, rr           , m(Register, d) m(Register, s)) \



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

namespace {
static auto $() {
	sizeof Register;
	sizeof Address;
	sizeof Instruction;
}
}


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

#define DECLARE_OUTPUT_BUILDER extern "C" __declspec(dllexport) bool tlang_build_output(Span<AstStatement *> global_statements, Bytecode const &bytecode)
using OutputBuilder = bool (*)(Span<AstStatement *> global_statements, Bytecode const &bytecode);

#define DECLARE_TARGET_INFORMATION_GETTER extern "C" __declspec(dllexport) void tlang_get_target_information(Compiler *compiler)
using TargetInformationGetter = void (*)(Compiler *compiler);

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

template <class T = void>
inline void print_bytecode(Span<Instruction> instructions, u64 idx_offset = 0) {
	u64 idx = idx_offset;
	for (auto i : instructions) {
		defer { idx++; };

#if BYTECODE_DEBUG
		if (i.comment.data) {
			with(ConsoleColor::dark_cyan,
				split(i.comment, u8'\n', [&](auto part) {
					auto str = tformat("// {}\n", part);
					print(str);
				})
			);
		}
#endif

		print("{} ", Format(idx, align_left(4, ' ')));

		print_instruction(i);
#if BYTECODE_DEBUG
		with(ConsoleColor::gray, print(" // bytecode.cpp:{}\n", i.line));
#endif
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
