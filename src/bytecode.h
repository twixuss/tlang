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

	bool is(Register r) {
		return base == r && !r1_scale_index && !r2_scale && !c;
	}

	Address() = default;
	explicit Address(Register base) : base(base) {}
};

inline Address operator+(Register r, s64 c) {
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
inline Address operator-(Register r, s64 c) { return r + (-c); }
inline Address operator-(Address a, s64 c) { return a + (-c); }

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
e(mov_rc             , m(Register, d) m(s64, s)) \
e(mov_rr             , m(Register, d) m(Register, s)) \
e(mov1_rm            , m(Register, d) m(Address, s)) \
e(mov2_rm            , m(Register, d) m(Address, s)) \
e(mov4_rm            , m(Register, d) m(Address, s)) \
e(mov8_rm            , m(Register, d) m(Address, s)) \
e(mov1_mc            , m(Address, d) m(s64, s)) \
e(mov2_mc            , m(Address, d) m(s64, s)) \
e(mov4_mc            , m(Address, d) m(s64, s)) \
e(mov8_mc            , m(Address, d) m(s64, s)) \
e(mov1_mr            , m(Address, d) m(Register, s)) \
e(mov2_mr            , m(Address, d) m(Register, s)) \
e(mov4_mr            , m(Address, d) m(Register, s)) \
e(mov8_mr            , m(Address, d) m(Register, s)) \
e(xchg_rr            , m(Register, a) m(Register, b)) \
e(xchg1_mr           , m(Address, a) m(Register, b)) \
e(xchg2_mr           , m(Address, a) m(Register, b)) \
e(xchg4_mr           , m(Address, a) m(Register, b)) \
e(xchg8_mr           , m(Address, a) m(Register, b)) \
e(movsx21_rm         , m(Register, d) m(Address, s)) \
e(movsx41_rm         , m(Register, d) m(Address, s)) \
e(movsx81_rm         , m(Register, d) m(Address, s)) \
e(movsx42_rm         , m(Register, d) m(Address, s)) \
e(movsx82_rm         , m(Register, d) m(Address, s)) \
e(movsx84_rm         , m(Register, d) m(Address, s)) \
e(movzx21_rm         , m(Register, d) m(Address, s)) \
e(movzx41_rm         , m(Register, d) m(Address, s)) \
e(movzx81_rm         , m(Register, d) m(Address, s)) \
e(movzx42_rm         , m(Register, d) m(Address, s)) \
e(movzx82_rm         , m(Register, d) m(Address, s)) \
e(movzx84_rm         , m(Register, d) m(Address, s)) \
e(movsx21_rr         , m(Register, d) m(Register, s)) \
e(movsx41_rr         , m(Register, d) m(Register, s)) \
e(movsx81_rr         , m(Register, d) m(Register, s)) \
e(movsx42_rr         , m(Register, d) m(Register, s)) \
e(movsx82_rr         , m(Register, d) m(Register, s)) \
e(movsx84_rr         , m(Register, d) m(Register, s)) \
e(movzx21_rr         , m(Register, d) m(Register, s)) \
e(movzx41_rr         , m(Register, d) m(Register, s)) \
e(movzx81_rr         , m(Register, d) m(Register, s)) \
e(movzx42_rr         , m(Register, d) m(Register, s)) \
e(movzx82_rr         , m(Register, d) m(Register, s)) \
e(movzx84_rr         , m(Register, d) m(Register, s)) \
e(lea                , m(Register, d) m(Address, s)) \
e(push_c             , m(s64, s)) \
e(push_r             , m(Register, s)) \
e(push_f             , m(XRegister, s)) \
e(push_m             , m(Address, s)) \
e(mov_re             , m(Register, d) m(String, s)) \
e(pop_r              , m(Register, d)) \
e(pop_f              , m(XRegister, d)) \
e(pop_m              , m(Address, d)) \
e(ret                , ) \
e(shr_rc             , m(Register, d) m(s64, s)) \
e(shr_rr             , m(Register, d) m(Register, s)) \
e(shr_rm             , m(Register, d) m(Address, s)) \
e(shr_mc             , m(Address, d) m(s64, s)) \
e(shr_mr             , m(Address, d) m(Register, s)) \
e(shl_rc             , m(Register, d) m(s64, s)) \
e(shl_rr             , m(Register, d) m(Register, s)) \
e(shl_rm             , m(Register, d) m(Address, s)) \
e(shl_mc             , m(Address, d) m(s64, s)) \
e(shl_mr             , m(Address, d) m(Register, s)) \
e(add_rc             , m(Register, d) m(s64, s)) \
e(add_rr             , m(Register, d) m(Register, s)) \
e(add_rm             , m(Register, d) m(Address, s)) \
e(add_mc             , m(Address, d) m(s64, s)) \
e(add_mr             , m(Address, d) m(Register, s)) \
e(sub_rc             , m(Register, d) m(s64, s)) \
e(sub_rr             , m(Register, d) m(Register, s)) \
e(sub_rm             , m(Register, d) m(Address, s)) \
e(sub_mc             , m(Address, d) m(s64, s)) \
e(sub_mr             , m(Address, d) m(Register, s)) \
e(mul_rc             , m(Register, d) m(s64, s)) \
e(mul_rr             , m(Register, d) m(Register, s)) \
e(mul_rm             , m(Register, d) m(Address, s)) \
e(mul_mc             , m(Address, d) m(s64, s)) \
e(mul_mr             , m(Address, d) m(Register, s)) \
e(div_rc             , m(Register, d) m(s64, s)) \
e(div_rr             , m(Register, d) m(Register, s)) \
e(div_rm             , m(Register, d) m(Address, s)) \
e(div_mc             , m(Address, d) m(s64, s)) \
e(div_mr             , m(Address, d) m(Register, s)) \
e(mod_rc             , m(Register, d) m(s64, s)) \
e(mod_rr             , m(Register, d) m(Register, s)) \
e(mod_rm             , m(Register, d) m(Address, s)) \
e(mod_mc             , m(Address, d) m(s64, s)) \
e(mod_mr             , m(Address, d) m(Register, s)) \
e(rotl_rc            , m(Register, d) m(s64, s)) \
e(rotr_rc            , m(Register, d) m(s64, s)) \
e(not_r              , m(Register, d)) \
e(not_m              , m(Address, d)) \
e(or_rc              , m(Register, d) m(s64, s)) \
e(or_rr              , m(Register, d) m(Register, s)) \
e(or1_rm             , m(Register, d) m(Address, s)) \
e(or2_rm             , m(Register, d) m(Address, s)) \
e(or4_rm             , m(Register, d) m(Address, s)) \
e(or8_rm             , m(Register, d) m(Address, s)) \
e(or_mc              , m(Address, d) m(s64, s)) \
e(or_mr              , m(Address, d) m(Register, s)) \
e(and_rc             , m(Register, d) m(s64, s)) \
e(and_rr             , m(Register, d) m(Register, s)) \
e(and_rm             , m(Register, d) m(Address, s)) \
e(and_mc             , m(Address, d) m(s64, s)) \
e(and_mr             , m(Address, d) m(Register, s)) \
e(xor_rc             , m(Register, d) m(s64, s)) \
e(xor_rr             , m(Register, d) m(Register, s)) \
e(xor_rm             , m(Register, d) m(Address, s)) \
e(xor_mc             , m(Address, d) m(s64, s)) \
e(xor_mr             , m(Address, d) m(Register, s)) \
e(negi_r             , m(Register, d)) \
e(negi8_m            , m(Address, d)) \
e(negi16_m           , m(Address, d)) \
e(negi32_m           , m(Address, d)) \
e(negi64_m           , m(Address, d)) \
/* Comparison with destination */ \
e(cmpu1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmpu2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmpu4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmpu8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmps1              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmps2              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmps4              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmps8              , m(Register, d) m(Register, a) m(Register, b) m(Comparison, c)) \
e(cmpstr             , m(Register, d) m(Address, a) m(Address, b)) /* d is count */ \
e(jz_cr              , m(s64, offset) m(Register, reg)) \
e(jnz_cr             , m(s64, offset) m(Register, reg)) \
/* Comparison without destination (uses flags) */ \
e(cmpf1              , m(Register, a) m(Register, b)) \
e(cmpf2              , m(Register, a) m(Register, b)) \
e(cmpf4              , m(Register, a) m(Register, b)) \
e(cmpf8              , m(Register, a) m(Register, b)) \
e(jef_c              , m(s64, offset)) \
e(jnef_c             , m(s64, offset)) \
e(jlf_c              , m(s64, offset)) \
e(jgf_c              , m(s64, offset)) \
e(jlef_c             , m(s64, offset)) \
e(jgef_c             , m(s64, offset)) \
e(jmp                , m(s64, offset)) \
e(call_c             , m(s64, constant) m(AstLambda *, lambda)) \
e(call_r             , m(Register, s)   m(AstLambda *, lambda)) \
e(call_m             , m(Address, s)    m(AstLambda *, lambda)) \
e(copyf_mmc          , m(Address, d) m(Address, s) m(s64, size)) \
e(copyb_mmc          , m(Address, d) m(Address, s) m(s64, size)) \
e(copyf_mmr          , m(Address, d) m(Address, s) m(Register, size)) \
e(copyb_mmr          , m(Address, d) m(Address, s) m(Register, size)) \
e(setf_mcc           , m(Address, d) m(s8, s) m(s32, size)) \
e(setb_mcc           , m(Address, d) m(s8, s) m(s32, size)) \
e(begin_lambda       , m(AstLambda *, lambda)) \
e(end_lambda         , m(AstLambda *, lambda)) \
e(cvt_f32_s32        , m(Register, d)) \
e(cvt_s32_f32        , m(Register, d)) \
e(cvt_f64_s64        , m(Register, d)) \
e(cvt_s64_f64        , m(Register, d)) \
e(mov_fr             , m(XRegister, d) m(Register, s)) \
e(mov_rf             , m(Register, d) m(XRegister, s)) \
e(mov1_xm            , m(XRegister, d) m(Address, s)) \
e(mov2_xm            , m(XRegister, d) m(Address, s)) \
e(mov4_xm            , m(XRegister, d) m(Address, s)) \
e(mov8_xm            , m(XRegister, d) m(Address, s)) \
e(add_ff             , m(Register, d) m(Register, s)) \
e(mul_ff             , m(Register, d) m(Register, s)) \
e(sub_ff             , m(Register, d) m(Register, s)) \
e(div_ff             , m(Register, d) m(Register, s)) \
e(xor_ff             , m(XRegister, d) m(XRegister, s)) \
e(tobool_r           , m(Register, d)) \
e(toboolnot_r        , m(Register, d)) \
e(jmp_label          , ) \
e(noop               , ) \
e(prepare_stack      , m(s64, byte_count)) \
e(debug_break        , ) \
e(debug_line         , m(u32, line)) \
e(debug_start_lambda , m(Expression<AstLambda>, lambda)) \



enum class InstructionKind : u8 {

#define e(name, ...) name,
ENUMERATE_INSTRUCTIONS
#undef e


	count,
};

// Make sure instruction count does not go over 256
static_assert((int)InstructionKind::count >= 127);

inline umm append(StringBuilder &builder, InstructionKind kind) {
	switch (kind) {
		using enum InstructionKind;
#define e(name, ...) case name: return append(builder, #name);
ENUMERATE_INSTRUCTIONS
#undef e
		default: return append(builder, "unknown");
	}
}

#pragma pack(push, 1)

struct Instruction {
	// put all the variants first to allow type punning from a variant back to `Instruction`
	union {
#define ADDRESS(name) Address name
#define e(name, ...) struct { __VA_ARGS__ } name;
#define m(type, name) type name;
ENUMERATE_INSTRUCTIONS
#undef m
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

inline static constexpr auto QWERTY1 = sizeof RegisterOrAddress;
inline static constexpr auto QWERTY2 = sizeof Instruction;

using ExternLibraries = Map<Span<utf8>, List<Span<utf8>>>;

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
	InstructionList instructions;
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

#define DECLARE_OUTPUT_BUILDER extern "C" __declspec(dllexport) void tlang_build_output(CompilerContext &context, Bytecode &bytecode)
using OutputBuilder = void (*)(CompilerContext &context, Bytecode &bytecode);

#define DECLARE_TARGET_INFORMATION_GETTER extern "C" __declspec(dllexport) void tlang_get_target_information(CompilerContext &context)
using TargetInformationGetter = void (*)(CompilerContext &context);
