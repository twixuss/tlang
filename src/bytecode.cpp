#include "bytecode.h"
#include "compiler.h"
#include "interpret.h"
// #include "x86_64.h"

// I don't know how optimizations will work with loading lambda parameters' addresses...
// TODO: Figure this out.
#define OPTIMIZE_BYTECODE 0

struct Relocation {
	umm instruction_index;
	AstLambda *lambda;
};

struct StringInfo {
	s64 data_and_size_offset;
	s64 string_offset;

	bool constant;
};

enum class KnownValueKind {
	constant,
	stack_offset,
};

struct KnownValue {
	KnownValueKind kind;
	union {
		s64 constant;
		s64 stack_offset;
	};
};

KnownValue known_constant(s64 constant) { return KnownValue{.kind = KnownValueKind::constant, .constant = constant}; }
KnownValue known_stack_offset(s64 stack_offset) { return KnownValue{.kind = KnownValueKind::stack_offset, .stack_offset = stack_offset}; }

struct RegistersState {
	Optional<KnownValue> state[register_count];
	Optional<KnownValue> &operator[](Register reg) { return state[(u8)reg]; }
};
/*
struct LambdaState {
	AstLambda *lambda = 0;
	RegistersState register_state = {};
};
*/

struct InstructionThatReferencesLambda {
	Instruction *instruction;
	AstLambda *lambda;
};

struct DefinitionAddress {
	bool is_known;
	union {
		RegisterOrAddress computed_address;
		Address           known_address;
	};
};

#if BYTECODE_DEBUG
#define MI(_kind, ...) (Instruction{._kind={__VA_ARGS__}, .kind = InstructionKind::_kind, .line=(u64)__LINE__,})
#else
#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind}
#endif

#define II(kind, ...) add_instruction(MI(kind, __VA_ARGS__))
#define I(kind, ...) (&add_instruction(MI(kind, __VA_ARGS__))->kind)

constexpr umm temporary_register_count = 3;

inline constexpr bool is_temporary_register(umm r) { return r < temporary_register_count; }
inline constexpr bool is_temporary_register(Register r) { return (umm)r < temporary_register_count; }

#define DEBUG_TEMPORARY_REGISTER_ALLOCATION BYTECODE_DEBUG

struct FrameBuilder {
	InstructionList instructions;

	RegisterSet available_registers;
	RegisterSet initial_available_registers;
	RegisterSet used_registers;
	BitSet<temporary_register_count> temporary_register_set;
#if DEBUG_TEMPORARY_REGISTER_ALLOCATION
	std::source_location debug_temporary_register_allocation_source_locations[temporary_register_count];
#endif

	s64 temporary_cursor = 0;
	s64 temporary_size = 0;
	Instruction *temporary_reserver = 0;

	s64 max_stack_space_used_for_call = 0;

	List<InstructionThatReferencesLambda> instructions_that_reference_lambdas;

	struct LoopJmp {
		umm index;
		Instruction *jmp;
		LoopControl control;
	};
	List<List<LoopJmp>> loop_control_stack;

	Scope *current_scope = 0;

	AstNode *current_node = 0;
	String comment;

	void init() {
		//constexpr umm
		for (umm i = allocatable_register_first; i < min(allocatable_register_first + compiler->general_purpose_register_count - temporary_register_count, allocatable_register_last + 1); ++i) {
			available_registers.set(i);
		}
		initial_available_registers = available_registers;

		for (umm i = 0; i < temporary_register_count; ++i)
			temporary_register_set.set(i);
	}
	void free() {}

	Optional<Register> allocate_register() {
#if DEBUG_TEMPORARY_REGISTER_ALLOCATION
		for (umm i = 0; i < temporary_register_count; ++i) {
			if (!temporary_register_set.get(i))
				immediate_error("{} allocated at {}", (Register)i, debug_temporary_register_allocation_source_locations[i]);
		}
#endif
		assert(temporary_register_set.count() == temporary_register_count, "Attempt to call 'append' or 'load_address_of' while a temporary register is being used.");

		auto r = available_registers.pop();
		if (r.has_value())
			used_registers.set(r.value());
		return r.map<Register>();
	}
	void free_register(Register reg) {
		// FIXME: Zero size register hack.
		if (reg == Register::constants)
			return;

		assert(!available_registers.get((umm)reg));
		available_registers.set((umm)reg);
	}

	Register allocate_temporary_register(std::source_location location = std::source_location::current()) {
		auto result = temporary_register_set.pop().value_or([this] {
			invalid_code_path(current_node->location, "INTERNAL ERROR: attempt to use too many temporary registers.");
			return 0;
		});
#if DEBUG_TEMPORARY_REGISTER_ALLOCATION
		debug_temporary_register_allocation_source_locations[result] = location;
#endif

		assert(is_temporary_register(result));
		return (Register)result;
	}
	void free_temporary_register(Register reg) {
		assert(is_temporary_register(reg));
		assert(!temporary_register_set.get((umm)reg));
		temporary_register_set.set((umm)reg);
	}

	Address allocate_temporary_space(s64 size) {
		size = ceil(size, 16ll);
		defer { temporary_cursor += size; };
		return Register::temporary + temporary_cursor;
	}

#define tmpreg(t) auto t = allocate_temporary_register(); defer { free_temporary_register(t); }

	RegisterOrAddress allocate_register_or_temporary(u64 size) {
		if (size == 0) {
			// FIXME: Zero size register hack.
			//        Since the size is zero, we should not write to the destination, so we can return anything here.
			//        But the problem is that this will be passed to free_register, so we also have to deal with that
			//        there.

			return Register::constants;
		}

		if (size <= compiler->stack_word_size) {
			auto reg = allocate_register();
			if (reg.has_value())
				return reg.value_unchecked();
		}

		return allocate_temporary_space(size);
	}

	Instruction *add_instruction(Instruction);

	void append(AstExpression       *, RegisterOrAddress);
	void append(AstBlock            *, RegisterOrAddress);
	void append(AstCall             *, RegisterOrAddress);
	void append(AstIdentifier       *, RegisterOrAddress);
	void append(AstLiteral          *, RegisterOrAddress);
	void append(AstBinaryOperator   *, RegisterOrAddress);
	void append(AstUnaryOperator    *, RegisterOrAddress);
	void append(AstSubscript        *, RegisterOrAddress);
	void append(AstLambda           *, RegisterOrAddress);
	void append(AstIf               *, RegisterOrAddress);
	void append(AstArrayInitializer *, RegisterOrAddress);
	void append(AstMatch            *, RegisterOrAddress);

	[[nodiscard]] RegisterOrAddress append(AstExpression *expression) {
		auto destination = allocate_register_or_temporary(get_size(expression->type));
		append(expression, destination);
		return destination;
	}

	void append(AstStatement           *);
	void append(AstDefinition          *);
	void append(AstReturn              *);
	void append(AstExpressionStatement *);
	void append(AstWhile               *);
	void append(AstAssert              *);
	void append(AstLoopControl         *);

	void append(Scope *scope, Optional<RegisterOrAddress> destination = {});
	void append_return(AstLambda *lambda, AstExpression *expression);

	void load_address_of(AstExpression *expression, RegisterOrAddress destination);
	void load_address_of(AstDefinition *definition, RegisterOrAddress destination);

	[[nodiscard]] RegisterOrAddress load_address_of(AstExpression *expression) { auto destination = allocate_register_or_temporary(compiler->stack_word_size); load_address_of(expression, destination); return destination; }
	[[nodiscard]] RegisterOrAddress load_address_of(AstDefinition *definition) { auto destination = allocate_register_or_temporary(compiler->stack_word_size); load_address_of(definition, destination); return destination; }

	[[nodiscard]] DefinitionAddress get_address_of(AstDefinition *definition);

	void append_memory_set(InstructionList &list, Address d, s64 s, s64 size);
	void append_memory_set(Address d, s64 s, s64 size);
	void append_struct_initializer(AstStruct *Struct, SmallList<AstExpression *> values, RegisterOrAddress destination);

	inline void mov_mr(Address dst, Register src, s64 size) {
		assert(size <= 8);
		switch (size) {
			case 1: I(mov1_mr, dst, src); break;
			case 2: I(mov2_mr, dst, src); break;
			case 4: I(mov4_mr, dst, src); break;
			case 8: I(mov8_mr, dst, src); break;
			case 3:
				I(mov2_mr, dst, src);
				I(rotr_rc, src, 2*8);
				I(mov1_mr, dst + 2, src);
				I(rotl_rc, src, 2*8);
				break;
			case 5:
				I(mov4_mr, dst, src);
				I(rotr_rc, src, 4*8);
				I(mov1_mr, dst + 4, src);
				I(rotl_rc, src, 4*8);
				break;
			case 6:
				I(mov4_mr, dst, src);
				I(rotr_rc, src, 4*8);
				I(mov2_mr, dst + 4, src);
				I(rotl_rc, src, 4*8);
				break;
			case 7:
				I(mov4_mr, dst, src);
				I(rotr_rc, src, 4*8);
				I(mov2_mr, dst + 4, src);
				I(rotr_rc, src, 2*8);
				I(mov1_mr, dst + 6, src);
				I(rotl_rc, src, 6*8);
				break;
		}
	}
	inline void mov_mc(Address dst, s64 src, s64 size) {
		assert(size <= 8);
		switch (size) {
			case 1: I(mov1_mc, dst, src); break;
			case 2: I(mov2_mc, dst, src); break;
			case 4: I(mov4_mc, dst, src); break;
			case 8: I(mov8_mc, dst, src); break;
			case 3:
				I(mov2_mc, dst, src);
				I(mov1_mc, dst + 2, src >> (2*8));
				break;
			case 5:
				I(mov4_mc, dst, src);
				I(mov1_mc, dst + 4, src >> (4*8));
				break;
			case 6:
				I(mov4_mc, dst, src);
				I(mov2_mc, dst + 4, src >> (4*8));
				break;
			case 7:
				I(mov4_mc, dst, src);
				I(mov2_mc, dst + 4, src >> (4*8));
				I(mov1_mc, dst + 6, src >> (6*8));
				break;
		}
	}
	inline void mov_rm(Register dst, Address src, s64 size) {
		assert(size <= 8);
		switch (size) {
			case 1: I(mov1_rm, dst, src); break;
			case 2: I(mov2_rm, dst, src); break;
			case 4: I(mov4_rm, dst, src); break;
			case 8: I(mov8_rm, dst, src); break;
	#if 1
			default: not_implemented();
	#else
			case 3:
				I(xor_rr, dst, dst);
				I(mov1_rm, dst, src + 2);
				I(shl_rc, dst, 2);
				I(or2_rm, dst, src);
				break;
			case 5:
				I(xor_rr, dst, dst);
				I(mov1_rm, dst, src + 4);
				I(shl_rc, dst, 4);
				I(or4_rm, dst, src);
				break;
			case 6:
				I(xor_rr, dst, dst);
				I(mov2_rm, dst, src + 4);
				I(shl_rc, dst, 4);
				I(or4_rm, dst, src);
				break;
			case 7:
				I(xor_rr, dst, dst);
				I(mov1_rm, dst, src + 6);
				I(shl_rc, dst, 2);
				I(or2_rm, dst, src + 4);
				I(shl_rc, dst, 4);
				I(or4_rm, dst, src);
				break;
	#endif

		}
	}

	inline void add_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(add_rc,   t, s); mov_mr(d, t, size); }
	inline void sub_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sub_rc,   t, s); mov_mr(d, t, size); }
	inline void mul_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(mul_rc,   t, s); mov_mr(d, t, size); }
	inline void divs_mc (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(divs_rc,  t, s); mov_mr(d, t, size); }
	inline void divu_mc (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(divu_rc,  t, s); mov_mr(d, t, size); }
	inline void mods_mc (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(mods_rc,  t, s); mov_mr(d, t, size); }
	inline void modu_mc (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(modu_rc,  t, s); mov_mr(d, t, size); }
	inline void xor_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(xor_rc,   t, s); mov_mr(d, t, size); }
	inline void and_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(and_rc,   t, s); mov_mr(d, t, size); }
	inline void  or_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I( or_rc,   t, s); mov_mr(d, t, size); }
	inline void shl_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(shl_rc,   t, s); mov_mr(d, t, size); }
	inline void sar_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sar_rc,   t, s); mov_mr(d, t, size); }
	inline void slr_mc  (Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(slr_rc,   t, s); mov_mr(d, t, size); }
	inline void sbxor_mc(Address d, s64 s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sbxor_rc, t, s); mov_mr(d, t, size); }

	inline void add_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(add_rr,   t, s); mov_mr(d, t, size); }
	inline void sub_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sub_rr,   t, s); mov_mr(d, t, size); }
	inline void mul_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(mul_rr,   t, s); mov_mr(d, t, size); }
	inline void divs_mr (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(divs_rr,  t, s); mov_mr(d, t, size); }
	inline void divu_mr (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(divu_rr,  t, s); mov_mr(d, t, size); }
	inline void mods_mr (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(mods_rr,  t, s); mov_mr(d, t, size); }
	inline void modu_mr (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(modu_rr,  t, s); mov_mr(d, t, size); }
	inline void xor_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(xor_rr,   t, s); mov_mr(d, t, size); }
	inline void and_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(and_rr,   t, s); mov_mr(d, t, size); }
	inline void  or_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I( or_rr,   t, s); mov_mr(d, t, size); }
	inline void shl_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(shl_rr,   t, s); mov_mr(d, t, size); }
	inline void sar_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sar_rr,   t, s); mov_mr(d, t, size); }
	inline void slr_mr  (Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(slr_rr,   t, s); mov_mr(d, t, size); }
	inline void sbxor_mr(Address d, Register s, s64 size) { tmpreg(t); mov_rm(t, d, size); I(sbxor_rr, t, s); mov_mr(d, t, size); }

	inline void not_m(Address d, s64 size) { tmpreg(t); mov_rm(t, d, size); I(not_r, t); mov_mr(d, t, size); }
	inline void negi_m(Address d, s64 size) { tmpreg(t); mov_rm(t, d, size); I(negi_r, t); mov_mr(d, t, size); }

	void copy(RegisterOrAddress dst, RegisterOrAddress src, s64 size, bool reverse);

	void with_definition_address_of(AstDefinition *definition, auto &&fn);

	void append_cast(RegisterOrAddress src, AstExpression *src_type, RegisterOrAddress dst, AstExpression *dst_type);
	void append_cast(AstExpression *src, RegisterOrAddress dst, AstExpression *dst_type);

	void check_null(Register pointer_reg, String location);

#if BYTECODE_DEBUG

	void set_comment(Instruction *i, Span<utf8> string) {
		i->comment = string;
	}

	void push_comment(Span<utf8> string) {
		if (comment.data) {
			comment = (String)concatenate(as_span(comment), '\n', string);
		} else {
			comment = (String)string;
		}
	}

#else

	void noop() {}

#define set_comment(...) noop()
#define push_comment(...) noop()

#endif
};


#define TMP_CHECK \
	auto start_tmp = temporary_cursor; \
	defer { assert(temporary_cursor == start_tmp); }
#define REG_CHECK \
	auto start_reg = available_registers.count(); \
	defer { assert(available_registers.count() == start_reg); }
#define TMP_REG_CHECK \
	auto start_tmp_reg = temporary_register_set.count(); \
	defer { assert(temporary_register_set.count() == start_tmp_reg); }

struct BytecodeBuilder {
	InstructionList builder;
	//SectionBuilder constant_data_builder;
	//SectionBuilder data_builder;
	//umm zero_data_size = 0;
	AstLambda *lambda = 0;

	List<InstructionThatReferencesLambda> instructions_that_reference_lambdas;

	List<Relocation> local_relocations;
	List<Relocation> global_relocations;

	Map<String, s64> constant_strings;

	// FrameBuilder *current_frame = 0;

	void optimize(InstructionList &instructions);

	void propagate_known_addresses(InstructionList &instructions);

	// NOTE: this optimization assumes that registers which are set before a jump are no longer used after a jump.
	void remove_redundant_instructions(InstructionList &instructions);

	void propagate_known_values(InstructionList &instructions);

	void append(AstLambda  *);
};


#if 0
#if BYTECODE_DEBUG
void remove_last_instruction(BytecodeBuilder &builder) {
	auto removed = instructions.pop_back();
	auto &back = instructions.back();
	if (back.comment.data) {
		if (removed.comment.data) {
			back.comment = format(u8"{}\n{}"s, back.comment, removed.comment);
		}
	} else {
		back.comment = removed.comment;
	}
}
#else
void remove_last_instruction(BytecodeBuilder &builder) {
	instructions.pop_back();
}
#endif
#endif

#define LOAD_ADDRESS_INTO_REGISTER(name, source) \
	auto _reg_or_addr = load_address_of(source); \
	defer { \
		if (_reg_or_addr.is_in_register) { \
			free_register(_reg_or_addr.reg); \
		} \
	}; \
	Register name; \
	defer { if (is_temporary_register(name)) free_temporary_register(name); }; \
	if (_reg_or_addr.is_in_register) { \
		name = _reg_or_addr.reg; \
	} else { \
		name = allocate_temporary_register(); \
		mov_rm(name, _reg_or_addr.address, compiler->stack_word_size); \
	}

#define APPEND_INTO_REGISTER(name, source) \
	assert(get_size(source->type) <= compiler->stack_word_size); \
	auto CONCAT(_, __LINE__) = append(source); \
	defer { \
		if (CONCAT(_, __LINE__).is_in_register) { free_register(CONCAT(_, __LINE__).reg); } \
	}; \
	Register name; \
	defer { if (is_temporary_register(name)) free_temporary_register(name); }; \
	if (CONCAT(_, __LINE__).is_in_register) { \
		name = CONCAT(_, __LINE__).reg; \
	} else { \
		name = allocate_temporary_register(); \
		mov_rm(name, CONCAT(_, __LINE__).address, get_size(source->type)); \
	}

#define APPEND2(name1, append1, source1, name2, append2, source2) \
	if constexpr (#append1##s == "append"s) assert(get_size(source1->type) <= compiler->stack_word_size); \
	if constexpr (#append2##s == "append"s) assert(get_size(source2->type) <= compiler->stack_word_size); \
	auto CONCAT(_1, __LINE__) = append1(source1); \
	auto CONCAT(_2, __LINE__) = append2(source2); \
	defer { \
		if (CONCAT(_1, __LINE__).is_in_register) { free_register(CONCAT(_1, __LINE__).reg); } \
		if (CONCAT(_2, __LINE__).is_in_register) { free_register(CONCAT(_2, __LINE__).reg); } \
	}; \
	Register name1; \
	Register name2; \
	defer { if (is_temporary_register(name1)) free_temporary_register(name1); }; \
	defer { if (is_temporary_register(name2)) free_temporary_register(name2); }; \
	if (CONCAT(_1, __LINE__).is_in_register) { \
		name1 = CONCAT(_1, __LINE__).reg; \
	} else { \
		name1 = allocate_temporary_register(); \
		mov_rm(name1, CONCAT(_1, __LINE__).address, get_size(source1->type)); \
	} \
	if (CONCAT(_2, __LINE__).is_in_register) { \
		name2 = CONCAT(_2, __LINE__).reg; \
	} else { \
		name2 = allocate_temporary_register(); \
		mov_rm(name2, CONCAT(_2, __LINE__).address, get_size(source2->type)); \
	}

void FrameBuilder::copy(RegisterOrAddress dst, RegisterOrAddress src, s64 size, bool reverse) {
	if (size == 0)
		return;

	if (dst.is_in_register) {
		if (src.is_in_register) {
			I(mov_rr, dst.reg, src.reg);
		} else {
			mov_rm(dst.reg, src.address, size);
		}
	} else {
		if (src.is_in_register) {
			mov_mr(dst.address, src.reg, size);
		} else {
			REDECLARE_VAL(src, src.address);
			REDECLARE_VAL(dst, dst.address);

			tmpreg(intermediary);

			if (size <= 16) {
				s64 copied = 0;

#define C(n) \
	while (copied + n <= size) { \
		if (reverse) { \
			I(mov##n##_rm, intermediary, src + size - copied - n); \
			I(mov##n##_mr, dst + size - copied - n, intermediary); \
		} else { \
			I(mov##n##_rm, intermediary, src + copied); \
			I(mov##n##_mr, dst + copied, intermediary); \
		} \
		copied += n; \
	}
				C(8);
				C(4);
				C(2);
				C(1);

#undef C
			} else {
				if (reverse) {
					I(copyb_mmc, dst, src, size);
				} else {
					I(copyf_mmc, dst, src, size);
				}
			}
		}
	}
}

Instruction *FrameBuilder::add_instruction(Instruction next) {
#if BYTECODE_DEBUG
	assert(current_node);
	next.node = current_node;
	next.comment = comment;
	comment = {};
#endif

	switch (next.kind) {
		using enum InstructionKind;
		//case mov8_rm:
		//case mov8_mr:
		//case mov8_mc:
		//	debug_break();
	}

#if 0
	auto &ls = *this->ls;
	auto &register_state = ls.register_state;
	auto &body_builder = ls.body_builder;

	using enum InstructionKind;

	// keep track of the stack and registers
	switch (next.kind) {
		case jmp_label:
		case jmp: {
			break;
		}
		case push_c: {
			REDECLARE_REF(next, next.push_c);

			stack_state.push(next.s);
			break;
		}
		case push_r: {
			REDECLARE_REF(next, next.push_r);

			stack_state.push(register_state[next.s]);
			break;
		}
		case push_f: {
			stack_state.push_unknown();
			break;
		}
		case push_m: {
			REDECLARE_REF(next, next.push_m);
#if OPTIMIZE_BYTECODE
			auto value = stack_state.get_value_at(next.s);
			if (value) {
				REDECLARE_REF(value, value.value_unchecked());
				if (value.kind == KnownValueKind::constant) {
					return II(push_c, value.constant);
				}
			}
#endif
			stack_state.push_unknown();
			break;
		}
		case push_a:
		case push_d:
		case push_u:
		case push_t: {
			stack_state.push_unknown();
			break;
		}
		case pop_r: {
			REDECLARE_REF(next, next.pop_r);

			register_state[next.d] = stack_state.pop();
			if (next.d == rs) {
				invalid_code_path("need to keep track of the stack here");
			}

#if OPTIMIZE_BYTECODE
			// NOTE: We don't use memory after stack pointer.
			// It is safe to do these optimizations.
			auto back = body_builder.back();
			switch (back.kind) {
				case push_c: {
					// push 1234  =>  mov r0, 1234
					// pop r0

					REDECLARE_REF(back, back.push_c);
					remove_last_instruction(builder);
					return II(mov_rc, next.d, back.s);
				}
				case push_r: {
					REDECLARE_REF(back, back.push_r);
					remove_last_instruction(builder);
					if (next.d == back.s) {
						// push r0  =>  *noop*
						// pop r0

						return 0;
					}

					// push r0  =>  mov r1, r0
					// pop r1
					return II(mov_rr, next.d, back.s);
				}
				case push_m: {
					// push [r0]  =>  mov r1, [r0]
					// pop r1

					REDECLARE_REF(back, back.push_m);
					remove_last_instruction(builder);
					switch (compiler->register_size) {
						case 4: return II(mov4_rm, next.d, back.s);
						case 8: return II(mov8_rm, next.d, back.s);
					}
				}
				case add_mc: {
					REDECLARE_REF(back, back.add_mc);
					if (back.d.is(rs)) {
						auto preback = body_builder.end()[-2];
						if (preback.kind == push_r) {
							// push r0         => lea r1, [r0 + 1234]
							// add [rs], 1234
							// pop r1

							REDECLARE_REF(preback, preback.push_r);
							remove_last_instruction(builder);
							remove_last_instruction(builder);
							return II(lea, next.d, preback.s + back.s);
						}
					}
					break;
				}
			}
#endif
			break;
		}
		case pop_f: {
			stack_state.pop();
			break;
		}
		case pop_m: {
			stack_state.pop();
			break;
		}
		case add_rc: {
			REDECLARE_REF(next, next.add_rc);

#if OPTIMIZE_BYTECODE
			if (next.s == 0)
				return 0;
#endif

			if (next.d == rs) {
				assert((next.s % compiler->stack_word_size) == 0);
				stack_state.offset(next.s);
			}

#if OPTIMIZE_BYTECODE
			auto &back = body_builder.back();
			switch (back.kind) {
				case add_rc: {
					REDECLARE_REF(back, back.add_rc);
					if (next.d == back.d) {
						// add r0, 16  =>  add r0, 48
						// add r0, 32

						back.s += next.s;
						return (Instruction *)&back;
					}
					break;
				}
				case sub_rc: {
					REDECLARE_REF(back, back.sub_rc);
					if (next.d == back.d) {
						// sub r0, 16  =>  sub r0, -16
						// add r0, 32

						back.s = next.s - back.s;
						return (Instruction *)&back;
					}
					break;
				}
			}
#endif
			break;
		}
		case sub_rc: {
			REDECLARE_REF(next, next.sub_rc);

#if OPTIMIZE_BYTECODE
			if (next.s == 0)
				return 0;
#endif

			if (next.d == rs) {
				assert((next.s % compiler->stack_word_size) == 0);
				stack_state.offset(-next.s);
			}

#if OPTIMIZE_BYTECODE
			auto &back = body_builder.back();
			switch (back.kind) {
				case add_rc: {
					REDECLARE_REF(back, back.add_rc);
					if (next.d == back.d) {
						// add r0, 16  =>  add r0, -16
						// sub r0, 32

						back.s -= next.s;
						return (Instruction *)&back;
					}
					break;
				}
				case sub_rc: {
					REDECLARE_REF(back, back.sub_rc);
					if (next.d == back.d) {
						// sub r0, 16  =>  sub r0, 48
						// sub r0, 32

						back.s += next.s;
						return (Instruction *)&back;
					}
					break;
				}
			}
#endif
			break;
		}
		case mov_rc: {
			REDECLARE_REF(next, next.mov_rc);

			register_state[next.d] = known_constant(next.s);
			break;
		}
		case mov_rr: {
			REDECLARE_REF(next, next.mov_rr);

			register_state[next.d] = register_state[next.s];

			break;
		}
		case mov1_rm: {
			REDECLARE_REF(next, next.mov1_rm);

			register_state[next.d].reset();
			break;
		}
		case mov2_rm: {
			REDECLARE_REF(next, next.mov2_rm);

			register_state[next.d].reset();
			break;
		}
		case mov4_rm: {
			REDECLARE_REF(next, next.mov4_rm);

			register_state[next.d].reset();
			break;
		}
		case mov8_rm: {
			REDECLARE_REF(next, next.mov8_rm);

#if OPTIMIZE_BYTECODE
			auto value = stack_state.get_value_at(next.s);
			if (value) {
				REDECLARE_REF(value, value.value_unchecked());
				switch (value.kind) {
					case KnownValueKind::constant:
						return II(mov_rc, next.d, value.constant);
					case KnownValueKind::stack_offset:
						return II(lea, next.d, rb+value.stack_offset);
				}
			}
			auto &back = body_builder.back();
			if (back.kind == lea) {
				REDECLARE_REF(back, back.lea);

				if (next.s.is(back.d)) {
					// This would be fine if we knew that r1 isn't used later...
					// It will not be used if we assing it to itself
					if (next.s.is(next.d)) {
						// lea r1, [rs+16]  =>  mov r1, [rs+16]
						// mov r1, [r1]

						remove_last_instruction(builder);
						return II(mov8_rm, next.d, back.s);
					}
				}
			}
#endif

			register_state[next.d].reset();
			break;
		}
		case mov8_mc: {
			REDECLARE_REF(next, next.mov8_mc);
			auto stack_address = stack_state.get_value_address_at(next.d);
			if (stack_address) {
				*stack_address = known_constant(next.s);
			}
			break;
		}
		case mov1_mr: {
			REDECLARE_REF(next, next.mov1_mr);

			auto stack_address = stack_state.get_value_address_at(next.d);
			if (stack_address) {
				*stack_address = null_opt;
			}

			break;
		}
		case mov2_mr: {
			REDECLARE_REF(next, next.mov2_mr);

			auto stack_address = stack_state.get_value_address_at(next.d);
			if (stack_address) {
				*stack_address = null_opt;
			}

			break;
		}
		case mov4_mr: {
			REDECLARE_REF(next, next.mov4_mr);

			auto stack_address = stack_state.get_value_address_at(next.d);
			if (stack_address) {
				*stack_address = null_opt;
			}

			break;
		}
		case mov8_mr: {
			REDECLARE_REF(next, next.mov8_mr);

#if OPTIMIZE_BYTECODE
			auto value = register_state[next.s];
			if (value) {
				REDECLARE_REF(value, value.value_unchecked());
				switch (value.kind) {
					case KnownValueKind::constant:
						return II(mov8_mc, next.d, value.constant);
				}
			}
#endif

			auto stack_address = stack_state.get_value_address_at(next.d);
			if (stack_address) {
				*stack_address = null_opt;
			}

			break;
		}
		case lea: {
			REDECLARE_REF(next, next.lea);
			if (next.s.base == rb) {
				if (next.s.r1_scale_index == 0) {
					if (next.s.r2_scale == 0) {
						// lea r0, [rb+x]
						register_state[next.d] = known_stack_offset(next.s.c);
					}
				}
			}
			break;
		}
	}
#endif
	// this does not use compiler->stack_word_size !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if 0
	using enum InstructionKind;
	switch (next.kind) {
		case push_c: {
			ls->stack_state.push(next.push_c.s);
			break;
		}
		case push_r: {
			stack_state.push(register_state[next.push_r.s]);
			break;
		}
		case push_m:
		case push_a:
		case push_d:
		case push_u:
		case push_t:
		case push_e: {
			stack_state.push_unknown();
			break;
		}
		case pop_r: {
			register_state[next.pop_r.d] = stack_state.pop();

			if (next.pop_r.d == rs) {
				stack_state.make_unknown();
			}

			auto back = body_builder->back();
			switch (back.kind) {
				case push_c:
					body_builder->pop_back();
					return I(mov_rc, next.pop_r.d, back.push_c.s);
				case push_r:
					body_builder->pop_back();
					return I(mov_rr, next.pop_r.d, back.push_r.s);
				case push_m:
					body_builder->pop_back();
					return I(mov8_rm, next.pop_r.d, back.push_m.s);
				case add_mc:
					if (back.add_mc.d.is(rs)) {
						auto preback = body_builder->end()[-2];
						if (preback.kind == push_r) {
							body_builder->pop_back();
							body_builder->pop_back();
							return I(lea, next.pop_r.d, preback.push_r.s + back.add_mc.s);
						}
					}
					break;
			}
			break;
		}
		case pop_m: {
			stack_state.pop();
		}
		case add_rc: {
			if (next.add_rc.s == 0)
				return 0;

			if (next.add_rc.d == rs) {
				assert((next.add_rc.s % 8) == 0);
				stack_state.offset(next.add_rc.s);
			}

			auto back = body_builder->back();
			if (back.kind == add_rc) {
				if (next.add_rc.d == back.add_rc.d) {
					body_builder->pop_back();
					return I(add_rc, next.add_rc.d, next.add_rc.s + back.add_rc.s);
				}
			}
			// else if (back.kind == push_c) {
			// 	if (next.add_rc.d == rs) {
			// 		body_builder->pop_back();
			// 		return I(mov8_mc, rs+-8, back.push_c.s); // TODO:size/speed: in x86-64 this instruction will take more space
			// 	}
			// }
			break;
		}
		case add_mc: {
			if (next.add_mc.s == 0)
				return 0;
			break;
		}
		case sub_rc: {
			if (next.sub_rc.s == 0)
				return 0;

			if (next.sub_rc.d == rs) {
				assert((next.sub_rc.s % 8) == 0);
				stack_state.offset(-next.sub_rc.s);
			}

			auto back = body_builder->back();
			switch (back.kind) {
				case sub_rc: {
					if (next.sub_rc.d == back.sub_rc.d) {
						body_builder->pop_back();
						return I(sub_rc, next.sub_rc.d, next.sub_rc.s + back.sub_rc.s);
					}
					break;
				}
			}
			break;
		}
		case sub_mc: {
			if (next.sub_mc.s == 0)
				return 0;
			break;
		}
		case mul_rc: {
			if (next.mul_rc.s == 0) {
				return I(xor_rr, next.mul_rc.d, next.mul_rc.d);
			} else if (next.mul_rc.s == 1) {
				return 0;
			} else if (next.mul_rc.s == -1) {
				return I(neg_r, next.mul_rc.d);
			} else {
				if (is_power_of_2(next.mul_rc.s)) {
					return I(shl_rc, next.mul_rc.d, log2(next.mul_rc.s));
				}
			}
			break;
		}
		case mul_mc: {
			if (next.mul_mc.s == 0) {
				return I(mov8_mc, next.mul_mc.d, 0);
			} else {
				if (is_power_of_2(next.mul_mc.s)) {
					return I(shl_mc, next.mul_mc.d, log2(next.mul_mc.s));
				}
			}
			break;
		}
		case mov_rc: {
			register_state[next.mov_rc.d] = next.mov_rc.s;
			break;
		}
		case mov_rr: {
			register_state[next.mov_rr.d] = register_state[next.mov_rr.s];
			break;
		}
		case mov1_rm: {
			register_state[next.mov1_rm.d].reset();
			break;
		}
		case mov2_rm: {
			register_state[next.mov2_rm.d].reset();
			break;
		}
		case mov4_rm: {
			register_state[next.mov4_rm.d].reset();
			break;
		}
		case mov8_rm: {
			if (next.mod_rm.s.r1_scale) {
				auto r = register_state[next.mod_rm.s.r1];
				if (r.has_value()) {
					next.mod_rm.s.c += r.value_unchecked() * next.mod_rm.s.r1_scale;
					next.mod_rm.s.r1_scale = 0;
				}
			}

			if (next.mov8_rm.s.is(rs)) {
				auto top = stack_state.top();
				if (top) {
					return I(mov_rc, next.mov8_rm.d, top.value_unchecked());
				}
			} else {
				register_state[next.mov8_rm.d].reset();
			}

			auto back = body_builder->back();
			if (back.kind == lea) {
				if (next.mov8_rm.s.is(back.lea.d)) { // address is exactly this register
					body_builder->pop_back();
					next.mov8_rm.s = back.lea.s;
					//return I(mov8_rm, next.mov8_rm.d, back.lea.s);
				} else if (next.mov8_rm.s.base == back.lea.d && !next.mov8_rm.s.r1_scale && !next.mov8_rm.s.r2_scale) { // constant offset may be here
					body_builder->pop_back();
					back.lea.s.c += next.mov8_rm.s.c;
					next.mov8_rm.s = back.lea.s;
					//return I(mov8_rm, next.mov8_rm.d, back.lea.s);
				}
			}
			// else if (back.kind == push_c) {
			// 	if (next.mov8_rm.s.is(rs)) {
			// 		return I(mov_rc, next.mov8_rm.d, back.push_c.s); // TODO:size/speed: in x86-64 this instruction will take more space
			// 	}
			// }
			break;
		}
	}

#endif

	return &instructions.add(next);
}

Address get_known_address_of(AstDefinition *definition) {
	if (definition->offset == -1) {
		invalid_code_path(definition->location, "INTERNAL ERROR: definition->offset is -1 in get_known_address_of");
	}

	auto offset = definition->offset;

	switch (get_definition_origin(definition))
	{
		case DefinitionOrigin::constants:
			if (definition->expression && is_lambda(definition->expression))
				return Register::instructions + offset;
			return Register::constants + offset;

		case DefinitionOrigin::rwdata:           return Register::rwdata + offset;
		case DefinitionOrigin::zeros:            return Register::zeros + offset;
		case DefinitionOrigin::return_parameter: return Address(Register::return_parameters);
		case DefinitionOrigin::parameter:        return Register::parameters + offset;
		case DefinitionOrigin::local:            return Register::locals + offset;
		default: invalid_code_path(definition->location);
	}
}

void FrameBuilder::load_address_of(AstDefinition *definition, RegisterOrAddress destination) {
	push_comment(format("load address of {} (definition_origin={})"str, definition->name, get_definition_origin(definition)));

	assert(definition->offset != -1);

	auto addr = get_known_address_of(definition);

	switch (get_definition_origin(definition)) {
		case DefinitionOrigin::parameter:
		case DefinitionOrigin::return_parameter: {
			if (get_size(definition->type) > compiler->stack_word_size) {
				// One layer of indirection
				if (destination.is_in_register) {
					I(lea, destination.reg, addr);
					I(mov8_rm, destination.reg, Address(destination.reg));
				} else {
					tmpreg(tmp);
					I(mov8_rm, tmp, addr);
					mov_mr(destination.address, tmp, compiler->stack_word_size);
				}
				return;
			}
			break;
		}
	}

	// Simple load
	if (destination.is_in_register) {
		I(lea, destination.reg, addr);
	} else {
		tmpreg(tmp);
		I(lea, tmp, addr);
		mov_mr(destination.address, tmp, compiler->stack_word_size);
	}
}
void FrameBuilder::load_address_of(AstExpression *expression, RegisterOrAddress destination) {
	push_comment(format(u8"load_address_of {}", expression->location));

	switch (expression->kind) {
		case Ast_Lambda: {
		load_address_of_lambda:

			auto lambda = (AstLambda *)expression;

			if (lambda->body) {
				Instruction *instr = 0;
				if (destination.is_in_register) {
					instr = II(lea, destination.reg, Address(Register::instructions));
				} else {
					tmpreg(tmp);
					instr = II(lea, tmp, Address(Register::instructions));
					mov_mr(destination.address, tmp, compiler->stack_word_size);
				}

				instructions_that_reference_lambdas.add({.instruction=instr, .lambda=lambda});
			} else {
				assert((s64)(s32)count_of(lambda->definition->name) == (s64)count_of(lambda->definition->name));
				if (destination.is_in_register) {
					I(mov_re, destination.reg, (String)lambda->definition->name);
				} else {
					tmpreg(tmp);
					I(mov_re, tmp, (String)lambda->definition->name);
					mov_mr(destination.address, tmp, compiler->stack_word_size);
				}
			}
			break;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition();

			if (definition->expression && definition->expression->kind == Ast_Lambda) {
				expression = definition->expression;
				goto load_address_of_lambda;

			} else {
				load_address_of(definition, destination);
			}
			break;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			assert(binop->operation == dot);

			assert(binop->right->kind == Ast_Identifier);
			auto right = (AstIdentifier *)binop->right;

			auto definition = right->definition();
			assert(definition);
			assert(definition->container_node);
			assert(definition->container_node->kind == Ast_Struct);

			auto offset = definition->offset;
			assert(offset != INVALID_MEMBER_OFFSET);

			if (is_pointer(binop->left->type))
				append(binop->left, destination);
			else
				load_address_of(binop->left, destination);

			if (offset) {
				if (destination.is_in_register) {
					I(add_rc, destination.reg, offset);
				} else {
					add_mc(destination.address, offset, compiler->stack_word_size);
				}
			}

			break;
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;

			// TODO: Clean up this copypasta.

			// :span hack:
			if (auto subtype = get_span_subtype(subscript->expression->type)) {
				load_address_of(subscript->expression, destination);
				if (destination.is_in_register) {
					mov_rm(destination.reg, Address(destination.reg), compiler->stack_word_size);
				} else {
					tmpreg(tmp);
					mov_rm(tmp, destination.address, compiler->stack_word_size);
					mov_rm(tmp, Address(tmp), compiler->stack_word_size);
					mov_mr(destination.address, tmp, compiler->stack_word_size);
				}

				APPEND_INTO_REGISTER(index, subscript->index_expression);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register) {
					I(add_rr, destination.reg, index);
				} else {
					add_mr(destination.address, index, compiler->stack_word_size);
				}
			} else if (auto pointer_type = as_pointer(subscript->expression->type)) {
				append(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register) {
					I(add_rr, destination.reg, index);
				} else {
					add_mr(destination.address, index, compiler->stack_word_size);
				}
			} else if (types_match(subscript->expression->type, compiler->builtin_string)) {
				assert(get_size(subscript->index_expression->type) <= compiler->stack_word_size);

				APPEND2(
					string, load_address_of, subscript->expression,
					index, append, subscript->index_expression
				);

				tmpreg(count);

				mov_rm(count, string + compiler->stack_word_size, compiler->stack_word_size);
				mov_rm(string, Address(string), compiler->stack_word_size);

				switch (compiler->stack_word_size) {
					case 4: I(cmpflag4, index, count); break;
					case 8: I(cmpflag8, index, count); break;
					default: invalid_code_path(subscript->location);
				}
				I(jlf_c, 2);
				push_comment(format(u8"bounds check failed for {}"s, subscript->location));
				I(debug_break);
				I(jmp_label);

				I(mul_rc, index, get_size(subscript->type));
				I(add_rr, index, string);

				if (destination.is_in_register)
					I(mov_rr, destination.reg, index);
				else
					mov_mr(destination.address, index, compiler->stack_word_size);
			} else {
				assert(direct_as<AstArray>(subscript->expression->type));
				load_address_of(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register) {
					I(add_rr, destination.reg, index);
				} else {
					add_mr(destination.address, index, compiler->stack_word_size);
				}
			}

			break;
			not_implemented();
#if 0
			auto subscript = (AstSubscript *)expression;
			auto element_size = get_size(subscript->type);

			if (is_pointer(subscript->expression->type)) {
				append_to_stack(subscript->expression);
				append_to_stack(subscript->index_expression);
				I(pop_r, r0);
				I(mul_rc, r0, element_size);
				I(add_mr, rs, r0);
				return {};
			} else {
				// TODO: this will not work with complex expression indexing
				auto destination = load_address_of(subscript->expression);
				assert(element_size);

				constexpr auto temp_r = r0;

				append_to_stack(subscript->index_expression);
				I(pop_r, temp_r);
				if (destination) {
					assert(destination.value_unchecked() != temp_r);

					auto is_valid_lea_register_scale = [&](s64 x) {
						static constexpr s64 max_lea_register_scale = 8;
						return is_power_of_2(x) && x <= max_lea_register_scale;
					};

					if (is_valid_lea_register_scale(element_size)) {
						Address a = {};
						a.base = destination.value_unchecked();
						a.r1 = temp_r;
						switch (element_size) {
							case 1: a.r1_scale_index = 1; break;
							case 2: a.r1_scale_index = 2; break;
							case 4: a.r1_scale_index = 3; break;
							case 8: a.r1_scale_index = 4; break;
							default: invalid_code_path();
						}
						I(lea, destination.value_unchecked(), a);
					} else {
						I(mul_rc, temp_r, element_size);
						I(add_rr, destination.value_unchecked(), temp_r);
					}
				} else {
					I(mul_rc, temp_r, element_size);
					I(add_mr, rs, temp_r);
				}
				return destination;
			}
#endif
			break;
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			assert(unop->operation == UnaryOperation::dereference);
			append(unop->expression, destination);
			break;
		}
		default:
			invalid_code_path(expression->location, "attempt to load address of unknown kind of expression");
	}
}

DefinitionAddress FrameBuilder::get_address_of(AstDefinition *definition) {

	auto addr = get_known_address_of(definition);

	switch (get_definition_origin(definition)) {
		case DefinitionOrigin::parameter:
		case DefinitionOrigin::return_parameter: {
			if (get_size(definition->type) > compiler->stack_word_size) {
				auto destination = allocate_register_or_temporary(compiler->stack_word_size);
				if (destination.is_in_register) {
					I(lea, destination.reg, addr);
					mov_rm(destination.reg, Address(destination.reg), compiler->stack_word_size);
				} else {
					tmpreg(tmp);
					mov_rm(tmp, addr, compiler->stack_word_size);
					mov_mr(destination.address, tmp, compiler->stack_word_size);
				}
				return {
					.is_known = false,
					.computed_address = destination,
				};
			}
			break;
		}
	}

	return {
		.is_known = true,
		.known_address = addr,
	};
}

void FrameBuilder::append_memory_set(Address d, s64 s, s64 size) {

	s &= 0xff;
	s |= s << 32;
	s |= s << 16;
	s |= s << 8;

	switch (size) {
		case 1: instructions.add(MI(mov1_mc, d, s)); break;
		case 2: instructions.add(MI(mov2_mc, d, s)); break;
		case 4: instructions.add(MI(mov4_mc, d, s)); break;
		case 8:
			if (compiler->stack_word_size == 8) {
				instructions.add(MI(mov8_mc, d, s));
				break;
			}
			// fallthrough
		default:
			instructions.add(MI(set_mcc, d, (s8)s, (s32)size));
			break;
	}
}

void FrameBuilder::append_struct_initializer(AstStruct *Struct, SmallList<AstExpression *> values, RegisterOrAddress destination) {
	Address tmp;
	if (destination.is_in_register) {
		debug_break();
		tmp = allocate_temporary_space(Struct->size);
	}

	{
		scoped_replace_if(destination, tmp, destination.is_in_register);
		auto struct_size = ceil(get_size(Struct), 8ll);

		push_comment(format("struct initializer {}"str, Struct->definition ? Struct->definition->name : where(Struct->location.data)));

		for (umm i = 0; i < values.count; ++i) {
			auto arg = values[i];
			auto member = Struct->data_members[i];
			auto arg_size = get_size(member->type);

			auto member_address = destination.address + member->offset;

			if (arg) {
				append(arg, member_address);
			} else {
				// zero initialize
				append_memory_set(member_address, 0, arg_size);
			}
		}
	}

	if (destination.is_in_register) {
		mov_rm(destination.reg, tmp, compiler->stack_word_size);
	}
}

void FrameBuilder::append(Scope *scope, Optional<RegisterOrAddress> destination) {
	scoped_replace(current_scope, scope);

	temporary_size = max(temporary_size, temporary_cursor);
	auto start_temporary_cursor = temporary_cursor;
	defer {
		temporary_size = max(temporary_size, temporary_cursor);
		temporary_cursor = start_temporary_cursor;
	};

	for (auto statement : scope->statement_list) {
		// if (statement->uid() == 1826)
		// 	debug_break();
		//if (statement->location == "glGenBuffers = @ wglGetProcAddress(\"glGenBuffers\\0\".data)")
		//	debug_break();

		push_comment((Span<utf8>)format("==== {}: {} ====", where(statement->location.data), statement->location));

		// temporary_cursor = 0;

		if (auto est = as<AstExpressionStatement>(statement);
			destination &&
			!(destination.value().is_in_register && destination.value().reg == Register::constants)
			&& statement == scope->statement_list.back()
			&& est
		) {
			append(est->expression, destination.value());
		} else {
			auto registers_before = scope->node->kind == Ast_Lambda ? initial_available_registers : available_registers;
			append(statement);
			assert(available_registers == registers_before);
		}
	}
	scope->defers_start_index = count_of(instructions);
	for (auto Defer : reverse_iterate(scope->bytecode_defers)) {
		append(Defer->scope);
	}
}

void FrameBuilder::with_definition_address_of(AstDefinition *definition, auto &&fn) {
	auto definition_address = get_address_of(definition);
	if (definition_address.is_known) {
		fn(definition_address.known_address);
	} else {
		auto computed_address = definition_address.computed_address;
		assert(computed_address.is_in_register);
		defer { free_register(computed_address.reg); };

		fn(Address(computed_address.reg));
	}
}

void FrameBuilder::append_return(AstLambda *lambda, AstExpression *expression) {
	if (expression) {
		with_definition_address_of(lambda->return_parameter, [&](Address address) {
			append(expression, address);
		});
	}

	Scope *scope = current_scope;
	while (scope) {
		for (auto Defer : reverse_iterate(scope->bytecode_defers)) {
			append(Defer->scope);
		}
		scope = scope->parent;
	}

	auto jump_index = (s64)count_of(instructions);
	auto return_jump = II(jmp, 0);

	lambda->return_jumps.add({return_jump, jump_index});
}

void FrameBuilder::append(AstStatement *statement) {
	scoped_replace(current_node, statement);
	switch (statement->kind) {
		case Ast_Definition:          return append((AstDefinition          *)statement);
		case Ast_Return:              return append((AstReturn              *)statement);
		case Ast_ExpressionStatement: return append((AstExpressionStatement *)statement);
		case Ast_While:               return append((AstWhile               *)statement);
		case Ast_LoopControl:         return append((AstLoopControl         *)statement);
		case Ast_Defer: {
			// defer is appended later, after appending a block.
			auto Defer = (AstDefer *)statement;
			Defer->scope->parent->bytecode_defers.add(Defer);
			return;
		}
		case Ast_Assert:
			return append((AstAssert *)statement);
		case Ast_Print:
		//case Ast_Import:
		case Ast_Parse:
		case Ast_Test:
		case Ast_Using:
		case Ast_OperatorDefinition:
			return;
		default: invalid_code_path(statement->location);
	}
}
void FrameBuilder::append(AstDefinition *definition) {
	assert(definition->type);

	switch (get_definition_origin(definition)) {
		case DefinitionOrigin::local: {
			assert(definition->offset != -1);
			assert((definition->offset & 0xf) == 0);

			auto addr = Register::locals + definition->offset;

			if (definition->expression) {
				append(definition->expression, addr);
			} else {
				if (auto Struct = direct_as<AstStruct>(definition->type); Struct && Struct->default_value) {
					copy(addr, Register::constants + Struct->default_value_offset, get_size(definition->type), false);
				} else {
					append_memory_set(addr, 0, get_size(definition->type));
				}
			}
			break;
		}
	}
}
void FrameBuilder::append(AstReturn *ret) {
	push_comment(u8"return"s);
	append_return(ret->lambda, ret->expression);
}
void FrameBuilder::append(AstWhile *While) {
	auto count_before_condition = count_of(instructions);
	I(jmp_label);

	decltype(Instruction::jz_cr) *jz;
	{
		APPEND_INTO_REGISTER(condition_register, While->condition);
		jz = I(jz_cr, 0, condition_register);
	}

	auto count_after_condition = count_of(instructions);

	loop_control_stack.add();

	append(While->scope);

	auto count_after_body = count_of(instructions);

	I(jmp, .offset=0)->offset = (s64)count_before_condition - (s64)count_after_body;

	I(jmp_label);

	jz->offset = (s64)count_after_body - (s64)count_after_condition + 2;

	for (auto &jmp : loop_control_stack.pop().value()) {
		switch (jmp.control) {
			case LoopControl::Break:
				jmp.jmp->jmp.offset = (s64)count_after_body - (s64)jmp.index + 2;
				break;
			case LoopControl::Continue:
				jmp.jmp->jmp.offset = (s64)count_before_condition - (s64)jmp.index;
				break;
			default:
				break;
		}
	}

}
void FrameBuilder::append(AstExpressionStatement *es) {
	// TODO: FIXME: this may allocate temporary space.
	// Figure a way to not produce the result.
	auto value = append(es->expression);
	if (value.is_in_register)
		free_register(value.reg);
}
void FrameBuilder::append(AstAssert *Assert) {
	if (Assert->is_constant)
		return;
	push_comment(format(u8"assert {}", Assert->location));

	APPEND_INTO_REGISTER(condition, Assert->condition);

	I(jnz_cr, 2, condition);
	push_comment(format(u8"Assertion failed: {}", Assert->condition->location));
	I(debug_error, format(u8"Assertion failed: {}\n{}", Assert->condition->location, where(Assert->condition->location.data)));
	I(jmp_label);
}
void FrameBuilder::append(AstLoopControl *LoopControl) {

	// TODO: FIXME: with that way of executing defers there may be A LOT of repeating instructions in
	// loops with a lot of breaks/continues an defers. Maybe there is a better way to do this?
	auto scope = LoopControl->parent_scope;
	while (1) {
		for (auto Defer : reverse_iterate(scope->bytecode_defers)) {
			append(Defer->scope);
		}
		if (scope->node->kind == Ast_While) {
			break;
		}
		scope = scope->parent;
	}
	auto instr = II(jmp);
	auto index = count_of(instructions);
	loop_control_stack.back().add({index, instr, LoopControl->control});
}

void FrameBuilder::append_cast(RegisterOrAddress src, AstExpression *src_type, RegisterOrAddress dst, AstExpression *dst_type) {
	auto get_internal_representation = [&](AstExpression *type) -> AstExpression * {
		if (is_pointer_internally(type))
			return compiler->builtin_u64.Struct;
		else if (auto Enum = direct_as<AstEnum>(type)) {
			assert(!Enum->underlying_type, "not implemented");
			return compiler->builtin_s64.Struct;
		} else
			return direct(type);
		return 0;
	};

	AstExpression *from = get_internal_representation(src_type);
	AstExpression *to   = get_internal_representation(dst_type);

	{
		auto array = as<AstArray>(from);
		auto subtype = get_span_subtype(to);
		if (array && subtype) {
			if (array->count == 0) {
				append_memory_set(dst.address, 0, compiler->stack_word_size*2);
			} else {
				if (src.is_in_register) {
					// Will this cause problems? Idk
					auto size = get_size(array);
					auto tmp = allocate_temporary_space(size);
					mov_rm(src.reg, tmp, size);

					src.is_in_register = false;
					src.address = tmp;
				}
				assert(!dst.is_in_register);

				tmpreg(tmp);

				I(lea, tmp, src.address);

				mov_mr(dst.address + 0,                         tmp,          compiler->stack_word_size);
				mov_mc(dst.address + compiler->stack_word_size, array->count, compiler->stack_word_size);
			}
			return;
		}
	}

	// it's C++ lol, why not?
#define BEGIN if (false) {}
#define END else { invalid_code_path(); }
#define FROM(x) else if (from == (compiler->builtin_##x.Struct))
#define TO(x) else if (to == (compiler->builtin_##x.Struct))


	// Integer to integer conversions:
	// If dst is bigger than source:
	//    extend the size depending on the signedness of source operand (sign extend for signed, zero extend for unsigned).
	// Otherwise this is a noop.

	if (::is_integer(from) && ::is_integer(to)) {
		bool extended = false;

#define C(z,d,s) \
	extended = true; \
	if (dst.is_in_register) { \
		if (src.is_in_register) I(mov##z##x##d##s##_rr, dst.reg, src.reg); \
		else                    I(mov##z##x##d##s##_rm, dst.reg, src.address); \
	} else { \
		tmpreg(r0); \
		if (src.is_in_register) I(mov##z##x##d##s##_rr, r0, src.reg); /* NOTE: movsx/movzx into a memory is not a thing! */ \
		else                    I(mov##z##x##d##s##_rm, r0, src.address); \
		I(mov##d##_mr, dst.address, r0); \
	}
		// NOTE: no END's there, because conversions that are not listed here are noops.
		BEGIN
		FROM(u8) {
			BEGIN
			TO(u16) { C(z,2,1); }
			TO(u32) { C(z,4,1); }
			TO(u64) { C(z,8,1); }
			TO(s16) { C(z,2,1); }
			TO(s32) { C(z,4,1); }
			TO(s64) { C(z,8,1); }
		}
		FROM(u16) {
			BEGIN
			TO(u32) { C(z,4,2); }
			TO(u64) { C(z,8,2); }
			TO(s32) { C(z,4,2); }
			TO(s64) { C(z,8,2); }
		}
		FROM(u32) {
			BEGIN
			TO(u64) { C(z,8,4); }
			TO(s64) { C(z,8,4); }
		}
		FROM(s8) {
			BEGIN
			TO(u16) { C(s,2,1); }
			TO(u32) { C(s,4,1); }
			TO(u64) { C(s,8,1); }
			TO(s16) { C(s,2,1); }
			TO(s32) { C(s,4,1); }
			TO(s64) { C(s,8,1); }
		}
		FROM(s16) {
			BEGIN
			TO(u32) { C(s,4,2); }
			TO(u64) { C(s,8,2); }
			TO(s32) { C(s,4,2); }
			TO(s64) { C(s,8,2); }
		}
		FROM(s32) {
			BEGIN
			TO(u64) { C(s,8,4); }
			TO(s64) { C(s,8,4); }
		}
#undef C
		if (!extended) {
			copy(dst, src, get_size(to), false);
		}
		return;
	}

	// Integer to float
	if (::is_integer(from) && ::is_float(to)) {
		tmpreg(x);

		copy(x, src, get_size(from), false);

		BEGIN
		FROM(s32) {
			BEGIN
			TO(f32) { I(cvt_s32_f32, x); }
			END
		}
		FROM(s64) {
			BEGIN
			TO(f64) { I(cvt_s64_f64, x); }
			END
		}
		END

		copy(dst, x, get_size(to), false);

		return;
	}

	// Float to integer
	if (::is_float(from) && ::is_integer(to)) {
		tmpreg(x);

		copy(x, src, get_size(from), false);

		BEGIN
		FROM(f32) {
			BEGIN
			TO(s32) { I(cvt_f32_s32, x); }
			END
		}
		FROM(f64) {
			BEGIN
			TO(s64) { I(cvt_f64_s64, x); }
			END
		}
		END

		copy(dst, x, get_size(to), false);

		return;
	}

	// Float to float
	if (::is_float(from) && ::is_float(to)) {
		tmpreg(x);

		copy(x, src, get_size(from), false);

		BEGIN
		FROM(f32) {
			BEGIN
			TO(f64) { I(cvt_f32_f64, x); }
			END
		}
		FROM(f64) {
			BEGIN
			TO(f32) { I(cvt_f64_f32, x); }
			END
		}
		END

		copy(dst, x, get_size(to), false);

		return;
	}

	// Integer to boolean
	if (::is_integer(from) && types_match(to, compiler->builtin_bool)) {
		tmpreg(x);
		copy(x, src, get_size(from), false);
		//switch (get_size(from)) {
		//	case 1: I(and_rc, x, 0xFF); break;
		//	case 2: I(and_rc, x, 0xFFFF); break;
		//	case 4: I(and_rc, x, 0xFFFFFFFF); break;
		//}
		I(tobool_r, x);
		copy(dst, x, 1, false);
		return;
	}

	// Boolean to integer
	if (types_match(from, compiler->builtin_bool) && ::is_integer(to)) {
		tmpreg(x);
		copy(x, src, 1, false);
		I(and_rc, x, 1);
		copy(dst, x, get_size(to), false);
		return;
	}

	if (auto option = as_option(from)) {
		if (to == compiler->builtin_bool.Struct) {

			// Option to boolean


			assert(!src.is_in_register, "not implemented");

			tmpreg(x);

			I(mov1_rm, x, src.address + get_size(option->expression));
			copy(dst, x, 1, false);

			return;
		}
	}

	if (auto option = as_option(to)) {

		// T to ?T

		assert(!dst.is_in_register, "not implemented");

		auto size = get_size(option->expression);

		copy(dst, src, size, false);
		I(mov1_mc, dst.address + size, 1);
		return;
	}

	if (auto src_array = as<AstArray>(from)) {
		if (auto dst_array = as<AstArray>(to)) {

			// [N]T to [N]U

			assert(!src.is_in_register);
			assert(!dst.is_in_register);

			auto src_elem_size = get_size(src_array->element_type);
			auto dst_elem_size = get_size(dst_array->element_type);

			for (u64 i = 0; i < src_array->count; ++i) {
				auto src_elem_addr = src.address + i * src_elem_size;
				auto dst_elem_addr = dst.address + i * src_elem_size;

				append_cast(src_elem_addr, src_array->element_type, dst_elem_addr, dst_array->element_type);
			}
			return;
		}
	}

	invalid_code_path("Can not generate bytecode for cast from {} to {}", type_to_string(src_type), type_to_string(dst_type));


#undef BEGIN
#undef END
#undef FROM
#undef TO
}
void FrameBuilder::append_cast(AstExpression *src, RegisterOrAddress dst, AstExpression *dst_type) {
	assert(!types_match(src->type, dst_type), "Don't generate redundant casts!");

	if (is_addressable(src)) {
		LOAD_ADDRESS_INTO_REGISTER(addr, src);

		append_cast(Address(addr), src->type, dst, dst_type);
	} else {

		auto val = append(src);
		defer {
			if (val.is_in_register)
				free_register(val.reg);
		};

		append_cast(val, src->type, dst, dst_type);
	}
}

void FrameBuilder::check_null(Register pointer_reg, String location) {
	tmpreg(cond);
	tmpreg(lowerbound);

	I(mov_rc, lowerbound, 4096);
	I(cmpu8, cond, pointer_reg, lowerbound, Comparison::ge);
	I(jnz_cr, 2, cond);
	I(debug_error, format(u8"{}:\n{} was null"s, where(location.data), location));
	I(jmp_label);
}

void FrameBuilder::append(AstExpression *expression, RegisterOrAddress destination) {
	scoped_replace(current_node, expression);
	switch (expression->kind) {
		case Ast_Identifier:       return append((AstIdentifier       *)expression, destination);
		case Ast_Literal:          return append((AstLiteral          *)expression, destination);
		case Ast_Call:             return append((AstCall             *)expression, destination);
		case Ast_BinaryOperator:   return append((AstBinaryOperator   *)expression, destination);
		case Ast_UnaryOperator:    return append((AstUnaryOperator    *)expression, destination);
		case Ast_Subscript:        return append((AstSubscript        *)expression, destination);
		case Ast_Lambda:           return append((AstLambda           *)expression, destination);
		case Ast_If:               return append((AstIf               *)expression, destination);
		case Ast_ArrayInitializer: return append((AstArrayInitializer *)expression, destination);
		case Ast_Block:            return append((AstBlock            *)expression, destination);
		case Ast_Match:            return append((AstMatch            *)expression, destination);
		default: invalid_code_path(expression->location);
	}
}
void FrameBuilder::append(AstBlock *block, RegisterOrAddress destination) {
	append(block->scope, destination);
}
void FrameBuilder::append(AstIf *If, RegisterOrAddress destination) {
#if 0
	if (If->is_constant) {
		// constant if's statements were brought outside already by the typechecker. No need to append it.
		return;
	}
#else
	if (If->is_constant) {
		// NOTE: constant if's scope is not merged into it's parent.
		auto scope = If->true_branch_was_taken ? If->true_block->scope : If->false_block->scope;
		append(scope);
		//if (ls) {
		//	// if we are in a lambda, append statements with all checks and defers etc.
		//	append(scope);
		//} else {
		//	for (auto statement : scope->statements) {
		//		append(statement);
		//	}
		//}
		return;
	}
#endif

	decltype(Instruction::jz_cr) *jz;
	{
		APPEND_INTO_REGISTER(condition_register, If->condition);
		jz = I(jz_cr, 0, condition_register);
	}

	auto true_branch_first_instruction_index = count_of(instructions);
	append(raw(If->true_block), destination);

	auto jmp = I(jmp, .offset=0);
	I(jmp_label);

	auto false_branch_first_instruction_index = count_of(instructions);
	append(raw(If->false_block), destination);

	auto false_end = count_of(instructions);

	I(jmp_label);

	jz->offset = false_branch_first_instruction_index - true_branch_first_instruction_index;
	jmp->offset = false_end - false_branch_first_instruction_index + 2;
}
void FrameBuilder::append(AstMatch *Match, RegisterOrAddress destination) {
	auto matchable = allocate_temporary_space(get_size(Match->expression->type));
	append(Match->expression, matchable);

	struct Jump {
		Instruction *instruction;
		umm index;
	};

	List<Jump> jumps_to_cases;
	List<Jump> jumps_out;
	Jump jump_to_default = {};

	bool has_default_case = false;

	{
		tmpreg(matchable_reg);
		mov_rm(matchable_reg, matchable, compiler->stack_word_size);
		for (auto &Case : Match->cases) {
			if (Case.expression) {
				tmpreg(case_reg);
				tmpreg(cmpresult_reg);

				I(mov_rc, case_reg, (s64)Case.value);

				switch (compiler->stack_word_size) {
					case 4: I(cmpu4, cmpresult_reg, matchable_reg, case_reg, Comparison::e); break;
					case 8: I(cmpu8, cmpresult_reg, matchable_reg, case_reg, Comparison::e); break;
					default: invalid_code_path(Match->location);
				}
				jumps_to_cases.add({II(jnz_cr, 0, cmpresult_reg), instructions.count});
			} else {
				has_default_case = true;
			}
		}
	}

	if (has_default_case) {
		jump_to_default = {II(jmp), instructions.count};
	} else {
		jumps_out.add({II(jmp), instructions.count});
	}

	umm case_index = 0;
	for (auto &Case : Match->cases) {
		I(jmp_label);
		if (Case.expression) {
			jumps_to_cases[case_index].instruction->jnz_cr.offset = instructions.count - jumps_to_cases[case_index].index;
		} else {
			jump_to_default.instruction->jmp.offset = instructions.count - jump_to_default.index;
		}

		append(raw(Case.block), destination);
		jumps_out.add({II(jmp), instructions.count});
		case_index++;
	}
	I(jmp_label);

	for (auto &jump_out : jumps_out) {
		jump_out.instruction->jmp.offset = instructions.count - jump_out.index;
	}
}
void FrameBuilder::append(AstBinaryOperator *bin, RegisterOrAddress destination) {
	push_comment(format(u8"binary {}"s, bin->location));

	auto left = bin->left;
	auto right = bin->right;

	switch (bin->operation) {
		using enum BinaryOperation;
		case dot: {
			switch (right->kind) {
				case Ast_Identifier: {
					AstStruct *Struct = 0;
					bool is_pointer = false;
					if (auto pointer = as_pointer(left->type)) {
						Struct = direct_as<AstStruct>(pointer->expression);
						is_pointer = true;
					} else {
						Struct = direct_as<AstStruct>(left->type);
					}

					if (Struct) {
						auto struct_size = get_size(left->type);


						assert(right->kind == Ast_Identifier);
						auto ident = (AstIdentifier *)right;
						auto member = ident->definition();
						assert(member);
						auto member_size = get_size(member->type);

						assert(find(Struct->data_members, member));

						Register member_address;
						bool should_free_member_address = false;
						if (is_pointer) {
							APPEND_INTO_REGISTER(_struct_address, bin->left);
							member_address = _struct_address;

							check_null(member_address, left->location);
						} else {
							if (is_addressable(bin->left)) {
								LOAD_ADDRESS_INTO_REGISTER(_struct_address, bin->left);
								member_address = _struct_address;
							} else {
								auto tmp = allocate_temporary_space(struct_size);
								append(bin->left, tmp);

								member_address = allocate_temporary_register();
								should_free_member_address = true;

								I(lea, member_address, tmp);
							}
						}
						I(add_rc, member_address, member->offset);

						copy(destination, Address(member_address), member_size, false);

						if (should_free_member_address)
							free_temporary_register(member_address);
					} else {
						not_implemented();
						//assert(is_sized_array(left->type));
					}

					break;
				}
				default: {
					invalid_code_path(bin->location);
					break;
				}
			}
			break;
		}
		case add:
		case sub:
		case mul:
		case div:
		case mod:
		case bor:
		case band:
		case bxor:
		case bsr:
		case bsl: {
			APPEND2(l, append, left, r, append, right);

			auto lt = direct(bin->left->type);

			if (lt == compiler->builtin_f32.Struct) {
				switch (bin->operation) {
					case add:  I(add4_ff, l, r); break;
					case sub:  I(sub4_ff, l, r); break;
					case mul:  I(mul4_ff, l, r); break;
					case div:  I(div4_ff, l, r); break;
					default: invalid_code_path(bin->location);
				}
			} else if (lt == compiler->builtin_f64.Struct) {
				switch (bin->operation) {
					case add:  I(add8_ff, l, r); break;
					case sub:  I(sub8_ff, l, r); break;
					case mul:  I(mul8_ff, l, r); break;
					case div:  I(div8_ff, l, r); break;
					default: invalid_code_path(bin->location);
				}
			} else if (::is_integer_or_pointer(lt)) {
				if (auto pointer = as_pointer(lt)) {
					I(mul_rc, r, get_size(pointer->expression));
				}
				switch (bin->operation) {
					case add:  I(add_rr, l, r); break;
					case sub:  I(sub_rr, l, r); break;
					case mul:  I(mul_rr, l, r); break;
					case div:  if (::is_signed(lt)) I(divs_rr, l, r); else I(divu_rr, l, r); break;
					case mod:  if (::is_signed(lt)) I(mods_rr, l, r); else I(modu_rr, l, r); break;
					case bor:  I( or_rr, l, r); break;
					case band: I(and_rr, l, r); break;
					case bxor: I(xor_rr, l, r); break;
					case bsr:  if (::is_signed(lt)) I(sar_rr, l, r); else I(slr_rr, l, r); break;
					case bsl:  I(shl_rr, l, r); break;
					default: invalid_code_path(bin->location);
				}
			} else {
				invalid_code_path(bin->location);
			}

			if (destination.is_in_register) {
				I(mov_rr, destination.reg, l);
			} else {
				mov_mr(destination.address, l, get_size(lt));
			}

			break;
		}
		case lt:
		case gt:
		case le:
		case ge:
		case eq:
		case ne: {
			if (types_match(left->type, compiler->builtin_string)) {
				auto la = append(left).ensure_address();
				auto ra = append(right).ensure_address();

				tmpreg(r0);
				tmpreg(r1);
				tmpreg(r2);

				// load counts
				mov_rm(r1, la + compiler->stack_word_size, compiler->stack_word_size);
				mov_rm(r2, ra + compiler->stack_word_size, compiler->stack_word_size);
				switch (compiler->stack_word_size) {
					case 4: I(cmpu4, r0, r1, r2, Comparison::e); break;
					case 8: I(cmpu8, r0, r1, r2, Comparison::e); break;
					default: invalid_code_path(bin->location);
				}
				I(jnz_cr, 3, r0);
				if (destination.is_in_register) {
					I(mov_rc, destination.reg, 0);
				} else {
					I(mov1_mc, destination.address, 0);
				}
				I(jmp, 6);

				I(jmp_label);
				// load pointers
				mov_rm(r0, la, compiler->stack_word_size);
				mov_rm(r1, ra, compiler->stack_word_size);
				I(cmpstr, r2, Address(r0), Address(r1));
				if (destination.is_in_register) {
					I(mov_rr, destination.reg, r2);
				} else {
					I(mov1_mr, destination.address, r2);
				}
				I(jmp_label);
				switch (bin->operation) {
					case eq: {
						break;
					}
					case ne: {
						if (destination.is_in_register) {
							I(toboolnot_r, destination.reg);
						} else {
							I(mov1_rm, r0, destination.address);
							I(toboolnot_r, r0);
							I(mov1_mr, destination.address, r0);
						}
						break;
					}
				}
				return;
			}

			APPEND2(
				rl, append, left,
				rr, append, right
			);

			auto comparison = comparison_from_binary_operation(bin->operation);

			// FIXME: deduplicate following branches
			if (destination.is_in_register) {
				if (::is_integer_internally(left->type)) {
					if (::is_signed(left->type)) {
						switch (get_size(left->type)) {
							case 1: I(cmps1, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmps2, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmps4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmps8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path(bin->location);
						}
					} else {
						switch (get_size(left->type)) {
							case 1: I(cmpu1, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmpu2, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmpu4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmpu8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path(bin->location);
						}
					}
				} else if (::is_float(left->type)) {
					switch (get_size(left->type)) {
						case 4: I(cmpf4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
						case 8: I(cmpf8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
						default: invalid_code_path(bin->location);
					}
				} else if (::types_match(left->type, compiler->builtin_bool)) {
					switch (comparison) {
						case Comparison::e:
							I(xor_rr, rl, rr);
							I(xor_rc, rl, 1);
							I(mov_rr, destination.reg, rl);
							break;
						case Comparison::ne:
							I(xor_rr, rl, rr);
							I(mov_rr, destination.reg, rl);
							break;
						default:
							not_implemented();
					}
				} else {
					not_implemented();
				}
			} else {
				tmpreg(r2);
				if (::is_integer_internally(left->type)) {
					if (::is_signed(left->type)) {
						switch (get_size(left->type)) {
							case 1: I(cmps1, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmps2, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmps4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmps8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path(bin->location);
						}
					} else {
						switch (get_size(left->type)) {
							case 1: I(cmpu1, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmpu2, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmpu4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmpu8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path(bin->location);
						}
					}
					I(mov1_mr, destination.address, r2);
				} else if (::is_float(left->type)) {
					switch (get_size(left->type)) {
						case 4: I(cmpf4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
						case 8: I(cmpf8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
						default: invalid_code_path(bin->location);
					}
					I(mov1_mr, destination.address, r2);
				} else if (::types_match(left->type, compiler->builtin_bool)) {
					switch (comparison) {
						case Comparison::e:
							I(xor_rr, rl, rr);
							I(xor_rc, rl, 1);
							I(mov1_mr, destination.address, rl);
							break;
						case Comparison::ne:
							I(xor_rr, rl, rr);
							I(mov1_mr, destination.address, rl);
							break;
						default:
							not_implemented();
					}
				} else {
					not_implemented();
				}
			}
			break;
		}
		case ass: {
#if 1
			auto dst_addr = load_address_of(left);
			defer { if (dst_addr.is_in_register) free_register(dst_addr.reg); };
			if (dst_addr.is_in_register) {

				//auto original = allocate_temporary_space(8);
				//I(mov8_mr, original, dst_addr.reg);

				append(right, Address(dst_addr.reg));

				//I(mov8_rm, r0, original);
				//I(cmpf8, r0, dst_addr.reg);
				//I(jef_c, 2);
				//push_comment(u8"DEBUG: address of binop->left was not preserved"s);
				//I(debug_break);
				//I(jmp_label);
			} else {
				auto src = append(right);
				defer {
					if (src.is_in_register) free_register(src.reg);
				};

				Register dst;
				defer { if (is_temporary_register(dst)) free_temporary_register(dst); };
				if (dst_addr.is_in_register) {
					dst = dst_addr.reg;
				} else {
					dst = allocate_temporary_register();
					mov_rm(dst, dst_addr.address, compiler->stack_word_size);
				}

				auto size = get_size(bin->left->type);

				copy(Address(dst), src, size, false);
			}
#else
			auto dst_rm = load_address_of(left);
			auto src = append(right);
			defer {
				if (dst_rm.is_in_register) free_register(dst_rm.reg);
				if (src.is_in_register) free_register(src.reg);
			};
			Register dst = r0;
			if (dst_rm.is_in_register) {
				dst = dst_rm.reg;
			} else {
				I(mov8_rm, dst, dst_rm.address);
			}

			auto size = get_size(bin->left->type);

			if (src.is_in_register) {
				mov_mr(size, dst, src.reg);
			} else {
				copy(dst, src.address, size, false, {}, {});
			}
#endif
			break;
		}
		case addass:
		case subass:
		case mulass:
		case divass:
		case modass:
		case borass:
		case bandass:
		case bxorass:
		case bslass:
		case bsrass: {
			assert(get_size(bin->right->type) <= compiler->stack_word_size);

			APPEND2(
				dst, load_address_of, bin->left,
				src, append, bin->right
			);

			if (::is_integer_or_pointer(bin->type)) {
				if (auto pointer = as_pointer(bin->left->type)) {
					I(mul_rc, src, get_size(pointer->expression));
				}

				auto size = get_size(bin->right->type);

				switch (bin->operation) {
					case addass:  add_mr(Address(dst), src, size); break;
					case subass:  sub_mr(Address(dst), src, size); break;
					case mulass:  mul_mr(Address(dst), src, size); break;
					case divass:  if (::is_signed(bin->left->type)) divs_mr(Address(dst), src, size); else divu_mr(Address(dst), src, size); break;
					case modass:  if (::is_signed(bin->left->type)) mods_mr(Address(dst), src, size); else modu_mr(Address(dst), src, size); break;
					case borass:   or_mr(Address(dst), src, size); break;
					case bandass: and_mr(Address(dst), src, size); break;
					case bxorass: xor_mr(Address(dst), src, size); break;
					case bslass:  shl_mr(Address(dst), src, size); break;
					case bsrass:  if (::is_signed(bin->left->type)) sar_mr(Address(dst), src, size); else slr_mr(Address(dst), src, size); break;
					default: invalid_code_path(bin->location);
				}
			} else if (::is_float(bin->type)) {
				tmpreg(r2);

				auto size = get_size(bin->type);
				mov_rm(r2, Address(dst), size);

				switch (size) {
					case 4:
						switch (bin->operation) {
							case addass:  I(add4_ff, r2, src); break;
							case subass:  I(sub4_ff, r2, src); break;
							case mulass:  I(mul4_ff, r2, src); break;
							case divass:  I(div4_ff, r2, src); break;
							default: invalid_code_path(bin->location);
						}
						break;
					case 8:
						switch (bin->operation) {
							case addass:  I(add8_ff, r2, src); break;
							case subass:  I(sub8_ff, r2, src); break;
							case mulass:  I(mul8_ff, r2, src); break;
							case divass:  I(div8_ff, r2, src); break;
							default: invalid_code_path(bin->location);
						}
						break;
					default: invalid_code_path(bin->location);
				}
				mov_mr(Address(dst), r2, size);
			} else {
				invalid_code_path(bin->location);
			}

			break;
		}
		case as: {
			append_cast(bin->left, destination, bin->right);
			break;
		}
		case lor: {
			if (destination.is_in_register) {
				append(bin->left, destination);

				auto jump = I(jnz_cr, 0, destination.reg);
				auto skip_start = instructions.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = instructions.count;

				jump->offset = skip_end - skip_start;
			} else {
				append(bin->left, destination);

				decltype(Instruction::jnz_cr) *jump;
				{
					tmpreg(r0);
					I(mov1_rm, r0, destination.address);
					jump = I(jnz_cr, 0, r0);
				}
				auto skip_start = instructions.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = instructions.count;

				jump->offset = skip_end - skip_start;
			}
			break;
		}
		case land: {
			if (destination.is_in_register) {
				append(bin->left, destination);

				auto jump = I(jz_cr, 0, destination.reg);
				auto skip_start = instructions.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = instructions.count;

				jump->offset = skip_end - skip_start;
			} else {
				append(bin->left, destination);

				decltype(Instruction::jz_cr) *jump;
				{
					tmpreg(r0);
					I(mov1_rm, r0, destination.address);
					jump = I(jz_cr, 0, r0);
				}

				auto skip_start = instructions.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = instructions.count;

				jump->offset = skip_end - skip_start;
			}
			break;
		}
		default:
			invalid_code_path(bin->location);
	}

	return;
}
void FrameBuilder::append(AstIdentifier *identifier, RegisterOrAddress destination) {
	auto definition = identifier->definition();

	if (definition->is_constant && definition->expression) {
		if (auto lambda = get_lambda(definition->expression)) {
			if (lambda->body) {
				assert(definition->offset != -1);
				if (destination.is_in_register) {
					I(lea, destination.reg, Register::instructions + definition->offset);
				} else {
					tmpreg(r0);
					I(lea, r0, Register::instructions + definition->offset);
					mov_mr(destination.address, r0, compiler->stack_word_size);
				}
			} else {
				if (destination.is_in_register) {
					I(mov_re, destination.reg, definition->name);
				} else {
					tmpreg(r0);
					I(mov_re, r0, definition->name);
					mov_mr(destination.address, r0, compiler->stack_word_size);
				}
			}
			return;
		}
	}

	auto definition_address = get_address_of(definition);
	if (definition_address.is_known) {
		copy(destination, definition_address.known_address, get_size(identifier->type), false);
	} else {
		auto computed_address = definition_address.computed_address;
		defer {
			if (computed_address.is_in_register)
				free_register(computed_address.reg);
		};
		Register definition_address_register;
		defer { if (is_temporary_register(definition_address_register)) free_temporary_register(definition_address_register); };
		if (computed_address.is_in_register) {
			definition_address_register = computed_address.reg;
		} else {
			definition_address_register = allocate_temporary_register();
			mov_rm(definition_address_register, computed_address.address, compiler->stack_word_size);
		}
		copy(destination, Address(definition_address_register), get_size(identifier->type), false);
	}
}
void FrameBuilder::append(AstCall *call, RegisterOrAddress destination) {
	/*
	tlang calling convetion

	before issuing a call instruction the stack state is as follows:

	stack:
	return value / address
	argument_0 value / address
	argument_1 value / address
	...
	argument_n value / address <- rsp
	*/

	auto word_size = compiler->stack_word_size;

	push_comment(format(u8"call {}", call->callable->location));

	//if (call->callable->location == "foo")
	//	debug_break();

	if (call->lambda_type) {
		auto lambda = call->lambda_type->lambda;

		// NOTE:
		// arguments with size <= compiler->stack_word_size are passed by value,
		// otherwise they are passed by pointer
		s64 parameters_bytes = tl::count(lambda->parameters, [](auto param){return !param->is_constant;}) * word_size;
		s32 return_parameter_bytes = word_size;

		auto return_value_size = get_size(lambda->return_parameter->type);

		auto stack_space_used_for_call = parameters_bytes + return_parameter_bytes;
		if (lambda->convention == CallingConvention::stdcall)
			stack_space_used_for_call += 32;
		max_stack_space_used_for_call = max(max_stack_space_used_for_call, ceil(stack_space_used_for_call, 16ll));

		auto return_value_address = Register::rs + parameters_bytes;

		auto append_arguments = [&] {
			// TODO: implement for 32-bit
			assert(word_size == 8);

			// We need to first append all the arguments into temporary space, then put them on the stack, because
			// nested function calls can overwrite the arguments.

			auto args_tmp = allocate_temporary_space(parameters_bytes);

			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto arg = call->sorted_arguments[i];
				auto param = lambda->parameters[i];

				auto arg_addr = args_tmp + (call->sorted_arguments.count-1-i)*8;

				auto size = ceil(get_size(arg->type), word_size);
				if (size > word_size) {
					// Put argument into temporary space and pass a pointer to it
					auto tmp = allocate_temporary_space(size);
					append(arg, tmp);

					tmpreg(r0);
					I(lea, r0, tmp);
					I(mov8_mr, arg_addr, r0);
				} else {
					// leave small argument on the stack.
					append(arg, arg_addr);
				}
			}

			if (return_value_size > word_size) {
				tmpreg(r0);
				I(lea, r0, destination.address);
				I(mov8_mr, return_value_address, r0);
			}

			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto src = args_tmp + (call->sorted_arguments.count-1-i)*word_size;
				tmpreg(r0);
				auto dst = Register::rs + (call->sorted_arguments.count-1-i)*word_size;
				I(mov8_rm, r0, src);
				I(mov8_mr, dst, r0);
			}
		};

		if (lambda->is_intrinsic) {
			append_arguments();

			assert(compiler->stack_word_size == 8);
			auto name = lambda->definition->name;
			if (name == "debug_break") {
				I(debug_break);
			} else if (name == "memcpy") {
				tmpreg(rdst);
				tmpreg(rsrc);
				tmpreg(rsize);
				I(mov8_rm, rsize, Address(Register::rs));
				I(mov8_rm, rsrc, Register::rs + 8);
				I(mov8_rm, rdst, Register::rs + 16);
				I(copyf_mmr, Address(rdst), Address(rsrc), rsize);
			} else if (name == "debug_print_int") {
				tmpreg(r0);
				I(mov8_rm, r0, Address(Register::rs));
				I(debug_print_int, r0);
			} else if (name == "sqrt_F32") {
				tmpreg(r0);
				I(mov4_rm, r0, Register::rs + 0);
				I(sqrt4_f, r0);
				copy(destination, r0, 4, false);
			} else if (name == "sqrt_F64") {
				tmpreg(r0);
				I(mov8_rm, r0, Register::rs + 0);
				I(sqrt8_f, r0);
				copy(destination, r0, 8, false);
			//} else if (name == "round") {
			//	assert(types_match(call->sorted_arguments[0]->type, compiler->builtin_f64));
			//	I(mov8_rm, r1, rs + 0);
			//	I(mov8_rm, r0, rs + 8);
			//	I(round8_f, r0, (RoundingMode)get_constant_integer(call->sorted_arguments[1]).value());
			} else {
				invalid_code_path(call->location, "Unknown intrinsic");
			}
			return;
		}

		assert(word_size == 8);

		auto &arguments = call->sorted_arguments;

		bool lambda_is_constant = is_constant(call->callable);

		append_arguments();

		assert(lambda->convention != CallingConvention::none);

		if (lambda_is_constant) {
			if (lambda->definition) { // null if polymorphic
				assert(lambda->definition->is_constant);
			}

			switch (lambda->convention) {
				case CallingConvention::tlang: {
					instructions_that_reference_lambdas.add({
						.instruction = II(call_c, -1, lambda),
						.lambda = lambda,
					});
					break;
				}
				case CallingConvention::stdcall: {
					LOAD_ADDRESS_INTO_REGISTER(fn_addr, call->callable);
					I(call_r, fn_addr, lambda);
					break;
				}
			}
		} else {
			APPEND_INTO_REGISTER(callable_reg, call->callable);
			I(call_r, callable_reg, lambda);
		}

		if (return_value_size && return_value_size <= word_size) {
			copy(destination, return_value_address, return_value_size, false);
		}
#if 0
			case CallingConvention::stdcall: {
				not_implemented();
				using namespace x86_64;

				s64 const shadow_space_size = 32;

				if (lambda->parameters.count < 4) {
					// shadow space
					parameters_bytes += 32;
				}

				for (auto argument : arguments) {
					assert(get_size(argument->type) <= 8);
				}

				::Address fnptr;
				if (!lambda_is_constant) {
					fnptr = allocate_temporary_space(compiler->stack_word_size);
					append(call->callable, fnptr);
				}

				append_arguments();

				auto move_arg = [&](int arg_index, s64 stack_offset) {
		 			if (lambda->parameters.count > arg_index)
						if (::is_float(lambda->parameters[arg_index]->type))
							I(mov8_xm, stdcall_float_registers[arg_index], rs + (lambda->parameters.count*8 - stack_offset));
						else
							I(mov8_rm, to_bc_register(stdcall_int_registers[arg_index]), rs + (lambda->parameters.count*8 - stack_offset));
				};

		 		move_arg(0,  8);
				move_arg(1, 16);
				move_arg(2, 24);
				move_arg(3, 32);

				if (lambda->parameters.count > 4) {
					push_comment("Swap argument order"str);

					// these are not used in bytecode and stdcall
					auto r0 = x86_64::to_bc_register(x86_64::Register64::r10);
					auto r1 = x86_64::to_bc_register(x86_64::Register64::r11);

					for (s64 i = 0; i < lambda->parameters.count / 2; ++i) {
						auto m0 = rs + i * 8;
						auto m1 = rs + (lambda->parameters.count-i-1)*8;
						I(mov8_rm, r0, m0);
						I(xchg8_mr, m1, r0);
						I(mov8_mr, m0, r0);
					}
				}

				auto function_address_register = to_bc_register(Register64::rax);
				if (lambda_is_constant) {
					load_address_of(call->callable, function_address_register);
					I(stdcall_r, function_address_register);
				} else {
					I(mov8_rm, function_address_register, fnptr);
					I(stdcall_r, function_address_register);
				}

				if (return_value_size) {
					copy(destination, to_bc_register(Register64::rax), return_value_size, false);
				}

				break;
			}
			default:
				invalid_code_path();
		}
#endif
		return;
	} else {
		auto Struct = direct_as<AstStruct>(call->callable);
		assert(Struct);

		append_struct_initializer(Struct, call->sorted_arguments, destination);
		return;
	}
	invalid_code_path(call->location);
}
void FrameBuilder::append(AstLiteral *literal, RegisterOrAddress destination) {
	if (literal->literal_kind == LiteralKind::string)
		push_comment(format(u8"literal \"{}\"", escape_string(literal->string.get())));
	else
		push_comment(format(u8"literal {}", literal->location));

	assert(!types_match(literal->type, compiler->builtin_unsized_integer.Struct));
	assert(!types_match(literal->type, compiler->builtin_unsized_float.Struct));
	auto dtype = direct(literal->type);

	using enum LiteralKind;

	switch (literal->literal_kind) {
		case string: {
			assert(!destination.is_in_register);
			assert(literal->string.offset != -1);
			tmpreg(r0);
			I(lea, r0, Register::constants + literal->string.offset);
			mov_mr(destination.address, r0, compiler->stack_word_size);
			mov_mc(destination.address+8, (s64)literal->string.count, compiler->stack_word_size);
			break;
		}
		case character:
			if (destination.is_in_register) I(mov_rc, destination.reg, (s64)literal->character);
			else                         I(mov1_mc, destination.address, (s64)literal->character);
			break;
		case Float:
			push_comment(format(u8"float {}", literal->Float));

			if (destination.is_in_register) {
				switch (get_size(literal->type)) {
					case 4: I(mov_rc, destination.reg, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
					case 8: I(mov_rc, destination.reg, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
					default: invalid_code_path(literal->location);
				}
			} else {
				switch (get_size(literal->type)) {
					case 4: I(mov4_mc, destination.address, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
					case 8: I(mov8_mc, destination.address, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
					default: invalid_code_path(literal->location);
				}
			}
			break;
		case boolean:
			if (destination.is_in_register) I(mov_rc, destination.reg, (s64)literal->Bool);
			else                         I(mov1_mc, destination.address, (s64)literal->Bool);
			break;
		case integer: {
			if (destination.is_in_register) {
				if (dtype == compiler->builtin_u8 .Struct|| dtype == compiler->builtin_s8.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == compiler->builtin_u16 .Struct|| dtype == compiler->builtin_s16.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == compiler->builtin_u32 .Struct|| dtype == compiler->builtin_s32.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == compiler->builtin_u64 .Struct|| dtype == compiler->builtin_s64 .Struct|| dtype == compiler->builtin_void.pointer)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == compiler->builtin_f32.Struct) {
					auto f = (f32)(s64)literal->integer;
					I(mov_rc, destination.reg, *(s32 *)&f);
				} else if (dtype == compiler->builtin_f64.Struct) {
					auto f = (f64)(s64)literal->integer;
					I(mov_rc, destination.reg, *(s64 *)&f);
				} else if (::is_pointer(literal->type) || direct_as<AstEnum>(literal->type))
					I(mov_rc, destination.reg, (s64)literal->integer);
				else invalid_code_path(literal->location);
			} else {
				if (dtype == compiler->builtin_u8 .Struct|| dtype == compiler->builtin_s8.Struct)
					I(mov1_mc, destination.address, (s64)literal->integer);
				else if (dtype == compiler->builtin_u16 .Struct|| dtype == compiler->builtin_s16.Struct)
					I(mov2_mc, destination.address, (s64)literal->integer);
				else if (dtype == compiler->builtin_u32 .Struct|| dtype == compiler->builtin_s32.Struct)
					I(mov4_mc, destination.address, (s64)literal->integer);
				else if (dtype == compiler->builtin_u64 .Struct|| dtype == compiler->builtin_s64 .Struct|| dtype == compiler->builtin_void.pointer)
					I(mov8_mc, destination.address, (s64)literal->integer);
				else if (dtype == compiler->builtin_f32.Struct) {
					auto f = (f32)(s64)literal->integer;
					I(mov4_mc, destination.address, *(s32 *)&f);
				} else if (dtype == compiler->builtin_f64.Struct) {
					auto f = (f64)(s64)literal->integer;
					I(mov8_mc, destination.address, *(s64 *)&f);
				} else if (::is_pointer(literal->type) || direct_as<AstEnum>(literal->type))
					mov_mc(destination.address, (s64)literal->integer, compiler->stack_word_size);
				else invalid_code_path(literal->location);
			}
			break;
		}
		case null: {
			if (destination.is_in_register) {
				I(xor_rr, destination.reg, destination.reg);
			} else {
				append_memory_set(destination.address, 0, get_size(literal->type));
			}
			break;
		}
		case Struct: {
			copy(destination, Register::constants + literal->struct_offset, get_size(literal->type), false);
			break;
		}
		default: invalid_code_path(literal->location);
	}
}
void FrameBuilder::append(AstUnaryOperator *unop, RegisterOrAddress destination) {
	push_comment(format(u8"unary {}", unop->location));
	switch (unop->operation) {
		using enum UnaryOperation;
		case minus: {
			append(unop->expression, destination);
			if (destination.is_in_register) {
				auto size = get_size(unop->type);
				if (::is_integer(unop->type)) {
					I(negi_r, destination.reg);
				} else if (::is_float(unop->type)) {
					switch (size) {
						case 4: I(sbxor_rc, destination.reg, 31); break;
						case 8: I(sbxor_rc, destination.reg, 63); break;
						default: invalid_code_path(unop->location);
					}
				} else {
					invalid_code_path(unop->location);
				}
			} else {
				auto size = get_size(unop->type);
				if (::is_integer(unop->type)) {
					negi_m(destination.address, size);
				} else if (::is_float(unop->type)) {
					switch (size) {
						case 4: sbxor_mc(destination.address, 31, 4); break;
						case 8: sbxor_mc(destination.address, 63, 8); break;
						default: invalid_code_path(unop->location);
					}
				} else {
					invalid_code_path(unop->location);
				}
			}

			break;
		}
		case address_of: {
			load_address_of(unop->expression, destination);
			break;
		}
		case dereference: {
			auto pointer = append(unop->expression);
			defer { if (pointer.is_in_register) free_register(pointer.reg); };

			tmpreg(r0);
			auto src = pointer.is_in_register ? pointer.reg : r0;
			if (!pointer.is_in_register)
				I(mov8_rm, src, pointer.address);

			check_null(src, unop->expression->location);

			tmpreg(r1);
			auto dst = destination.is_in_register ? destination.reg : r1;
			if (!destination.is_in_register)
				I(mov8_rm, dst, destination.address);

			auto size = get_size(unop->type);
			if (size <= 8) {

				switch (size) {
					case 1: I(movzx81_rm, dst, Address(src)); break;
					case 2: I(movzx82_rm, dst, Address(src)); break;
					case 4: I(movzx84_rm, dst, Address(src)); break;
					case 8: I(mov8_rm,    dst, Address(src)); break;

					case 3: {
						tmpreg(r2);
						I(movzx82_rm, dst, Address(src));
						I(movzx81_rm, r2, src + 2);
						I(shl_rc, dst, 2);
						I(or_rr, dst, r2);
						break;
					}

					default: invalid_code_path(unop->location);
				}

				if (!destination.is_in_register)
					I(mov8_mr, destination.address, dst);
			} else {
				assert(!destination.is_in_register);
				copy(destination.address, Address(src), size, false);
			}
			break;
		}
		case lnot: {
			append(unop->expression, destination);
			if (destination.is_in_register) {
				I(not_r, destination.reg);
				I(and_rc, destination.reg, 1);
			} else {
				not_m(destination.address, get_size(unop->type));
				and_mc(destination.address, 1, get_size(unop->type));
			}
			break;
		}
		case bnot: {
			append(unop->expression, destination);
			if (destination.is_in_register) {
				I(not_r, destination.reg);
			} else {
				not_m(destination.address, get_size(unop->type));
			}
			break;
		}
		case unwrap: {
			auto option = append(unop->expression);
			defer { if (option.is_in_register) free_register(option.reg); };
			assert(!option.is_in_register);
			assert(!destination.is_in_register);

			auto value_size = get_size(unop->type);

			{
				tmpreg(r0);
				I(mov1_rm, r0, option.address + value_size);
				I(jnz_cr, 2, r0);
			}
			push_comment(format("{} didn't have a value. unwrap failed."str, unop->expression->location));
			I(debug_break);
			I(jmp_label);

			copy(destination.address, option.address, value_size, false);
			break;
		}
		case autocast: {
			append(unop->expression, destination);
			break;
		}
		case move_to_temporary: {
			auto size = get_size(unop->expression->type);

			auto tmp = allocate_temporary_space(size);

			append(unop->expression, tmp);

			if (destination.is_in_register) {
				I(lea, destination.reg, tmp);
			} else {
				tmpreg(r0);
				I(lea, r0, tmp);
				I(mov8_mr, destination.address, r0);
			}
			break;
		}
		case pack: {
			append(unop->expression, destination);
			break;
		}
		default:
			invalid_code_path(unop->location);
	}
}
void FrameBuilder::append(AstSubscript *subscript, RegisterOrAddress destination) {
	auto subscriptable_type = subscript->expression->type;

	if (as<AstArray>(subscriptable_type) ||
		get_span_subtype(subscriptable_type) ||
		types_match(subscriptable_type, compiler->builtin_string) ||
		is_pointer(subscriptable_type)
	) {
		if (is_addressable(subscript->expression)) {
			LOAD_ADDRESS_INTO_REGISTER(addr, subscript);

			auto size = get_size(subscript->type);

			copy(destination, Address(addr), size, false);
		} else {
			auto tmp = allocate_temporary_space(get_size(subscriptable_type));
			append(subscript->expression, tmp);

			APPEND_INTO_REGISTER(idx, subscript->index_expression);

			auto elem_size = get_size(subscript->type);

			I(mul_rc, idx, elem_size);

			tmpreg(x);
			I(lea, x, tmp);
			I(add_rr, idx, x);

			copy(destination, Address(idx), elem_size, false);
		}
	} else if (::is_integer(subscriptable_type)) {
		APPEND2(
			value, append, subscript->expression,
			bit_index, append, subscript->index_expression
		);

		I(slr_rr, value, bit_index);
		I(and_rc, value, 1);

		copy(destination, value, 1, false);
	} else {
		invalid_code_path(subscript->location);
	}

	return;

	not_implemented();
#if 0
	assert(!destination);
	push_comment(format(u8"subscript {}", subscript->location));
	auto element_size = get_size(subscript->type);
	assert(element_size);


	// NOTE: order of evaluation matters

	if (is_power_of_2(element_size) && element_size <= 8) {
		auto index_register = r0;
		auto base_register = r1;

		append_to_stack(subscript->index_expression);

		Optional<Register> addr_opt;
		if (::is_pointer(subscript->expression->type)) {
			// pointer indexing
			append_to_stack(subscript->expression);
			I(pop_r, base_register);
		} else if (auto span = as_span(subscript->expression->type)) {
			// span indexing
			append_to_stack(subscript->expression);
			I(pop_r, base_register);
			I(add_rc, rs, compiler->stack_word_size);
		} else {
			// array indexing
			addr_opt = load_address_of(subscript->expression);
			if (addr_opt) {
				base_register = addr_opt.value_unchecked();
				free_register(base_register);
			} else {
				I(pop_r, base_register);
			}
		}
		I(pop_r, index_register);

		Address a = {};
		a.base = base_register;
		a.r1 = index_register;
		switch (element_size) {
			case 1: a.r1_scale_index = 1; break;
			case 2: a.r1_scale_index = 2; break;
			case 4: a.r1_scale_index = 3; break;
			case 8: a.r1_scale_index = 4; break;
			default: invalid_code_path();
		}
		I(push_m, a);
	} else {
		append_to_stack(subscript->index_expression);
		if (::is_pointer(subscript->expression->type)) {
			// pointer indexing
			append_to_stack(subscript->expression);
		} else if (auto span = as_span(subscript->expression->type)) {
			// span indexing
			append_to_stack(subscript->expression);

			// replace count with data
			I(pop_r, r0);
			I(mov8_mr, rs, r0);
		} else {
			// array indexing
			// :PUSH_ADDRESS: TODO: Replace this with load_address_of
			push_address_of(subscript->expression);
		}
		I(pop_r, r0); // array address
		I(pop_r, r1); // index

		I(mul_rc, r1, element_size);

		I(add_rr, r0, r1);
		// now r0 contains element's address

		// reserve space on stack
		I(sub_rc, rs, element_size);

		I(push_r, rs);// destination
		I(push_r, r0);// source
		copy(element_size, false, subscript->location, u8"stack"s);
	}
	return {};
#endif
}
void FrameBuilder::append(AstLambda *lambda, RegisterOrAddress destination) {
	load_address_of(lambda, destination);
}
void FrameBuilder::append(AstArrayInitializer *ArrayInitializer, RegisterOrAddress destination) {
	push_comment(format("ArrayInitializer {}"str, ArrayInitializer->location));

	auto array_type = as<AstArray>(ArrayInitializer->type);
	assert(array_type);
	auto elem_type = array_type->element_type;

	auto elem_size = get_size(elem_type);

	if (destination.is_in_register) {
		if (ArrayInitializer->elements.count == 0) {
			// Nothing to do.
		} else if (ArrayInitializer->elements.count == 1) {
			append(ArrayInitializer->elements[0], destination);
		} else {
			invalid_code_path(ArrayInitializer->location, "FIXME: Bytecode generation for such small arrays is not implemented.");
			auto tmp = allocate_temporary_space(elem_size);
			for (umm i = 0; i < ArrayInitializer->elements.count; ++i) {
				auto element = ArrayInitializer->elements[i];
				append(element, destination.address + i*elem_size);
			}
		}
	} else {
		for (umm i = 0; i < ArrayInitializer->elements.count; ++i) {
			auto element = ArrayInitializer->elements[i];
			append(element, destination.address + i*elem_size);
		}
	}
}

void BytecodeBuilder::optimize(InstructionList &instructions) {
	//propagate_known_addresses(instructions);
	//remove_redundant_instructions(instructions);
	//propagate_known_values(instructions);
}
#if 0
void BytecodeBuilder::propagate_known_addresses(InstructionList &instructions) {
	// Example transformation:
	//
	// lea r0, [locals + 16]  >>  lea r0, [locals + 16]
	// add r0, 4				  add r0, 4
	// mov r1, [r0]				  mov r1, [locals + 20]
	//
	// Note that we can't delete the lea and add yet,
	Optional<Address> register_state[register_count];

	auto modify = [&] (Address &a) {
		if (a.r1_scale_index == 0 && a.r2_scale == 0) {
			if (auto addr = register_state[(u8)a.base]) {
				a = addr.value_unchecked() + a.c;
			}
		}
	};

	for (auto &i : instructions) {
		switch (i.kind) {
			using enum InstructionKind;
			case jmp_label:
			case jmp:
			case jz_cr:
			case jnz_cr:
			case jef_c:
			case jnef_c:
			case jgf_c:
			case jlf_c:
			case jlef_c:
			case jgef_c:
			case call_c:
			case call_m:
			case call_r:
				for (auto &s : register_state)
					s = null_opt;
				break;

			case lea: {
				if (i.lea.s.base == locals ||
					i.lea.s.base == constants ||
					i.lea.s.base == rwdata ||
					i.lea.s.base == zeros ||
					i.lea.s.base == rb ||
					i.lea.s.base == Register::instructions)
				{
					register_state[(u8)i.lea.d] = i.lea.s;
					// i.kind = noop;
				} else {
					register_state[(u8)i.lea.d] = null_opt;
				}
				break;
			}
			case mov_rc: {
				register_state[(u8)i.mov_rc.d] = null_opt;
				break;
			}
			case mov_rr: {
				register_state[(u8)i.mov_rr.d] = register_state[(u8)i.mov_rr.s];
				break;
			}
			case mov1_rm: { modify(i.mov1_rm.s); register_state[(u8)i.mov1_rm.d] = null_opt; break; }
			case mov2_rm: { modify(i.mov2_rm.s); register_state[(u8)i.mov2_rm.d] = null_opt; break; }
			case mov4_rm: { modify(i.mov4_rm.s); register_state[(u8)i.mov4_rm.d] = null_opt; break; }
			case mov8_rm: { modify(i.mov8_rm.s); register_state[(u8)i.mov8_rm.d] = null_opt; break; }
			case mov1_mc: { modify(i.mov1_mc.d); break; }
			case mov2_mc: { modify(i.mov2_mc.d); break; }
			case mov4_mc: { modify(i.mov4_mc.d); break; }
			case mov8_mc: { modify(i.mov8_mc.d); break; }
			case mov1_mr: { modify(i.mov1_mr.d); break; }
			case mov2_mr: { modify(i.mov2_mr.d); break; }
			case mov4_mr: { modify(i.mov4_mr.d); break; }
			case mov8_mr: { modify(i.mov8_mr.d); break; }
			case add_rc: { register_state[(u8)i.add_rc.d].apply([&] (auto &addr) { addr.c += i.add_rc.s; }); break; }
			case sub_rc: { register_state[(u8)i.sub_rc.d].apply([&] (auto &addr) { addr.c -= i.sub_rc.s; }); break; }


			case add_mc: { modify(i.add_mc.d); break; }
			case sub_mc: { modify(i.sub_mc.d); break; }


case xchg_rr            :
case xchg1_mr           :
case xchg2_mr           :
case xchg4_mr           :
case xchg8_mr           :
case movsx21_rm         :
case movsx41_rm         :
case movsx81_rm         :
case movsx42_rm         :
case movsx82_rm         :
case movsx84_rm         :
case movzx21_rm         :
case movzx41_rm         :
case movzx81_rm         :
case movzx42_rm         :
case movzx82_rm         :
case movzx84_rm         :
case movsx21_rr         :
case movsx41_rr         :
case movsx81_rr         :
case movsx42_rr         :
case movsx82_rr         :
case movsx84_rr         :
case movzx21_rr         :
case movzx41_rr         :
case movzx81_rr         :
case movzx42_rr         :
case movzx82_rr         :
case movzx84_rr         :
case push_c             :
case push_r             :
case push_f             :
case push_m             :
case mov_re             :
case pop_r              :
case pop_f              :
case pop_m              :
case ret                :
case shr_rc             :
case shr_rr             :
case shr_rm             :
case shr_mc             :
case shr_mr             :
case shl_rc             :
case shl_rr             :
case shl_rm             :
case shl_mc             :
case shl_mr             :
case add_rr             :
case add_rm             :
case add_mr             :
case sub_rr             :
case sub_rm             :
case sub_mr             :
case mul_rc             :
case mul_rr             :
case mul_rm             :
case mul_mc             :
case mul_mr             :
case div_rc             :
case div_rr             :
case div_rm             :
case div_mc             :
case div_mr             :
case mod_rc             :
case mod_rr             :
case mod_rm             :
case mod_mc             :
case mod_mr             :
case not_r              :
case not_m              :
case or_rc              :
case or_rr              :
case or1_rm              :
case or2_rm              :
case or4_rm              :
case or8_rm              :
case or_mc              :
case or_mr              :
case and_rc             :
case and_rr             :
case and_rm             :
case and_mc             :
case and_mr             :
case xor_rc             :
case xor_rr             :
case xor_rm             :
case xor1_mc             :
case xor2_mc             :
case xor4_mc             :
case xor8_mc             :
case xor_mr             :
case negi_r             :
case negi8_m            :
case negi16_m           :
case negi32_m           :
case negi64_m           :
/* Comparison with destination */ \
case cmpu1              :
case cmpu2              :
case cmpu4              :
case cmpu8              :
case cmps1              :
case cmps2              :
case cmps4              :
case cmps8              :
case cmpstr             :
/* Comparison without destination (uses flags) */ \
case cmpf1              :
case cmpf2              :
case cmpf4              :
case cmpf8              :
case copyf_mmc          :
case copyb_mmc          :
case copyf_mmr          :
case copyb_mmr          :
case set_mcc           :
case begin_lambda       :
case end_lambda         :
case cvt_f32_s32        :
case cvt_s32_f32        :
case cvt_f64_s64        :
case cvt_s64_f64        :
case mov_fr             :
case mov_rf             :
case mov1_xm            :
case mov2_xm            :
case mov4_xm            :
case mov8_xm            :
case add_ff        :
case mul_ff        :
case sub_ff        :
case div_ff        :
case xor_ff             :
case tobool_r           :
case toboolnot_r        :
case noop               :
case prepare_stack      :
case debug_break        :
case debug_line         :
case debug_start_lambda :

			default: {
				for (auto &s : register_state)
					s = null_opt;
				//with(ConsoleColor::yellow, print("propagate_known_addresses: unhandled {}\n", i.kind));
				break;
			}
		}
	}
}
void BytecodeBuilder::remove_redundant_instructions(InstructionList &instructions) {
	struct RegisterState {
		Instruction *setter;
		List<Instruction *> modders;
		bool was_used;
	};

	RegisterState register_state[register_count] = {};

	for (auto &i : instructions) {
		auto set = [&](Register dst) {
			auto &state = register_state[(u8)dst];
			if (state.setter && !state.was_used) {
				state.setter->kind = InstructionKind::noop;
			}
			state.setter = &i;
			state.modders.clear();
			state.was_used = false;
		};
		auto use = [&](auto src) {
			using Src = decltype(src);
			if constexpr (is_same<Src, Register>) {
				register_state[(u8)src].was_used = true;
			} else {
				static_assert(is_same<Src, Address>);
				register_state[(u8)src.base].was_used = true;
				if (src.r1_scale_index) register_state[(u8)src.r1].was_used = true;
				if (src.r2_scale)       register_state[(u8)src.r2].was_used = true;
			}
		};
		auto mod = [&](Register src) {
			register_state[(u8)src].modders.add(&i);
		};

		// 1. read a register/address - use
		// 2. modify a register       - mod
		// 3. overwrite a register    - set

		switch (i.kind) {
			using enum InstructionKind;
			case lea:     use(i.lea.s);     set(i.lea.d);     break;
			case mov_rc:                    set(i.mov_rc.d);  break;
			case mov_rr:  use(i.mov_rr.s);  set(i.mov_rr.d);  break;
			case mov_re:                    set(i.mov_re.d);  break;
			case mov1_rm: use(i.mov1_rm.s); set(i.mov1_rm.d); break;
			case mov2_rm: use(i.mov2_rm.s); set(i.mov2_rm.d); break;
			case mov4_rm: use(i.mov4_rm.s); set(i.mov4_rm.d); break;
			case mov8_rm: use(i.mov8_rm.s); set(i.mov8_rm.d); break;
			case mov1_mr: use(i.mov1_mr.d); use(i.mov1_mr.s); break;
			case mov2_mr: use(i.mov2_mr.d); use(i.mov2_mr.s); break;
			case mov4_mr: use(i.mov4_mr.d); use(i.mov4_mr.s); break;
			case mov8_mr: use(i.mov8_mr.d); use(i.mov8_mr.s); break;
			case mov1_mc: use(i.mov1_mc.d);                   break;
			case mov2_mc: use(i.mov2_mc.d);                   break;
			case mov4_mc: use(i.mov4_mc.d);                   break;
			case mov8_mc: use(i.mov8_mc.d);                   break;
			case movsx21_rm: use(i.movsx21_rm.s); set(i.movsx21_rm.d); break;
			case movsx41_rm: use(i.movsx41_rm.s); set(i.movsx41_rm.d); break;
			case movsx81_rm: use(i.movsx81_rm.s); set(i.movsx81_rm.d); break;
			case movsx42_rm: use(i.movsx42_rm.s); set(i.movsx42_rm.d); break;
			case movsx82_rm: use(i.movsx82_rm.s); set(i.movsx82_rm.d); break;
			case movsx84_rm: use(i.movsx84_rm.s); set(i.movsx84_rm.d); break;
			case movzx21_rm: use(i.movzx21_rm.s); set(i.movzx21_rm.d); break;
			case movzx41_rm: use(i.movzx41_rm.s); set(i.movzx41_rm.d); break;
			case movzx81_rm: use(i.movzx81_rm.s); set(i.movzx81_rm.d); break;
			case movzx42_rm: use(i.movzx42_rm.s); set(i.movzx42_rm.d); break;
			case movzx82_rm: use(i.movzx82_rm.s); set(i.movzx82_rm.d); break;
			case movzx84_rm: use(i.movzx84_rm.s); set(i.movzx84_rm.d); break;
			case movsx21_rr: use(i.movsx21_rr.s); set(i.movsx21_rr.d); break;
			case movsx41_rr: use(i.movsx41_rr.s); set(i.movsx41_rr.d); break;
			case movsx81_rr: use(i.movsx81_rr.s); set(i.movsx81_rr.d); break;
			case movsx42_rr: use(i.movsx42_rr.s); set(i.movsx42_rr.d); break;
			case movsx82_rr: use(i.movsx82_rr.s); set(i.movsx82_rr.d); break;
			case movsx84_rr: use(i.movsx84_rr.s); set(i.movsx84_rr.d); break;
			case movzx21_rr: use(i.movzx21_rr.s); set(i.movzx21_rr.d); break;
			case movzx41_rr: use(i.movzx41_rr.s); set(i.movzx41_rr.d); break;
			case movzx81_rr: use(i.movzx81_rr.s); set(i.movzx81_rr.d); break;
			case movzx42_rr: use(i.movzx42_rr.s); set(i.movzx42_rr.d); break;
			case movzx82_rr: use(i.movzx82_rr.s); set(i.movzx82_rr.d); break;
			case movzx84_rr: use(i.movzx84_rr.s); set(i.movzx84_rr.d); break;
			case add_rc:                  mod(i.add_rc.d); break;
			case add_rr: use(i.add_rr.s); mod(i.add_rr.d); break;
			case add_rm: use(i.add_rm.s); mod(i.add_rm.d); break;
			case add_mc:                  use(i.add_mc.d); break;
			case add_mr: use(i.add_mr.s); use(i.add_mr.d); break;
			case sub_rc:                  use(i.sub_rc.d); break;
			case sub_rr: use(i.sub_rr.s); use(i.sub_rr.d); break;
			case sub_rm: use(i.sub_rm.s); use(i.sub_rm.d); break;
			case sub_mc:                  use(i.sub_mc.d); break;
			case sub_mr: use(i.sub_mr.s); use(i.sub_mr.d); break;
			case mul_rc:                  use(i.mul_rc.d); break;
			case mul_rr: use(i.mul_rr.s); use(i.mul_rr.d); break;
			case mul_rm: use(i.mul_rm.s); use(i.mul_rm.d); break;
			case mul_mc:                  use(i.mul_mc.d); break;
			case mul_mr: use(i.mul_mr.s); use(i.mul_mr.d); break;
			case div_rc:                  use(i.div_rc.d); break;
			case div_rr: use(i.div_rr.s); use(i.div_rr.d); break;
			case div_rm: use(i.div_rm.s); use(i.div_rm.d); break;
			case div_mc:                  use(i.div_mc.d); break;
			case div_mr: use(i.div_mr.s); use(i.div_mr.d); break;
			case mod_rc:                  use(i.mod_rc.d); break;
			case mod_rr: use(i.mod_rr.s); use(i.mod_rr.d); break;
			case mod_rm: use(i.mod_rm.s); use(i.mod_rm.d); break;
			case mod_mc:                  use(i.mod_mc.d); break;
			case mod_mr: use(i.mod_mr.s); use(i.mod_mr.d); break;
			case xor_rc:                  use(i.xor_rc.d); break;
			case xor_rr: use(i.xor_rr.s); use(i.xor_rr.d); break;
			case xor_rm: use(i.xor_rm.s); use(i.xor_rm.d); break;
			case xor1_mc:                 use(i.xor1_mc.d); break;
			case xor2_mc:                 use(i.xor2_mc.d); break;
			case xor4_mc:                 use(i.xor4_mc.d); break;
			case xor8_mc:                 use(i.xor8_mc.d); break;
			case xor_mr: use(i.xor_mr.s); use(i.xor_mr.d); break;
			case and_rc:                  use(i.and_rc.d); break;
			case and_rr: use(i.and_rr.s); use(i.and_rr.d); break;
			case and_rm: use(i.and_rm.s); use(i.and_rm.d); break;
			case and_mc:                  use(i.and_mc.d); break;
			case and_mr: use(i.and_mr.s); use(i.and_mr.d); break;
			case or_rc:                   use(i.or_rc.d);  break;
			case or_rr:  use(i.or_rr.s);  use(i.or_rr.d);  break;
			case or1_rm: use(i.or1_rm.s); use(i.or1_rm.d); break;
			case or2_rm: use(i.or2_rm.s); use(i.or2_rm.d); break;
			case or4_rm: use(i.or4_rm.s); use(i.or4_rm.d); break;
			case or8_rm: use(i.or8_rm.s); use(i.or8_rm.d); break;
			case or_mc:                   use(i.or_mc.d);  break;
			case or_mr:  use(i.or_mr.s);  use(i.or_mr.d);  break;
			case shl_rc:                  use(i.shl_rc.d); break;
			case shl_rr: use(i.shl_rr.s); use(i.shl_rr.d); break;
			case shl_rm: use(i.shl_rm.s); use(i.shl_rm.d); break;
			case shl_mc:                  use(i.shl_mc.d); break;
			case shl_mr: use(i.shl_mr.s); use(i.shl_mr.d); break;
			case shr_rc:                  use(i.shr_rc.d); break;
			case shr_rr: use(i.shr_rr.s); use(i.shr_rr.d); break;
			case shr_rm: use(i.shr_rm.s); use(i.shr_rm.d); break;
			case shr_mc:                  use(i.shr_mc.d); break;
			case shr_mr: use(i.shr_mr.s); use(i.shr_mr.d); break;
			case copyf_mmc: use(i.copyf_mmc.s);                        use(i.copyf_mmc.d); break;
			case copyb_mmc:	use(i.copyb_mmc.s);                        use(i.copyb_mmc.d); break;
			case copyf_mmr:	use(i.copyf_mmr.s); use(i.copyf_mmr.size); use(i.copyf_mmr.d); break;
			case copyb_mmr:	use(i.copyb_mmr.s); use(i.copyb_mmr.size); use(i.copyb_mmr.d); break;
			case set_mcc: use(i.set_mcc.d); break;
			case call_c:                  break;
			case call_r: use(i.call_r.s); break;
			case call_m: use(i.call_m.s); break;
				not_implemented();
#if 0
				use(i.stdcall_r.s);
				use(x86_64::to_bc_register(x86_64::Register64::rcx));
				use(x86_64::to_bc_register(x86_64::Register64::rdx));
				use(x86_64::to_bc_register(x86_64::Register64::r8));
				use(x86_64::to_bc_register(x86_64::Register64::r9));
#endif
				break;
			case xchg_rr:  use(i.xchg_rr .a); use(i.xchg_rr .b); break;
			case xchg1_mr: use(i.xchg1_mr.a); use(i.xchg1_mr.b); break;
			case xchg2_mr: use(i.xchg2_mr.a); use(i.xchg2_mr.b); break;
			case xchg4_mr: use(i.xchg4_mr.a); use(i.xchg4_mr.b); break;
			case xchg8_mr: use(i.xchg8_mr.a); use(i.xchg8_mr.b); break;
			case cmpu1: use(i.cmpu1.a); use(i.cmpu1.b); set(i.cmpu1.d); break;
			case cmpu2:	use(i.cmpu2.a); use(i.cmpu2.b); set(i.cmpu2.d); break;
			case cmpu4:	use(i.cmpu4.a); use(i.cmpu4.b); set(i.cmpu4.d); break;
			case cmpu8:	use(i.cmpu8.a); use(i.cmpu8.b); set(i.cmpu8.d); break;
			case cmps1:	use(i.cmps1.a); use(i.cmps1.b); set(i.cmps1.d); break;
			case cmps2:	use(i.cmps2.a); use(i.cmps2.b); set(i.cmps2.d); break;
			case cmps4:	use(i.cmps4.a); use(i.cmps4.b); set(i.cmps4.d); break;
			case cmps8:	use(i.cmps8.a); use(i.cmps8.b); set(i.cmps8.d); break;
			case not_r:     use(i.not_r   .d); break;
			case not_m:     use(i.not_m   .d); break;
			case negi_r:    use(i.negi_r  .d); break;
			case negi8_m:   use(i.negi8_m .d); break;
			case negi16_m:  use(i.negi16_m.d); break;
			case negi32_m:  use(i.negi32_m.d); break;
			case negi64_m:  use(i.negi64_m.d); break;

			case jmp_label: {
				for (auto &state : register_state) {
					state.setter = 0;
				}
				break;
			}
			case jmp:
			case jz_cr:
			case jnz_cr:
			case jef_c:
			case jnef_c:
			case jlf_c:
			case jgf_c:
			case jlef_c:
			case jgef_c:
			{
				switch (i.kind) {
					case jmp:
					case jef_c:
					case jnef_c:
					case jlf_c:
					case jgf_c:
					case jlef_c:
					case jgef_c:
						break;
					case jz_cr: use(i.jz_cr.reg); break;
					case jnz_cr: use(i.jnz_cr.reg); break;
					default: invalid_code_path();
				}
				for (auto &state : register_state) {
					if (state.setter && !state.was_used) {
						state.setter->kind = InstructionKind::noop;
						for (auto &modder : state.modders) {
							modder->kind = InstructionKind::noop;
						}
					}
					state.setter = 0;
				}
				break;
			}

			case push_c:
			case push_r:
			case push_f:
			case push_m:
			case pop_r:
			case pop_f:
			case pop_m:
			case ret:
			case begin_lambda:
			case end_lambda:
			case cvt_f32_s32:
			case cvt_s32_f32:
			case cvt_f64_s64:
			case cvt_s64_f64:
			case mov_fr:
			case mov_rf:
			case mov1_xm:
			case mov2_xm:
			case mov4_xm:
			case mov8_xm:
			case add_ff:
			case mul_ff:
			case sub_ff:
			case div_ff:
			case xor_ff:
			case tobool_r:
			case toboolnot_r:
			case prepare_stack:
				not_implemented();
				break;
			case noop:
			case debug_break:
			case debug_line:
			case debug_start_lambda:
				break;
			default: {
				with(ConsoleColor::yellow, print("remove_redundant_instructions: unhandled {}\n", i.kind));
				for (auto &state : register_state) {
					state = {};
				}
				break;
			}
		}
	}

	for (auto &state : register_state) {
		if (state.setter && !state.was_used) {
			state.setter->kind = InstructionKind::noop;
			for (auto &modder : state.modders) {
				modder->kind = InstructionKind::noop;
			}
		}
	}
}
void BytecodeBuilder::propagate_known_values(InstructionList &instructions) {

	enum ValueKind {
		unknown,
		constant,
		address,
	};

	struct Value {
		ValueKind kind;
		union {
			s64 constant;
			Address address;
		};
	};

	Value registers[register_count] = {};

	static constexpr umm capacity = 0x10000;
	struct StackState {
		u8 buffer[capacity] = {};
		u64 known[capacity / 64] = {};

		umm i(s64 offset) {
			assert(offset < 0);
			return capacity + offset;
		}

		// set known
		void sk(s64 offset) {
			known[i(offset) / 64] |= (u64)1 << (u64)(i(offset) % 64);
		}

		// read known
		bool rk(s64 offset) {
			return known[i(offset) / 64] & ((u64)1 << (u64)(i(offset) % 64));
		}

		// write n bytes
		void w1(u8 v, s64 offset) {
			buffer[i(offset)] = v;
			sk(offset);
		}
		void w2(u16 v, s64 offset) {
			*(u16 *)(buffer + i(offset)) = v;
			sk(offset + 0);
			sk(offset + 1);
		}
		void w4(u32 v, s64 offset) {
			*(u32 *)(buffer + i(offset)) = v;
			sk(offset + 0);
			sk(offset + 1);
			sk(offset + 2);
			sk(offset + 3);
		}
		void w8(u64 v, s64 offset) {
			*(u64 *)(buffer + i(offset)) = v;
			sk(offset + 0);
			sk(offset + 1);
			sk(offset + 2);
			sk(offset + 3);
			sk(offset + 4);
			sk(offset + 5);
			sk(offset + 6);
			sk(offset + 7);
		}
		void w(Span<u8> v, s64 offset) {
			memcpy(buffer + i(offset), v.data, v.count);
			for (umm i = 0; i < v.count; ++i)
				sk(offset + i);
		}

		// set n bytes unknown
		void u1(s64 offset) {
			known[i(offset) / 64] &= ~((u64)1 << (u64)(i(offset) % 64));
		}
		void u2(s64 offset) {
			u1(offset + 0);
			u1(offset + 1);
		}
		void u4(s64 offset) {
			u1(offset + 0);
			u1(offset + 1);
			u1(offset + 2);
			u1(offset + 3);
		}
		void u8(s64 offset) {
			u1(offset + 0);
			u1(offset + 1);
			u1(offset + 2);
			u1(offset + 3);
			u1(offset + 4);
			u1(offset + 5);
			u1(offset + 6);
			u1(offset + 7);
		}
		void u(umm size, s64 offset) {
			for (umm i = 0; i < size; ++i)
				u1(offset + i);
		}

		// read n bytes
		Optional<tl::u8> r1(s64 offset) {
			if (rk(offset))
				return buffer[i(offset)];
			return {};
		}
		Optional<u16> r2(s64 offset) {
			if (rk(offset + 0) &&
				rk(offset + 1))
				return *(u16 *)(buffer + i(offset));
			return {};
		}
		Optional<u32> r4(s64 offset) {
			if (rk(offset + 0) &&
				rk(offset + 1) &&
				rk(offset + 2) &&
				rk(offset + 3))
				return *(u32 *)(buffer + i(offset));
			return {};
		}
		Optional<u64> r8(s64 offset) {
			if (rk(offset + 0) &&
				rk(offset + 1) &&
				rk(offset + 2) &&
				rk(offset + 3) &&
				rk(offset + 4) &&
				rk(offset + 5) &&
				rk(offset + 6) &&
				rk(offset + 7))
				return *(u64 *)(buffer + i(offset));
			return {};
		}
	};

	StackState stack;

	auto r = [&](Register r) -> Value & {
		return registers[(u8)r];
	};

	for (auto &i : instructions) {
		auto mov_mr = [&](umm size, Address d, Register s) {
			if (d.base == locals) {
				assert(d.r1_scale_index == 0);
				assert(d.r2_scale == 0);
			}
			auto &src = r(s);
			switch (src.kind) {
				case unknown:
				case address:
					if (d.base == locals)
						stack.u(size, d.c);
					break;
				case constant:
					if (d.base == locals)
						stack.w(value_as_bytes(src.constant).subspan(0, size), d.c);
					switch (size) {
						case 1: i = MI(mov1_mc, d, src.constant); break;
						case 2: i = MI(mov2_mc, d, src.constant); break;
						case 4: i = MI(mov4_mc, d, src.constant); break;
						case 8: i = MI(mov8_mc, d, src.constant); break;
						default: invalid_code_path();
					}
					break;
			}
		};

		switch (i.kind) {
			using enum InstructionKind;
			case lea: {
				REDECLARE_REF(i, i.lea);
				if (i.s.base == locals ||
					i.s.base == constants ||
					i.s.base == rwdata ||
					i.s.base == zeros ||
					i.s.base == Register::instructions)
				{
					r(i.d).kind = address;
					r(i.d).address = i.s;
				} else {
					r(i.d).kind = unknown;
				}
				break;
			}
			case mov_rc: {
				REDECLARE_REF(i, i.mov_rc);
				r(i.d).kind = constant;
				r(i.d).constant = i.s;
				break;
			}
			case mov_rr: {
				auto &dst = r(i.mov_rr.d);
				auto &src = r(i.mov_rr.s);
				switch (src.kind) {
					case unknown:
						dst.kind = unknown;
						break;
					case constant:
						i = MI(mov_rc, i.mov_rr.d, src.constant);
						break;
					case address:
						i = MI(lea, i.mov_rr.d, src.address);
						break;
				}
				break;
			}
			case mov_re: {
				REDECLARE_REF(i, i.mov_re);
				r(i.d).kind = unknown;
				break;
			}
			case mov1_mr: {
				if (i.mov1_mr.d.base == locals) {
					assert(i.mov1_mr.d.r1_scale_index == 0);
					assert(i.mov1_mr.d.r2_scale == 0);
				}
				auto &src = r(i.mov1_mr.s);
				switch (src.kind) {
					case unknown:
					case address:
						if (i.mov1_mr.d.base == locals)
							stack.u1(i.mov1_mr.d.c);
						break;
					case constant:
						if (i.mov1_mr.d.base == locals)
							stack.w1(src.constant, i.mov1_mr.d.c);
						i = MI(mov1_mc, i.mov1_mr.d, src.constant);
						break;
				}
				break;
			}
			case mov2_mr: {
				if (i.mov2_mr.d.base == locals) {
					assert(i.mov2_mr.d.r1_scale_index == 0);
					assert(i.mov2_mr.d.r2_scale == 0);
				}
				auto &src = r(i.mov2_mr.s);
				switch (src.kind) {
					case unknown:
					case address:
						if (i.mov2_mr.d.base == locals)
							stack.u2(i.mov2_mr.d.c);
						break;
					case constant:
						if (i.mov2_mr.d.base == locals)
							stack.w2(src.constant, i.mov2_mr.d.c);
						i = MI(mov2_mc, i.mov2_mr.d, src.constant);
						break;
				}
				break;
			}
			case mov4_mr: {
				if (i.mov4_mr.d.base == locals) {
					assert(i.mov4_mr.d.r1_scale_index == 0);
					assert(i.mov4_mr.d.r2_scale == 0);
				}
				auto &src = r(i.mov4_mr.s);
				switch (src.kind) {
					case unknown:
					case address:
						if (i.mov4_mr.d.base == locals)
							stack.u4(i.mov4_mr.d.c);
						break;
					case constant:
						if (i.mov4_mr.d.base == locals)
							stack.w4(src.constant, i.mov4_mr.d.c);
						i = MI(mov4_mc, i.mov4_mr.d, src.constant);
						break;
				}
				break;
			}
			case mov8_mr: {
				if (i.mov8_mr.d.base == locals) {
					assert(i.mov8_mr.d.r1_scale_index == 0);
					assert(i.mov8_mr.d.r2_scale == 0);
				}
				auto &src = r(i.mov8_mr.s);
				switch (src.kind) {
					case unknown:
					case address:
						if (i.mov8_mr.d.base == locals)
							stack.u8(i.mov8_mr.d.c);
						break;
					case constant:
						if (i.mov8_mr.d.base == locals)
							stack.w8(src.constant, i.mov8_mr.d.c);
						i = MI(mov8_mc, i.mov8_mr.d, src.constant);
						break;
				}
				break;
			}
			case mov1_rm: {
				r(i.mov1_rm.d).kind = unknown;
				if (i.mov1_rm.s.base == locals) {
					assert(i.mov1_rm.s.r1_scale_index == 0);
					assert(i.mov1_rm.s.r2_scale == 0);
					if (auto val = stack.r1(i.mov1_rm.s.c)) {
						r(i.mov1_rm.d).kind = constant;
						r(i.mov1_rm.d).constant = val.value_unchecked();
						i = MI(mov_rc, i.mov1_rm.d, (s64)val.value_unchecked());
					}
				}
				break;
			}
			case mov2_rm: {
				r(i.mov2_rm.d).kind = unknown;
				if (i.mov2_rm.s.base == locals) {
					assert(i.mov2_rm.s.r1_scale_index == 0);
					assert(i.mov2_rm.s.r2_scale == 0);
					if (auto val = stack.r2(i.mov2_rm.s.c)) {
						r(i.mov2_rm.d).kind = constant;
						r(i.mov2_rm.d).constant = val.value_unchecked();
						i = MI(mov_rc, i.mov2_rm.d, (s64)val.value_unchecked());
					}
				}
				break;
			}
			case mov4_rm: {
				r(i.mov4_rm.d).kind = unknown;
				if (i.mov4_rm.s.base == locals) {
					assert(i.mov4_rm.s.r1_scale_index == 0);
					assert(i.mov4_rm.s.r2_scale == 0);
					if (auto val = stack.r4(i.mov4_rm.s.c)) {
						r(i.mov4_rm.d).kind = constant;
						r(i.mov4_rm.d).constant = val.value_unchecked();
						i = MI(mov_rc, i.mov4_rm.d, (s64)val.value_unchecked());
					}
				}
				break;
			}
			case mov8_rm: {
				r(i.mov8_rm.d).kind = unknown;
				if (i.mov8_rm.s.base == locals) {
					assert(i.mov8_rm.s.r1_scale_index == 0);
					assert(i.mov8_rm.s.r2_scale == 0);
					if (auto val = stack.r8(i.mov8_rm.s.c)) {
						r(i.mov8_rm.d).kind = constant;
						r(i.mov8_rm.d).constant = val.value_unchecked();
						i = MI(mov_rc, i.mov8_rm.d, (s64)val.value_unchecked());
					}
				}
				break;
			}
			case mov1_mc: {
				REDECLARE_REF(i, i.mov1_mc);
				if (i.d.base == locals) {
					assert(i.d.r1_scale_index == 0);
					assert(i.d.r2_scale == 0);
					stack.w1(i.s, i.d.c);
				}
				break;
			}
			case mov2_mc: {
				REDECLARE_REF(i, i.mov2_mc);
				if (i.d.base == locals) {
					assert(i.d.r1_scale_index == 0);
					assert(i.d.r2_scale == 0);
					stack.w2(i.s, i.d.c);
				}
				break;
			}
			case mov4_mc: {
				REDECLARE_REF(i, i.mov4_mc);
				if (i.d.base == locals) {
					assert(i.d.r1_scale_index == 0);
					assert(i.d.r2_scale == 0);
					stack.w4(i.s, i.d.c);
				}
				break;
			}
			case mov8_mc: {
				REDECLARE_REF(i, i.mov8_mc);
				if (i.d.base == locals) {
					assert(i.d.r1_scale_index == 0);
					assert(i.d.r2_scale == 0);
					stack.w8(i.s, i.d.c);
				}
				break;
			}
			case add_rc: {
				auto &dst = r(i.add_rc.d);

				switch (dst.kind) {
					case unknown:
						break;
					case constant:
						dst.constant += i.add_rc.s;
						break;
					case address:
						dst.address.c += i.add_rc.s;
						break;
				}

				break;
			}
			case add_rr: {
				auto &dst = r(i.add_rr.d);
				auto &src = r(i.add_rr.s);

				if (src.kind == constant) {
					i = MI(add_rc, i.add_rr.d, src.constant);
				}

				// NOTE: Instruction may be replaced now, don't use it.

				switch (dst.kind) {
					case unknown:
						break;
					case constant:
						switch (src.kind) {
							case unknown:
								break;
							case constant:
								dst.constant += src.constant;
								break;
							case address:
								dst.kind = address;
								dst.address = src.address + dst.constant;
								break;
						}
						break;
					case address:
						switch (src.kind) {
							case unknown:
								break;
							case constant:
								dst.address.c += src.constant;
								break;
							case address:
								break;
						}
						break;
				}

				break;
			}
			case add_mc: {
				break;
			}
			case add_mr: {
				auto &src = r(i.add_mr.s);
				if (src.kind == constant) {
					i = MI(add_mc, i.add_mr.d, src.constant);
				}

				// NOTE: Instruction may be replaced now, don't use it.

				break;
			}
			case jmp_label: {
				for (auto &r : registers) {
					r.kind = unknown;
				}
				for (auto &k : stack.known) {
					k = 0;
				}
				break;
			}
			case jmp:
			case jz_cr:
			case jnz_cr: {
				break;
			}
			case movsx21_rm:
			case movsx41_rm:
			case movsx81_rm:
			case movsx42_rm:
			case movsx82_rm:
			case movsx84_rm:
			case movzx21_rm:
			case movzx41_rm:
			case movzx81_rm:
			case movzx42_rm:
			case movzx82_rm:
			case movzx84_rm:
			case movsx21_rr:
			case movsx41_rr:
			case movsx81_rr:
			case movsx42_rr:
			case movsx82_rr:
			case movsx84_rr:
			case movzx21_rr:
			case movzx41_rr:
			case movzx81_rr:
			case movzx42_rr:
			case movzx82_rr:
			case movzx84_rr:
			case add_rm:
			case sub_rc:
			case sub_rr:
			case sub_rm:
			case sub_mc:
			case sub_mr:
			case mul_rc:
			case mul_rr:
			case mul_rm:
			case mul_mc:
			case mul_mr:
			case div_rc:
			case div_rr:
			case div_rm:
			case div_mc:
			case div_mr:
			case mod_rc:
			case mod_rr:
			case mod_rm:
			case mod_mc:
			case mod_mr:
			case xor_rc:
			case xor_rr:
			case xor_rm:
			case xor_mc:
			case xor_mr:
			case and_rc:
			case and_rr:
			case and_rm:
			case and_mc:
			case and_mr:
			case or_rc:
			case or_rr:
			case or1_rm:
			case or2_rm:
			case or4_rm:
			case or8_rm:
			case or_mc:
			case or_mr:
			case shl_rc:
			case shl_rr:
			case shl_rm:
			case shl_mc:
			case shl_mr:
			case shr_rc:
			case shr_rr:
			case shr_rm:
			case shr_mc:
			case shr_mr:
			case copyf_mmc:
			case copyb_mmc:
			case copyf_mmr:
			case copyb_mmr:
			case set_mcc:
			case call_c:
			case call_r:
			case call_m:
			case xchg_rr:
			case xchg1_mr:
			case xchg2_mr:
			case xchg4_mr:
			case xchg8_mr:
			case cmpu1:
			case cmpu2:
			case cmpu4:
			case cmpu8:
			case cmps1:
			case cmps2:
			case cmps4:
			case cmps8:
			case not_r:
			case not_m:
			case negi_r:
			case negi8_m:
			case negi16_m:
			case negi32_m:
			case negi64_m:
			case push_c:
			case push_r:
			case push_f:
			case push_m:
			case pop_r:
			case pop_f:
			case pop_m:
			case ret:
			case begin_lambda:
			case end_lambda:
			case cvt_f32_s32:
			case cvt_s32_f32:
			case cvt_f64_s64:
			case cvt_s64_f64:
			case mov_fr:
			case mov_rf:
			case mov1_xm:
			case mov2_xm:
			case mov4_xm:
			case mov8_xm:
			case add_ff:
			case mul_ff:
			case sub_ff:
			case div_ff:
			case xor_ff:
			case tobool_r:
			case toboolnot_r:
			case prepare_stack:
				not_implemented();
				break;
			case noop:
			case debug_break:
			case debug_line:
			case debug_start_lambda:
				break;
		}
	}
}
#endif

void BytecodeBuilder::append(AstLambda *lambda) {
	assert(!lambda->is_macro);

	FrameBuilder frame;
	frame.init();
	defer { frame.free(); };

	// ls.stack_state.init(get_size(lambda->return_parameter->type));

	frame.current_node = lambda;

	auto first_instruction = count_of(builder);

	assert(lambda->location_in_bytecode == -1);

	lambda->location_in_bytecode = first_instruction;
	if (lambda->definition)
		lambda->definition->offset = first_instruction;


	if (lambda->is_poly) {
		for (auto hardened : lambda->cached_instantiations) {
			append(hardened.lambda);
		}
		return;
	}

	lambda->return_parameter->offset = 0;
	assert(lambda->return_parameter->container_node);
	assert(lambda->return_parameter->container_node->kind == Ast_Lambda);
	assert(get_definition_origin(lambda->return_parameter) == DefinitionOrigin::return_parameter);

	// scoped_replace(current_frame, &frame);

	scoped_replace(lambda, lambda);

	auto return_value_size = ceil(get_size(lambda->return_parameter->type), compiler->stack_word_size);

	//if (lambda->definition->name == u8"print_string"s)
	//	debug_break();


	if (lambda->definition) {
		frame.push_comment(format(u8"lambda {} {}", lambda->definition->name, lambda->location));
	} else {
		frame.push_comment(format(u8"lambda {} {}", where(lambda->location.data), lambda->location));
	}
	for (auto param : lambda->parameters) {
		frame.push_comment(format("{} (__int64*)(rbp+{}),{}"str, param->name, compiler->stack_word_size + lambda->parameters_size - param->offset, ceil(get_size(param->type), 8ll)/8ll));
	}


#define BI(_kind, ...) \
frame.add_instruction(MI(_kind, __VA_ARGS__))

	BI(jmp_label);

	BI(begin_lambda, lambda);

	if (return_value_size) {
		frame.with_definition_address_of(lambda->return_parameter, [&](Address address) {
			frame.push_comment(u8"zero out the return value"s);
			frame.append_memory_set(address, 0, return_value_size);
		});
	}

	frame.with_definition_address_of(lambda->return_parameter, [&](Address address) {
		frame.append(lambda->body, address);
	});

	// NOTE: Because lambda->body is not necessarily a block, temporary size might not be set, because it is updated per block.
	frame.temporary_size = max(frame.temporary_size, frame.temporary_cursor);

	//frame.append(lambda->body_scope);

	// if (types_match(lambda->body->type, lambda->return_parameter->type)) {
	// 	frame.with_definition_address_of(lambda->return_parameter, [&](Address address) {
	// 		frame.append(lambda->body, address);
	// 	});
	// } else {
	// 	frame.append(lambda->body);
	// }

	// if (compiler->optimize) {
	// 	// TODO: skip iterations if nothing was changed.
	// 	for (umm i = 0; i < compiler->optimization_pass_count; ++i) {
	// 		optimize(instructions);
	// 	}
	// }

	if (lambda->print_bytecode) {
		print_bytecode(with(temporary_allocator, to_list(frame.instructions)));
	}


	auto return_location = count_of(frame.instructions);
	BI(jmp_label);
	BI(end_lambda, lambda);

	for (auto i : lambda->return_jumps) {
		auto offset = return_location - i.index;
		if (offset == 1) {
			i.jmp->kind = InstructionKind::noop;
		} else {
			i.jmp->jmp.offset = offset;
		}
	}

	add_steal(&builder, &frame.instructions);

	instructions_that_reference_lambdas.add(frame.instructions_that_reference_lambdas);


	lambda->used_registers = frame.used_registers;
	lambda->temporary_size = frame.temporary_size;
	lambda->max_stack_space_used_for_call = frame.max_stack_space_used_for_call;
}

Bytecode build_bytecode() {
	timed_function(compiler->profiler);

	assert(compiler->general_purpose_register_count != 0);

	Bytecode result;

	auto _builder = new BytecodeBuilder();
	defer { delete _builder; };

	auto &builder = *_builder;

	for (auto lambda : compiler->lambdas_with_body) {
		if (compiler->enable_dce && lambda->definition && !lambda->definition->is_referenced) {
			// immediate_info(lambda->location, "K.O.");
			continue;
		}
		builder.append(lambda);
	}

	// for_each(global_scope.statements, [&](auto statement) {
	// 	if (statement->kind == Ast_ExpressionStatement && ((AstExpressionStatement *)statement)->expression->kind == Ast_Lambda) {
	// 		auto lambda = (AstLambda *)((AstExpressionStatement *)statement)->expression;
	// 		assert(lambda->is_evaluated_at_compile_time);
	// 	} else {
	// 		builder.append(statement);
	// 	}
	// });

	//print_bytecode(builder.builder);

	for (auto i : builder.instructions_that_reference_lambdas) {
		assert(i.lambda->location_in_bytecode != -1);
		switch (i.instruction->kind) {
			case InstructionKind::call_c: {
				i.instruction->call_c.constant = i.lambda->location_in_bytecode;
				break;
			}
			case InstructionKind::lea: {
				i.instruction->lea.s.c = i.lambda->location_in_bytecode;
				break;
			}
		}
		int cpp_debugging_is_awesome = 5;
	}

	result.instructions = to_list(builder.builder);

	for (auto i : result.instructions) {
		switch (i.kind) {
			using enum InstructionKind;
			case call_c: assert(i.call_c.constant != -1); break;
		}
	}

	//for (auto lambda : builder.consteval_lambdas) {
	//	run_bytecode(compiler, result.instructions, lambda, result.extern_libraries);
	//}

	return result;
}
