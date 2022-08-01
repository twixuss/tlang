#include "bytecode.h"
#include "ast.h"
#include "interpret.h"
// #include "x86_64.h"

// I don't know how optimizations will work with loading lambda parameters' addresses...
// TODO: Figure this out.
#define OPTIMIZE_BYTECODE 0

using enum Register;
using enum XRegister;

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

struct FrameBuilder {
	InstructionList instructions;

	RegisterSet available_registers;
	RegisterSet initial_available_registers;
	RegisterSet used_registers;

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
		for (u32 i = (u32)allocatable_register_start; i < (u32)allocatable_register_end; ++i) {
			available_registers.set(i);
		}
		initial_available_registers = available_registers;
	}
	void free() {}

	Optional<Register> allocate_register() {
		auto r = available_registers.pop();
		if (r.has_value())
			used_registers.set(r.value());
		return r.map<Register>();
	}
	void free_register(Register reg) {
		assert(!available_registers.get((umm)reg));
		available_registers.set((umm)reg);
	}

	Address allocate_temporary_space(s64 size) {
		size = ceil(size, 16ll);
		defer { temporary_cursor += size; };
		return temporary + temporary_cursor;
	}

	RegisterOrAddress allocate_register_or_temporary(u64 size) {
		if (size <= compiler.stack_word_size) {
			auto reg = allocate_register();
			if (reg.has_value())
				return reg.value_unchecked();
		}

		return allocate_temporary_space(size);
	}

	Instruction *add_instruction(Instruction);

	void append(AstExpression     *, RegisterOrAddress);
	void append(AstCall           *, RegisterOrAddress);
	void append(AstIdentifier     *, RegisterOrAddress);
	void append(AstLiteral        *, RegisterOrAddress);
	void append(AstBinaryOperator *, RegisterOrAddress);
	void append(AstUnaryOperator  *, RegisterOrAddress);
	void append(AstSubscript      *, RegisterOrAddress);
	void append(AstLambda         *, RegisterOrAddress);
	void append(AstIfx            *, RegisterOrAddress);
	void append(AstPack           *, RegisterOrAddress);

	[[nodiscard]] RegisterOrAddress append(AstExpression *expression) {
		auto destination = allocate_register_or_temporary(get_size(expression->type));
		append(expression, destination);
		return destination;
	}

	void append(AstStatement           *);
	void append(AstDefinition          *);
	void append(AstReturn              *);
	void append(AstIf                  *);
	void append(AstExpressionStatement *);
	void append(AstWhile               *);
	void append(AstBlock               *);
	void append(AstAssert              *);
	void append(AstLoopControl         *);
	void append(AstMatch               *);

	void append(Scope *scope);

	void load_address_of(AstExpression *expression, RegisterOrAddress destination);
	void load_address_of(AstDefinition *definition, RegisterOrAddress destination);

	RegisterOrAddress load_address_of(AstExpression *expression) { auto destination = allocate_register_or_temporary(compiler.stack_word_size); load_address_of(expression, destination); return destination; }
	RegisterOrAddress load_address_of(AstDefinition *definition) { auto destination = allocate_register_or_temporary(compiler.stack_word_size); load_address_of(definition, destination); return destination; }

	DefinitionAddress get_address_of(AstDefinition *definition);

	void append_memory_set(InstructionList &list, Address d, s64 s, s64 size);
	void append_memory_set(Address d, s64 s, s64 size);
	void append_struct_initializer(AstStruct *Struct, SmallList<AstExpression *> values, RegisterOrAddress destination);

	void mov_mc(s64 size, Address dst, s64 src);
	void mov_mr(s64 size, Address dst, Register src);
	void mov_rm(s64 size, Register dst, Address src);

	inline void mov_mc(Address dst, s64 src) { return mov_mc(compiler.stack_word_size, dst, src); }
	inline void mov_mr(Address dst, Register src) { return mov_mr(compiler.stack_word_size, dst, src); }
	inline void mov_rm(Register dst, Address src) { return mov_rm(compiler.stack_word_size, dst, src); }

	void copy(RegisterOrAddress dst, RegisterOrAddress src, s64 size, bool reverse);

	void with_definition_address_of(AstDefinition *definition, auto &&fn);


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

#define MI(_kind, ...) (Instruction{._kind={__VA_ARGS__}, .kind = InstructionKind::_kind, .line=(u64)__LINE__,})

#else

#define set_comment(...)
#define push_comment(...)

#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind}

#endif
};

struct BytecodeBuilder {
	InstructionList builder;
	//SectionBuilder constant_data_builder;
	//SectionBuilder data_builder;
	//umm zero_data_size = 0;
	AstLambda *lambda = 0;

	List<InstructionThatReferencesLambda> instructions_that_reference_lambdas;

	List<Relocation> local_relocations;
	List<Relocation> global_relocations;

	HashMap<String, s64> constant_strings;

	// FrameBuilder *current_frame = 0;

	void optimize(InstructionList &instructions);

	void propagate_known_addresses(InstructionList &instructions);

	// NOTE: this optimization assumes that registers which are set before a jump are no longer used after a jump.
	void remove_redundant_instructions(InstructionList &instructions);

	void propagate_known_values(InstructionList &instructions);

	void append(AstLambda  *);
};


#define II(kind, ...) add_instruction(MI(kind, __VA_ARGS__))
#define I(kind, ...) (&add_instruction(MI(kind, __VA_ARGS__))->kind)

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

#define LOAD_ADDRESS_INTO_REGISTER(name, source, fallback) \
	auto _reg_or_addr = load_address_of(source); \
	defer { \
		if (_reg_or_addr.is_in_register) { \
			free_register(_reg_or_addr.reg); \
		} \
	}; \
	Register name = fallback; \
	if (_reg_or_addr.is_in_register) { \
		name = _reg_or_addr.reg; \
	} else { \
		mov_rm(name, _reg_or_addr.address); \
	}

#define APPEND_INTO_REGISTER(name, source, fallback) \
	assert(get_size(source->type) <= compiler.stack_word_size); \
	auto CONCAT(_, __LINE__) = append(source); \
	defer { \
		if (CONCAT(_, __LINE__).is_in_register) { \
			free_register(CONCAT(_, __LINE__).reg); \
		} \
	}; \
	Register name = fallback; \
	if (CONCAT(_, __LINE__).is_in_register) { \
		name = CONCAT(_, __LINE__).reg; \
	} else { \
		mov_rm(get_size(source->type), name, CONCAT(_, __LINE__).address); \
	}

void FrameBuilder::mov_mr(s64 size, Address dst, Register src) {
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
void FrameBuilder::mov_mc(s64 size, Address dst, s64 src) {
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
void FrameBuilder::mov_rm(s64 size, Register dst, Address src) {
	assert(size <= 8);
	switch (size) {
		case 1: I(mov1_rm, dst, src); break;
		case 2: I(mov2_rm, dst, src); break;
		case 4: I(mov4_rm, dst, src); break;
		case 8: I(mov8_rm, dst, src); break;

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

	}
}

void FrameBuilder::copy(RegisterOrAddress dst, RegisterOrAddress src, s64 size, bool reverse) {
	if (size == 0)
		return;

	if (dst.is_in_register) {
		if (src.is_in_register) {
			I(mov_rr, dst.reg, src.reg);
		} else {
			mov_rm(size, dst.reg, src.address);
		}
	} else {
		if (src.is_in_register) {
			mov_mr(size, dst.address, src.reg);
		} else {
			REDECLARE_VAL(src, src.address);
			REDECLARE_VAL(dst, dst.address);

			StaticSet<Register, 3> set;
			set.get_or_insert(r0);
			set.get_or_insert(r1);
			set.get_or_insert(r2);

			auto remove_from_set = [&](Address address) {
				set.remove(address.base);
				if (address.r1_scale_index) set.remove(address.r1);
				if (address.r2_scale)       set.remove(address.r2);
			};

			remove_from_set(dst);
			remove_from_set(src);

			auto intermediary = set.pop().value();

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
					switch (compiler.register_size) {
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
				assert((next.s % compiler.stack_word_size) == 0);
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
				assert((next.s % compiler.stack_word_size) == 0);
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
	// this does not use compiler.stack_word_size !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

// if has enough registers, returns the value of an expression in them.
// empty if failed to allocate registers.
using ValueRegisters = StaticList<Register, 8>;

ValueRegisters value_registers(Register a) {
	ValueRegisters result;
	result.add(a);
	return result;
}

ValueRegisters value_registers(Optional<Register> a) {
	ValueRegisters result;
	if (a)
		result.add(a.value_unchecked());
	return result;
}

ValueRegisters value_registers(Register a, Register b) {
	ValueRegisters result;
	result.add(a);
	result.add(b);
	return result;
}

inline umm append(StringBuilder &b, LambdaDefinitionLocation d) {
	switch (d) {
		using enum LambdaDefinitionLocation;
		case body:			  return append(b, "body");
		case parameter:		  return append(b, "parameter");
		case return_parameter: return append(b, "return_parameter");
	}
	return 0;
}

void check_destination(RegisterOrAddress destination) {
	if (destination.is_in_register) {
		assert(destination.reg != r0);
		assert(destination.reg != r1);
		assert(destination.reg != r2);
	} else {
		assert(destination.address.base != r0);
		assert(destination.address.r1_scale_index == 0);
		assert(destination.address.r2_scale == 0);
	}
}

enum class DefinitionOrigin {
	unknown,
	constants,
	rwdata,
	zeros,
	return_parameter,
	parameter,
	local,
};

DefinitionOrigin get_definition_origin(AstDefinition *definition) {
	if (definition->container_node && !definition->is_constant) {
		switch (definition->container_node->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter:
					case LambdaDefinitionLocation::body: {

						auto parent_lambda = (AstLambda *)definition->container_node;

						switch (definition->definition_location) {
							case LambdaDefinitionLocation::return_parameter:
								return DefinitionOrigin::return_parameter;

							case LambdaDefinitionLocation::parameter:
								return DefinitionOrigin::parameter;

							case LambdaDefinitionLocation::body:
								return DefinitionOrigin::local;
							default: invalid_code_path();
						}
						break;
					}
					default: invalid_code_path();
				}
			}
			default: invalid_code_path();
		}
	} else {
		if (definition->is_constant) {
			return DefinitionOrigin::constants;
		} else {
			if (definition->expression) {
				return DefinitionOrigin::rwdata;
			} else {
				return DefinitionOrigin::zeros;
			}
		}
	}
}

Address get_known_address_of(AstDefinition *definition) {
	assert(definition->offset != -1);

	if (definition->container_node && !definition->is_constant) {
		switch (definition->container_node->kind) {
			case Ast_Lambda: {
				auto parent_lambda = (AstLambda *)definition->container_node;
				assert(parent_lambda->kind == Ast_Lambda);
				assert(parent_lambda->parameters_size != -1);

				switch (definition->definition_location) {
					case LambdaDefinitionLocation::return_parameter:
						return Address(return_parameters);

					case LambdaDefinitionLocation::parameter:
						return parameters + definition->offset;

					case LambdaDefinitionLocation::body: {
						return locals + definition->offset;
					}
					default: invalid_code_path();
				}
				break;
			}
			default: invalid_code_path();
		}
	} else {
		// Global
		auto offset = definition->offset;

		if (definition->expression && is_lambda(definition->expression))
			return Register::instructions + offset;

		if (definition->is_constant)
			return constants + offset;

		if (definition->expression) {
			return rwdata + offset;
		} else {
			return zeros + offset;
		}
	}
}

void FrameBuilder::load_address_of(AstDefinition *definition, RegisterOrAddress destination) {
	push_comment(format("load address of {} (definition_location={})"str, definition->name, definition->definition_location));

	assert(definition->offset != -1);

	if (definition->container_node && !definition->is_constant) {
		switch (definition->container_node->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::body: {
						auto addr = get_known_address_of(definition);
						if (destination.is_in_register) {
							I(lea, destination.reg, addr);
						} else {
							I(lea, r0, addr);
							mov_mr(destination.address, r0);
						}
						break;
					}
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter: {
						auto addr = get_known_address_of(definition);
						if (get_size(definition->type) <= compiler.stack_word_size) {
							if (destination.is_in_register) {
								I(lea, destination.reg, addr);
							} else {
								I(lea, r0, addr);
								mov_mr(destination.address, r0);
							}
						} else {
							if (destination.is_in_register) {
								I(lea, destination.reg, addr);
								I(mov8_rm, destination.reg, Address(destination.reg));
							} else {
								I(mov8_rm, r0, addr);
								mov_mr(destination.address, r0);
							}
						}
						break;
					}
					default: invalid_code_path();
				}
				break;
			}
			default: invalid_code_path();
		}
	} else {
		auto addr = get_known_address_of(definition);
		if (destination.is_in_register) {
			I(lea, destination.reg, addr);
		} else {
			I(lea, r0, addr);
			mov_mr(destination.address, r0);
		}
	}
}
void FrameBuilder::load_address_of(AstExpression *expression, RegisterOrAddress destination) {
	check_destination(destination);

	//if (expression->location == "where.data[i + j]")
	//	debug_break();

	push_comment(format(u8"load_address_of {}", expression->location));

	switch (expression->kind) {
		case Ast_Lambda: {
		load_address_of_lambda:

			auto lambda = (AstLambda *)expression;

			if (lambda->has_body) {
				Instruction *instr = 0;
				if (destination.is_in_register) { instr = II(lea, destination.reg, Address(Register::instructions)); }
				else                            { instr = II(lea, r0, Address(Register::instructions)); mov_mr(destination.address, r0); }

				instructions_that_reference_lambdas.add({.instruction=instr, .lambda=lambda});
			} else {
				assert((s64)(s32)count_of(lambda->definition->name) == (s64)count_of(lambda->definition->name));
				if (destination.is_in_register) {
					I(mov_re, destination.reg, (String)lambda->definition->name);
				} else {
					I(mov_re, r0, (String)lambda->definition->name);
					mov_mr(destination.address, r0);
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
				if (destination.is_in_register) I(add_rc, destination.reg, offset);
				else                         I(add_mc, destination.address, offset);
			}

			break;
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;

			// TODO: Clean up this copypasta.

			if (auto span = direct_as<AstSpan>(subscript->expression->type)) {
				load_address_of(subscript->expression, destination);
				if (destination.is_in_register) {
					mov_rm(destination.reg, Address(destination.reg));
				} else {
					mov_rm(r0, destination.address);
					mov_rm(r0, Address(r0));
					mov_mr(destination.address, r0);
				}

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register)
					I(add_rr, destination.reg, index);
				else
					I(add_mr, destination.address, index);
			} else if (auto pointer_type = as_pointer(subscript->expression->type)) {
				append(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register)
					I(add_rr, destination.reg, index);
				else
					I(add_mr, destination.address, index);
			} else if (types_match(subscript->expression->type, builtin_string)) {
				assert(get_size(subscript->index_expression->type) <= compiler.stack_word_size);

				auto _reg_or_addr1 = load_address_of(subscript->expression);
				auto _reg_or_addr2 = append(subscript->index_expression);
				defer {
					if (_reg_or_addr1.is_in_register) free_register(_reg_or_addr1.reg);
					if (_reg_or_addr2.is_in_register) free_register(_reg_or_addr2.reg);
				};

				Register string = r0;
				Register index = r1;

				if (_reg_or_addr1.is_in_register) string = _reg_or_addr1.reg;
				else                           mov_rm(string, _reg_or_addr1.address);

				if (_reg_or_addr2.is_in_register) index = _reg_or_addr2.reg;
				else                           mov_rm(index, _reg_or_addr2.address);

				auto count = r2;

				mov_rm(count, string + compiler.stack_word_size);
				mov_rm(string, Address(string));

				switch (compiler.stack_word_size) {
					case 4: I(cmpflag4, index, count); break;
					case 8: I(cmpflag8, index, count); break;
					default: invalid_code_path();
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
					mov_mr(destination.address, index);
			} else {
				assert(direct_as<AstSubscript>(subscript->expression->type));
				load_address_of(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_in_register)
					I(add_rr, destination.reg, index);
				else
					I(add_mr, destination.address, index);
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
			invalid_code_path("attempt to load address of unknown kind of expression");
	}
}

DefinitionAddress FrameBuilder::get_address_of(AstDefinition *definition) {
	push_comment(format("load address of {} (definition_location={})"str, definition->name, definition->definition_location));

	if (definition->container_node && !definition->is_constant) {
		switch (definition->container_node->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::body: {
						return {
							.is_known = true,
							.known_address = get_known_address_of(definition),
						};
					}
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter: {
						auto addr = get_known_address_of(definition);
						if (get_size(definition->type) <= compiler.stack_word_size) {
							return {
								.is_known = true,
								.known_address = addr,
							};
						} else {
							auto destination = allocate_register_or_temporary(compiler.stack_word_size);
							if (destination.is_in_register) {
								I(lea, destination.reg, addr);
								mov_rm(destination.reg, Address(destination.reg));
							} else {
								mov_rm(r0, addr);
								mov_mr(destination.address, r0);
							}
							return {
								.is_known = false,
								.computed_address = destination,
							};
						}
					}
					default: invalid_code_path();
				}
				break;
			}
			default: invalid_code_path();
		}
	} else {
		return {
			.is_known = true,
			.known_address = get_known_address_of(definition),
		};
	}
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
			if (compiler.stack_word_size == 8) {
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
		mov_rm(destination.reg, tmp);
	}
}

void FrameBuilder::append(Scope *scope) {
	scoped_replace(current_scope, scope);

	temporary_size = max(temporary_size, temporary_cursor);
	auto start_temporary_cursor = temporary_cursor;
	defer {
		temporary_size = max(temporary_size, temporary_cursor);
		temporary_cursor = start_temporary_cursor;
	};

	for (auto statement : scope->statements) {
		// if (statement->uid() == 1826)
		// 	debug_break();
		//if (statement->location == "glGenBuffers = @ wglGetProcAddress(\"glGenBuffers\\0\".data)")
		//	debug_break();

		push_comment((Span<utf8>)format("==== {}: {} ====", where(statement->location.data), statement->location));

		// temporary_cursor = 0;

		assert(available_registers == initial_available_registers);
		append(statement);
		assert(available_registers == initial_available_registers);
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

void FrameBuilder::append(AstStatement *statement) {
	scoped_replace(current_node, statement);
	switch (statement->kind) {
		case Ast_Definition:          return append((AstDefinition          *)statement);
		case Ast_Return:              return append((AstReturn              *)statement);
		case Ast_If:                  return append((AstIf                  *)statement);
		case Ast_ExpressionStatement: return append((AstExpressionStatement *)statement);
		case Ast_While:               return append((AstWhile               *)statement);
		case Ast_Block:               return append((AstBlock               *)statement);
		case Ast_LoopControl:         return append((AstLoopControl         *)statement);
		case Ast_Match:               return append((AstMatch               *)statement);
		case Ast_OperatorDefinition:
			not_implemented();
			//append(((AstOperatorDefinition*)statement)->lambda, false, r0);
			return;
		case Ast_Defer: {
			// defer is appended later, after appending a block.
			auto Defer = (AstDefer *)statement;
			Defer->scope->parent->bytecode_defers.add(Defer);
			return;
		}
		case Ast_Assert:
			return append((AstAssert *)statement);
		case Ast_Print:
		case Ast_Import:
		case Ast_Parse:
		case Ast_Test:
		case Ast_Using:
			return;
		default: invalid_code_path();
	}
}
void FrameBuilder::append(AstDefinition *definition) {
	assert(definition->type);

	if (definition->container_node && !definition->is_constant) {
		switch (definition->container_node->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::body: {
						assert(definition->offset != -1);
						assert((definition->offset & 0xf) == 0);
						// push_comment(format("(__int64*)(rbp+{}),{}"str, definition->offset, ceil(get_size(definition->type),16ll)/16ll));

						auto addr = locals + definition->offset;

						if (definition->expression) {
							append(definition->expression, addr);
						} else {
							append_memory_set(addr, 0, get_size(definition->type));
						}
						break;
					}
					default: invalid_code_path();
				}
				break;
			}
		}
	} else {
		// Global

		// definition->expression was evaluated at typecheck time an is already in the appropriate section,
		// nothing to do here.
	}
#if 0
	//if (definition->_uid == 99)
	//	debug_break();


	// TODO: how can this happen
	if (definition->built_in) {
		invalid_code_path();
		return;
	}


	auto definition_size = get_size(definition->type);
	assert(definition_size != -1);

	// Don't do anything for constant definitions in lambdas
	if (definition->container_node && definition->container_node->kind != Ast_Struct && definition->is_constant) {
		return;
	}

	if (definition->expression && definition->expression->kind == Ast_Lambda) {
		// NOTE: no need to append lambdas here anymore. they are appended in build_bytecode
		// assert_always(append((AstLambda *)definition->expression, false).count == 0);
		return;
	}

	if (definition->expression) {
		if (definition->expression->kind == Ast_Struct) {
			auto Struct = (AstStruct *)definition->expression;
			for_each (Struct->scope->definitions, [&](auto, auto members) {
				for (auto member : members) {
					if (member->is_constant && member->expression && member->expression->kind == Ast_Lambda) {
						append(member);
					}
				}
			});
		}
	}

	if (definition->expression && is_type(definition->expression))
		return;

	if (definition->container_node) {
		if (definition->container_node->kind == Ast_Lambda) {
			auto parent_lambda = (AstLambda *)definition->container_node;
			push_comment(format(u8"definition {}", definition->name));
			assert(!definition->is_parameter);

			auto size = ceil(definition_size, compiler.stack_word_size);

			// definition->bytecode_offset = parent_lambda->offset_accumulator;
			// parent_lambda->offset_accumulator += size;

			if (definition->expression) {
				append(definition->expression, rb - definition->offset);


				//if (types_match(definition->expression->type, builtin_noinit.Struct)) {
				//	I(sub_rc, rs, size);
				//} else {
				//	dont_care_about_definition_spacing = true;
				//	defer { dont_care_about_definition_spacing = false; };
				//
				//	auto cursor_before = ls->stack_state.cursor;
				//	append_to_stack(definition->expression);
				//	auto cursor_after = ls->stack_state.cursor;
				//
				//	auto size_with_spacing = (cursor_after - cursor_before) * compiler.stack_word_size;
				//	auto size_diff = size_with_spacing - ceil(size, compiler.stack_word_size);
				//
				//	// account for spacing
				//	assert(size_diff == 0 || size_diff == 8);
				//	definition->bytecode_offset += size_diff;
				//	parent_lambda->offset_accumulator += size_diff;
				//}
			} else {
				push_zeros(size);
			}
		} else {
			invalid_code_path();
		}
	} else {
#if 0
		if (definition->is_constant) {
			if (definition->expression) {
				auto literal = (AstLiteral *)get_literal(definition->expression);
				if (!literal)
					literal = definition->evaluated;
				assert(literal);
				definition->bytecode_offset = append_constant_data(literal);


			} else {
				definition->bytecode_offset = constant_data_builder.allocate(get_size(definition->type));
			}
		} else {
			if (definition->expression) {
				definition->bytecode_offset = data_builder.append(value_as_bytes((s64)get_constant_integer(definition->expression).value()));
			} else {
				definition->bytecode_offset = allocate_zero_data(definition_size);
			}
		}
#endif
	}
#endif
}
void FrameBuilder::append(AstReturn *ret) {
	push_comment(u8"return"s);

	auto lambda = ret->lambda;

	if (ret->expression) {
		with_definition_address_of(ret->lambda->return_parameter, [&](Address address) {
			append(ret->expression, address);
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
void FrameBuilder::append(AstIf *If) {
#if 0
	if (If->is_constant) {
		// constant if's statements were brought outside already by the typechecker. No need to append it.
		return;
	}
#else
	if (If->is_constant) {
		// NOTE: constant if's scope is not merged into it's parent.
		auto scope = If->true_branch_was_taken ? If->true_scope : If->false_scope;
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
		APPEND_INTO_REGISTER(condition_register, If->condition, r0);
		jz = I(jz_cr, 0, condition_register);
	}

	auto true_branch_first_instruction_index = count_of(instructions);
	append(If->true_scope);

	auto jmp = I(jmp, .offset=0);
	I(jmp_label);

	auto false_branch_first_instruction_index = count_of(instructions);
	append(If->false_scope);

	auto false_end = count_of(instructions);

	I(jmp_label);

	jz->offset = false_branch_first_instruction_index - true_branch_first_instruction_index;
	jmp->offset = false_end - false_branch_first_instruction_index + 2;
}
void FrameBuilder::append(AstWhile *While) {
	auto count_before_condition = count_of(instructions);
	I(jmp_label);

	decltype(Instruction::jz_cr) *jz;
	{
		APPEND_INTO_REGISTER(condition_register, While->condition, r0);
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
void FrameBuilder::append(AstBlock *block) {
	append(block->scope);
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

	APPEND_INTO_REGISTER(condition, Assert->condition, r0);

	I(jnz_cr, 2, condition);
	push_comment(format(u8"Assertion failed: {}", Assert->condition->location));
	I(debug_break);
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
void FrameBuilder::append(AstMatch *Match) {
	auto matchable = allocate_temporary_space(get_size(Match->expression->type));
	append(Match->expression, matchable);

	struct Jump {
		Instruction *instruction;
		umm index;
	};

	List<Jump> jumps_to_cases;
	List<Jump> jumps_out;
	Jump jump_to_default;

	bool has_default_case = false;

	mov_rm(r0, matchable);
	for (auto &Case : Match->cases) {
		if (Case.expression) {
			I(mov_rc, r1, (s64)get_constant_integer(Case.expression).value());
			switch (compiler.stack_word_size) {
				case 4: I(cmpu4, r2, r0, r1, Comparison::e); break;
				case 8: I(cmpu8, r2, r0, r1, Comparison::e); break;
				default: invalid_code_path();
			}
			jumps_to_cases.add({II(jnz_cr, 0, r2), instructions.count});
		} else {
			has_default_case = true;
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

		append(Case.scope);
		jumps_out.add({II(jmp), instructions.count});
		case_index++;
	}
	I(jmp_label);

	for (auto &jump_out : jumps_out) {
		jump_out.instruction->jmp.offset = instructions.count - jump_out.index;
	}
}

void FrameBuilder::append(AstExpression *expression, RegisterOrAddress destination) {
	scoped_replace(current_node, expression);
	switch (expression->kind) {
		case Ast_Identifier:     return append((AstIdentifier     *)expression, destination);
		case Ast_Literal:        return append((AstLiteral        *)expression, destination);
		case Ast_Call:           return append((AstCall           *)expression, destination);
		case Ast_BinaryOperator: return append((AstBinaryOperator *)expression, destination);
		case Ast_UnaryOperator:  return append((AstUnaryOperator  *)expression, destination);
		case Ast_Subscript:      return append((AstSubscript      *)expression, destination);
		case Ast_Lambda:         return append((AstLambda         *)expression, destination);
		case Ast_Ifx:            return append((AstIfx            *)expression, destination);
		case Ast_Pack:           return append((AstPack           *)expression, destination);
		default: invalid_code_path();
	}
}
void FrameBuilder::append(AstBinaryOperator *bin, RegisterOrAddress destination) {
	check_destination(destination);

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

						Register member_address;
						if (is_pointer) {
							APPEND_INTO_REGISTER(_struct_address, bin->left, r0);
							member_address = _struct_address;
						} else {
							if (is_addressable(bin->left)) {
								LOAD_ADDRESS_INTO_REGISTER(_struct_address, bin->left, r0);
								member_address = _struct_address;
							} else {
								auto tmp = allocate_temporary_space(struct_size);
								append(bin->left, tmp);
								member_address = r0;
								I(lea, member_address, tmp);
							}
						}
						I(add_rc, member_address, member->offset);

						copy(destination, Address(member_address), member_size, false);
					} else {
						not_implemented();
						assert(is_sized_array(left->type));
					}

					break;
				}
				default: {
					invalid_code_path();
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
			if (destination.is_in_register) {
				append(left, destination);
				APPEND_INTO_REGISTER(rr, right, r1);

				auto lt = direct(bin->left->type);

				if (lt == builtin_f32.Struct) {
					switch (bin->operation) {
						case add:  I(add4_ff, destination.reg, rr); break;
						case sub:  I(sub4_ff, destination.reg, rr); break;
						case mul:  I(mul4_ff, destination.reg, rr); break;
						case div:  I(div4_ff, destination.reg, rr); break;
						default: invalid_code_path();
					}
				} else if (lt == builtin_f64.Struct) {
					switch (bin->operation) {
						case add:  I(add8_ff, destination.reg, rr); break;
						case sub:  I(sub8_ff, destination.reg, rr); break;
						case mul:  I(mul8_ff, destination.reg, rr); break;
						case div:  I(div8_ff, destination.reg, rr); break;
						default: invalid_code_path();
					}
				} else if (::is_integer_or_pointer(lt)) {
					if (auto pointer = as_pointer(lt)) {
						I(mul_rc, rr, get_size(pointer->expression));
					}
					switch (bin->operation) {
						case add:  I(add_rr, destination.reg, rr); break;
						case sub:  I(sub_rr, destination.reg, rr); break;
						case mul:  I(mul_rr, destination.reg, rr); break;
						case div:  if (::is_signed(lt)) I(divs_rr, destination.reg, rr); else I(divu_rr, destination.reg, rr); break;
						case mod:  if (::is_signed(lt)) I(mods_rr, destination.reg, rr); else I(modu_rr, destination.reg, rr); break;
						case bor:  I( or_rr, destination.reg, rr); break;
						case band: I(and_rr, destination.reg, rr); break;
						case bxor: I(xor_rr, destination.reg, rr); break;
						case bsr:  I(shr_rr, destination.reg, rr); break;
						case bsl:  I(shl_rr, destination.reg, rr); break;
						default: invalid_code_path();
					}
				} else {
					invalid_code_path();
				}
			} else {
				assert(get_size(left ->type) <= compiler.stack_word_size);
				assert(get_size(right->type) <= compiler.stack_word_size);
				auto left_  = append(left);
				auto right_ = append(right);
				defer {
					if (left_ .is_in_register) free_register(left_.reg);
					if (right_.is_in_register) free_register(right_.reg);
				};
				Register l = r0;
				Register r = r1;
				if (left_.is_in_register) {
					l = left_.reg;
				} else {
					mov_rm(l, left_.address);
				}
				if (right_.is_in_register) {
					r = right_.reg;
				} else {
					mov_rm(r, right_.address);
				}

				auto lt = direct(bin->left->type);

				if (lt == builtin_f32.Struct) {
					switch (bin->operation) {
						case add:  I(add4_ff, l, r); break;
						case sub:  I(sub4_ff, l, r); break;
						case mul:  I(mul4_ff, l, r); break;
						case div:  I(div4_ff, l, r); break;
						default: invalid_code_path();
					}
				} else if (lt == builtin_f64.Struct) {
					switch (bin->operation) {
						case add:  I(add8_ff, l, r); break;
						case sub:  I(sub8_ff, l, r); break;
						case mul:  I(mul8_ff, l, r); break;
						case div:  I(div8_ff, l, r); break;
						default: invalid_code_path();
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
						case bsr:  I(shr_rr, l, r); break;
						case bsl:  I(shl_rr, l, r); break;
						default: invalid_code_path();
					}
				} else {
					invalid_code_path();
				}

				mov_mr(get_size(lt), destination.address, l);
			}

			break;
		}
		case lt:
		case gt:
		case le:
		case ge:
		case eq:
		case ne: {
			if (types_match(left->type, builtin_string)) {
				auto la = append(left).ensure_address();
				auto ra = append(right).ensure_address();

				// load counts
				mov_rm(r1, la + compiler.stack_word_size);
				mov_rm(r2, ra + compiler.stack_word_size);
				switch (compiler.stack_word_size) {
					case 4: I(cmpu4, r0, r1, r2, Comparison::e); break;
					case 8: I(cmpu8, r0, r1, r2, Comparison::e); break;
					default: invalid_code_path();
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
				mov_rm(r0, la);
				mov_rm(r1, ra);
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

			assert(get_size(left->type) <= compiler.stack_word_size);
			assert(get_size(right->type) <= compiler.stack_word_size);
			auto la = append(left);
			auto ra = append(right);
			defer {
				if (la.is_in_register) free_register(la.reg);
				if (ra.is_in_register) free_register(ra.reg);
			};
			Register rl = r0;
			Register rr = r1;
			if (la.is_in_register) {
				rl = la.reg;
			} else {
				mov_rm(rl, la.address);
			}
			if (ra.is_in_register) {
				rr = ra.reg;
			} else {
				mov_rm(rr, ra.address);
			}

			auto comparison = comparison_from_binary_operation(bin->operation);

			if (destination.is_in_register) {
				if (::is_integer_internally(left->type)) {
					if (::is_signed(left->type)) {
						switch (get_size(left->type)) {
							case 1: I(cmps1, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmps2, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmps4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmps8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path();
						}
					} else {
						switch (get_size(left->type)) {
							case 1: I(cmpu1, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmpu2, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmpu4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmpu8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path();
						}
					}
				} else if (::is_float(left->type)) {
					switch (get_size(left->type)) {
						case 4: I(cmpf4, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
						case 8: I(cmpf8, .d=destination.reg, .a=rl, .b=rr, .c = comparison); break;
						default: invalid_code_path();
					}
				} else {
					not_implemented();
				}
			} else {
				if (::is_integer_internally(left->type)) {
					if (::is_signed(left->type)) {
						switch (get_size(left->type)) {
							case 1: I(cmps1, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmps2, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmps4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmps8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path();
						}
					} else {
						switch (get_size(left->type)) {
							case 1: I(cmpu1, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 2: I(cmpu2, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 4: I(cmpu4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							case 8: I(cmpu8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
							default: invalid_code_path();
						}
					}
					I(mov1_mr, destination.address, r2);
				} else if (::is_float(left->type)) {
					switch (get_size(left->type)) {
						case 4: I(cmpf4, .d=r2, .a=rl, .b=rr, .c = comparison); break;
						case 8: I(cmpf8, .d=r2, .a=rl, .b=rr, .c = comparison); break;
						default: invalid_code_path();
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
				Register dst = r0;
				if (dst_addr.is_in_register) {
					dst = dst_addr.reg;
				} else {
					mov_rm(dst, dst_addr.address);
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
			assert(get_size(bin->right->type) <= compiler.stack_word_size);

			auto _left = load_address_of(bin->left);
			auto _right = append(bin->right);
			defer {
				if (_left.is_in_register)  free_register(_left.reg);
				if (_right.is_in_register) free_register(_right.reg);
			};

			Register dst = r0;
			Register src = r1;

			if (_left.is_in_register) {
				dst = _left.reg;
			} else {
				mov_rm(dst, _left.address);
			}

			if (_right.is_in_register) {
				src = _right.reg;
			} else {
				mov_rm(get_size(bin->right->type), src, _right.address);
			}

			if (::is_integer_or_pointer(bin->type)) {
				if (auto pointer = as_pointer(bin->left->type)) {
					I(mul_rc, src, get_size(pointer->expression));
				}
				switch (bin->operation) {
					case addass:  I(add_mr, Address(dst), src); break;
					case subass:  I(sub_mr, Address(dst), src); break;
					case mulass:  I(mul_mr, Address(dst), src); break;
					case divass:  if (::is_signed(bin->left->type)) I(divs_mr, Address(dst), src); else I(divu_mr, Address(dst), src); break;
					case modass:  if (::is_signed(bin->left->type)) I(mods_mr, Address(dst), src); else I(modu_mr, Address(dst), src); break;
					case borass:  I( or_mr, Address(dst), src); break;
					case bandass: I(and_mr, Address(dst), src); break;
					case bxorass: I(xor_mr, Address(dst), src); break;
					case bslass:  I(shl_mr, Address(dst), src); break;
					case bsrass:  I(shr_mr, Address(dst), src); break;
					default: invalid_code_path();
				}
			} else if (::is_float(bin->type)) {
				auto size = get_size(bin->type);
				mov_rm(size, r2, Address(dst));

				switch (size) {
					case 4:
						switch (bin->operation) {
							case addass:  I(add4_ff, r2, src); break;
							case subass:  I(sub4_ff, r2, src); break;
							case mulass:  I(mul4_ff, r2, src); break;
							case divass:  I(div4_ff, r2, src); break;
							default: invalid_code_path();
						}
						break;
					case 8:
						switch (bin->operation) {
							case addass:  I(add8_ff, r2, src); break;
							case subass:  I(sub8_ff, r2, src); break;
							case mulass:  I(mul8_ff, r2, src); break;
							case divass:  I(div8_ff, r2, src); break;
							default: invalid_code_path();
						}
						break;
					default: invalid_code_path();
				}
				mov_mr(size, Address(dst), r2);
			} else {
				invalid_code_path();
			}

			break;
		}
		case as: {
			auto cast = bin;

			auto get_internal_representation = [&](AstExpression *type) -> AstExpression * {
				if (is_pointer_internally(type))
					return builtin_u64.Struct;
				else if (auto Enum = direct_as<AstEnum>(type)) {
					assert(!Enum->underlying_type, "not implemented");
					return builtin_s64.Struct;
				} else
					return direct(type);
				return 0;
			};

			AstExpression *from = get_internal_representation(cast->left->type);
			AstExpression *to   = get_internal_representation(cast->type);

			{
				auto array = as_array(from);
				auto span = as_span(to);
				if (array && span) {
					assert(!destination.is_in_register);
					assert(compiler.stack_word_size == 8);

					if (is_addressable(left)) {
						load_address_of(left, destination);
					} else {
						auto size = ceil(get_size(array), compiler.stack_word_size);
						auto tmp = allocate_temporary_space(size);

						append(left, tmp);

						I(lea, r0, tmp);
						I(mov8_mr, destination.address, r0);
					}
					I(mov8_mc, destination.address+8, (s64)get_constant_integer(array->index_expression).value());
					return;
				}
			}


			// TODO: FIXME: HACK:
			// extremely dumb way to access data and count members of span
			if (auto span = as_span(from)) {
				auto from_value = append(cast->left);

				assert(!from_value.is_in_register, "not implemented");

				if (::is_integer(bin->right)) {
					auto count_addr = from_value.address + compiler.stack_word_size;
					if (destination.is_in_register) {
						I(mov8_rm, destination.reg, count_addr);
					} else {
						I(mov8_rm, r0, count_addr);
						I(mov8_mr, destination.address, r0);
					}
				} else if (::is_pointer(bin->right)) {
					auto data_addr = from_value.address;
					if (destination.is_in_register) {
						I(mov8_rm, destination.reg, data_addr);
					} else {
						I(mov8_rm, r0, data_addr);
						I(mov8_mr, destination.address, r0);
					}
				} else {
					invalid_code_path();
				}
				return;
			}


			// Integer builderersions:
			// If destination is bigger than source:
			//    extend the size depending on the signedness of source operand (sign extend for signed, zero extend for unsigned).
			// Otherwise this is a noop.

			if (::is_integer(from) && ::is_integer(to)) {
				//auto uncasted = destination;
				//append(cast->left, destination);
				auto uncasted = append(cast->left);
				defer { if (uncasted.is_in_register) free_register(uncasted.reg); };

				bool extended = false;

#define C(z,d,s) \
	extended = true; \
	if (destination.is_in_register) { \
		if (uncasted.is_in_register) I(mov##z##x##d##s##_rr, destination.reg, uncasted.reg); \
		else                      I(mov##z##x##d##s##_rm, destination.reg, uncasted.address); \
	} else { \
		if (uncasted.is_in_register) I(mov##z##x##d##s##_rr, r0, uncasted.reg); /* NOTE: movsx/movzx into a memory is not a thing! */ \
		else                      I(mov##z##x##d##s##_rm, r0, uncasted.address); \
		I(mov##d##_mr, destination.address, r0); \
	}

				if (false) {
				} else if (from == builtin_u8.Struct) {
					if (false) {}
					else if (to == builtin_u16.Struct) { C(z,2,1); }
					else if (to == builtin_u32.Struct) { C(z,4,1); }
					else if (to == builtin_u64.Struct) { C(z,8,1); }
					else if (to == builtin_s16.Struct) { C(z,2,1); }
					else if (to == builtin_s32.Struct) { C(z,4,1); }
					else if (to == builtin_s64.Struct) { C(z,8,1); }
				} else if (from == builtin_u16.Struct) {
					if (false) {}
					else if (to == builtin_u32.Struct) { C(z,4,2); }
					else if (to == builtin_u64.Struct) { C(z,8,2); }
					else if (to == builtin_s32.Struct) { C(z,4,2); }
					else if (to == builtin_s64.Struct) { C(z,8,2); }
				} else if (from == builtin_u32.Struct) {
					if (false) {}
					else if (to == builtin_u64.Struct) { C(z,8,4); }
					else if (to == builtin_s64.Struct) { C(z,8,4); }
				} else if (from == builtin_s8.Struct) {
					if (false) {}
					else if (to == builtin_u16.Struct) { C(s,2,1); }
					else if (to == builtin_u32.Struct) { C(s,4,1); }
					else if (to == builtin_u64.Struct) { C(s,8,1); }
					else if (to == builtin_s16.Struct) { C(s,2,1); }
					else if (to == builtin_s32.Struct) { C(s,4,1); }
					else if (to == builtin_s64.Struct) { C(s,8,1); }
				} else if (from == builtin_s16.Struct) {
					if (false) {}
					else if (to == builtin_u32.Struct) { C(s,4,2); }
					else if (to == builtin_u64.Struct) { C(s,8,2); }
					else if (to == builtin_s32.Struct) { C(s,4,2); }
					else if (to == builtin_s64.Struct) { C(s,8,2); }
				} else if (from == builtin_s32.Struct) {
					if (false) {}
					else if (to == builtin_u64.Struct) { C(s,8,4); }
					else if (to == builtin_s64.Struct) { C(s,8,4); }
				}
#undef C
				if (!extended) {
					copy(destination, uncasted, get_size(to), false);
				}
				return;
			}
			if (::is_integer(from) && ::is_float(to)) {
				// FIXME: cast->left may be bigger than destination!!!
				append(cast->left, destination);

				if (destination.is_in_register) {
					if (false) {
					} else if (from == builtin_s32.Struct) {
						if (false) {}
						else if (to == builtin_f32.Struct) { I(cvt_s32_f32, destination.reg); }
						else { invalid_code_path(); }
					} else if (from == builtin_s64.Struct) {
						if (false) {}
						else if (to == builtin_f64.Struct) { I(cvt_s64_f64, destination.reg); }
						else { invalid_code_path(); }
					} else {
						invalid_code_path();
					}
				} else {
					if (false) {
					} else if (from == builtin_s32.Struct) {
						I(mov4_rm, r0, destination.address);
						if (false) {}
						else if (to == builtin_f32.Struct) { I(cvt_s32_f32, r0); }
						else { invalid_code_path(); }
						I(mov4_mr, destination.address, r0);
					} else if (from == builtin_s64.Struct) {
						I(mov8_rm, r0, destination.address);
						if (false) {}
						else if (to == builtin_f64.Struct) { I(cvt_s64_f64, r0); }
						else { invalid_code_path(); }
						I(mov8_mr, destination.address, r0);
					} else {
						invalid_code_path();
					}
				}
				return;
			}

			if (::is_integer(to) && ::is_float(from)) {
				// FIXME: cast->left may be bigger than destination!!!
				append(cast->left, destination);

				if (destination.is_in_register) {
					if (false) {
					} else if (from == builtin_f64.Struct) {
						if (false) {}
						else if (to == builtin_s64.Struct) { I(cvt_f64_s64, destination.reg); }
						else { invalid_code_path(); }
					} else {
						invalid_code_path();
					}
				} else {
					if (false) {
					} else if (from == builtin_f64.Struct) {
						I(mov8_rm, r0, destination.address);
						if (false) {}
						else if (to == builtin_s64.Struct) { I(cvt_f64_s64, r0); }
						else { invalid_code_path(); }
						I(mov8_mr, destination.address, r0);
					} else {
						invalid_code_path();
					}
				}
				return;
			}

			if (::is_float(to) && ::is_float(from)) {
				// FIXME: cast->left may be bigger than destination!!!
				append(cast->left, destination);

				if (destination.is_in_register) {
					if (false) {
					} else if (from == builtin_f64.Struct) {
						if (false) {}
						else if (to == builtin_f32.Struct) { I(cvt_f64_f32, destination.reg); }
						else { invalid_code_path(); }
					} else if (from == builtin_f32.Struct) {
						if (false) {}
						else if (to == builtin_f64.Struct) { I(cvt_f32_f64, destination.reg); }
						else { invalid_code_path(); }
					} else {
						invalid_code_path();
					}
				} else {
					if (false) {
					} else if (from == builtin_f64.Struct) {
						I(mov8_rm, r0, destination.address);
						if (false) {}
						else if (to == builtin_f32.Struct) { I(cvt_f64_f32, r0); }
						else { invalid_code_path(); }
						I(mov4_mr, destination.address, r0);
					} else if (from == builtin_f32.Struct) {
						I(mov4_rm, r0, destination.address);
						if (false) {}
						else if (to == builtin_f64.Struct) { I(cvt_f32_f64, r0); }
						else { invalid_code_path(); }
						I(mov8_mr, destination.address, r0);
					} else {
						invalid_code_path();
					}
				}
				return;
			}

			if (auto option = as_option(from)) {
				if (to == builtin_bool.Struct) {
					auto tmp_size = get_size(option);
					auto tmp = allocate_temporary_space(tmp_size);
					defer { temporary_cursor -= tmp_size; };

					append(cast->left, tmp);

					if (destination.is_in_register) {
						I(mov1_rm, destination.reg, tmp + get_size(option->expression));
					} else {
						I(mov1_rm, r0, tmp + get_size(option->expression));
						I(mov1_mr, destination.address, r0);
					}

					return;
				}
			}

			if (auto option = as_option(to)) {
				assert(!destination.is_in_register, "not implemented");
				append(cast->left, destination);
				I(mov1_mc, destination.address + get_size(option->expression), 1);
				return;
			}

			invalid_code_path();
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
				I(mov1_rm, r0, destination.address);

				auto jump = I(jnz_cr, 0, destination.reg);
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
				I(mov1_rm, r0, destination.address);

				auto jump = I(jz_cr, 0, r0);
				auto skip_start = instructions.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = instructions.count;

				jump->offset = skip_end - skip_start;
			}
			break;
		}
		default:
			invalid_code_path();
	}

	return;
	not_implemented();
#if 0
	assert(!destination);
	push_comment(format(u8"binary {}"s, bin->location));

	auto left = bin->left;
	auto right = bin->right;

	using enum BinaryOperation;
	if (bin->operation == dot) {
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

					// assert(struct_size % stack_word_size == 0);
					// assert(member_size % stack_word_size == 0);


					if (is_pointer) {

						if (member->is_constant) {
							invalid_code_path("not implemented");
							I(push_c, member->bytecode_offset);
						} else {
							// The plan is simple:
							// 1. Reserve space for eventual value
							// 2. Append pointer to stack
							// 3. Append struct pointer
							// 4. Copy the member from struct to reserved space
							assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);

							push_comment(u8"1. Reserve space for eventual value"s);
							I(sub_rc, rs, ceil(member_size, compiler.stack_word_size));

							//if (member == Struct->members.back()) {
							//	I(add_rc, rs, struct_size - member_size); // just throw away rest of the struct
							//} else
							{
								/*

								Example on 64 bit architecture

								a :: struct {
									data: *void;
									count: uint;
								}
																			rs
									20      28      30      38      40      48      50
								  0 |------||------||------||------||------||------||------| ffff
											38      48      data    count   data    ????????

								*/

								push_comment(u8"2. Append destination pointer"s);
								I(push_r, rs); // destination

								push_comment(u8"3. Append pointer to the struct"s);
								append_to_stack(left);
								I(add_mc, rs, member->offset_in_struct);

								push_comment(u8"4. Copy the member"s);
								copy(member_size, true, bin->location, u8"stack"s);
							}
						}
					} else {
						if (member->is_constant) {
							invalid_code_path("not implemented");
							I(push_c, member->bytecode_offset);
						} else {
							if (false && is_addressable(bin->left)) {
								// TODO: FIXME: DEBUG: this doesn't work
								push_comment(u8"1. Reserve space for eventual value"s);
								I(sub_rc, rs, ceil(member_size, compiler.stack_word_size));

								push_comment(u8"3. Copy the member from struct to reserved space"s);

								auto member_src = load_address_of(bin->left).value_or([&]{I(pop_r, r1); return r1;});

								copy_a(rs, member_src + member->offset_in_struct, member_size, false, bin->location, u8"stack"s);
							} else {
								// The plan is simple:
								// 1. Reserve space for eventual value
								// 2. Append the struct
								// 3. Copy the member from struct to reserved space
								// 4. Remove struct from the stack
								assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);

								push_comment(u8"1. Reserve space for eventual value"s);
								I(sub_rc, rs, ceil(member_size, compiler.stack_word_size));

								push_comment(u8"2. Append the struct"s);
								append_to_stack(left);

								//if (member == Struct->members.back()) {
								//	I(add_rc, rs, struct_size - member_size); // just throw away rest of the struct
								//} else
								{
									/*

									Example on 64 bit architecture

									a :: struct {
										data: *void;
										count: uint;
									}
																				rs
										20      28      30      38      40      48      50
									  0 |------||------||------||------||------||------||------| ffff
												38      48      data    count   data    ????????

									*/

									push_comment(u8"3. Copy the member from struct to reserved space"s);
									I(push_r, rs); // destination
									I(add_mc, rs, ceil(struct_size, compiler.stack_word_size));

									I(push_r, rs); // source
									I(add_mc, rs, compiler.stack_word_size + member->offset_in_struct);

									copy(member_size, true, bin->location, u8"stack"s);

									push_comment(u8"4. Remove struct from the stack"s);
									I(add_rc, rs, ceil(struct_size, compiler.stack_word_size));
								}
							}
						}
					}
				} else {
					assert(is_sized_array(left->type));
				}

				break;
			}
			default: {
				invalid_code_path();
				break;
			}
		}
	} else if (bin->operation == as) {
		auto cast = bin;

		auto get_internal_representation = [&](AstExpression *type) -> AstExpression * {
			if (is_pointer_internally(type))
				return builtin_u64.Struct;
			else if (auto Enum = direct_as<AstEnum>(type)) {
				assert(!Enum->underlying_type, "not implemented");
				return builtin_s64.Struct;
			} else
				return direct(type);
			return 0;
		};

		AstExpression *from = get_internal_representation(cast->left->type);
		AstExpression *to = get_internal_representation(cast->type);

		{
			auto array = as_array(from);
			auto span = as_span(to);
			if (array && span) {
				I(push_c, (s64)get_constant_integer(array->index_expression).value());
				if (is_addressable(left)) {
					push_address_of(left);
				} else {
					auto size = ceil(get_size(array), compiler.stack_word_size);

					append_to_stack(left);

					auto offset = rb-temporary_cursor-size;
					copy_a(offset, rs, size, false, "cast"str, "temporary"str);

					I(add_rc, rs, size);
					I(lea, r0, offset);
					I(push_r, r0);

					temporary_cursor += size;
				}
				return {};
			}
		}


		append_to_stack(cast->left);

		{
			// TODO: FIXME: HACK:
			// extremely dumb way to access data and count members of span
			auto span = as_span(from);
			if (span) {
				if (::is_integer(to)) {
					//       count => count <- rs
					// rs -> data
					I(add_rc, rs, compiler.stack_word_size);
				} else if (::is_pointer(to)) {
					//       count => data <- rs
					// rs -> data
					I(pop_r, r0);
					I(mov8_mr, rs, r0);
				} else {
					invalid_code_path();
				}
				return {};
			}
		}


		// Here are integer cases
		//   source to    size destination operation
		// unsigned to  bigger    unsigned zero extend
		// unsigned to    same    unsigned noop
		// unsigned to smaller    unsigned noop
		// unsigned to  bigger      signed zero extend
		// unsigned to    same      signed noop
		// unsigned to smaller      signed noop
		//   signed to  bigger    unsigned sign extend
		//   signed to    same    unsigned noop
		//   signed to smaller    unsigned noop
		//   signed to  bigger      signed sign extend
		//   signed to    same      signed noop
		//   signed to smaller      signed noop

		if (false) {
		} else if (from == builtin_u8.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { I(and_mc, rs, 0xff); return {}; } // discard bits that could be garbage
			else if (to == builtin_u32.Struct) { I(and_mc, rs, 0xff); return {}; }
			else if (to == builtin_u64.Struct) { I(and_mc, rs, 0xff); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { I(and_mc, rs, 0xff); return {}; }
			else if (to == builtin_s32.Struct) { I(and_mc, rs, 0xff); return {}; }
			else if (to == builtin_s64.Struct) { I(and_mc, rs, 0xff); return {}; }
		} else if (from == builtin_u16.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == builtin_u64.Struct) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == builtin_s64.Struct) { I(and_mc, rs, 0xffff); return {}; }
		} else if (from == builtin_u32.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { return {}; }
			else if (to == builtin_u64.Struct) { I(and_mc, rs, 0xffffffff); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { return {}; }
			else if (to == builtin_s64.Struct) { I(and_mc, rs, 0xffffffff); return {}; }
		} else if (from == builtin_u64.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { return {}; }
			else if (to == builtin_u64.Struct) { return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { return {}; }
			else if (to == builtin_s64.Struct) { return {}; }
		} else if (from == builtin_s8.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); return {}; } // discard bits that could be garbage
			else if (to == builtin_u32.Struct) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == builtin_u64.Struct) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); return {}; }
			else if (to == builtin_s32.Struct) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == builtin_s64.Struct) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
		} else if (from == builtin_s16.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == builtin_u64.Struct) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == builtin_s64.Struct) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
		} else if (from == builtin_s32.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { return {}; }
			else if (to == builtin_u64.Struct) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { return {}; }
			else if (to == builtin_s64.Struct) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == builtin_f32.Struct) { I(cvt_s32_f32); return {}; }
		} else if (from == builtin_s64.Struct) {
			if (false) { return {}; }
			else if (to == builtin_u8.Struct) { return {}; }
			else if (to == builtin_u16.Struct) { return {}; }
			else if (to == builtin_u32.Struct) { return {}; }
			else if (to == builtin_u64.Struct) { return {}; }
			else if (to == builtin_s8.Struct) { return {}; }
			else if (to == builtin_s16.Struct) { return {}; }
			else if (to == builtin_s32.Struct) { return {}; }
			else if (to == builtin_s64.Struct) { return {}; }
			else if (to == builtin_f64.Struct) { I(cvt_s64_f64); return {}; }
		} else if (from == builtin_f64.Struct) {
			if (false) { return {}; }
			else if (to == builtin_s64.Struct) { I(cvt_f64_s64); return {}; }
		}

		if (to == builtin_bool .Struct&& as_option(from)) {
			I(mov1_rm, r0, rs);
			I(add_rc, rs, get_size(from));
			I(push_r, r0);
			return {};
		}

		if (auto option = as_option(to)) {
			assert(types_match(option->expression, from));
			//     string as ?string
			//      count    count
			// rs -> data    data
			//               has_value <- rs

			I(sub_rc, rs, get_align(from));
			I(mov1_mc, rs, 1);
			return {};
		}

		invalid_code_path();
		return {};
	} else {
		switch (bin->operation) {
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
				append_to_stack(left);
				append_to_stack(right);

				auto lt = direct(bin->left->type);

				if (lt == builtin_f32.Struct) {
					I(pop_f, x1);
					I(pop_f, x0);
					switch (bin->operation) {
						case add:  I(add_f32_f32, x0, x1); break;
						case sub:  I(sub_f32_f32, x0, x1); break;
						case mul:  I(mul_f32_f32, x0, x1); break;
						case div:  I(div_f32_f32, x0, x1); break;
						// case mod:  I(mod_f64, x0, x1); break;
						default: invalid_code_path();
					}
					I(push_f, x0);
				} else if (lt == builtin_f64.Struct) {
					I(pop_f, x1);
					I(pop_f, x0);
					switch (bin->operation) {
						case add:  I(add_f64_f64, x0, x1); break;
						case sub:  I(sub_f64_f64, x0, x1); break;
						case mul:  I(mul_f64_f64, x0, x1); break;
						case div:  I(div_f64_f64, x0, x1); break;
						// case mod:  I(mod_f64, x0, x1); break;
						default: invalid_code_path();
					}
					I(push_f, x0);
				} else {
					assert(::is_integer(bin->type) || ::is_pointer(bin->type));
					I(pop_r, r0);
					switch (bin->operation) {
						case add:  I(add_mr, rs, r0); break;
						case sub:  I(sub_mr, rs, r0); break;
						case mul:  I(mul_mr, rs, r0); break;
						case div:  I(div_mr, rs, r0); break;
						case mod:  I(mod_mr, rs, r0); break;
						case bor:  I( or_mr, rs, r0); break;
						case band: I(and_mr, rs, r0); break;
						case bxor: I(xor_mr, rs, r0); break;
						case bsr:  I(shr_mr, rs, r0); break;
						case bsl:  I(shl_mr, rs, r0); break;
						default: invalid_code_path();
					}
				}
				break;
			}
			case ass: {
				auto bytes_to_write = get_size(left->type);
				assert(bytes_to_write);

				auto expr_size = get_size(right->type);
				assert(bytes_to_write == expr_size);

				auto dst_opt = load_address_of(left);

				if (dst_opt) {
					auto dst = dst_opt.value_unchecked();
					auto right_registers = append(right);
					if (right_registers.count) {
						// TODO: use these registers without pushing them to the stack
						for (auto r : reverse(right_registers)) {
							I(push_r, r);
							free_register(r);
						}
					}
					copy(dst, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					free_register(dst);

					push_comment(u8"remove right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, compiler.stack_word_size));
				} else {
					append_to_stack(right);

					 // load dest address
					switch (compiler.register_size) {
						case 8: I(mov8_rm, r0, rs + ceil(bytes_to_write, compiler.stack_word_size)); break;
						case 4: I(mov4_rm, r0, rs + ceil(bytes_to_write, compiler.stack_word_size)); break;
					}

					copy(r0, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

					push_comment(u8"remove left address and right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, compiler.stack_word_size) + compiler.stack_word_size);
				}

				break;


				// BTW this code is unreachable

				// :PUSH_ADDRESS: TODO: Replace this with load_address_of
				push_address_of(left); // destination address

				I(push_r, rs); // source address
				I(add_mc, rs, compiler.stack_word_size);

				assert(bytes_to_write);

				copy(bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write, compiler.stack_word_size));

				// Finish this thing that uses registers
#if 0
				auto destination_address = load_address_of(left); // destination address
				if (destination_address) {
					REDECLARE_VAL(destination_address, destination_address.value_unchecked());

					auto source_address = allocate_register(builder);
					if (source_address) {
						REDECLARE_VAL(source_address, source_address.value_unchecked());

						copy(destination_address, source_address, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					} else {

					}

					free_register(destination_address);
				} else {
				}

				I(push_r, rs); // source address
				I(add_mc, rs, compiler.stack_word_size);

				assert(bytes_to_write);

				copy(bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write, compiler.stack_word_size));
#endif

				break;
			}
			case lt:
			case gt:
			case le:
			case ge:
			case eq:
			case ne: {
				append_to_stack(left);
				append_to_stack(right);
				auto comparison = comparison_from_binary_operation(bin->operation);

				I(pop_r, r1); // right
				I(pop_r, r0); // left
				if (::is_signed(left->type)) {
					switch (get_size(left->type)) {
						case 1: I(cmps1, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 2: I(cmps2, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 4: I(cmps4, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 8: I(cmps8, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						default: invalid_code_path();
					}
				} else {
					switch (get_size(left->type)) {
						case 1: I(cmpu1, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 2: I(cmpu2, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 4: I(cmpu4, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						case 8: I(cmpu8, .d=r2, .a=r0, .b=r1, .c = comparison); break;
						default: invalid_code_path();
					}
				}
				I(push_r, r2);
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
				append_to_stack(right);

				auto destination_address_opt = load_address_of(left);
				Register destination_address = r0;
				if (destination_address_opt) {
					destination_address = destination_address_opt.value_unchecked();
				} else {
					I(pop_r, destination_address);
				}

				I(pop_r, r1); // value

				switch (bin->operation) {
					case addass:  I(add_mr, destination_address, r1); break;
					case subass:  I(sub_mr, destination_address, r1); break;
					case mulass:  I(mul_mr, destination_address, r1); break;
					case divass:  I(div_mr, destination_address, r1); break;
					case modass:  I(mod_mr, destination_address, r1); break;
					case borass:  I( or_mr, destination_address, r1); break;
					case bandass: I(and_mr, destination_address, r1); break;
					case bxorass: I(xor_mr, destination_address, r1); break;
					case bslass:  I(shr_mr, destination_address, r1); break;
					case bsrass:  I(shl_mr, destination_address, r1); break;
					default: {
						invalid_code_path();
						break;
					}
				}

				break;
			}
			default: {
				invalid_code_path();
				break;
			}
		}
		return {};
	}
	return {};
#endif
}
void FrameBuilder::append(AstIdentifier *identifier, RegisterOrAddress destination) {
	check_destination(destination);

	auto definition = identifier->definition();

	if (definition->is_constant && definition->expression) {
		if (auto lambda = get_lambda(definition->expression)) {
			if (lambda->has_body) {
				assert(definition->offset != -1);
				if (destination.is_in_register) {
					I(lea, destination.reg, Register::instructions + definition->offset);
				} else {
					I(lea, r0, Register::instructions + definition->offset);
					mov_mr(destination.address, r0);
				}
			} else {
				if (destination.is_in_register) {
					I(mov_re, destination.reg, definition->name);
				} else {
					I(mov_re, r0, definition->name);
					mov_mr(destination.address, r0);
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
		Register definition_address_register = r0;
		if (computed_address.is_in_register) {
			definition_address_register = computed_address.reg;
		} else {
			mov_rm(compiler.stack_word_size, definition_address_register, computed_address.address);
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

	auto word_size = compiler.stack_word_size;

	check_destination(destination);

	push_comment(format(u8"call {}", call->callable->location));

	if (call->lambda_type) {
		auto lambda = call->lambda_type->lambda;
		bool is_member = lambda->is_member;

		// each argument's size is not more than compiler.stack_word_size.
		// bigger arguments are passed as pointers.
		s64 parameters_bytes = (tl::count(lambda->parameters, [](auto param){return !param->is_constant;}) + is_member) * word_size;
		s32 return_parameter_bytes = word_size;

		auto return_value_size = get_size(lambda->return_parameter->type);

		auto stack_space_used_for_call = parameters_bytes + return_parameter_bytes;
		if (lambda->convention == CallingConvention::stdcall)
			stack_space_used_for_call += 32;
		max_stack_space_used_for_call = max(max_stack_space_used_for_call, ceil(stack_space_used_for_call, 16ll));

		auto return_value_address = rs + parameters_bytes;

		auto append_arguments = [&] {
			// TODO: implement for 32-bit
			assert(word_size == 8);

			// Append all arguments to stack
			// if (is_member) {
			// 	assert(call->callable->kind == Ast_BinaryOperator);
			// 	auto bin = (AstBinaryOperator *)call->callable;
			// 	assert(bin->operation == BinaryOperation::dot);
			// 	assert(is_addressable(bin->left));
			// 	push_address_of(bin->left);
			// }


			// We need to first append all the arguments into temporary space, then put them on the stack, because
			// nested function calls can overwrite the arguments.

			auto args_tmp = allocate_temporary_space(parameters_bytes);

			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto arg = call->sorted_arguments[i];

				auto arg_addr = args_tmp + (call->sorted_arguments.count-1-i)*8;

				auto size = ceil(get_size(arg->type), word_size);
				if (size > word_size) {
					// Put argument into temporary space and pass a pointer to it
					auto tmp = allocate_temporary_space(size);
					append(arg, tmp);

					I(lea, r0, tmp);
					I(mov8_mr, arg_addr, r0);
				} else {
					// leave small argument on the stack.
					append(arg, arg_addr);
				}
			}

			if (return_value_size > word_size) {
				I(lea, r0, destination.address);
				I(mov8_mr, return_value_address, r0);
			}

			// TODO:
			// Get rid of extra stack manipulations by just pushing each argument to the stack with moving stack pointer.


			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto src = args_tmp + (call->sorted_arguments.count-1-i)*word_size;
				auto dst = rs + (call->sorted_arguments.count-1-i)*word_size;
				I(mov8_rm, r0, src);
				I(mov8_mr, dst, r0);
			}
		};

		if (lambda->is_intrinsic) {
			append_arguments();

			auto name = lambda->definition->name;
			if (name == "debug_break") {
				I(debug_break);
			} else if (name == "memcpy") {
				auto rdst = r0;
				auto rsrc = r1;
				auto rsize = r2;
				I(mov8_rm, rsize, Address(rs));
				I(mov8_rm, rsrc, rs + 8);
				I(mov8_rm, rdst, rs + 16);
				I(copyf_mmr, Address(rdst), Address(rsrc), rsize);
			} else if (name == "debug_print_int") {
				I(mov8_rm, r0, Address(rs));
				I(debug_print_int, r0);
			} else if (name == "round") {
				assert(types_match(call->sorted_arguments[0]->type, builtin_f64));
				I(mov8_rm, r1, rs + 0);
				I(mov8_rm, r0, rs + 8);
				I(round8_f, r0, (RoundingMode)get_constant_integer(call->sorted_arguments[1]).value());
			} else {
				compiler.immediate_error(call->location, "Unknown intrinsic");
				exit(1);
			}
			return;
		}

		assert(word_size == 8);

		auto &arguments = call->sorted_arguments;
		bool lambda_is_constant = is_constant(call->callable);

		append_arguments();

		assert(lambda->convention != CallingConvention::none);

		if (lambda_is_constant || is_member) {
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
					LOAD_ADDRESS_INTO_REGISTER(fn_addr, call->callable, r0);
					I(call_r, fn_addr, lambda);
					break;
				}
			}
		} else {
			APPEND_INTO_REGISTER(callable_reg, call->callable, r0);
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
					fnptr = allocate_temporary_space(compiler.stack_word_size);
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
	invalid_code_path();
}
void FrameBuilder::append(AstLiteral *literal, RegisterOrAddress destination) {
	check_destination(destination);

	if (literal->literal_kind == LiteralKind::string)
		push_comment(format(u8"literal \"{}\"", escape_string(literal->string.get())));
	else
		push_comment(format(u8"literal {}", literal->location));

	assert(!types_match(literal->type, builtin_unsized_integer.Struct));
	assert(!types_match(literal->type, builtin_unsized_float.Struct));
	auto dtype = direct(literal->type);

	using enum LiteralKind;

	switch (literal->literal_kind) {
		case string: {
			assert(!destination.is_in_register);
			assert(literal->string.offset != -1);
			I(lea, r0, constants + literal->string.offset);
			mov_mr(destination.address, r0);
			mov_mc(compiler.stack_word_size, destination.address+8, (s64)literal->string.count);
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
					default: invalid_code_path();
				}
			} else {
				switch (get_size(literal->type)) {
					case 4: I(mov4_mc, destination.address, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
					case 8: I(mov8_mc, destination.address, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
					default: invalid_code_path();
				}
			}
			break;
		case boolean:
			if (destination.is_in_register) I(mov_rc, destination.reg, (s64)literal->Bool);
			else                         I(mov1_mc, destination.address, (s64)literal->Bool);
			break;
		case integer: {
			if (destination.is_in_register) {
				if (dtype == builtin_u8 .Struct|| dtype == builtin_s8.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == builtin_u16 .Struct|| dtype == builtin_s16.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == builtin_u32 .Struct|| dtype == builtin_s32.Struct)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == builtin_u64 .Struct|| dtype == builtin_s64 .Struct|| dtype == builtin_void.pointer)
					I(mov_rc, destination.reg, (s64)literal->integer);
				else if (dtype == builtin_f32.Struct) {
					auto f = (f32)(s64)literal->integer;
					I(mov_rc, destination.reg, *(s32 *)&f);
				} else if (dtype == builtin_f64.Struct) {
					auto f = (f64)(s64)literal->integer;
					I(mov_rc, destination.reg, *(s64 *)&f);
				} else if (::is_pointer(literal->type) || direct_as<AstEnum>(literal->type))
					I(mov_rc, destination.reg, (s64)literal->integer);
				else invalid_code_path();
			} else {
				if (dtype == builtin_u8 .Struct|| dtype == builtin_s8.Struct)
					I(mov1_mc, destination.address, (s64)literal->integer);
				else if (dtype == builtin_u16 .Struct|| dtype == builtin_s16.Struct)
					I(mov2_mc, destination.address, (s64)literal->integer);
				else if (dtype == builtin_u32 .Struct|| dtype == builtin_s32.Struct)
					I(mov4_mc, destination.address, (s64)literal->integer);
				else if (dtype == builtin_u64 .Struct|| dtype == builtin_s64 .Struct|| dtype == builtin_void.pointer)
					I(mov8_mc, destination.address, (s64)literal->integer);
				else if (dtype == builtin_f32.Struct) {
					auto f = (f32)(s64)literal->integer;
					I(mov4_mc, destination.address, *(s32 *)&f);
				} else if (dtype == builtin_f64.Struct) {
					auto f = (f64)(s64)literal->integer;
					I(mov8_mc, destination.address, *(s64 *)&f);
				} else if (::is_pointer(literal->type) || direct_as<AstEnum>(literal->type))
					mov_mc(destination.address, (s64)literal->integer);
				else invalid_code_path();
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
		default: invalid_code_path();
	}
}
void FrameBuilder::append(AstUnaryOperator *unop, RegisterOrAddress destination) {
	check_destination(destination);

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
						case 4:
							I(mov_fr, x0, destination.reg);
							I(mov_rc, r0, (s64)0x8000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(mov_rf, destination.reg, x0);
							break;
						case 8:
							I(mov_fr, x0, destination.reg);
							I(mov_rc, r0, (s64)0x8000'0000'0000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(mov_rf, destination.reg, x0);
							break;
						default: invalid_code_path();
					}
				} else {
					invalid_code_path();
				}
			} else {
				auto size = get_size(unop->type);
				if (::is_integer(unop->type)) {
					switch (size) {
						case 1: I(negi8_m,  destination.address); break;
						case 2: I(negi16_m, destination.address); break;
						case 4: I(negi32_m, destination.address); break;
						case 8: I(negi64_m, destination.address); break;
						default: invalid_code_path();
					}
				} else if (::is_float(unop->type)) {
					switch (size) {
						case 4: I(xor4_mc, destination.address, (s64)0x8000'0000); break;
						case 8: I(xor8_mc, destination.address, (s64)0x8000'0000'0000'0000); break;
						default: invalid_code_path();
					}
				} else {
					invalid_code_path();
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

			auto src = pointer.is_in_register ? pointer.reg : r0;
			if (!pointer.is_in_register)
				I(mov8_rm, src, pointer.address);

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
					default: invalid_code_path();
				}

				if (!destination.is_in_register)
					I(mov8_mr, destination.address, dst);
			} else {
				assert(!destination.is_in_register);
				copy(destination.address, Address(src), size, false);
			}
			break;
		}
		case bnot: {
			append(unop->expression, destination);
			if (destination.is_in_register) {
				I(not_r, destination.reg);
			} else {
				I(not_m, destination.address);
			}
			break;
		}
		case unwrap: {
			auto option = append(unop->expression);
			defer { if (option.is_in_register) free_register(option.reg); };
			assert(!option.is_in_register);
			assert(!destination.is_in_register);

			auto value_size = get_size(unop->type);

			I(mov1_rm, r0, option.address + value_size);
			I(jnz_cr, 2, r0);
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
		case internal_move_to_temporary: {
			auto size = get_size(unop->expression->type);

			auto tmp = allocate_temporary_space(size);

			append(unop->expression, tmp);

			if (destination.is_in_register) {
				I(lea, destination.reg, tmp);
			} else {
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
			invalid_code_path();
	}
}
void FrameBuilder::append(AstSubscript *subscript, RegisterOrAddress destination) {
	check_destination(destination);

	if (is_addressable(subscript->expression)) {
		LOAD_ADDRESS_INTO_REGISTER(addr, subscript, r0);

		auto size = get_size(subscript->type);

		copy(destination, Address(addr), size, false);
	} else {
		not_implemented();
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
			I(add_rc, rs, compiler.stack_word_size);
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
	check_destination(destination);
	load_address_of(lambda, destination);
}
void FrameBuilder::append(AstIfx *If, RegisterOrAddress destination) {
	check_destination(destination);

	push_comment(format(u8"ifx {}", If->location));

	APPEND_INTO_REGISTER(condition_register, If->condition, r0);

	auto jz = I(jz_cr, 0, condition_register);

	auto true_branch_first_instruction_index = count_of(instructions);
	append(If->true_expression, destination);

	auto jmp = I(jmp, .offset=0);
	I(jmp_label);

	auto false_branch_first_instruction_index = count_of(instructions);
	append(If->false_expression, destination);

	auto false_end = count_of(instructions);

	I(jmp_label);

	jz->offset = false_branch_first_instruction_index - true_branch_first_instruction_index;
	jmp->offset = false_end - false_branch_first_instruction_index + 2;
}
void FrameBuilder::append(AstPack *pack, RegisterOrAddress destination) {
	check_destination(destination);

	push_comment(format("pack {}"str, pack->location));
	assert(!destination.is_in_register);

	assert(pack->type->kind == Ast_Subscript);
	auto type = ((AstSubscript *)pack->type)->expression;

	auto elem_size = get_size(type);

	for (umm i = 0; i < pack->expressions.count; ++i) {
		auto expression = pack->expressions[i];
		append(expression, destination.address + i*elem_size);
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
	FrameBuilder frame;
	frame.init();
	defer { frame.free(); };

	// ls.stack_state.init(get_size(lambda->return_parameter->type));

	frame.current_scope = lambda->body_scope;
	frame.current_node = lambda;

	auto first_instruction = count_of(builder);

	assert(lambda->location_in_bytecode == -1);

	lambda->location_in_bytecode = first_instruction;
	if (lambda->definition)
		lambda->definition->offset = first_instruction;


	if (lambda->is_poly) {
		for (auto hardened : lambda->hardened_polys) {
			append(hardened.lambda);
		}
		return;
	}

	lambda->return_parameter->offset = 0;
	assert(lambda->return_parameter->container_node);
	assert(lambda->return_parameter->container_node->kind == Ast_Lambda);
	assert(lambda->return_parameter->definition_location == LambdaDefinitionLocation::return_parameter);

	// scoped_replace(current_frame, &frame);

	scoped_replace(lambda, lambda);

	auto return_value_size = ceil(get_size(lambda->return_parameter->type), compiler.stack_word_size);

	//if (lambda->definition->name == u8"print_string"s)
	//	debug_break();


	if (lambda->definition) {
		frame.push_comment(format(u8"lambda {} {}", lambda->definition->name, lambda->location));
	} else {
		frame.push_comment(format(u8"lambda {} {}", where(lambda->location.data), lambda->location));
	}
	for (auto param : lambda->parameters) {
		frame.push_comment(format("{} (__int64*)(rbp+{}),{}"str, param->name, compiler.stack_word_size + lambda->parameters_size - param->offset, ceil(get_size(param->type), 8ll)/8ll));
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

	frame.append(lambda->body_scope);

	// if (compiler.optimize) {
	// 	// TODO: skip iterations if nothing was changed.
	// 	for (umm i = 0; i < compiler.optimization_pass_count; ++i) {
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
	timed_function(compiler.profiler);

	assert(compiler.general_purpose_register_count != 0);

	Bytecode result;

	auto _builder = new BytecodeBuilder();
	defer { delete _builder; };

	auto &builder = *_builder;

	for (auto lambda : compiler.lambdas_with_body) {
		builder.append(lambda);
	}

	for (auto lambda : compiler.lambdas_without_body) {
		if (lambda->extern_library.data) {
			compiler.extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
		}
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
