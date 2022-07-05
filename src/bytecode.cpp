#include "bytecode.h"
#include "ast.h"
#include "x86_64.h"

// I don't know how optimizations will work with loading lambda parameters' addresses...
// TODO: Figure this out.
#define OPTIMIZE_BYTECODE 0

const Array<StaticList<u8, 2>, (u16)InstructionKind::count> address_members_offsets = [](){
	Array<StaticList<u8, 2>, (u16)InstructionKind::count> result = {};

#define m(type, name) \
	if (is_same<type, Address>) \
		result[index].add(offsetof(InstName, name));

#define e(name, ...) \
[&](){ \
	using InstName = decltype(Instruction::name); \
	constexpr u8 index = (u8)InstructionKind::name; \
	__VA_ARGS__ \
}();

	ENUMERATE_INSTRUCTIONS
#undef e
#undef m

#undef OFFSET

	return result;
}();

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
	Optional<KnownValue> state[(u32)Register::count];
	Optional<KnownValue> &operator[](Register reg) { return state[(u8)reg]; }
};

struct RegisterSet {
	u64 bits = 0;

	void push(Register r) {
		auto bit = 1 << (int)r;
		assert(!(bits & bit));
		bits |= bit;
	}
	Optional<Register> pop() {
		auto index = find_lowest_one_bit(bits);
		if (index == ~0)
			return {};
		bits &= ~(1 << index);
		return (Register)index;
	}
};

struct LambdaState {
	InstructionList body_builder;
	RegisterSet available_registers;
	RegistersState register_state = {};
	Scope *current_scope = 0;
	decltype(Instruction::push_used_registers) *push_used_registers = 0;
	decltype(Instruction::pop_used_registers) *pop_used_registers = 0;
	u64 used_registers_mask = 0;
	s64 temporary_size = 0;
	s64 temporary_cursor = 0;
	Instruction *temporary_reserver = 0;
	s64 max_stack_space_used_for_call = 0;

	void init() {
		for (int i = 5; i < min((int)Register::r8, context.general_purpose_register_count); ++i) {
			auto r = (Register)i;

			// these are used for argument swapping in stdcall
			assert(r != x86_64::to_bc_register(x86_64::Register64::r10));
			assert(r != x86_64::to_bc_register(x86_64::Register64::r11));

			available_registers.push(r);
		}
	}
	void free() {
		// `body_builder` should not be freed, builder will steal it.
	}
};

struct InstructionThatReferencesLambda {
	Instruction *instruction;
	AstLambda *lambda;
};

struct Converter {
	InstructionList builder;
	//SectionBuilder constant_data_builder;
	//SectionBuilder data_builder;
	//umm zero_data_size = 0;
	AstLambda *lambda = 0;

	List<Relocation> local_relocations;
	List<Relocation> global_relocations;

	HashMap<String, s64> constant_strings;

	ExternLibraries extern_libraries;

	LambdaState *ls;

	List<InstructionThatReferencesLambda> instructions_that_reference_lambdas;

	struct LoopJmp {
		umm index;
		Instruction *jmp;
		LoopControl control;
	};
	List<List<LoopJmp>> loop_control_stack;

	String comment;

	Optional<Register> allocate_register() {
		auto r = ls->available_registers.pop();
		if (r.has_value())
			ls->used_registers_mask |= 1 << (u64)r.value_unchecked();
		return r;
	}
	void free_register(Register reg) {
		ls->available_registers.push(reg);
	}

	Address allocate_temporary_space(s64 size) {
		size = ceil(size, 16ll);
		ls->temporary_cursor += size;
		return temporary - ls->temporary_cursor;
	}

	RegisterOrAddress get_destination(Optional<RegisterOrAddress> destination, u64 size) {
		if (destination.has_value())
			return destination.value_unchecked();

		if (size <= context.stack_word_size) {
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
	void append(AstLambda         *, bool load_address, RegisterOrAddress);
	void append(AstIfx            *, RegisterOrAddress);
	void append(AstPack           *, RegisterOrAddress);

	[[nodiscard]] RegisterOrAddress append(AstExpression *expression) {
		auto destination = get_destination({}, get_size(expression->type));
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

	void append(Scope &scope);


	void load_address_of(AstExpression *expression, RegisterOrAddress destination);
	void load_address_of(AstDefinition *definition, RegisterOrAddress destination);

	RegisterOrAddress load_address_of(AstExpression *expression) { auto destination = get_destination({}, context.stack_word_size); load_address_of(expression, destination); return destination; }
	RegisterOrAddress load_address_of(AstDefinition *definition) { auto destination = get_destination({}, context.stack_word_size); load_address_of(definition, destination); return destination; }

	void append_memory_copy(Address dst, Address src, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name);
	void append_memory_set(InstructionList &list, Address d, s64 s, s64 size, bool reverse);
	void append_memory_set(Address d, s64 s, s64 size, bool reverse);
	void append_struct_initializer(AstStruct *Struct, SmallList<AstExpression *> values, RegisterOrAddress destination);

	void optimize(InstructionList &instructions);

	void propagate_known_addresses(InstructionList &instructions);

	// NOTE: this optimization assumes that registers which are set before a jump are no longer used after a jump.
	void remove_redundant_instructions(InstructionList &instructions);

	void propagate_known_values(InstructionList &instructions);

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


#define II(kind, ...) add_instruction(MI(kind, __VA_ARGS__))
#define I(kind, ...) (&add_instruction(MI(kind, __VA_ARGS__))->kind)

#if 0
#if BYTECODE_DEBUG
void remove_last_instruction(Converter &conv) {
	auto removed = ls->body_builder.pop_back();
	auto &back = ls->body_builder.back();
	if (back.comment.data) {
		if (removed.comment.data) {
			back.comment = format(u8"{}\n{}"s, back.comment, removed.comment);
		}
	} else {
		back.comment = removed.comment;
	}
}
#else
void remove_last_instruction(Converter &conv) {
	ls->body_builder.pop_back();
}
#endif
#endif

#define LOAD_ADDRESS_INTO_REGISTER(name, source, fallback) \
	auto _reg_or_addr = load_address_of(source); \
	defer { \
		if (_reg_or_addr.is_register) { \
			free_register(_reg_or_addr.reg); \
		} \
	}; \
	Register name = fallback; \
	if (_reg_or_addr.is_register) { \
		name = _reg_or_addr.reg; \
	} else { \
		I(mov8_rm, name, _reg_or_addr.address); \
	}

#define APPEND_INTO_REGISTER(name, source, fallback) \
	assert(get_size(source->type) <= context.stack_word_size); \
	auto CONCAT(_, __LINE__) = append(source); \
	defer { \
		if (CONCAT(_, __LINE__).is_register) { \
			free_register(CONCAT(_, __LINE__).reg); \
		} \
	}; \
	Register name = fallback; \
	if (CONCAT(_, __LINE__).is_register) { \
		name = CONCAT(_, __LINE__).reg; \
	} else { \
		I(mov8_rm, name, CONCAT(_, __LINE__).address); \
	}

void print_bytecode(InstructionList &instructions);

Instruction *Converter::add_instruction(Instruction next) {
#if BYTECODE_DEBUG
	next.comment = comment;
	comment = {};
#endif

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
					remove_last_instruction(conv);
					return II(mov_rc, next.d, back.s);
				}
				case push_r: {
					REDECLARE_REF(back, back.push_r);
					remove_last_instruction(conv);
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
					remove_last_instruction(conv);
					switch (context.register_size) {
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
							remove_last_instruction(conv);
							remove_last_instruction(conv);
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
				assert((next.s % context.stack_word_size) == 0);
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
				assert((next.s % context.stack_word_size) == 0);
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

						remove_last_instruction(conv);
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
	// this does not use context.stack_word_size !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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

	return &ls->body_builder.add(next);
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

void Converter::append(AstExpression *expression, RegisterOrAddress destination) {
	switch (expression->kind) {
		case Ast_Identifier:     return append((AstIdentifier     *)expression, destination);
		case Ast_Literal:        return append((AstLiteral        *)expression, destination);
		case Ast_Call:           return append((AstCall           *)expression, destination);
		case Ast_BinaryOperator: return append((AstBinaryOperator *)expression, destination);
		case Ast_UnaryOperator:  return append((AstUnaryOperator  *)expression, destination);
		case Ast_Subscript:      return append((AstSubscript      *)expression, destination);
		case Ast_Lambda:         return append((AstLambda         *)expression, true, destination);
		case Ast_Ifx:            return append((AstIfx            *)expression, destination);
		case Ast_Pack:           return append((AstPack           *)expression, destination);
		default: invalid_code_path();
	}
}

void Converter::append(AstStatement *statement) {
	// if (ls)
	// 	I(debug_line, get_line_number(statement->location.data));
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
			append(((AstOperatorDefinition*)statement)->lambda, false, r0);
			return;
		case Ast_Defer: {
			// defer is appended later, after appending a block.
			auto Defer = (AstDefer *)statement;
			Defer->scope.parent->bytecode_defers.add(Defer);
			return;
		}
		case Ast_Assert:
			return append((AstAssert *)statement);
		case Ast_Print:
		case Ast_Import:
		case Ast_Parse:
		case Ast_Test:
			return;
		default: invalid_code_path();
	}
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
	if (destination.is_register) {
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
	if (definition->parent_lambda_or_struct && !definition->is_constant) {
		switch (definition->parent_lambda_or_struct->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter:
					case LambdaDefinitionLocation::body: {

						auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;

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
	s64 definition_size = ceil(get_size(definition->type), context.stack_word_size);

	assert(definition->offset != -1);

	if (definition->parent_lambda_or_struct && !definition->is_constant) {
		switch (definition->parent_lambda_or_struct->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter:
					case LambdaDefinitionLocation::body: {

						auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;
						assert(parent_lambda->kind == Ast_Lambda);
						assert(parent_lambda->parameters_size != -1);

						s64 const stack_base_register_size = context.stack_word_size;
						s64 const return_address_size = context.stack_word_size;
						s64 const parameters_end_offset = stack_base_register_size + return_address_size + parent_lambda->parameters_size;
						s64 const return_parameters_start_offset = parameters_end_offset;

						Instruction *offset_instr = 0;

						switch (definition->definition_location) {
							case LambdaDefinitionLocation::return_parameter:
								return rb + return_parameters_start_offset;

							case LambdaDefinitionLocation::parameter:
								return rb + parameters_end_offset - 8 - definition->offset;

							case LambdaDefinitionLocation::body: {
								return locals + definition->offset;
							}
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
		//
		// Global
		//
		auto offset = definition->offset;
		if (definition->is_constant) {
			return constants + offset;
		} else {
			if (definition->expression) {
				return rwdata + offset;
			} else {
				return zeros + offset;
			}
		}
	}
}

void Converter::load_address_of(AstDefinition *definition, RegisterOrAddress destination) {
	push_comment(format("load address of {} (definition_location={})"str, definition->name, definition->definition_location));

	s64 definition_size = ceil(get_size(definition->type), context.stack_word_size);

	assert(definition->offset != -1);

	if (definition->parent_lambda_or_struct && !definition->is_constant) {
		switch (definition->parent_lambda_or_struct->kind) {
			case Ast_Lambda: {
				switch (definition->definition_location) {
					case LambdaDefinitionLocation::body: {
						auto addr = get_known_address_of(definition);
						if (destination.is_register) {
							I(lea, destination.reg, addr);
						} else {
							I(lea, r0, addr);
							I(mov8_mr, destination.address, r0);
						}
						break;
					}
					case LambdaDefinitionLocation::return_parameter:
					case LambdaDefinitionLocation::parameter: {
						auto addr = get_known_address_of(definition);
						if (get_size(definition->type) <= context.stack_word_size) {
							if (destination.is_register) {
								I(lea, destination.reg, addr);
							} else {
								I(lea, r0, addr);
								I(mov8_mr, destination.address, r0);
							}
						} else {
							if (destination.is_register) {
								I(lea, destination.reg, addr);
								I(mov8_rm, destination.reg, destination.reg);
							} else {
								I(mov8_rm, r0, addr);
								I(mov8_mr, destination.address, r0);
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
		if (destination.is_register) {
			I(lea, destination.reg, addr);
		} else {
			I(lea, r0, addr);
			I(mov8_mr, destination.address, r0);
		}
	}
}
void Converter::load_address_of(AstExpression *expression, RegisterOrAddress destination) {
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
				if (destination.is_register) { instr = II(lea, destination.reg, instructions); }
				else                         { instr = II(lea, r0, instructions); I(mov8_mr, destination.address, r0); }

				instructions_that_reference_lambdas.add({.instruction=instr, .lambda=lambda});
			} else {
				assert((s64)(s32)count_of(lambda->definition->name) == (s64)count_of(lambda->definition->name));
				if (destination.is_register) {
					I(mov_re, destination.reg, (String)lambda->definition->name);
				} else {
					I(mov_re, r0, (String)lambda->definition->name);
					I(mov8_mr, destination.address, r0);
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
			assert(definition->parent_lambda_or_struct);
			assert(definition->parent_lambda_or_struct->kind == Ast_Struct);

			auto offset = definition->offset;
			assert(offset != INVALID_MEMBER_OFFSET);

			if (is_pointer(binop->left->type))
				append(binop->left, destination);
			else
				load_address_of(binop->left, destination);

			if (offset) {
				if (destination.is_register) I(add_rc, destination.reg, offset);
				else                         I(add_mc, destination.address, offset);
			}

			break;
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;

			// TODO: Clean up this copypasta.

			if (auto span = direct_as<AstSpan>(subscript->expression->type)) {
				load_address_of(subscript->expression, destination);
				if (destination.is_register) {
					I(mov8_rm, destination.reg, destination.reg);
				} else {
					I(mov8_rm, r0, destination.address);
					I(mov8_rm, r0, r0);
					I(mov8_mr, destination.address, r0);
				}

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_register)
					I(add_rr, destination.reg, index);
				else
					I(add_mr, destination.address, index);
			} else if (auto pointer_type = as_pointer(subscript->expression->type)) {
				append(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_register)
					I(add_rr, destination.reg, index);
				else
					I(add_mr, destination.address, index);
			} else if (types_match(subscript->expression->type, builtin_string)) {
				assert(get_size(subscript->index_expression->type) <= context.stack_word_size);

				auto _reg_or_addr1 = load_address_of(subscript->expression);
				auto _reg_or_addr2 = append(subscript->index_expression);
				defer {
					if (_reg_or_addr1.is_register) free_register(_reg_or_addr1.reg);
					if (_reg_or_addr2.is_register) free_register(_reg_or_addr2.reg);
				};

				Register string = r0;
				Register index = r1;

				if (_reg_or_addr1.is_register) string = _reg_or_addr1.reg;
				else                           I(mov8_rm, string, _reg_or_addr1.address);

				if (_reg_or_addr2.is_register) index = _reg_or_addr2.reg;
				else                           I(mov8_rm, index, _reg_or_addr2.address);

				auto count = r2;

				I(mov8_rm, count, string + 8);
				I(mov8_rm, string, string);

				I(cmpf8, index, count);
				I(jlf_c, 2);
				push_comment(format(u8"bounds check failed for {}"s, subscript->location));
				I(debug_break);
				I(jmp_label);

				I(mul_rc, index, get_size(subscript->type));
				I(add_rr, index, string);

				if (destination.is_register)
					I(mov_rr, destination.reg, index);
				else
					I(mov8_mr, destination.address, index);
			} else {
				assert(direct_as<AstSubscript>(subscript->expression->type));
				load_address_of(subscript->expression, destination);

				APPEND_INTO_REGISTER(index, subscript->index_expression, r0);

				I(mul_rc, index, get_size(subscript->type));

				if (destination.is_register)
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
			not_implemented();
#if 0
			auto unop = (AstUnaryOperator *)expression;
			assert(unop->operation == UnaryOperation::dereference);
			append_to_stack(unop->expression);
			return {}; // right now result is always on the stack
#endif
			break;
		}
		default:
			invalid_code_path("attempt to load address of unknown kind of expression");
	}
}

template <class T, umm capacity>
struct StaticSet {

	T *begin() { return data; }
	T const *begin() const { return data; }
	T *end() { return data + count; }
	T const *end() const { return data + count; }

	T *find(T const &value) {
		for (auto &it : *this) {
			if (it == value)
				return &it;
		}
		return 0;
	}
	T &get_or_insert(T const &value) {
		if (auto found = find(value))
			return *found;
		return data[count++] = value;
	}

	bool remove(T const &value) {
		if (auto found = find(value)) {
			--count;
			memcpy(found, found + 1, sizeof(T) * (end() - found));
			return true;
		}
		return false;
	}

	Optional<T> pop() {
		if (count)
			return data[--count];
		return {};
	}

	union {
		T data[capacity];
	};
	umm count = 0;
};

void Converter::append_memory_copy(Address dst, Address src, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	if (bytes_to_copy == 0)
		return;

	push_comment(format(u8"copy {} bytes from {} into {}, reverse={}"s, bytes_to_copy, from_name, to_name, reverse));

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

	switch (bytes_to_copy) {
		case 1:
		case 2:
		case 4:
		case 8:
		case 16: {
			if (bytes_to_copy == 16) {
				if (reverse) {
					I(mov8_rm, intermediary, src+8);
					I(mov8_mr, dst+8, intermediary);
					I(mov8_rm, intermediary, src);
					I(mov8_mr, dst, intermediary);
				} else {
					I(mov8_rm, intermediary, src);
					I(mov8_mr, dst, intermediary);
					I(mov8_rm, intermediary, src+8);
					I(mov8_mr, dst+8, intermediary);
				}
			} else if (bytes_to_copy == 8) {
				I(mov8_rm, intermediary, src);
				I(mov8_mr, dst, intermediary);
			} else if (bytes_to_copy == 4) {
				I(mov4_rm, intermediary, src);
				I(mov4_mr, dst, intermediary);
			} else if (bytes_to_copy == 2) {
				I(mov2_rm, intermediary, src);
				I(mov2_mr, dst, intermediary);
			} else if (bytes_to_copy == 1) {
				I(mov1_rm, intermediary, src);
				I(mov1_mr, dst, intermediary);
			}
			break;
		}
		default: {
			if (reverse) {
				I(copyb_mmc, dst, src, bytes_to_copy);
			} else {
				I(copyf_mmc, dst, src, bytes_to_copy);
			}
			break;
		}
	}
}

void Converter::append_memory_set(InstructionList &list, Address d, s64 s, s64 size, bool reverse) {

	s &= 0xff;
	s |= s << 32;
	s |= s << 16;
	s |= s << 8;

	switch (size) {
		case 1: list.add(MI(mov1_mc, d, s)); break;
		case 2: list.add(MI(mov2_mc, d, s)); break;
		case 4: list.add(MI(mov4_mc, d, s)); break;
		case 8: list.add(MI(mov8_mc, d, s)); break;
		default:
			if (reverse)
				list.add(MI(setb_mcc, d, (s8)s, (s32)size));
			else
				list.add(MI(setf_mcc, d, (s8)s, (s32)size));
			break;
	}
}
void Converter::append_memory_set(Address d, s64 s, s64 size, bool reverse) {
	append_memory_set(ls->body_builder, d, s, size, reverse);
}

void Converter::append_struct_initializer(AstStruct *Struct, SmallList<AstExpression *> values, RegisterOrAddress destination) {
	assert(!destination.is_register);

	auto struct_size = ceil(get_size(Struct), 8ll);

	push_comment(format("struct initializer {}"str, Struct->definition ? Struct->definition->name : where(Struct->location.data)));

	for (umm i = 0; i < values.count; ++i) {
		auto arg = values[i];
		auto member = Struct->data_members[i];
		auto arg_size = get_size(member->type);

		auto member_address = destination.address + member->offset;

		if (arg) {
			assert(types_match(arg->type, member->type));

			append(arg, member_address);
		} else {
			// zero initialize
			append_memory_set(member_address, 0, arg_size, false);
		}
	}
}

void Converter::append(Scope &scope) {
	scoped_replace(ls->current_scope, &scope);
	for (auto statement : scope.statements) {
		// if (statement->uid() == 1826)
		// 	debug_break();
		//if (statement->location == "glGenBuffers = @ wglGetProcAddress(\"glGenBuffers\\0\".data)")
		//	debug_break();

		push_comment((Span<utf8>)format("==== {}: {} ====", where(statement->location.data), statement->location));

		ls->temporary_size = max(ls->temporary_size, ls->temporary_cursor);
		ls->temporary_cursor = 0;

		append(statement);
	}
	scope.defers_start_index = count_of(ls->body_builder);
	for (auto Defer : reverse(scope.bytecode_defers)) {
		append(Defer->scope);
	}
}

void Converter::append(AstDefinition *definition) {
	assert(definition->type);

	if (definition->parent_lambda_or_struct && !definition->is_constant) {
		switch (definition->parent_lambda_or_struct->kind) {
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
							append_memory_set(addr, 0, get_size(definition->type), false);
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
	if (definition->parent_lambda_or_struct && definition->parent_lambda_or_struct->kind != Ast_Struct && definition->is_constant) {
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
			for_each (Struct->scope.definitions, [&](auto, auto members) {
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

	if (definition->parent_lambda_or_struct) {
		if (definition->parent_lambda_or_struct->kind == Ast_Lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;
			push_comment(format(u8"definition {}", definition->name));
			assert(!definition->is_parameter);

			auto size = ceil(definition_size, context.stack_word_size);

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
				//	auto size_with_spacing = (cursor_after - cursor_before) * context.stack_word_size;
				//	auto size_diff = size_with_spacing - ceil(size, context.stack_word_size);
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
void Converter::append(AstReturn *ret) {
	push_comment(u8"return"s);

	auto lambda = ret->lambda;

	if (ret->expression) {
		LOAD_ADDRESS_INTO_REGISTER(destination, ret->lambda->return_parameter, r0);
		assert(destination != r0);

		append(ret->expression, destination + 0);
	}

	Scope *scope = ls->current_scope;
	while (scope) {
		for (auto Defer : reverse(scope->bytecode_defers)) {
			append(Defer->scope);
		}
		scope = scope->parent;
	}

	auto jump_index = (s64)count_of(ls->body_builder);
	auto return_jump = II(jmp, 0);

	lambda->return_jumps.add({return_jump, jump_index});
}
void Converter::append(AstIf *If) {
#if 0
	if (If->is_constant) {
		// constant if's statements were brought outside already by the typechecker. No need to append it.
		return;
	}
#else
	if (If->is_constant) {
		// NOTE: constant if's scope is not merged into it's parent.
		auto scope = If->true_branch_was_taken ? &If->true_scope : &If->false_scope;
		if (ls) {
			// if we are in a lambda, append statements with all checks and defers etc.
			append(*scope);
		} else {
			for (auto statement : scope->statements) {
				append(statement);
			}
		}
		return;
	}
#endif

	APPEND_INTO_REGISTER(condition_register, If->condition, r0);

	auto jz = I(jz_cr, 0, condition_register);

	auto true_branch_first_instruction_index = count_of(ls->body_builder);
	append(If->true_scope);

	auto jmp = I(jmp, .offset=0);
	I(jmp_label);

	auto false_branch_first_instruction_index = count_of(ls->body_builder);
	append(If->false_scope);

	auto false_end = count_of(ls->body_builder);

	I(jmp_label);

	jz->offset = false_branch_first_instruction_index - true_branch_first_instruction_index;
	jmp->offset = false_end - false_branch_first_instruction_index + 2;
}
void Converter::append(AstWhile *While) {
	auto count_before_condition = count_of(ls->body_builder);
	I(jmp_label);

	APPEND_INTO_REGISTER(condition_register, While->condition, r0);
	auto jz = I(jz_cr, 0, condition_register);

	auto count_after_condition = count_of(ls->body_builder);

	loop_control_stack.add();

	append(While->scope);

	auto count_after_body = count_of(ls->body_builder);

	I(jmp, .offset=0)->offset = (s64)count_before_condition - (s64)count_after_body;

	I(jmp_label);

	jz->offset = (s64)count_after_body - (s64)count_after_condition + 2;

	for (auto &jmp : loop_control_stack.pop()) {
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
void Converter::append(AstBlock *block) {
	append(block->scope);
}
void Converter::append(AstExpressionStatement *es) {
	// TODO: FIXME: this may allocate temporary space.
	// Figure a way to not produce the result.
	auto value = append(es->expression);
	if (value.is_register)
		free_register(value.reg);
}
void Converter::append(AstAssert *Assert) {
	if (Assert->is_constant)
		return;
	push_comment(format(u8"assert {}", Assert->location));

	APPEND_INTO_REGISTER(condition, Assert->condition, r0);

	I(jnz_cr, 2, condition);
	push_comment(format(u8"Assertion failed: {}", Assert->condition->location));
	I(debug_break);
	I(jmp_label);
}
void Converter::append(AstLoopControl *LoopControl) {

	// TODO: FIXME: with that way of executing defers there may be A LOT of repeating instructions in
	// loops with a lot of breaks/continues an defers. Maybe there is a better way to do this?
	auto scope = LoopControl->parent_scope;
	while (1) {
		for (auto Defer : reverse(scope->bytecode_defers)) {
			append(Defer->scope);
		}
		if (scope->node->kind == Ast_While) {
			break;
		}
		scope = scope->parent;
	}
	loop_control_stack.back().add({count_of(ls->body_builder), II(jmp), LoopControl->control});
}
void Converter::append(AstMatch *Match) {
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

	I(mov8_rm, r0, matchable);
	for (auto &Case : Match->cases) {
		if (Case.expression) {
			I(mov_rc, r1, (s64)get_constant_integer(Case.expression).value());
			I(cmpu8, r2, r0, r1, Comparison::e);
			jumps_to_cases.add({II(jnz_cr, 0, r2), ls->body_builder.count});
		} else {
			has_default_case = true;
		}
	}

	if (has_default_case) {
		jump_to_default = {II(jmp), ls->body_builder.count};
	} else {
		jumps_out.add({II(jmp), ls->body_builder.count});
	}

	umm case_index = 0;
	for (auto &Case : Match->cases) {
		I(jmp_label);
		if (Case.expression) {
			jumps_to_cases[case_index].instruction->jnz_cr.offset = ls->body_builder.count - jumps_to_cases[case_index].index;
		} else {
			jump_to_default.instruction->jmp.offset = ls->body_builder.count - jump_to_default.index;
		}

		append(Case.scope);
		jumps_out.add({II(jmp), ls->body_builder.count});
		case_index++;
	}
	I(jmp_label);

	for (auto &jump_out : jumps_out) {
		jump_out.instruction->jmp.offset = ls->body_builder.count - jump_out.index;
	}
}

void Converter::append(AstBinaryOperator *bin, RegisterOrAddress destination) {
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

						if (destination.is_register) {
							switch (member_size) {
								case 1: I(mov1_rm, destination.reg, member_address); break;
								case 2: I(mov2_rm, destination.reg, member_address); break;
								case 4: I(mov4_rm, destination.reg, member_address); break;
								case 8: I(mov8_rm, destination.reg, member_address); break;
								default: not_implemented(); break;
							}
						} else {
							append_memory_copy(destination.address, member_address, member_size, false, "struct member"str, "somewhere else"str);
						}
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
			if (destination.is_register) {
				append(left, destination);
				APPEND_INTO_REGISTER(rr, right, r1);

				auto lt = direct(bin->left->type);

				if (lt == builtin_f32.Struct) {
					not_implemented();
				} else if (lt == builtin_f64.Struct) {
					not_implemented();
				} else {
					switch (bin->operation) {
						case add:  I(add_rr, destination.reg, rr); break;
						case sub:  I(sub_rr, destination.reg, rr); break;
						case mul:  I(mul_rr, destination.reg, rr); break;
						case div:  I(div_rr, destination.reg, rr); break;
						case mod:  I(mod_rr, destination.reg, rr); break;
						case bor:  I( or_rr, destination.reg, rr); break;
						case band: I(and_rr, destination.reg, rr); break;
						case bxor: I(xor_rr, destination.reg, rr); break;
						case bsr:  I(shr_rr, destination.reg, rr); break;
						case bsl:  I(shl_rr, destination.reg, rr); break;
						default: invalid_code_path();
					}
				}

			} else {
				assert(get_size(left ->type) <= context.stack_word_size);
				assert(get_size(right->type) <= context.stack_word_size);
				auto left_  = append(left);
				auto right_ = append(right);
				defer {
					if (left_ .is_register) free_register(left_.reg);
					if (right_.is_register) free_register(right_.reg);
				};
				Register l = r0;
				Register r = r1;
				if (left_.is_register) {
					l = left_.reg;
				} else {
					I(mov8_rm, l, left_.address);
				}
				if (right_.is_register) {
					r = right_.reg;
				} else {
					I(mov8_rm, r, right_.address);
				}

				auto lt = direct(bin->left->type);

				if (lt == builtin_f32.Struct) {
					not_implemented();
				} else if (lt == builtin_f64.Struct) {
					not_implemented();
				} else {
					switch (bin->operation) {
						case add:  I(add_rr, l, r); break;
						case sub:  I(sub_rr, l, r); break;
						case mul:  I(mul_rr, l, r); break;
						case div:  I(div_rr, l, r); break;
						case mod:  I(mod_rr, l, r); break;
						case bor:  I( or_rr, l, r); break;
						case band: I(and_rr, l, r); break;
						case bxor: I(xor_rr, l, r); break;
						case bsr:  I(shr_rr, l, r); break;
						case bsl:  I(shl_rr, l, r); break;
						default: invalid_code_path();
					}
				}

				switch (get_size(lt)) {
					case 1: I(mov1_mr, destination.address, l); break;
					case 2: I(mov2_mr, destination.address, l); break;
					case 4: I(mov4_mr, destination.address, l); break;
					case 8: I(mov8_mr, destination.address, l); break;
					default: invalid_code_path();
				}
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
				I(mov8_rm, r1, la + 8);
				I(mov8_rm, r2, ra + 8);
				I(cmpu8, r0, r1, r2, Comparison::e);
				I(jnz_cr, 3, r0);
				if (destination.is_register) {
					I(mov_rc, destination.reg, 0);
				} else {
					I(mov1_mc, destination.address, 0);
				}
				I(jmp, 6);

				I(jmp_label);
				// load pointers
				I(mov8_rm, r0, la);
				I(mov8_rm, r1, ra);
				I(cmpstr, r2, r0, r1);
				if (destination.is_register) {
					I(mov_rr, destination.reg, r2);
				} else {
					I(mov1_mr, destination.address, r2);
				}
				I(jmp_label);
				return;
			}

			assert(get_size(left->type) <= context.stack_word_size);
			assert(get_size(right->type) <= context.stack_word_size);
			auto la = append(left);
			auto ra = append(right);
			defer {
				if (la.is_register) free_register(la.reg);
				if (ra.is_register) free_register(ra.reg);
			};
			Register rl = r0;
			Register rr = r1;
			if (la.is_register) {
				rl = la.reg;
			} else {
				I(mov8_rm, rl, la.address);
			}
			if (ra.is_register) {
				rr = ra.reg;
			} else {
				I(mov8_rm, rr, ra.address);
			}

			auto comparison = comparison_from_binary_operation(bin->operation);

			if (destination.is_register) {
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
			} else {
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
			}
			break;
		}
		case ass: {
			auto dst_rm = load_address_of(left);
			auto src = append(right);
			defer {
				if (dst_rm.is_register) free_register(dst_rm.reg);
				if (src.is_register) free_register(src.reg);
			};
			Register dst = r0;
			if (dst_rm.is_register) {
				dst = dst_rm.reg;
			} else {
				I(mov8_rm, dst, dst_rm.address);
			}

			auto size = get_size(bin->left->type);

			if (src.is_register) {
				switch (size) {
					case 1: I(mov1_mr, dst, src.reg); break;
					case 2: I(mov2_mr, dst, src.reg); break;
					case 4: I(mov4_mr, dst, src.reg); break;
					case 8: I(mov8_mr, dst, src.reg); break;
				}
			} else {
				append_memory_copy(dst, src.address, size, false, {}, {});
			}
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
			LOAD_ADDRESS_INTO_REGISTER(dst, bin->left, r0);

			APPEND_INTO_REGISTER(src, bin->right, r1);

			switch (bin->operation) {
				case addass:  I(add_mr, dst, src); break;
				case subass:  I(sub_mr, dst, src); break;
				case mulass:  I(mul_mr, dst, src); break;
				case divass:  I(div_mr, dst, src); break;
				case modass:  I(mod_mr, dst, src); break;
				case borass:  I( or_mr, dst, src); break;
				case bandass: I(and_mr, dst, src); break;
				case bxorass: I(xor_mr, dst, src); break;
				case bslass:  I(shl_mr, dst, src); break;
				case bsrass:  I(shr_mr, dst, src); break;
				default: invalid_code_path();
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
					assert(!destination.is_register);
					assert(context.stack_word_size == 8);

					if (is_addressable(left)) {
						load_address_of(left, destination);
					} else {
						auto size = ceil(get_size(array), context.stack_word_size);
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

				assert(!from_value.is_register, "not implemented");

				if (::is_integer(bin->right)) {
					auto count_addr = from_value.address + context.stack_word_size;
					if (destination.is_register) {
						I(mov8_rm, destination.reg, count_addr);
					} else {
						I(mov8_rm, r0, count_addr);
						I(mov8_mr, destination.address, r0);
					}
				} else if (::is_pointer(bin->right)) {
					auto data_addr = from_value.address;
					if (destination.is_register) {
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


			// Integer conversions:
			// If destination is bigger than source:
			//    extend the size depending on the signedness of source operand (sign extend for signed, zero extend for unsigned).
			// Otherwise this is a noop.

			if (::is_integer(from) && ::is_integer(to)) {
				append(cast->left, destination);

				if (destination.is_register) {
					if (false) {
					} else if (from == builtin_u8.Struct) {
						if (false) {}
						else if (to == builtin_u16.Struct) { I(movzx21_rr, destination.reg, destination.reg); }
						else if (to == builtin_u32.Struct) { I(movzx41_rr, destination.reg, destination.reg); }
						else if (to == builtin_u64.Struct) { I(movzx81_rr, destination.reg, destination.reg); }
						else if (to == builtin_s16.Struct) { I(movzx21_rr, destination.reg, destination.reg); }
						else if (to == builtin_s32.Struct) { I(movzx41_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movzx81_rr, destination.reg, destination.reg); }
					} else if (from == builtin_u16.Struct) {
						if (false) {}
						else if (to == builtin_u32.Struct) { I(movzx42_rr, destination.reg, destination.reg); }
						else if (to == builtin_u64.Struct) { I(movzx82_rr, destination.reg, destination.reg); }
						else if (to == builtin_s32.Struct) { I(movzx42_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movzx82_rr, destination.reg, destination.reg); }
					} else if (from == builtin_u32.Struct) {
						if (false) {}
						else if (to == builtin_u64.Struct) { I(movzx84_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movzx84_rr, destination.reg, destination.reg); }
					} else if (from == builtin_s8.Struct) {
						if (false) {}
						else if (to == builtin_u16.Struct) { I(movsx21_rr, destination.reg, destination.reg); }
						else if (to == builtin_u32.Struct) { I(movsx41_rr, destination.reg, destination.reg); }
						else if (to == builtin_u64.Struct) { I(movsx81_rr, destination.reg, destination.reg); }
						else if (to == builtin_s16.Struct) { I(movsx21_rr, destination.reg, destination.reg); }
						else if (to == builtin_s32.Struct) { I(movsx41_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movsx81_rr, destination.reg, destination.reg); }
					} else if (from == builtin_s16.Struct) {
						if (false) {}
						else if (to == builtin_u32.Struct) { I(movsx42_rr, destination.reg, destination.reg); }
						else if (to == builtin_u64.Struct) { I(movsx82_rr, destination.reg, destination.reg); }
						else if (to == builtin_s32.Struct) { I(movsx42_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movsx82_rr, destination.reg, destination.reg); }
					} else if (from == builtin_s32.Struct) {
						if (false) {}
						else if (to == builtin_u64.Struct) { I(movsx84_rr, destination.reg, destination.reg); }
						else if (to == builtin_s64.Struct) { I(movsx84_rr, destination.reg, destination.reg); }
					}
				} else {
					if (false) {
					} else if (from == builtin_u8.Struct) {
						if (false) {}
						else if (to == builtin_u16.Struct) { I(and_mc, destination.address, 0xff); }
						else if (to == builtin_u32.Struct) { I(and_mc, destination.address, 0xff); }
						else if (to == builtin_u64.Struct) { I(and_mc, destination.address, 0xff); }
						else if (to == builtin_s16.Struct) { I(and_mc, destination.address, 0xff); }
						else if (to == builtin_s32.Struct) { I(and_mc, destination.address, 0xff); }
						else if (to == builtin_s64.Struct) { I(and_mc, destination.address, 0xff); }
					} else if (from == builtin_u16.Struct) {
						if (false) {}
						else if (to == builtin_u32.Struct) { I(and_mc, destination.address, 0xff'ff); }
						else if (to == builtin_u64.Struct) { I(and_mc, destination.address, 0xff'ff); }
						else if (to == builtin_s32.Struct) { I(and_mc, destination.address, 0xff'ff); }
						else if (to == builtin_s64.Struct) { I(and_mc, destination.address, 0xff'ff); }
					} else if (from == builtin_u32.Struct) {
						if (false) {}
						else if (to == builtin_u64.Struct) { I(and_mc, destination.address, 0xff'ff'ff'ff); }
						else if (to == builtin_s64.Struct) { I(and_mc, destination.address, 0xff'ff'ff'ff); }
					} else if (from == builtin_s8.Struct) {
						if (false) {}
						else if (to == builtin_u16.Struct) { I(movsx21_rm, r0, destination.address); I(mov2_mr, destination.address, r0); }
						else if (to == builtin_u32.Struct) { I(movsx41_rm, r0, destination.address); I(mov4_mr, destination.address, r0); }
						else if (to == builtin_u64.Struct) { I(movsx81_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
						else if (to == builtin_s16.Struct) { I(movsx21_rm, r0, destination.address); I(mov2_mr, destination.address, r0); }
						else if (to == builtin_s32.Struct) { I(movsx41_rm, r0, destination.address); I(mov4_mr, destination.address, r0); }
						else if (to == builtin_s64.Struct) { I(movsx81_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
					} else if (from == builtin_s16.Struct) {
						if (false) {}
						else if (to == builtin_u32.Struct) { I(movsx42_rm, r0, destination.address); I(mov4_mr, destination.address, r0); }
						else if (to == builtin_u64.Struct) { I(movsx82_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
						else if (to == builtin_s32.Struct) { I(movsx42_rm, r0, destination.address); I(mov4_mr, destination.address, r0); }
						else if (to == builtin_s64.Struct) { I(movsx82_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
					} else if (from == builtin_s32.Struct) {
						if (false) {}
						else if (to == builtin_u64.Struct) { I(movsx84_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
						else if (to == builtin_s64.Struct) { I(movsx84_rm, r0, destination.address); I(mov8_mr, destination.address, r0); }
					}
				}
				return;
			}


			if (auto option = as_option(from)) {
				if (to == builtin_bool.Struct) {
					auto tmp_size = get_size(option);
					auto tmp = allocate_temporary_space(tmp_size);
					defer { ls->temporary_cursor -= tmp_size; };

					append(cast->left, tmp);

					if (destination.is_register) {
						I(mov1_rm, destination.reg, tmp + get_size(option->expression));
					} else {
						I(mov1_rm, r0, tmp + get_size(option->expression));
						I(mov1_mr, destination.address, r0);
					}

					return;
				}
			}

			if (auto option = as_option(to)) {
				assert(!destination.is_register, "not implemented");
				append(cast->left, destination);
				I(mov1_mc, destination.address + get_size(option->expression), 1);
				return;
			}

			invalid_code_path();
			break;
		}
		case lor: {
			if (destination.is_register) {
				append(bin->left, destination);

				auto jump = I(jnz_cr, 0, destination.reg);
				auto skip_start = ls->body_builder.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = ls->body_builder.count;

				jump->offset = skip_end - skip_start;
			} else {
				append(bin->left, destination);
				I(mov1_rm, r0, destination.address);

				auto jump = I(jnz_cr, 0, destination.reg);
				auto skip_start = ls->body_builder.count;

				append(bin->right, destination);

				I(jmp_label);
				auto skip_end = ls->body_builder.count;

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
							I(sub_rc, rs, ceil(member_size, context.stack_word_size));

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
								append_memory_copy(member_size, true, bin->location, u8"stack"s);
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
								I(sub_rc, rs, ceil(member_size, context.stack_word_size));

								push_comment(u8"3. Copy the member from struct to reserved space"s);

								auto member_src = load_address_of(bin->left).value_or([&]{I(pop_r, r1); return r1;});

								append_memory_copy_a(rs, member_src + member->offset_in_struct, member_size, false, bin->location, u8"stack"s);
							} else {
								// The plan is simple:
								// 1. Reserve space for eventual value
								// 2. Append the struct
								// 3. Copy the member from struct to reserved space
								// 4. Remove struct from the stack
								assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);

								push_comment(u8"1. Reserve space for eventual value"s);
								I(sub_rc, rs, ceil(member_size, context.stack_word_size));

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
									I(add_mc, rs, ceil(struct_size, context.stack_word_size));

									I(push_r, rs); // source
									I(add_mc, rs, context.stack_word_size + member->offset_in_struct);

									append_memory_copy(member_size, true, bin->location, u8"stack"s);

									push_comment(u8"4. Remove struct from the stack"s);
									I(add_rc, rs, ceil(struct_size, context.stack_word_size));
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
					auto size = ceil(get_size(array), context.stack_word_size);

					append_to_stack(left);

					auto offset = rb-ls->temporary_cursor-size;
					append_memory_copy_a(offset, rs, size, false, "cast"str, "temporary"str);

					I(add_rc, rs, size);
					I(lea, r0, offset);
					I(push_r, r0);

					ls->temporary_cursor += size;
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
					I(add_rc, rs, context.stack_word_size);
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
					append_memory_copy(dst, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					free_register(dst);

					push_comment(u8"remove right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size));
				} else {
					append_to_stack(right);

					 // load dest address
					switch (context.register_size) {
						case 8: I(mov8_rm, r0, rs + ceil(bytes_to_write, context.stack_word_size)); break;
						case 4: I(mov4_rm, r0, rs + ceil(bytes_to_write, context.stack_word_size)); break;
					}

					append_memory_copy(r0, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

					push_comment(u8"remove left address and right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size) + context.stack_word_size);
				}

				break;


				// BTW this code is unreachable

				// :PUSH_ADDRESS: TODO: Replace this with load_address_of
				push_address_of(left); // destination address

				I(push_r, rs); // source address
				I(add_mc, rs, context.stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size));

				// Finish this thing that uses registers
#if 0
				auto destination_address = load_address_of(left); // destination address
				if (destination_address) {
					REDECLARE_VAL(destination_address, destination_address.value_unchecked());

					auto source_address = allocate_register(conv);
					if (source_address) {
						REDECLARE_VAL(source_address, source_address.value_unchecked());

						append_memory_copy(destination_address, source_address, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					} else {

					}

					free_register(destination_address);
				} else {
				}

				I(push_r, rs); // source address
				I(add_mc, rs, context.stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size));
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
void Converter::append(AstIdentifier *identifier, RegisterOrAddress destination) {
	check_destination(destination);

	auto definition = identifier->definition();

	LOAD_ADDRESS_INTO_REGISTER(definition_address, definition, r0);

	if (destination.is_register) {
		I(mov8_rm, destination.reg, definition_address);
	} else {
		append_memory_copy(destination.address, definition_address, get_size(identifier->type), false, "definition"str, "identifier"str);
	}

	return;
	not_implemented();
#if 0
	//if (identifier->name == "abc")
	//	debug_break();

	assert(!destination);
	push_comment(format(u8"load identifer {}", identifier->location));

	auto definition = identifier->definition();

	if (definition->expression && definition->expression->kind == Ast_Lambda) {
		// :PUSH_ADDRESS: TODO: Replace this with load_address_of
		push_address_of(identifier);
		return {};
	} else {
		auto size = get_size(identifier->type); // NOT definition->type because it can be unsized (not hardened)
		assert(size != -1);

		if (size <= context.register_size) {
			auto addr = load_address_of(identifier);
			if (addr) {
				REDECLARE_VAL(addr, addr.value_unchecked());
				switch (size) {
					case 1: I(mov1_rm, addr, addr); break;
					case 2: I(mov2_rm, addr, addr); break;
					case 4: I(mov4_rm, addr, addr); break;
					case 8: I(mov8_rm, addr, addr); break;
					default:
						// TODO: allow values of size 3,5,6,7 to be passed in registers.
						invalid_code_path("not implemented");
						// this is wrong. will read bytes out of bounds
						I(push_m, addr);
						return {};
				}
				return value_registers(addr);
			} else {
				// NOTE: load_address_of failed to allocate a register.
				// I think there is no way there is an available one.
				// No point in allocating for result.
				I(pop_r, r0);
				I(push_m, r0);
				return {};
			}
		} else {
			// TODO: pass big values in registers
			I(sub_rc, rs, ceil(size, context.stack_word_size));
			I(push_r, rs);
			// :PUSH_ADDRESS: TODO: Replace this with load_address_of
			push_address_of(identifier);
			append_memory_copy(size, false, identifier->location, u8"stack"s);
			return {};
		}
	}
	invalid_code_path();
#endif
}
void Converter::append(AstCall *call, RegisterOrAddress destination) {
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

	auto word_size = context.stack_word_size;

	check_destination(destination);

	push_comment(format(u8"call {}", call->callable->location));

	if (call->lambda_type) {
		auto lambda = call->lambda_type->lambda;
		bool is_member = lambda->is_member;

		// each argument's size is not more than context.stack_word_size.
		// bigger arguments are passed as pointers.
		s64 parameters_bytes = (tl::count(lambda->parameters, [](auto param){return !param->is_constant;}) + is_member) * word_size;
		s32 return_parameter_bytes = word_size;

		auto return_value_size = get_size(lambda->return_parameter->type);

		auto stack_space_used_for_call = parameters_bytes + return_parameter_bytes;
		if (lambda->convention == CallingConvention::stdcall)
			stack_space_used_for_call += 32;
		ls->max_stack_space_used_for_call = max(ls->max_stack_space_used_for_call, ceil(stack_space_used_for_call, 16ll));

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

			for (auto param : lambda->parameters)
				assert(!param->is_constant); // not implemented: need to erase constants from call->sorted_arguments first



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
			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto src = args_tmp + (call->sorted_arguments.count-1-i)*word_size;
				auto dst = rs + (call->sorted_arguments.count-1-i)*word_size;
				I(mov8_rm, r0, src);
				I(mov8_mr, dst, r0);
			}
		};

		if (lambda->is_intrinsic) {
			auto name = lambda->definition->name;
			if (name == "debug_break") {
				I(debug_break);
			} else if (name == "memcpy") {
				append_arguments();
				auto rdst = r0;
				auto rsrc = r1;
				auto rsize = r2;
				I(mov8_rm, rsize, rs);
				I(mov8_rm, rsrc, rs + 8);
				I(mov8_rm, rdst, rs + 16);
				I(copyf_mmr, rdst, rsrc, rsize);
			} else {
				invalid_code_path("Unknown intrinsic");
			}
			return;
		}

		assert(word_size == 8);

		if (is_member)
			assert(call->sorted_arguments.count == lambda->parameters.count+1);
		else
			assert(call->sorted_arguments.count == lambda->parameters.count);

		auto &arguments = call->sorted_arguments;
		bool lambda_is_constant = is_constant(call->callable);

		switch (lambda->convention) {
			case CallingConvention::tlang: {
				append_arguments();

				if (lambda_is_constant || is_member) {
					if (lambda->definition) { // null if polymorphic
						assert(lambda->definition->is_constant);
					}
					instructions_that_reference_lambdas.add({
						.instruction = II(call_c, -1),
						.lambda = lambda,
					});
				} else {
					// Whi did i write this?
					// auto rax = x86_64::to_bc_register(x86_64::Register64::rax);
					// I(pop_r, rax);
					// I(call_r, rax);

					APPEND_INTO_REGISTER(callable_reg, call->callable, r0);
					I(call_r, r0);

				}

				if (return_value_size && return_value_size <= word_size) {
					if (destination.is_register) {
						switch (return_value_size) {
							case 1: I(mov1_rm, destination.reg, return_value_address); break;
							case 2: I(mov2_rm, destination.reg, return_value_address); break;
							case 4: I(mov4_rm, destination.reg, return_value_address); break;
							case 8: I(mov8_rm, destination.reg, return_value_address); break;
							default: not_implemented();
						}
					} else {
						switch (return_value_size) {
							case 1: I(mov1_rm, r0, return_value_address); I(mov1_mr, destination.address, r0); break;
							case 2: I(mov2_rm, r0, return_value_address); I(mov2_mr, destination.address, r0); break;
							case 4: I(mov4_rm, r0, return_value_address); I(mov4_mr, destination.address, r0); break;
							case 8: I(mov8_rm, r0, return_value_address); I(mov8_mr, destination.address, r0); break;
							default: not_implemented();
						}
					}
				}
				break;
			}
			case CallingConvention::stdcall: {

				using namespace x86_64;

				s64 const shadow_space_size = 32;

				if (lambda->parameters.count < 4) {
					// shadow space
					parameters_bytes += 32;
				}

				for (auto argument : arguments) {
					assert(get_size(argument->type) <= 8);
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
						I(xchg8_m, m1, r0);
						I(mov8_mr, m0, r0);
					}
				}

				auto function_address_register = to_bc_register(Register64::rax);
				if (lambda_is_constant) {
					load_address_of(call->callable, function_address_register);
					I(stdcall_r, function_address_register);
				} else {
					not_implemented();
					// append_to_stack(call->callable);
					// I(pop_r, function_address_register);
					// I(call_r, function_address_register);
				}

				if (!types_match(call->type, builtin_void.Struct)) {
					assert(get_size(lambda->return_parameter->type) > 0);
					if (destination.is_register)
						I(mov_rr, destination.reg, to_bc_register(Register64::rax));
					else {
						switch (get_size(call->type)) {
							case 1: I(mov1_mr, destination.address, to_bc_register(Register64::rax)); break;
							case 2: I(mov2_mr, destination.address, to_bc_register(Register64::rax)); break;
							case 4: I(mov4_mr, destination.address, to_bc_register(Register64::rax)); break;
							case 8: I(mov8_mr, destination.address, to_bc_register(Register64::rax)); break;
							default: not_implemented();
						}
					}
				}

				break;
			}
			default:
				invalid_code_path();
		}
		return;
	} else {
		auto Struct = direct_as<AstStruct>(call->callable);
		assert(Struct);

		append_struct_initializer(Struct, call->sorted_arguments, destination);
		return;
	}
	invalid_code_path();
}
void Converter::append(AstLiteral *literal, RegisterOrAddress destination) {
	check_destination(destination);

	if (literal->literal_kind == LiteralKind::string)
		push_comment(format(u8"literal \"{}\"", escape_string(literal->string.get())));
	else
		push_comment(format(u8"literal {}", literal->location));

	assert(!types_match(literal->type, builtin_unsized_integer.Struct));
	assert(!types_match(literal->type, builtin_unsized_float.Struct));
	auto dtype = direct(literal->type);

	using enum LiteralKind;


	// auto destination = allocate_register(conv);
	// if (destination) {
	// 	REDECLARE_VAL(destination, destination.value_unchecked());
	//
	// 	switch (literal->literal_kind) {
	// 		case noinit:
	// 			// just return whatever was in the register before.
	// 			break;
	// 		case string: {
	// 			// TODO: deduplicate strings
	//
	// 			auto destination2 = allocate_register(conv);
	// 			if (destination2) {
	// 				REDECLARE_VAL(destination2, destination2.value_unchecked());
	//
	// 				auto data = allocate_data(constant_data_builder, as_bytes((Span<utf8>)literal->string));
	// 				I(mov_ra, destination, data);
	// 				I(mov_rc, destination2, (s64)literal->string.count);
	//
	// 				literal->string_data_offset = data;
	//
	// 				ValueRegisters result;
	// 				result.add(destination);
	// 				result.add(destination2);
	// 				return result;
	// 			} else {
	// 				free_register(destination);
	// 				goto fallback_to_stack_string;
	// 			}
	// 			break;
	// 		}
	// 		case character:
	// 			I(mov_rc, destination, literal->character);
	// 			break;
	// 		case Float:
	// 			push_comment(format(u8"float {}", literal->Float));
	//
	// 			switch (get_size(literal->type)) {
	// 				case 4: I(mov_rc, destination, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
	// 				case 8: I(mov_rc, destination, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
	// 				default: invalid_code_path();
	// 			}
	// 			break;
	// 		case boolean:
	// 			I(mov_rc, destination, (u8)literal->Bool);
	// 			break;
	// 		case integer: {
	// 			if (dtype == builtin_u8 .Struct||
	// 				dtype == builtin_s8.Struct)
	// 				I(mov_rc, destination, (u8)literal->integer);
	// 			else if (dtype == builtin_u16 .Struct||
	// 					 dtype == builtin_s16.Struct)
	// 				I(mov_rc, destination, (u16)literal->integer);
	// 			else if (dtype == builtin_u32 .Struct||
	// 					 dtype == builtin_s32.Struct)
	// 				I(mov_rc, destination, (u32)literal->integer);
	// 			else if (dtype == builtin_u64 .Struct||
	// 					 dtype == builtin_s64 .Struct||
	// 					 dtype == builtin_pointer_to_void.Struct)
	// 				I(mov_rc, destination, (s64)literal->integer);
	// 			else if (dtype == builtin_f32.Struct) {
	// 				auto f = (f32)(s64)literal->integer;
	// 				I(mov_rc, destination, *(s32 *)&f);
	// 			} else if (dtype == builtin_f64.Struct) {
	// 				auto f = (f64)(s64)literal->integer;
	// 				I(mov_rc, destination, *(s64 *)&f);
	// 			}
	// 			else if (literal->type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)literal->type)->operation == '*')
	// 				I(mov_rc, destination, (s64)literal->integer);
	// 			else invalid_code_path();
	// 			break;
	// 		}
	// 	}
	// 	ValueRegisters result;
	// 	result.add(destination);
	// 	return result;
	// }

	// append_strings(literal);

	// TODO: FIXME: there is a lot of copypasta
	switch (literal->literal_kind) {
		case string: {
			assert(!destination.is_register);
			assert(context.stack_word_size == 8);
			assert(literal->string.offset != -1);
			I(lea, r0, constants + literal->string.offset);
			I(mov8_mr, destination.address, r0);
			I(mov8_mc, destination.address+8, (s64)literal->string.count);
			break;
		}
		case character:
			if (destination.is_register) I(mov_rc, destination.reg, (s64)literal->character);
			else                         I(mov1_mc, destination.address, (s64)literal->character);
			break;
		case Float:
			push_comment(format(u8"float {}", literal->Float));

			if (destination.is_register) {
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
			if (destination.is_register) I(mov_rc, destination.reg, (s64)literal->Bool);
			else                         I(mov1_mc, destination.address, (s64)literal->Bool);
			break;
		case integer: {
			assert(context.stack_word_size == 8);
			if (destination.is_register) {
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
				}
				else if (literal->type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)literal->type)->operation == UnaryOperation::pointer)
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
				}
				else if (literal->type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)literal->type)->operation == UnaryOperation::pointer)
					I(mov8_mc, destination.address, (s64)literal->integer);
				else invalid_code_path();
			}
			break;
		}
		case null: {
			if (destination.is_register) {
				I(xor_rr, destination.reg, destination.reg);
			} else {
				append_memory_set(destination.address, 0, get_size(literal->type), false);
			}
			break;
		}
		default: invalid_code_path();
	}
}
void Converter::append(AstUnaryOperator *unop, RegisterOrAddress destination) {
	check_destination(destination);

	push_comment(format(u8"unary {}", unop->location));
	switch (unop->operation) {
		using enum UnaryOperation;
		case minus: {
			append(unop->expression, destination);
			if (destination.is_register) {
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
					not_implemented();
#if 0
					switch (size) {
						case 4:
							I(mov_fm, x0, destination.address);
							I(mov_rc, r0, (s64)0x8000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(push_f, x0);
							break;
						case 8:
							I(mov_fm, x0, destination.address);
							I(mov_rc, r0, (s64)0x8000'0000'0000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(push_f, x0);
							break;
						default: invalid_code_path();
					}
#endif
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
			defer { if (pointer.is_register) free_register(pointer.reg); };

			auto src = pointer.is_register ? pointer.reg : r0;
			if (!pointer.is_register)
				I(mov8_rm, src, pointer.address);

			auto dst = destination.is_register ? destination.reg : r1;
			if (!destination.is_register)
				I(mov8_rm, dst, destination.address);

			auto size = get_size(unop->type);
			if (size <= 8) {

				switch (size) {
					case 1: I(movzx81_rm, dst, src); break;
					case 2: I(movzx82_rm, dst, src); break;
					case 4: I(movzx84_rm, dst, src); break;
					case 8: I(mov8_rm,    dst, src); break;
					default: invalid_code_path();
				}

				if (!destination.is_register)
					I(mov8_mr, destination.address, dst);
			} else {
				assert(!destination.is_register);
				append_memory_copy(destination.address, src, size, false, {}, {});
			}
			break;
		}
		case bnot: {
			append(unop->expression, destination);
			if (destination.is_register) {
				I(not_r, destination.reg);
			} else {
				I(not_m, destination.address);
			}
			break;
		}
		case unwrap: {
			auto option = append(unop->expression);
			defer { if (option.is_register) free_register(option.reg); };
			assert(!option.is_register);
			assert(!destination.is_register);

			auto value_size = get_size(unop->type);

			I(mov1_rm, r0, option.address + value_size);
			I(jnz_cr, 2, r0);
			push_comment(format("{} didn't have a value. unwrap failed."str, unop->expression->location));
			I(debug_break);
			I(jmp_label);

			append_memory_copy(destination.address, option.address, value_size, false, {}, {});
			break;
		}
		case autocast: {
			not_implemented();
			append(unop->expression, destination);
			break;
		}
		case internal_move_to_temporary: {
			auto size = get_size(unop->expression->type);

			auto tmp = allocate_temporary_space(size);

			append(unop->expression, tmp);

			if (destination.is_register) {
				I(lea, destination.reg, tmp);
			} else {
				I(lea, r0, tmp);
				I(mov8_mr, destination.address, r0);
			}
			break;;
		}
		default:
			invalid_code_path();
	}
}
void Converter::append(AstSubscript *subscript, RegisterOrAddress destination) {
	check_destination(destination);

	if (is_addressable(subscript->expression)) {
		LOAD_ADDRESS_INTO_REGISTER(addr, subscript, r0);

		auto size = get_size(subscript->type);

		if (destination.is_register) {
			switch (size) {
				case 1: I(mov1_rm, destination.reg, addr); break;
				case 2: I(mov2_rm, destination.reg, addr); break;
				case 4: I(mov4_rm, destination.reg, addr); break;
				case 8: I(mov8_rm, destination.reg, addr); break;
			}
		} else {
			append_memory_copy(destination.address, addr, size, false, {}, {});
		}
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
			I(add_rc, rs, context.stack_word_size);
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
		append_memory_copy(element_size, false, subscript->location, u8"stack"s);
	}
	return {};
#endif
}
void Converter::append(AstLambda *lambda, bool load_address, RegisterOrAddress destination) {
	if (lambda->location_in_bytecode == -1) {
		if (lambda->is_poly) {
			assert(!load_address);

			for (auto hardened : lambda->hardened_polys) {
				append(hardened.lambda, false, r0);
			}
			return;
		}

		lambda->return_parameter->offset = 0;
		assert(lambda->return_parameter->parent_lambda_or_struct);
		assert(lambda->return_parameter->parent_lambda_or_struct->kind == Ast_Lambda);
		assert(lambda->return_parameter->definition_location == LambdaDefinitionLocation::return_parameter);

		if (lambda->has_body) {
			if (types_match(lambda->return_parameter->type, builtin_type.Struct)) {
				if (load_address) {
					invalid_code_path("can't push address of lambda that returns a type");
				}
				return;
			}

			LambdaState new_ls;
			new_ls.init();
			// ls.stack_state.init(get_size(lambda->return_parameter->type));

			new_ls.current_scope = &lambda->body_scope;

			auto old_ls = ls;
			ls = &new_ls;
			defer {
				ls = old_ls;
				new_ls.free();
			};

			scoped_replace(lambda, lambda);

			auto return_value_size = ceil(get_size(lambda->return_parameter->type), context.stack_word_size);

			//if (lambda->definition->name == u8"print_string"s)
			//	debug_break();


			if (lambda->definition) {
				push_comment(format(u8"lambda {} {}", lambda->definition->name, lambda->location));
			} else {
				push_comment(format(u8"lambda {} {}", where(lambda->location.data), lambda->location));
			}
			for (auto param : lambda->parameters) {
				push_comment(format("{} (__int64*)(rbp+{}),{}"str, param->name, context.stack_word_size + lambda->parameters_size - param->offset, ceil(get_size(param->type), 8ll)/8ll));
			}

#define BI(_kind, ...) \
	(&builder.add(MI(_kind, __VA_ARGS__))._kind)

#define BII(_kind, ...) \
	(&builder.add(MI(_kind, __VA_ARGS__)))

			lambda->location_in_bytecode = count_of(builder);

			lambda->first_instruction = &builder.add(MI(jmp_label));

			auto &stack_preparer = builder.add(MI(prepare_stack, 0));
			stack_preparer.comment = "stack preparer"str;

		// 	BI(debug_start_lambda, lambda);

			if (lambda->convention == CallingConvention::stdcall) {
				assert(context.stack_word_size == 8);
				using namespace x86_64;

				// what we have right now is:
				//
				// REGISTERS:
				// arg0 rcx or xmm0
				// arg1	rdx or xmm1
				// arg2	r8  or xmm2
				// arg3	r9  or xmm3
				//
				// STACK:
				// arg5
				// arg4
				// shadow
				// shadow
				// shadow
				// shadow <- rsp aligned to 16
				//
				// we need to get this:
				//
				// arg0
				// arg1
				// arg2
				// arg3
				// arg4
				// arg5 <- rsp aligned to 16
				//

				auto push_argument = [&](int arg_index) {
					if (lambda->parameters.count > arg_index)
						if (::is_float(lambda->parameters[arg_index]->type))
							BI(push_f, stdcall_float_registers[arg_index]);
						else
							BI(push_r, to_bc_register(stdcall_int_registers[arg_index]));
				};

				push_comment("reserve space for return value"str);
				assert(return_value_size <= context.stack_word_size);
				BI(sub_rc, rs, return_value_size);

				push_argument(0);
				push_argument(1);
				push_argument(2);
				push_argument(3);

				if (lambda->parameters.count > 4) {
					constexpr s64 return_address_size = 8;
					constexpr s64 shadow_space_size = 32;
					constexpr s64 first_four_arguments_size = 32;

					s64 offset = first_four_arguments_size + return_value_size + return_address_size + shadow_space_size;
					for (s64 i = 4; i < lambda->parameters.count; ++i) {
						BI(push_m, rs + offset);
						offset += 16;
					}
				}
				push_comment(u8"dummy return address"s);
				BI(push_c, 0xdeadc0d);
			}

			BI(push_r, rb);
			BI(mov_rr, rb, rs);
			ls->push_used_registers = BI(push_used_registers);
			auto rsp_setter = BI(lea, rs, rs + lambda->stack_cursor);

			{
				LOAD_ADDRESS_INTO_REGISTER(return_value_address, lambda->return_parameter, r0);
				assert(return_value_address != r0);
				push_comment(u8"zero out the return value"s);
				append_memory_set(return_value_address, 0, return_value_size, false);
			}

			ls->temporary_reserver = II(noop);

			//if (lambda->print_bytecode)
			//		debug_break();
			append(lambda->body_scope);

			if (context.optimize) {
				// TODO: skip iterations if nothing was changed.
				for (umm i = 0; i < context.optimization_pass_count; ++i) {
					optimize(ls->body_builder);
				}
			}

			rsp_setter->s.c -= ls->max_stack_space_used_for_call;

			// FIXME: HACK: hardcoded value for stdcall
			auto saved_registers_size = lambda->convention == CallingConvention::stdcall ? 64 : ceil(count_bits(ls->used_registers_mask)*8, 16u);

			ls->temporary_size = max(ls->temporary_size, ls->temporary_cursor);
			*ls->temporary_reserver = MI(sub_rc, rs, ls->temporary_size);
			set_comment(ls->temporary_reserver, "maybe reserve space for temporary values."str);
			auto locals_offset = ls->temporary_size + saved_registers_size;

			auto return_location = count_of(ls->body_builder);

			for (auto i : lambda->return_jumps) {
				auto offset = return_location - i.index;
				if (offset == 1) {
					i.jmp->kind = InstructionKind::noop;
				} else {
					i.jmp->jmp.offset = offset;
				}
			}

			if (lambda->print_bytecode) {
				print_bytecode(ls->body_builder);
			}

			// Patch local address
			auto patch = [&] (Address &address) {
				if (address.base == locals) {
					address.base = rb;
					address.c -= locals_offset;
				} else if (address.base == temporary) {
					address.base = rb;
				}
			};

			// SPEED: maybe store instructions that need to be patched in a list when they are pushed into builder?
#if 1
			for (auto &i : ls->body_builder) {
				for (auto addr_offset : address_members_offsets[(u8)i.kind]) {
					patch(*(Address *)((u8 *)&i + addr_offset));
				}
			}
#endif

			I(jmp_label);

			I(lea, rs, rb-saved_registers_size);

			ls->pop_used_registers = I(pop_used_registers);

			if (lambda->convention == CallingConvention::stdcall) {
				// FIXME: push and pop only necessary registers
				// HACK: -1 means push all non-volatile registers according to x64 stdcall convention.
				ls->push_used_registers->mask =
				ls-> pop_used_registers->mask = -1;
			} else {
				ls->push_used_registers->mask =
				ls-> pop_used_registers->mask = ls->used_registers_mask;
			}
			I(pop_r, rb);

			if (lambda->convention == CallingConvention::stdcall) {
				push_comment(u8"pop dummy return address and arguments"s);
				s64 const return_address_size = context.stack_word_size;
				I(add_rc, rs, return_address_size + (s64)lambda->parameters.count * context.stack_word_size);
				push_comment(u8"put return value into rax"s);
				I(pop_r, x86_64::to_bc_register(x86_64::Register64::rax));
			}


			I(ret);

//			if (lambda->definition && lambda->definition->name == "main")
//				debug_break();

			auto used_bytes = -lambda->stack_cursor + ls->temporary_size + ls->max_stack_space_used_for_call;
			if (used_bytes >= 4096) {
				stack_preparer.prepare_stack.byte_count = used_bytes;
			} else {
				stack_preparer.kind = InstructionKind::noop;
			}

#if 1
			add_steal(&builder, &ls->body_builder);
#else
			add(&builder, ls->body_builder);
#endif
		} else {
			lambda->definition->offset = 0;
			if (lambda->extern_library.data) {
				extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
			}
		}
	}

	if (load_address) {
		check_destination(destination);
		load_address_of(lambda, destination);
	}
}
void Converter::append(AstIfx *If, RegisterOrAddress destination) {
	check_destination(destination);

	push_comment(format(u8"ifx {}", If->location));

	APPEND_INTO_REGISTER(condition_register, If->condition, r0);

	auto jz = I(jz_cr, 0, condition_register);

	auto true_branch_first_instruction_index = count_of(ls->body_builder);
	append(If->true_expression, destination);

	auto jmp = I(jmp, .offset=0);
	I(jmp_label);

	auto false_branch_first_instruction_index = count_of(ls->body_builder);
	append(If->false_expression, destination);

	auto false_end = count_of(ls->body_builder);

	I(jmp_label);

	jz->offset = false_branch_first_instruction_index - true_branch_first_instruction_index;
	jmp->offset = false_end - false_branch_first_instruction_index + 2;
}
void Converter::append(AstPack *pack, RegisterOrAddress destination) {
	check_destination(destination);

	push_comment(format("pack {}"str, pack->location));
	assert(!destination.is_register);

	assert(pack->type->kind == Ast_Subscript);
	auto type = ((AstSubscript *)pack->type)->expression;

	auto elem_size = get_size(type);

	for (umm i = 0; i < pack->expressions.count; ++i) {
		auto expression = pack->expressions[i];
		append(expression, destination.address + i*elem_size);
	}
}

void Converter::optimize(InstructionList &instructions) {
	propagate_known_addresses(instructions);
	remove_redundant_instructions(instructions);
	propagate_known_values(instructions);
}
void Converter::propagate_known_addresses(InstructionList &instructions) {
	// Example transformation:
	//
	// lea r0, [locals + 16]  >>  lea r0, [locals + 16]
	// add r0, 4				  add r0, 4
	// mov r1, [r0]				  mov r1, [locals + 20]
	//
	// Note that we can't delete the lea and add yet,
	Optional<Address> register_state[(u8)Register::count];

	auto modify_address_if_known = [&] (Address &a) {
		if (a.r1_scale_index == 0 && a.r2_scale == 0) {
			if (auto addr = register_state[(u8)a.base]) {
				a = addr.value_unchecked() + a.c;
			}
		}
	};

	for (auto &i : instructions) {
		switch (i.kind) {
			using enum InstructionKind;
			case jmp:
			case jz_cr:
			case jnz_cr:
			case jmp_label:
				for (auto &s : register_state)
					s = null_opt;
				break;

			case lea: {
				if (i.lea.s.base == locals ||
					i.lea.s.base == constants ||
					i.lea.s.base == rwdata ||
					i.lea.s.base == zeros ||
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
			case mov1_rm: {
				modify_address_if_known(i.mov1_rm.s);
				register_state[(u8)i.mov1_rm.d] = null_opt;
				break;
			}
			case mov2_rm: {
				modify_address_if_known(i.mov2_rm.s);
				register_state[(u8)i.mov2_rm.d] = null_opt;
				break;
			}
			case mov4_rm: {
				modify_address_if_known(i.mov4_rm.s);
				register_state[(u8)i.mov4_rm.d] = null_opt;
				break;
			}
			case mov8_rm: {
				modify_address_if_known(i.mov8_rm.s);
				register_state[(u8)i.mov8_rm.d] = null_opt;
				break;
			}
			case add_rc: {
				if (auto &addr = register_state[(u8)i.add_rc.d]) {
					addr.value_unchecked().c += i.add_rc.s;
				}
				break;
			}
			case sub_rc: {
				if (auto &addr = register_state[(u8)i.sub_rc.d]) {
					addr.value_unchecked().c += i.sub_rc.s;
				}
				break;
			}


			case add_mc: {
				modify_address_if_known(i.add_mc.d);
				break;
			}
		}
	}
}
void Converter::remove_redundant_instructions(InstructionList &instructions) {
	struct RegisterState {
		Instruction *setter;
		List<Instruction *> modders;
		bool was_used;
	};

	RegisterState register_state[(u8)Register::count] = {};

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
			case xor_mc:                  use(i.xor_mc.d); break;
			case xor_mr: use(i.xor_mr.s); use(i.xor_mr.d); break;
			case and_rc:                  use(i.and_rc.d); break;
			case and_rr: use(i.and_rr.s); use(i.and_rr.d); break;
			case and_rm: use(i.and_rm.s); use(i.and_rm.d); break;
			case and_mc:                  use(i.and_mc.d); break;
			case and_mr: use(i.and_mr.s); use(i.and_mr.d); break;
			case or_rc:                   use(i.or_rc.d);  break;
			case or_rr:  use(i.or_rr.s);  use(i.or_rr.d);  break;
			case or_rm:  use(i.or_rm.s);  use(i.or_rm.d);  break;
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
			case setf_mcc: use(i.setf_mcc.d); break;
			case setb_mcc: use(i.setf_mcc.d); break;
			case call_c:                  break;
			case call_r: use(i.call_r.s); break;
			case call_m: use(i.call_m.s); break;
			case stdcall_r:
				use(i.stdcall_r.s);
				use(x86_64::to_bc_register(x86_64::Register64::rcx));
				use(x86_64::to_bc_register(x86_64::Register64::rdx));
				use(x86_64::to_bc_register(x86_64::Register64::r8));
				use(x86_64::to_bc_register(x86_64::Register64::r9));
				break;
			case xchg_r:  use(i.xchg_r .a); use(i.xchg_r .b); break;
			case xchg1_m: use(i.xchg1_m.a); use(i.xchg1_m.b); break;
			case xchg2_m: use(i.xchg2_m.a); use(i.xchg2_m.b); break;
			case xchg4_m: use(i.xchg4_m.a); use(i.xchg4_m.b); break;
			case xchg8_m: use(i.xchg8_m.a); use(i.xchg8_m.b); break;
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
			case jnz_cr: {
				switch (i.kind) {
					case jmp: break;
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
			case add_f32_f32:
			case add_f64_f64:
			case mul_f32_f32:
			case mul_f64_f64:
			case sub_f32_f32:
			case sub_f64_f64:
			case div_f32_f32:
			case div_f64_f64:
			case xor_ff:
			case tobool_r:
			case toboolnot_r:
			case push_used_registers:
			case pop_used_registers:
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

	for (auto &state : register_state) {
		if (state.setter && !state.was_used) {
			state.setter->kind = InstructionKind::noop;
			for (auto &modder : state.modders) {
				modder->kind = InstructionKind::noop;
			}
		}
	}
}
void Converter::propagate_known_values(InstructionList &instructions) {

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

	Value registers[(u8)Register::count] = {};

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
			case or_rm:
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
			case setf_mcc:
			case setb_mcc:
			case call_c:
			case call_r:
			case call_m:
			case stdcall_r:
			case xchg_r:
			case xchg1_m:
			case xchg2_m:
			case xchg4_m:
			case xchg8_m:
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
			case add_f32_f32:
			case add_f64_f64:
			case mul_f32_f32:
			case mul_f64_f64:
			case sub_f32_f32:
			case sub_f64_f64:
			case div_f32_f32:
			case div_f64_f64:
			case xor_ff:
			case tobool_r:
			case toboolnot_r:
			case push_used_registers:
			case pop_used_registers:
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

inline umm append(StringBuilder &builder, Register r) {
	switch (r) {
		using enum Register;
		case r0: return append(builder, "r0");
		case r1: return append(builder, "r1");
		case r2: return append(builder, "r2");
		case r3: return append(builder, "r3");
		case r4: return append(builder, "r4");
		case r5: return append(builder, "r5");
		case r6: return append(builder, "r6");
		case r7: return append(builder, "r7");
		case r8: return append(builder, "r8");
		case r9: return append(builder, "r9");
		case r10: return append(builder, "r10");
		case rs: return append(builder, "rs");
		case rb: return append(builder, "rb");
		case parameters: return append(builder, "parameters");
		case locals: return append(builder, "locals");
		case temporary: return append(builder, "tmp");
		case constants: return append(builder, "constants");
		case rwdata: return append(builder, "rwdata");
		case zeros: return append(builder, "zeros");
		case instructions: return append(builder, "instructions");
	}
	invalid_code_path();
	return {};
}
inline umm append(StringBuilder &builder, XRegister r) {
	using enum XRegister;
	switch (r) {
		case x0: return append(builder, "x0");
		case x1: return append(builder, "x1");
		case x2: return append(builder, "x2");
		case x3: return append(builder, "x3");
	}
	invalid_code_path();
	return {};
}
inline umm append(StringBuilder &builder, Address a) {
	umm result = 0;
	result += append(builder, '[');
	result += append(builder, a.base);
	if (a.r1_scale_index) {
		if (a.r2_scale) {
			invalid_code_path("not implemented");
		} else {
			result += append(builder, '+');
			result += append(builder, a.r1);
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

void print_bytecode(InstructionList &instructions) {
	auto replacer = Scoped(ConsoleColor::white);

	u64 idx = 0;
	for (auto i : instructions) {
		defer { idx++; };

		if (i.comment.data) {
			with(ConsoleColor::dark_cyan,
				split(i.comment, u8'\n', [&](auto part) {
					auto str = tformat("// {}\n", part);
					print(str);
				})
			);
		}

		print("{} ", Format(idx, align_left(4, ' ')));

		switch (i.kind) {
			using enum InstructionKind;
			case mov_rr: print("mov {}, {}", i.mov_rr.d, i.mov_rr.s); break;
			case mov_rc: print("mov {}, {}", i.mov_rc.d, i.mov_rc.s); break;

			case mov1_mc: print("mov1 {}, {}", i.mov1_mc.d, i.mov1_mc.s); break;
			case mov2_mc: print("mov2 {}, {}", i.mov2_mc.d, i.mov2_mc.s); break;
			case mov4_mc: print("mov4 {}, {}", i.mov4_mc.d, i.mov4_mc.s); break;
			case mov8_mc: print("mov8 {}, {}", i.mov8_mc.d, i.mov8_mc.s); break;

			case mov1_rm: print("mov1 {}, {}", i.mov1_rm.d, i.mov1_rm.s); break;
			case mov2_rm: print("mov2 {}, {}", i.mov2_rm.d, i.mov2_rm.s); break;
			case mov4_rm: print("mov4 {}, {}", i.mov4_rm.d, i.mov4_rm.s); break;
			case mov8_rm: print("mov8 {}, {}", i.mov8_rm.d, i.mov8_rm.s); break;

			case mov1_mr: print("mov1 {}, {}", i.mov1_mr.d, i.mov1_mr.s); break;
			case mov2_mr: print("mov2 {}, {}", i.mov2_mr.d, i.mov2_mr.s); break;
			case mov4_mr: print("mov4 {}, {}", i.mov4_mr.d, i.mov4_mr.s); break;
			case mov8_mr: print("mov8 {}, {}", i.mov8_mr.d, i.mov8_mr.s); break;

			case push_c: print("push {}", i.push_c.s); break;
			case push_r: print("push {}", i.push_r.s); break;
			case push_f: print("push {}", i.push_f.s); break;
			case push_m: print("push {}", i.push_m.s); break;

			case pop_r: print("pop {}", i.pop_r.d); break;
			case pop_f: print("pop {}", i.pop_f.d); break;

			case ret: print("ret"); break;

			case shl_rc: print("shl {}, {}", i.shl_rc.d, i.shl_rc.s); break;
			case shl_rr: print("shl {}, {}", i.shl_rr.d, i.shl_rr.s); break;
			case shl_rm: print("shl {}, {}", i.shl_rm.d, i.shl_rm.s); break;

			case shr_rc: print("shr {}, {}", i.shr_rc.d, i.shr_rc.s); break;
			case shr_rr: print("shr {}, {}", i.shr_rr.d, i.shr_rr.s); break;
			case shr_rm: print("shr {}, {}", i.shr_rm.d, i.shr_rm.s); break;

			case add_rc: print("add {}, {}", i.add_rc.d, i.add_rc.s); break;
			case add_rr: print("add {}, {}", i.add_rr.d, i.add_rr.s); break;
			case add_mc: print("add {}, {}", i.add_mc.d, i.add_mc.s); break;
			case add_mr: print("add {}, {}", i.add_mr.d, i.add_mr.s); break;

			case sub_rc: print("sub {}, {}", i.sub_rc.d, i.sub_rc.s); break;
			case sub_rr: print("sub {}, {}", i.sub_rr.d, i.sub_rr.s); break;
			case sub_mc: print("sub {}, {}", i.sub_mc.d, i.sub_mc.s); break;
			case sub_mr: print("sub {}, {}", i.sub_mr.d, i.sub_mr.s); break;

			case mul_rc: print("smul {}, {}", i.mul_rc.d, i.mul_rc.s); break;
			case mul_rr: print("smul {}, {}", i.mul_rr.d, i.mul_rr.s); break;
			case mul_rm: print("smul {}, {}", i.mul_rm.d, i.mul_rm.s); break;

			case div_rc: print("sdiv {}, {}", i.div_rc.d, i.div_rc.s); break;
			case div_rr: print("sdiv {}, {}", i.div_rr.d, i.div_rr.s); break;
			case div_rm: print("sdiv {}, {}", i.div_rm.d, i.div_rm.s); break;

			case mod_rc: print("mod {}, {}", i.mod_rc.d, i.mod_rc.s); break;
			case mod_rr: print("mod {}, {}", i.mod_rr.d, i.mod_rr.s); break;
			case mod_rm: print("mod {}, {}", i.mod_rm.d, i.mod_rm.s); break;

			case negi_r: print("neg {}", i.negi_r.d); break;
			case negi8_m:  print("neg1 {}", i.negi8_m.d); break;
			case negi16_m: print("neg2 {}", i.negi16_m.d); break;
			case negi32_m: print("neg4 {}", i.negi32_m.d); break;
			case negi64_m: print("neg8 {}", i.negi64_m.d); break;

			case or_rc: print("or {}, {}", i. or_rc.d, i. or_rc.s); break;
			case or_rr: print("or {}, {}", i. or_rr.d, i. or_rr.s); break;
			case or_mr: print("or {}, {}", i. or_mr.d, i. or_mr.s); break;

			case and_rc: print("and {}, {}", i.and_rc.d, i.and_rc.s); break;
			case and_rr: print("and {}, {}", i.and_rr.d, i.and_rr.s); break;
			case and_mc: print("and {}, {}", i.and_mc.d, i.and_mc.s); break;
			case and_mr: print("and {}, {}", i.and_mr.d, i.and_mr.s); break;

			case xor_rr: print("xor {}, {}"        , i.xor_rr.d, i.xor_rr.s); break;
			case xor_mr: print("xor qword {}, {}", i.xor_mr.d, i.xor_mr.s); break;

			case cmps1: print("cmps1 {}, {} {} {}", i.cmps1.d, i.cmps1.a, i.cmps1.c, i.cmps1.b); break;
			case cmps2: print("cmps2 {}, {} {} {}", i.cmps2.d, i.cmps2.a, i.cmps2.c, i.cmps2.b); break;
			case cmps4: print("cmps4 {}, {} {} {}", i.cmps4.d, i.cmps4.a, i.cmps4.c, i.cmps4.b); break;
			case cmps8: print("cmps8 {}, {} {} {}", i.cmps8.d, i.cmps8.a, i.cmps8.c, i.cmps8.b); break;
			case cmpu1: print("cmps1 {}, {} {} {}", i.cmpu1.d, i.cmpu1.a, i.cmpu1.c, i.cmpu1.b); break;
			case cmpu2: print("cmps2 {}, {} {} {}", i.cmpu2.d, i.cmpu2.a, i.cmpu2.c, i.cmpu2.b); break;
			case cmpu4: print("cmps4 {}, {} {} {}", i.cmpu4.d, i.cmpu4.a, i.cmpu4.c, i.cmpu4.b); break;
			case cmpu8: print("cmps8 {}, {} {} {}", i.cmpu8.d, i.cmpu8.a, i.cmpu8.c, i.cmpu8.b); break;

			case jmp: print("jmp {}", idx + i.jmp.offset); break;

			case jz_cr:  print("jz {}, {}",  idx + i.jz_cr .offset, i.jz_cr .reg); break;
			case jnz_cr: print("jnz {}, {}", idx + i.jnz_cr.offset, i.jnz_cr.reg); break;

				// Here move into rcx must be last, because it can be source for rdi or rsi
			case copyf_mmc: print("copyf {}, {}, {}", i.copyf_mmc.d, i.copyf_mmc.s, i.copyf_mmc.size); break;
			case copyb_mmc: print("copyb {}, {}, {}", i.copyb_mmc.d, i.copyb_mmc.s, i.copyb_mmc.size); break;
			case copyf_mmr: print("copyf {}, {}, {}", i.copyf_mmr.d, i.copyf_mmr.s, i.copyf_mmr.size); break;
			case copyb_mmr: print("copyb {}, {}, {}", i.copyb_mmr.d, i.copyb_mmr.s, i.copyb_mmr.size); break;

			case setf_mcc: print("setf {}, {}, {}", i.setf_mcc.d, i.setf_mcc.s, i.setf_mcc.size); break;
			case setb_mcc: print("setb {}, {}, {}", i.setf_mcc.d, i.setf_mcc.s, i.setf_mcc.size); break;

			case call_c: print("call {}", i.call_c.constant); break;
			case call_r: print("call {}", i.call_r.s); break;
			case call_m: print("call {}", i.call_m.s); break;

			case lea: print("lea {}, {}", i.lea.d, i.lea.s); break;

			case cvt_f32_s32: print("cvt_f32_s32"); break;
			case cvt_s32_f32: print("cvt_s32_f32"); break;

			case cvt_f64_s64: print("cvt_f64_s64"); break;
			case cvt_s64_f64: print("cvt_s64_f64"); break;

			case mov_fr: print("mov {}, {}", i.mov_fr.d, i.mov_fr.s); break;
			case mov_rf: print("mov {}, {}", i.mov_rf.d, i.mov_rf.s); break;

			case mov1_xm: print("mov1 {}, {}", i.mov1_xm.d, i.mov1_xm.s); break;
			case mov2_xm: print("mov2 {}, {}", i.mov2_xm.d, i.mov2_xm.s); break;
			case mov4_xm: print("mov4 {}, {}", i.mov4_xm.d, i.mov4_xm.s); break;
			case mov8_xm: print("mov8 {}, {}", i.mov8_xm.d, i.mov8_xm.s); break;

			case add_f32_f32: print("add {}, {}", i.add_f32_f32.d, i.add_f32_f32.s); break;
			case sub_f32_f32: print("sub {}, {}", i.sub_f32_f32.d, i.sub_f32_f32.s); break;
			case mul_f32_f32: print("mul {}, {}", i.mul_f32_f32.d, i.mul_f32_f32.s); break;
			case div_f32_f32: print("div {}, {}", i.div_f32_f32.d, i.div_f32_f32.s); break;

			case add_f64_f64: print("add {}, {}", i.add_f64_f64.d, i.add_f64_f64.s); break;
			case sub_f64_f64: print("sub {}, {}", i.sub_f64_f64.d, i.sub_f64_f64.s); break;
			case mul_f64_f64: print("mul {}, {}", i.mul_f64_f64.d, i.mul_f64_f64.s); break;
			case div_f64_f64: print("div {}, {}", i.div_f64_f64.d, i.div_f64_f64.s); break;

			case xor_ff: print("xor {}, {}", i.xor_ff.d, i.xor_ff.s); break;

			case tobool_r:    print("tobool {}", i.tobool_r.d); break;
			case toboolnot_r: print("toboolnot {}", i.toboolnot_r.d); break;
			case not_r: print("not {}", i.not_r.d); break;
			case not_m: print("not {}", i.not_m.d); break;
			case jmp_label: break;
			case noop: break;
			case debug_break: print("debug_break"s); break;

			case movsx21_rm: print("movsx21 {}, {}", i.movsx21_rm.d, i.movsx21_rm.s); break;
			case movsx41_rm: print("movsx41 {}, {}", i.movsx41_rm.d, i.movsx41_rm.s); break;
			case movsx81_rm: print("movsx81 {}, {}", i.movsx81_rm.d, i.movsx81_rm.s); break;
			case movsx42_rm: print("movsx42 {}, {}", i.movsx42_rm.d, i.movsx42_rm.s); break;
			case movsx82_rm: print("movsx82 {}, {}", i.movsx82_rm.d, i.movsx82_rm.s); break;
			case movsx84_rm: print("movsx84 {}, {}", i.movsx84_rm.d, i.movsx84_rm.s); break;

			case movzx21_rm: print("movzx21 {}, {}", i.movsx21_rm.d, i.movsx21_rm.s); break;
			case movzx41_rm: print("movzx41 {}, {}", i.movsx41_rm.d, i.movsx41_rm.s); break;
			case movzx81_rm: print("movzx81 {}, {}", i.movsx81_rm.d, i.movsx81_rm.s); break;
			case movzx42_rm: print("movzx42 {}, {}", i.movsx42_rm.d, i.movsx42_rm.s); break;
			case movzx82_rm: print("movzx82 {}, {}", i.movsx82_rm.d, i.movsx82_rm.s); break;
			case movzx84_rm: print("movzx84 {}, {}", i.movsx84_rm.d, i.movsx84_rm.s); break;

			case push_used_registers: print("push_used_registers"); break;
			case pop_used_registers: print("pop_used_registers"); break;
			case xchg_r: print("xchg {}, {}", i.xchg_r.a, i.xchg_r.b); break;
			case xchg1_m: print("xchg1 {}, {}", i.xchg1_m.a, i.xchg1_m.b); break;
			case xchg2_m: print("xchg2 {}, {}", i.xchg2_m.a, i.xchg2_m.b); break;
			case xchg4_m: print("xchg4 {}, {}", i.xchg4_m.a, i.xchg4_m.b); break;
			case xchg8_m: print("xchg8 {}, {}", i.xchg8_m.a, i.xchg8_m.b); break;

			case prepare_stack:      print("prepare_stack");      break;
			case debug_line:         print("debug_line");         break;
			case debug_start_lambda: print("debug_start_lambda"); break;


			default: print("unknown {}", (u64)i.kind); break;
		}
		with(ConsoleColor::gray, print(" // bytecode.cpp:{}\n", i.line));
	}
}

Bytecode build_bytecode() {
	timed_function(context.profiler);

	assert(context.general_purpose_register_count != 0);

	Bytecode result;

	auto _conv = new Converter();
	defer { delete _conv; };

	auto &conv = *_conv;

	for (auto lambda : context.lambdas_with_body) {
		conv.append(lambda, false, r0);
	}

	for (auto lambda : context.lambdas_without_body) {
		if (lambda->extern_library.data) {
			result.extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
		}
	}

	for_each(global_scope.statements, [&](auto statement) {
		conv.append(statement);
	});

	result.instructions = conv.builder;

	//print_bytecode(conv.builder);

	for (auto i : conv.instructions_that_reference_lambdas) {
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
	}

	return result;
}
