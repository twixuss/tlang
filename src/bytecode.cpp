#include "bytecode.h"
#include "ast.h"
#include "x86_64.h"

#define OPTIMIZE_BYTECODE 1

struct Relocation {
	umm instruction_index;
	AstLambda *lambda;
};

struct StringInfo {
	s64 data_and_size_offset;
	s64 string_offset;

	bool constant;
};

struct RegistersState {
	Optional<s64> state[(u32)Register::count];
	Optional<s64> &operator[](Register reg) { return state[(u8)reg]; }
};

struct StackState {
	inline static constexpr u64 unknown = (u64)-1;
	List<Optional<s64>> data;
	s64 cursor = 1; // included return address pushed by `call` instruction

	void push(Optional<s64> v) {
		if (cursor == unknown)
			return;
		data.resize(cursor + 1);
		data[cursor] = v;
		cursor++;
	}
	Optional<s64> pop() {
		if (cursor == unknown)
			return null_opt;
		assert(cursor);
		return data[--cursor];
	}
	// positive amount shrinks the stack,
	// negative - grows
	void offset(s64 amount) {
		if (cursor == unknown)
			return;
		if (amount > 0) {
			assert(cursor >= amount);
		}
		cursor -= amount;
	}
	Optional<s64> top() {
		if (cursor == unknown)
			return null_opt;
		return data[cursor-1];
	}

	void make_unknown() {
		cursor = unknown;
	}
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
	StackState stack_state = {};
	Scope *current_scope = 0;

	void init() {
		for (int i = 5; i < min((int)Register::r8, context.general_purpose_register_count); ++i) {
			available_registers.push((Register)i);
		}
	}
	void free() {
		// `body_builder` may be not freed, master builder will steal it anyway.
		tl::free(stack_state.data);
	}
};

struct Converter {
	InstructionList builder;
	StringBuilder constant_data_builder;
	StringBuilder data_builder;
	umm zero_data_size = 0;
	AstLambda *lambda = 0;

	List<Relocation> local_relocations;
	List<Relocation> global_relocations;

	List<StringInfo> constant_strings;

	ExternLibraries extern_libraries;

	LambdaState *ls;
};

Optional<Register> allocate_register(Converter &conv) {
	return conv.ls->available_registers.pop();
}
void free_register(Converter &conv, Register reg) {
	conv.ls->available_registers.push(reg);
}

using enum Register;
using enum XRegister;

s64 allocate_data(StringBuilder &conv, Span<u8> string) {
	auto result = conv.count();
	append_bytes(conv, string);
	return result;
}
s64 allocate_data(StringBuilder &conv, s64 count) {
	auto result = conv.count();
	while (count--)
		append_bytes(conv, '\0');
	return result;
}

s64 allocate_zero_data(Converter &conv, s64 byte_count) {
	auto result = conv.zero_data_size;
	conv.zero_data_size += byte_count;
	return result;
}

#if BYTECODE_DEBUG

void push_comment(Converter &conv, Span<utf8> string) {
	auto &back = conv.ls->body_builder.back();
	if (back.comment) {
		back.comment = (utf8 *)concatenate(as_span(back.comment), ';', string, '\0').data;
	} else {
		back.comment = null_terminate(string).data;
	}
}

#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind, .line=(u64)__LINE__,}

#else

#define push_comment(...)

#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind}

#endif


#define II(kind, ...) add_instruction(conv, MI(kind, __VA_ARGS__))
#define I(kind, ...) (&add_instruction(conv, MI(kind, __VA_ARGS__))->kind)

#if BYTECODE_DEBUG
void remove_last_instruction(Converter &conv) {
	auto removed = conv.ls->body_builder.pop_back();
	auto &back = conv.ls->body_builder.back();
	if (back.comment) {
		if (removed.comment) {
			back.comment = format(u8"{};{}\0"s, back.comment, removed.comment).data;
		}
	} else {
		back.comment = removed.comment;
	}
}
#else
void remove_last_instruction(Converter &conv) {
	conv.ls->body_builder.pop_back();
}
#endif

// If that address contains known value, return it.
// Use this function to optimize reads from stack memory.
Optional<s64> get_value_at(Converter &conv, Address addr) {
	auto &ls = *conv.ls;
	auto &stack_state = ls.stack_state;

	if (addr.base == rs) {
		if (addr.c % context.stack_word_size == 0) {
			auto offset = stack_state.cursor - 1 - addr.c / context.stack_word_size;
			if (0 <= offset && offset < stack_state.data.count) {
				return stack_state.data[offset];
			}
		}
	} else if (addr.base == rb) {
		if (addr.c % context.stack_word_size == 0) {
			auto offset = -addr.c / context.stack_word_size + 1;
			if (0 <= offset && offset < stack_state.data.count) {
				return stack_state.data[offset];
			}
		}
	}

	return {};
}

Instruction *add_instruction(Converter &conv, Instruction next) {
	auto &ls = *conv.ls;
	auto &stack_state = ls.stack_state;
	auto &register_state = ls.register_state;
	auto &body_builder = ls.body_builder;

	using enum InstructionKind;

	// keep track of the stack and registers
	switch (next.kind) {
		case push_c: {
			REDECLARE_REF(next, next.push_c);

			stack_state.push(next.s);
			break;
			break;
		}
		case push_r: {
			REDECLARE_REF(next, next.push_r);

			stack_state.push(register_state[next.s]);
			break;
		}
		case push_m: {
			REDECLARE_REF(next, next.push_m);
#if OPTIMIZE_BYTECODE
			auto value = get_value_at(conv, next.s);
			if (value) {
				return II(push_c, value.value_unchecked());
			}
#endif
			stack_state.push(null_opt);
			break;
		}
		case push_a:
		case push_d:
		case push_u:
		case push_t: {
			stack_state.push(null_opt);
			break;
		}
		case pop_r: {
			REDECLARE_REF(next, next.pop_r);

			register_state[next.d] = stack_state.pop();
			if (next.d == rs) {
				invalid_code_path("need to keep track of the stack here");
				stack_state.make_unknown();
			}

#if OPTIMIZE_BYTECODE
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
		case pop_m: {
			stack_state.pop();
		}
		case add_rc: {
			REDECLARE_REF(next, next.add_rc);

			if (next.s == 0)
				return 0;

			if (next.d == rs) {
				assert((next.s % context.stack_word_size) == 0);
				stack_state.offset(next.s / context.stack_word_size);
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

			if (next.s == 0)
				return 0;

			if (next.d == rs) {
				assert((next.s % context.stack_word_size) == 0);
				stack_state.offset(-next.s / context.stack_word_size);
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

			register_state[next.d] = next.s;
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
			auto value = get_value_at(conv, next.s);
			if (value) {
				return II(mov_rc, next.d, value.value_unchecked());
			}
			auto &back = body_builder.back();
			if (back.kind == lea) {
				REDECLARE_REF(back, back.lea);

				if (next.s.is(back.d)) {
					remove_last_instruction(conv);
					return II(mov8_rm, next.d, back.s);
				}
			}
#endif

			register_state[next.d].reset();
			break;
		}
	}

	// this does not use context.stack_word_size !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#if 0
	using enum InstructionKind;
	switch (next.kind) {
		case push_c: {
			conv.ls->stack_state.push(next.push_c.s);
			break;
		}
		case push_r: {
			conv.stack_state.push(conv.register_state[next.push_r.s]);
			break;
		}
		case push_m:
		case push_a:
		case push_d:
		case push_u:
		case push_t:
		case push_e: {
			conv.stack_state.push(null_opt);
			break;
		}
		case pop_r: {
			conv.register_state[next.pop_r.d] = conv.stack_state.pop();

			if (next.pop_r.d == rs) {
				conv.stack_state.make_unknown();
			}

			auto back = conv.body_builder->back();
			switch (back.kind) {
				case push_c:
					conv.body_builder->pop_back();
					return I(mov_rc, next.pop_r.d, back.push_c.s);
				case push_r:
					conv.body_builder->pop_back();
					return I(mov_rr, next.pop_r.d, back.push_r.s);
				case push_m:
					conv.body_builder->pop_back();
					return I(mov8_rm, next.pop_r.d, back.push_m.s);
				case add_mc:
					if (back.add_mc.d.is(rs)) {
						auto preback = conv.body_builder->end()[-2];
						if (preback.kind == push_r) {
							conv.body_builder->pop_back();
							conv.body_builder->pop_back();
							return I(lea, next.pop_r.d, preback.push_r.s + back.add_mc.s);
						}
					}
					break;
			}
			break;
		}
		case pop_m: {
			conv.stack_state.pop();
		}
		case add_rc: {
			if (next.add_rc.s == 0)
				return 0;

			if (next.add_rc.d == rs) {
				assert((next.add_rc.s % 8) == 0);
				conv.stack_state.offset(next.add_rc.s/8);
			}

			auto back = conv.body_builder->back();
			if (back.kind == add_rc) {
				if (next.add_rc.d == back.add_rc.d) {
					conv.body_builder->pop_back();
					return I(add_rc, next.add_rc.d, next.add_rc.s + back.add_rc.s);
				}
			}
			// else if (back.kind == push_c) {
			// 	if (next.add_rc.d == rs) {
			// 		conv.body_builder->pop_back();
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
				conv.stack_state.offset(-next.sub_rc.s/8);
			}

			auto back = conv.body_builder->back();
			switch (back.kind) {
				case sub_rc: {
					if (next.sub_rc.d == back.sub_rc.d) {
						conv.body_builder->pop_back();
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
			conv.register_state[next.mov_rc.d] = next.mov_rc.s;
			break;
		}
		case mov_rr: {
			conv.register_state[next.mov_rr.d] = conv.register_state[next.mov_rr.s];
			break;
		}
		case mov1_rm: {
			conv.register_state[next.mov1_rm.d].reset();
			break;
		}
		case mov2_rm: {
			conv.register_state[next.mov2_rm.d].reset();
			break;
		}
		case mov4_rm: {
			conv.register_state[next.mov4_rm.d].reset();
			break;
		}
		case mov8_rm: {
			if (next.mod_rm.s.r1_scale) {
				auto r = conv.register_state[next.mod_rm.s.r1];
				if (r.has_value()) {
					next.mod_rm.s.c += r.value_unchecked() * next.mod_rm.s.r1_scale;
					next.mod_rm.s.r1_scale = 0;
				}
			}

			if (next.mov8_rm.s.is(rs)) {
				auto top = conv.stack_state.top();
				if (top) {
					return I(mov_rc, next.mov8_rm.d, top.value_unchecked());
				}
			} else {
				conv.register_state[next.mov8_rm.d].reset();
			}

			auto back = conv.body_builder->back();
			if (back.kind == lea) {
				if (next.mov8_rm.s.is(back.lea.d)) { // address is exactly this register
					conv.body_builder->pop_back();
					next.mov8_rm.s = back.lea.s;
					//return I(mov8_rm, next.mov8_rm.d, back.lea.s);
				} else if (next.mov8_rm.s.base == back.lea.d && !next.mov8_rm.s.r1_scale && !next.mov8_rm.s.r2_scale) { // constant offset may be here
					conv.body_builder->pop_back();
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

	return &conv.ls->body_builder.add(next);
}

// if has enough registers, returns the value of an expression in them.
// empty if failed to allocate registers.
using ValueRegisters = StaticList<Register, 8>;

ValueRegisters value_registers(Register a) {
	ValueRegisters result;
	result.add(a);
	return result;
}

ValueRegisters value_registers(Register a, Register b) {
	ValueRegisters result;
	result.add(a);
	result.add(b);
	return result;
}

[[nodiscard]] static ValueRegisters append(Converter &, AstCall *);
[[nodiscard]] static ValueRegisters append(Converter &, AstIdentifier *);
[[nodiscard]] static ValueRegisters append(Converter &, AstLiteral *);
[[nodiscard]] static ValueRegisters append(Converter &, AstBinaryOperator *);
[[nodiscard]] static ValueRegisters append(Converter &, AstUnaryOperator *);
[[nodiscard]] static ValueRegisters append(Converter &, AstSubscript *);
[[nodiscard]] static ValueRegisters append(Converter &, AstCast *);
[[nodiscard]] static ValueRegisters append(Converter &, AstLambda *, bool push_address);
[[nodiscard]] static ValueRegisters append(Converter &, AstIfx *);

[[nodiscard]] static ValueRegisters append(Converter &conv, AstExpression *expression) {
	switch (expression->kind) {
		case Ast_identifier:           return append(conv, (AstIdentifier *)expression);
		case Ast_literal:              return append(conv, (AstLiteral *)expression);
		case Ast_call:                 return append(conv, (AstCall *)expression);
		case Ast_binary_operator:      return append(conv, (AstBinaryOperator*)expression);
		case Ast_unary_operator:       return append(conv, (AstUnaryOperator*)expression);
		case Ast_subscript:            return append(conv, (AstSubscript*)expression);
		case Ast_cast:                 return append(conv, (AstCast*)expression);
		case Ast_lambda:               return append(conv, (AstLambda*)expression, true);
		case Ast_ifx:                  return append(conv, (AstIfx*)expression);
		default: invalid_code_path();
	}
}

[[deprecated("Don't use 'append_to_stack'. Use registers directly.")]]
static void append_to_stack(Converter &conv, AstExpression *expression) {
	auto registers = append(conv, expression);
	if (registers.count) {
		// TODO: use these registers without pushing them to the stack
		for (auto r : reverse(registers)) {
			I(push_r, r);
			free_register(conv, r);
		}
	}
}

static void append(Converter &, AstDefinition *);
static void append(Converter &, AstReturn *);
static void append(Converter &, AstIf *);
static void append(Converter &, AstExpressionStatement *);
static void append(Converter &, AstWhile *);
static void append(Converter &, AstBlock *);

static void append(Converter &conv, AstStatement *statement) {
	switch (statement->kind) {
		case Ast_definition:           return append(conv, (AstDefinition *)statement);
		case Ast_return:               return append(conv, (AstReturn *)statement);
		case Ast_if:                   return append(conv, (AstIf*)statement);
		case Ast_expression_statement: return append(conv, (AstExpressionStatement*)statement);
		case Ast_while:                return append(conv, (AstWhile*)statement);
		case Ast_block:                return append(conv, (AstBlock*)statement);
		case Ast_defer: {
			// defer is appended later, after appending a block.
			auto Defer = (AstDefer *)statement;
			Defer->scope.parent->bytecode_defers.add(Defer);
			return;
		}
		case Ast_assert:
		case Ast_print:
		case Ast_import:
		case Ast_test:                 return;
		default: invalid_code_path();
	}
}

/*
	string :: struct {
		data : *void;
		count : uint;
	}

	0
			....
	global data:
global0 ->	data
			count
global1 ->	data
			count
global2 ->	data
			count
			....
	stack:
			....
	local2->data <- rs (grows ^^^)
			count
	local1->data
			count
	local0->data
			count
			old rb <- rb
			return address
	arg2 ->	data
			count
	arg1 ->	data
			count
	arg0 ->	data
			count
			....
	ffff
*/

static void push_address_of(Converter &conv, AstExpression *expression);

#include <optional>

static Optional<Register> load_address_of(Converter &conv, AstExpression *expression);

// loads address into a register if there is one available and returns it
// or pushes the address onto the stack and returns empty optional
static Optional<Register> load_address_of(Optional<Register> destination, Converter &conv, AstExpression *expression) {
	push_comment(conv, format(u8"load_address_of {}", expression->location));
	switch (expression->kind) {
		case Ast_lambda: {
		push_address_of_lambda:

			auto lambda = (AstLambda *)expression;
			if (!destination)
				destination = allocate_register(conv);

			if (lambda->has_body) {
				// TODO: YIELD!!!!!!
				assert(lambda->location_in_bytecode != -1);
				if (destination) I(mov_rt, destination.value_unchecked(), lambda->location_in_bytecode);
				else             I(push_t, lambda->location_in_bytecode);
			} else {
				assert((s64)(s32)lambda->definition->name.count == (s64)lambda->definition->name.count);
				if (destination) {
					I(mov_re, destination.value_unchecked(), lambda->definition->name.data, (s32)lambda->definition->name.count);
				} else {
					I(mov_re, r0, lambda->definition->name.data, (s32)lambda->definition->name.count);
					I(push_r, r0);
				}
			}
			return destination;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition;

			if (definition->expression && definition->expression->kind == Ast_lambda) {
				expression = definition->expression;
				goto push_address_of_lambda;

			} else {
				if (!destination)
					destination = allocate_register(conv);
				s64 definition_size = ceil(get_size(definition->type), context.stack_word_size);

				if (definition->parent_block) {
					if (definition->parent_block->kind == Ast_lambda) {
						auto parent_lambda = (AstLambda *)definition->parent_block;

						s64 offset = 0;

						// ret0
						// ret1
						// ret2
						// ret3
						// arg0
						// arg1
						// arg2
						// arg3
						// return address
						// rbp <- rbp
						// local0
						// local1
						// local2
						// local3

						s64 const stack_base_register_size = context.stack_word_size;
						s64 const return_address_size = context.stack_word_size;
						s64 const parameters_end_offset = stack_base_register_size + return_address_size + parent_lambda->parameters_size;
						s64 const return_parameters_start_offset = parameters_end_offset;

						if (definition->is_parameter) {
							// Function Parameter
							// Arguments are pushed from left to right
							offset = parameters_end_offset - definition_size - definition->bytecode_offset;
						} else if (definition->is_return_parameter) {
							// Return parameter
							offset = return_parameters_start_offset;
						} else {
							// Local
							offset = -definition_size - definition->bytecode_offset;
						}

						if (destination) {
							I(lea, destination.value_unchecked(), rb + offset);
						} else {
							I(push_r, rb);
							I(add_mc, rs, offset);
						}
					} else {
						invalid_code_path();
					}
				} else {

					// Global constants and variables

					//
					// TODO_OFFSET: Remove this AND PIECE ABOVE after
					// It would be better to get rid of append here
					// by calculating global variables' offsets at typecheck time
					//
					if (definition->bytecode_offset == INVALID_DATA_OFFSET) {
						append(conv, definition);
						assert(definition->bytecode_offset != INVALID_DATA_OFFSET);
					}
					if (definition->is_constant) {
						if (destination) I(mov_ra, destination.value_unchecked(), definition->bytecode_offset);
						else             I(push_a, definition->bytecode_offset);
					} else {
						if (definition->expression) {
							if (destination) I(mov_rd, destination.value_unchecked(), definition->bytecode_offset);
							else             I(push_d, definition->bytecode_offset);
						} else {
							if (destination) I(mov_ru, destination.value_unchecked(), definition->bytecode_offset);
							else             I(push_u, definition->bytecode_offset);
						}
					}
				}
				return destination;
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			assert(binop->operation == dot);
			assert(binop->right->kind == Ast_identifier);
			auto offset = ((AstIdentifier *)binop->right)->definition->offset_in_struct;
			assert(offset != INVALID_MEMBER_OFFSET);
			auto destination = load_address_of(conv, binop->left);
			if (offset) {
				if (destination) I(add_rc, destination.value_unchecked(), offset);
				else             I(add_mc, rs, offset);
			}
			return destination;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			auto element_size = get_size(subscript->type);

			if (is_pointer(subscript->expression->type)) {
				append_to_stack(conv, subscript->expression);
				append_to_stack(conv, subscript->index_expression);
				I(pop_r, r0);
				I(mul_rc, r0, element_size);
				I(add_mr, rs, r0);
				return {};
			} else {
				// TODO: this will not work with complex expression indexing
				auto destination = load_address_of(conv, subscript->expression);
				assert(element_size);

				constexpr auto temp_r = r0;

				append_to_stack(conv, subscript->index_expression);
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
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;
			assert(unop->operation == '*');
			append_to_stack(conv, unop->expression);
			return {}; // right now result is always on the stack
		}
		default:
			invalid_code_path("loading address of that type of expression is not implemented");
	}
	invalid_code_path("value was not returned");
}

static Optional<Register> load_address_of(Converter &conv, AstExpression *expression) {
	return load_address_of({}, conv, expression);
}


// :PUSH_ADDRESS:
// Eventually this function should be replaced with load_address_of.
// Pushing address onto the stack all the time is not a got thing.
static void push_address_of(Converter &conv, AstExpression *expression) {
#if 1
	auto address = load_address_of(conv, expression);
	if (address) {
		I(push_r, address.value_unchecked());
	}
	return;
#else
	push_comment(conv, format(u8"push_address_of {}", expression->location));
	switch (expression->kind) {
		case Ast_lambda: {
		push_address_of_lambda:

			auto lambda = (AstLambda *)expression;
			if (lambda->has_body) {
				// TODO: YIELD!!!!!!
				assert(lambda->location_in_bytecode != -1);
				I(push_t, lambda->location_in_bytecode);
			} else {
				I(mov_re, r0, lambda->definition->name);
				I(push_r, r0);
			}
			break;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition;

			if (definition->expression && definition->expression->kind == Ast_lambda) {
				expression = definition->expression;
				goto push_address_of_lambda;

			} else {
				s64 size = get_size(definition->type);

				if (definition->parent_block) {
					if (definition->parent_block->kind == Ast_lambda) {
						auto parent_lambda = (AstLambda *)definition->parent_block;

						s64 offset = 0;

						if (definition->is_parameter) {
							// Function Parameter
							offset = definition->bytecode_offset + 16; // skip 16 bytes of rb and return address
						} else if (definition->is_return_parameter) {
							// Return parameter
							offset = 16 + parent_lambda->parameters_size;
						} else {
							// Local
							offset = -(definition->bytecode_offset + ceil(size, 8ll));
						}


						I(push_r, rb);
						I(add_mc, rs, offset);
					} else {
						invalid_code_path();
					}
				} else {

					// Global constants and variables

					//
					// TODO_OFFSET: Remove this AND PIECE ABOVE after
					// It would be better to get rid of append here
					// by calculating global variables' offsets at typecheck time
					//
					if (definition->bytecode_offset == INVALID_DATA_OFFSET) {
						append(conv, definition);
						assert(definition->bytecode_offset != INVALID_DATA_OFFSET);
					}
					if (definition->is_constant) {
						I(push_a, definition->bytecode_offset);
					} else {
						if (definition->expression) {
							I(push_d, definition->bytecode_offset);
						} else {
							I(push_u, definition->bytecode_offset);
						}
					}
				}
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			assert(binop->operation == dot);
			push_address_of(conv, binop->left);
			assert(binop->right->kind == Ast_identifier);
			auto offset = ((AstIdentifier *)binop->right)->definition->offset_in_struct;
			assert(offset != INVALID_MEMBER_OFFSET);
			if (offset) {
				I(add_mc, rs, offset);
			}
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			push_address_of(conv, subscript->expression);

			append(conv, subscript->index_expression);
			I(pop_r, r0);

			auto element_size = get_size(subscript->type);
			assert(element_size);
			I(mul_rc, r0, element_size);

			I(add_mr, rs, r0);

			break;
		}
		default:
			invalid_code_path();
	}
#endif
}

// if source      is not provided, it is popped from the stack
// if destination is not provided, it is popped from the stack
// So if both adresses are on the stack, first you should push destination, then source
static void append_memory_copy(Converter &conv, Optional<Register> _dst, Optional<Register> _src, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	if (bytes_to_copy == 0)
		return;

	push_comment(conv, format(u8"copy {} bytes from {} into {}, reverse={}"s, bytes_to_copy, from_name, to_name, reverse));

	// r0, r1 and r2 are not allocatable, so we can use them
	auto src = _src ? _src.value_unchecked() : (I(pop_r, r0), r0);
	auto dst = _dst ? _dst.value_unchecked() : (I(pop_r, r1), r1);

	if (bytes_to_copy <= context.register_size) {
		switch (bytes_to_copy) {
			case 1:
			case 2:
			case 4:
			case 8: {
				constexpr auto tmp = r2;
				if (bytes_to_copy == 8) {
					I(mov8_rm, tmp, src);
					I(mov8_mr, dst, tmp);
				} else if (bytes_to_copy == 4) {
					I(mov4_rm, tmp, src);
					I(mov4_mr, dst, tmp);
				} else if (bytes_to_copy == 2) {
					I(mov2_rm, tmp, src);
					I(mov2_mr, dst, tmp);
				} else if (bytes_to_copy == 1) {
					I(mov1_rm, tmp, src);
					I(mov1_mr, dst, tmp);
				}
				return;
			}
			default:
				break;
		}
	}

	// TODO: FIXME: BUG: it is likely that the same register will be used for different things here
	if (reverse) {
		I(copyb_mmc, dst, src, bytes_to_copy);
	} else {
		I(copyf_mmc, dst, src, bytes_to_copy);
	}
}

static void append_memory_copy_a(Converter &conv, Address dst, Address src, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	if (bytes_to_copy == 0)
		return;

	push_comment(conv, format(u8"copy {} bytes from {} into {}, reverse={}"s, bytes_to_copy, from_name, to_name, reverse));

	static constexpr auto intermediary = r0;

	assert(dst.base != intermediary);
	if (dst.r1_scale_index != 0) assert(dst.r1 != intermediary);
	if (dst.r2_scale != 0) assert(dst.r2 != intermediary);
	assert(src.base != intermediary);
	if (src.r1_scale_index != 0) assert(src.r1 != intermediary);
	if (src.r2_scale != 0) assert(src.r2 != intermediary);

	switch (bytes_to_copy) {
		case 1:
		case 2:
		case 4:
		case 8: {
			if (bytes_to_copy == 8) {
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
			invalid_code_path("not implemented");
			break;
		}
	}
}

//
// Expects pointers to destination and source on the stack
// First you should push destination, then source
// Pops the addresses
static void append_memory_copy(Converter &conv, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	append_memory_copy(conv, Optional<Register>{}, Optional<Register>{}, bytes_to_copy, reverse, from_name, to_name);
}

static void append_memory_set(Converter &conv, Address d, s64 s, s64 size) {
	s64 i = 0;
	while (size > 0) {
		switch (context.stack_word_size) {
			case 8:
				I(mov8_mc, d + i, s);
				size -= 8;
				i += 8;
				break;
			case 4:
				I(mov4_mc, d + i, s);
				size -= 4;
				i += 4;
				break;
		}
	}
	assert(size == 0);
}

static void push_zeros(Converter &conv, s64 size) {
	// on x86-64 memset is taking 17 bytes
	// and push 0 is taking 2 bytes
	// so we can do 8 pushes and still be smaller than using memset

	// TODO: figure out the threshold for x86 32 bit
	auto const threshold = 8*8;
	if (size > threshold) {
		I(sub_rc, rs, size);
		assert((s64)(s32)size == size);
		I(set_mcc, rs, 0, (s32)size);
	} else {
		auto remaining_bytes = size;
		while (remaining_bytes > 0) {
			I(push_c, 0);
			remaining_bytes -= context.stack_word_size;
		}
		assert(remaining_bytes == 0);
	}
}

static void ensure_present_in_bytecode(Converter &conv, AstLambda *lambda) {
	if (lambda->location_in_bytecode == -1) {
		assert_always(append(conv, lambda, false).count == 0);
	}
	assert(lambda->location_in_bytecode != -1);
}

static void append(Converter &conv, Scope &scope) {
	auto old_scope = conv.ls->current_scope;
	conv.ls->current_scope = &scope;
	defer { conv.ls->current_scope = old_scope; };
	for (auto statement : scope.statements) {
		append(conv, statement);
	}
	for (auto Defer : reverse(scope.bytecode_defers)) {
		append(conv, Defer->scope);
	}
}

static void append(Converter &conv, AstDefinition *definition) {
	if (definition->built_in)
		return;

	// TODO_OFFSET:
	// Remove this after
	if (definition->bytecode_offset != INVALID_DATA_OFFSET) {
		return;
	}

	auto definition_size = get_size(definition->type);

	// Don't do anything for constant definitions in lambdas
	if (definition->parent_block && definition->parent_block->kind != Ast_struct && definition->is_constant) {
		return;
	}

	if (definition->expression && definition->expression->kind == Ast_lambda) {
		assert_always(append(conv, (AstLambda *)definition->expression, false).count == 0);
		return;
	}

	if (definition->expression && is_type(definition->expression))
		return;

	if (definition->parent_block) {
		if (definition->parent_block->kind == Ast_lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_block;
			push_comment(conv, format(u8"definition {}", definition->name));
			assert(!definition->is_parameter);

			auto size = ceil(definition_size, context.stack_word_size);
			assert(size);

			definition->bytecode_offset = parent_lambda->offset_accumulator;
			parent_lambda->offset_accumulator += size;

			if (definition->expression) {
				if (definition->expression->type == type_noinit) {
					I(sub_rc, rs, size);
				} else {
					auto expression_registers = append(conv, definition->expression);
					if (expression_registers.count) {
						for (auto r : reverse(expression_registers)) {
							I(push_r, r);
							free_register(conv, r);
						}
					}
				}
			} else {
				push_zeros(conv, size);
			}
		} else {
			invalid_code_path();
		}
	} else {
		if (definition->is_constant) {
			if (definition->expression) {
				auto literal = (AstLiteral *)get_literal(definition->expression);


				switch (literal->literal_kind) {
					case LiteralKind::integer: {
						definition->bytecode_offset = allocate_data(conv.constant_data_builder, value_as_bytes((s64)literal->integer));
						break;
					}
					case LiteralKind::string: {
						invalid_code_path("not implemented");
						s64 string_size;
						switch (context.register_size) {
							case 4: string_size = 8; break;
							case 8: string_size = 16; break;
						}
						auto offset = allocate_data(conv.constant_data_builder, string_size);
						definition->bytecode_offset = offset;
						conv.constant_strings.add({offset, literal->string_data_offset, true});
						break;
					}
					default:
						invalid_code_path();
				}
			} else {
				definition->bytecode_offset = allocate_data(conv.constant_data_builder, get_size(definition->type));
			}
		} else {
			if (definition->expression) {
				definition->bytecode_offset = allocate_data(conv.data_builder, value_as_bytes((s64)get_constant_integer(definition->expression).value()));
			} else {
				definition->bytecode_offset = allocate_zero_data(conv, definition_size);
			}
		}
	}
}
static void append(Converter &conv, AstReturn *ret) {
	push_comment(conv, u8"return"s);

	auto lambda = ret->lambda;

	if (ret->expression) {
		auto expression_registers = append(conv, ret->expression);
		if (expression_registers.count) {
			auto size = get_size(ret->expression->type);

			// :PUSH_ADDRESS: TODO: replace this with load_address_of
			if (size <= 8) {
				assert(context.stack_word_size == 8); // TODO: implement for x86 32 bit
				I(mov8_mr, rb+(context.stack_word_size*2+lambda->parameters_size), expression_registers[0]);
			} else {
				invalid_code_path("not implemented");
			}

			for (auto r : expression_registers) {
				free_register(conv, r);
			}
		} else {
			auto size = get_size(ret->expression->type);

			// :PUSH_ADDRESS: TODO: replace this with load_address_of
			if (size <= 8) {
				I(pop_r, r0);

				assert(context.stack_word_size == 8); // TODO: implement for x86 32 bit
				I(mov8_mr, rb+(16+lambda->parameters_size), r0);
			} else {
				// destination
				I(push_r, rb);
				I(add_mc, rs, context.stack_word_size*2 + lambda->parameters_size);

				// source
				I(push_r, rs);
				I(add_mc, rs, context.stack_word_size);

				append_memory_copy(conv, size, false, u8"expression"s, u8"parameter"s);
			}
		}
	}

	Scope *scope = conv.ls->current_scope;
	while (scope) {
		for (auto Defer : reverse(scope->bytecode_defers)) {
			append(conv, Defer->scope);
		}
		scope = scope->parent;
	}

	auto jump_index = (s64)count_of(conv.ls->body_builder);
	auto return_jump = II(jmp, 0);

	lambda->return_jumps.add({return_jump, jump_index});
}
static void append(Converter &conv, AstIf *If) {
#if 0
	if (If->is_constant) {
		// constant if's statements were brought outside already by the typechecker. No need to append it.
		return;
	}
#else
	if (If->is_constant) {
		// NOTE: constant if's scope is not merged into it's parent.
		auto scope = If->true_branch_was_taken ? &If->true_scope : &If->false_scope;
		for (auto statement : scope->statements) {
			append(conv, statement);
		}
		return;
	}
#endif

	auto start_offset = conv.lambda->offset_accumulator;

	append_to_stack(conv, If->condition);

	I(pop_r, r0);
	auto jz = I(jz_cr, 0, r0);

	auto true_start = count_of(conv.ls->body_builder);
	append(conv, If->true_scope);

	auto end_offset_true = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size_true = end_offset_true - start_offset;

	I(add_rc, rs, allocated_size_true);
	auto jmp = I(jmp, .offset=0);

	conv.lambda->offset_accumulator = start_offset;

	auto false_start = count_of(conv.ls->body_builder);

	// :DUMMYNOOP:
	// also acts as optimization barrier
	II(noop)->labeled = true;

	append(conv, If->false_scope);

	auto end_offset_false = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size_false = end_offset_false - start_offset;

	I(add_rc, rs, allocated_size_false);
	auto false_end = count_of(conv.ls->body_builder);

	// :DUMMYNOOP:
	// Next instruction after else block must be labeled,
	// but we don't have it yet. So we add a noop so we can label it.
	II(noop)->labeled = true;

	jz->offset = false_start - true_start + 1;
	jmp->offset = false_end - false_start + 1;
}
static void append(Converter &conv, AstWhile *While) {
	auto start_offset = conv.lambda->offset_accumulator;

	auto count_before_condition = count_of(conv.ls->body_builder);
	append_to_stack(conv, While->condition);

	I(pop_r, r0);
	auto jz = I(jz_cr, 0, r0);
	auto count_after_condition = count_of(conv.ls->body_builder);

	append(conv, While->scope);

	auto end_offset = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size = end_offset - start_offset;

	I(add_rc, rs, allocated_size);
	auto count_after_body = count_of(conv.ls->body_builder);
	I(jmp, .offset=0)->offset = (s64)count_before_condition - (s64)count_after_body;

	conv.ls->body_builder[count_before_condition].labeled = true;

	// :DUMMYNOOP:
	// Next instruction after else block must be labeled,
	// but we don't have it yet. So we add a noop so we can label it.
	II(noop)->labeled = true;


	jz->offset = (s64)count_after_body - (s64)count_after_condition + 2;
}
static void append(Converter &conv, AstBlock *block) {
	push_comment(conv, u8"block"s);

	auto start_offset = conv.lambda->offset_accumulator;

	append(conv, block->scope);

	auto end_offset = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size = end_offset - start_offset;

	I(add_rc, rs, allocated_size);
}
static void append(Converter &conv, AstExpressionStatement *es) {
	append_to_stack(conv, es->expression);
	switch (es->expression->kind) {
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)es->expression;
			using enum BinaryOperation;
			if (
				bin->operation == ass ||
				bin->operation == addass ||
				bin->operation == subass ||
				bin->operation == mulass ||
				bin->operation == divass ||
				bin->operation == modass ||
				bin->operation == borass ||
				bin->operation == bandass ||
				bin->operation == bxorass ||
				bin->operation == bslass ||
				bin->operation == bsrass
			) {
				// these do not push anyting
				return;
			}
			break;
		}

		case Ast_call: {
			// discard return value
			auto call = (AstCall *)es->expression;
			auto size = ceil(get_size(call->type), context.stack_word_size);
			if (size) {
				I(add_rc, rs, size);
			}
			return;
		}
		case Ast_import:
			return;
	}

	invalid_code_path();
}
static ValueRegisters append(Converter &conv, AstBinaryOperator *bin) {
	push_comment(conv, format(u8"binary {}"s, operator_string(bin->operation)));

	auto left = bin->left;
	auto right = bin->right;

	using enum BinaryOperation;
	if (bin->operation == dot) {
		switch (right->kind) {
			case Ast_identifier: {
				auto Struct = get_struct(left->type);
				if (Struct) {
					auto struct_size = get_size(left->type);


					assert(right->kind == Ast_identifier);
					auto ident = (AstIdentifier *)right;
					auto member = ident->definition;
					assert(member);
					auto member_size = get_size(member->type);

					// assert(struct_size % stack_word_size == 0);
					// assert(member_size % stack_word_size == 0);

					if (member->is_constant) {
						invalid_code_path("not implemented");
						I(push_c, member->bytecode_offset);
					} else {
						// The plan is simple:
						// 1. Reserve space for eventual value
						// 2. Append the struct
						// 3. Copy the member from struct to reserved space
						// 4. Remove struct from the stack
						assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);

						push_comment(conv, u8"1. Reserve space for eventual value"s);
						I(sub_rc, rs, ceil(member_size, context.stack_word_size));

						push_comment(conv, u8"2. Append the struct"s);
						append_to_stack(conv, left);

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

							push_comment(conv, u8"3. Copy the member from struct to reserved space"s);
							I(push_r, rs); // destination
							I(add_mc, rs, ceil(struct_size, context.stack_word_size));

							I(push_r, rs); // source
							I(add_mc, rs, context.stack_word_size + member->offset_in_struct);

							append_memory_copy(conv, member_size, true, bin->location, u8"stack"s);

							push_comment(conv, u8"4. Remove struct from the stack"s);
							I(add_rc, rs, ceil(struct_size, context.stack_word_size));
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
				append_to_stack(conv, left);
				append_to_stack(conv, right);

				auto lt = direct(bin->left->type);

				if (lt == type_f32) {
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
				} else if (lt == type_f64) {
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
					assert(::is_integer(bin->type));
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

				auto dst_opt = load_address_of(conv, left);

				if (dst_opt) {
					auto dst = dst_opt.value_unchecked();
					auto right_registers = append(conv, right);
					if (right_registers.count) {
						// TODO: use these registers without pushing them to the stack
						for (auto r : reverse(right_registers)) {
							I(push_r, r);
							free_register(conv, r);
						}
					}
					append_memory_copy(conv, dst, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					free_register(conv, dst);

					push_comment(conv, u8"remove right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size));
				} else {
					append_to_stack(conv, right);

					 // load dest address
					switch (context.register_size) {
						case 8: I(mov8_rm, r0, rs + ceil(bytes_to_write, context.stack_word_size)); break;
						case 4: I(mov4_rm, r0, rs + ceil(bytes_to_write, context.stack_word_size)); break;
					}

					append_memory_copy(conv, r0, rs, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

					push_comment(conv, u8"remove left address and right from the stack"s);
					I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size) + context.stack_word_size);
				}

				break;


				// BTW this code is unreachable

				// :PUSH_ADDRESS: TODO: Replace this with load_address_of
				push_address_of(conv, left); // destination address

				I(push_r, rs); // source address
				I(add_mc, rs, context.stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(conv, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write, context.stack_word_size));

				// Finish this thing that uses registers
#if 0
				auto destination_address = load_address_of(conv, left); // destination address
				if (destination_address) {
					REDECLARE_VAL(destination_address, destination_address.value_unchecked());

					auto source_address = allocate_register(conv);
					if (source_address) {
						REDECLARE_VAL(source_address, source_address.value_unchecked());

						append_memory_copy(conv, destination_address, source_address, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses
					} else {

					}

					free_register(conv, destination_address);
				} else {
				}

				I(push_r, rs); // source address
				I(add_mc, rs, context.stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(conv, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

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
				append_to_stack(conv, left);
				append_to_stack(conv, right);
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
				I(push_r, r2); // left
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
				append_to_stack(conv, right);

				auto destination_address_opt = load_address_of(conv, left);
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
}
static ValueRegisters append(Converter &conv, AstIdentifier *identifier) {
	push_comment(conv, format(u8"load identifer {}", identifier->name));

	auto definition = identifier->definition;
	assert(definition->bytecode_offset != -1);

	if (definition->expression && definition->expression->kind == Ast_lambda) {
		// :PUSH_ADDRESS: TODO: Replace this with load_address_of
		push_address_of(conv, identifier);
		return {};
	} else {
		auto size = get_size(identifier->type); // NOT definition->type because it can be unsized (not hardened)
		assert(size);

		if (size <= context.register_size) {
			auto addr = load_address_of(conv, identifier);
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
#if 0
				// debug_break();
				return value_registers(addr);
#else
				I(push_r, addr);
				// runtime crashes with this TODO fix
				// free_register(conv, addr);
				return {};
#endif
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
			push_address_of(conv, identifier);
			append_memory_copy(conv, size, false, identifier->location, u8"stack"s);
			return {};
		}
	}
	invalid_code_path();
}
static ValueRegisters append(Converter &conv, AstCall *call) {
	push_comment(conv, format(u8"call '{}'", call->callable->location));

	assert(call->callable->type->kind == Ast_lambda);
	auto lambda = (AstLambda *)call->callable->type;

	if (lambda->is_intrinsic) {
		auto name = lambda->definition->name;
		if (name == "debug_break") {
			I(debug_break);
		} else {
			invalid_code_path("Unknown intrinsic");
		}
		return {};
	}

	assert(context.stack_word_size == 8);

	if (lambda->has_body)
		ensure_present_in_bytecode(conv, lambda);

	auto arguments = get_arguments(call);
	bool lambda_is_constant = is_constant(call->callable);

	assert(lambda_is_constant);

	auto start_stack_size = conv.ls->stack_state.cursor;
	defer {
		// ensure we have the right amount of data on the stack
		auto expected_size = ceil(get_size(lambda->return_parameter->type), 8ll);
		auto actual_size = (conv.ls->stack_state.cursor - start_stack_size) * 8;
		assert(expected_size == actual_size);
	};

	//if (lambda->definition->name == u8"print_string"s)
	//	debug_break();

	switch (lambda->convention) {
		case CallingConvention::tlang: {
			if (!lambda->has_body) {
				immediate_error(lambda->location, "Unexpected lambda with no body");
				assert(lambda->has_body);
			}

			//
			// Calculate sizes of all things that go on the stack - lambda pointer, return value and arguments
			//
			s64 bytes_pushed_before_call = 0;

			s64 return_parameters_size_on_stack = ceil(get_size(call->type), 8ll);
			bytes_pushed_before_call += return_parameters_size_on_stack;

			s64 arguments_size_on_stack = lambda->parameters_size;
			bytes_pushed_before_call += arguments_size_on_stack;

			if (!lambda_is_constant) {
				bytes_pushed_before_call += 8; // lamda address will be pushed
			}


			bool stack_was_realigned = false;
			if ((conv.ls->stack_state.cursor * 8 + bytes_pushed_before_call) % 16 != 0) {
				push_comment(conv, u8"align the stack to 16 bytes"s);
				I(sub_rc, rs, 8);
				stack_was_realigned = true;
			}

			push_comment(conv, u8"reserve space for return value"s);
			I(sub_rc, rs, return_parameters_size_on_stack);

			for (auto argument : arguments) {
				append_to_stack(conv, argument);
			}

			if (lambda_is_constant) {
				I(call_c, lambda->location_in_bytecode, lambda);
				I(add_rc, rs, arguments_size_on_stack);
				if (stack_was_realigned) {
					append_memory_copy_a(conv, rs+8, rs, return_parameters_size_on_stack, true, u8"16 aligned stack"s, u8"16 unaligned stack"s);
					I(add_rc, rs, 8);
				}
			} else {
				invalid_code_path("not implemented");
				append_to_stack(conv, call->callable);
			}

			break;
		}
		case CallingConvention::stdcall: {
			using namespace x86_64;

			s64 const shadow_space_size = 32;

			// Sum all the sizes so we know if we need to align the stack
			s64 bytes_pushed_before_call = 0;

			bytes_pushed_before_call += lambda->parameters_size;

			// duplicated arguments (4th, 5th and so on)
			if (lambda->parameters.count > 4)
				bytes_pushed_before_call += 8 * (lambda->parameters.count - 4);

			 // shadow space
			bytes_pushed_before_call += 32;



			bool stack_was_realigned = false;
			if ((conv.ls->stack_state.cursor * 8 + bytes_pushed_before_call) % 16 != 0) {
				push_comment(conv, u8"align the stack to 16 bytes"s);
				I(sub_rc, rs, 8);
				stack_was_realigned = true;
			}



			for (auto argument : arguments) {
				assert(get_size(argument->type) <= 8);
				append_to_stack(conv, argument);
			}

			// we have this:
			//
			// STACK:
			// arg0
			// arg1
			// arg2
			// arg3
			// arg4
			// arg5 <- rs aligned to 16
			//
			// we need to get this:
			// REGISTERS:
			// arg0: rcx or xmm0
			// arg1: rdx or xmm1
			// arg2: r8  or xmm2
			// arg3: r9  or xmm3
			//
			// STACK:
			// arg5
			// arg4
			// shadow
			// shadow
			// shadow
			// shadow <- rsp aligned to 16
			//

			auto move_arg = [&](int arg_index, s64 stack_offset) {
		 		if (lambda->parameters.count > arg_index)
					if (::is_float(lambda->parameters[arg_index]->type))
						I(mov8_xm, stdcall_float_registers[arg_index], rs + (lambda->parameters_size - stack_offset));
					else
						I(mov8_rm, to_bc_register(stdcall_int_registers[arg_index]), rs + (lambda->parameters_size - stack_offset));
			};

		 	move_arg(0,  8);
			move_arg(1, 16);
			move_arg(2, 24);
			move_arg(3, 32);

			s64 offset = 0;
			for (s64 i = 4; i < lambda->parameters.count; ++i) {
				I(push_m, rs + offset);
				offset += 16;
			}

			push_comment(conv, u8"reserve shadow space"s);
			I(sub_rc, rs, 32);

			auto function_address_register = to_bc_register(Register64::rax);
			load_address_of(function_address_register, conv, call->callable);
			I(call_r, function_address_register);

			push_comment(conv, u8"remove shadow space"s);
			I(add_rc, rs, 32);
			if (lambda->parameters.count > 4) {
				push_comment(conv, u8"remove argument duplicates"s);
				I(add_rc, rs, ((s64)lambda->parameters.count - 4) * 8);
			}

			push_comment(conv, u8"remove original arguments"s);
			I(add_rc, rs, (s64)lambda->parameters.count * 8);

			if (stack_was_realigned) {
				push_comment(conv, u8"restore stack before alignment"s);
				I(add_rc, rs, 8);
			}

			if (!types_match(call->type, type_void)) {
				assert(get_size(lambda->return_parameter->type) > 0);
				I(push_r, to_bc_register(Register64::rax));
			}

			break;
		}
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstLiteral *literal) {
	if (literal->literal_kind == LiteralKind::string)
		push_comment(conv, format(u8"literal \"{}\"", escape_string(literal->string)));
	else
		push_comment(conv, format(u8"literal {}", literal->location));

	assert(literal->type != type_unsized_integer);
	assert(literal->type != type_unsized_float);
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
	// 				auto data = allocate_data(conv.constant_data_builder, as_bytes((Span<utf8>)literal->string));
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
	// 				free_register(conv, destination);
	// 				goto fallback_to_stack_string;
	// 			}
	// 			break;
	// 		}
	// 		case character:
	// 			I(mov_rc, destination, literal->character);
	// 			break;
	// 		case Float:
	// 			push_comment(conv, format(u8"float {}", literal->Float));
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
	// 			if (dtype == type_u8 ||
	// 				dtype == type_s8)
	// 				I(mov_rc, destination, (u8)literal->integer);
	// 			else if (dtype == type_u16 ||
	// 					 dtype == type_s16)
	// 				I(mov_rc, destination, (u16)literal->integer);
	// 			else if (dtype == type_u32 ||
	// 					 dtype == type_s32)
	// 				I(mov_rc, destination, (u32)literal->integer);
	// 			else if (dtype == type_u64 ||
	// 					 dtype == type_s64 ||
	// 					 dtype == type_pointer_to_void)
	// 				I(mov_rc, destination, (s64)literal->integer);
	// 			else if (dtype == type_f32) {
	// 				auto f = (f32)(s64)literal->integer;
	// 				I(mov_rc, destination, *(s32 *)&f);
	// 			} else if (dtype == type_f64) {
	// 				auto f = (f64)(s64)literal->integer;
	// 				I(mov_rc, destination, *(s64 *)&f);
	// 			}
	// 			else if (literal->type->kind == Ast_unary_operator && ((AstUnaryOperator *)literal->type)->operation == '*')
	// 				I(mov_rc, destination, (s64)literal->integer);
	// 			else invalid_code_path();
	// 			break;
	// 		}
	// 	}
	// 	ValueRegisters result;
	// 	result.add(destination);
	// 	return result;
	// }

	switch (literal->literal_kind) {
		case noinit:
			I(sub_rc, rs, ceil(get_size(literal->type), context.stack_word_size));
			break;
		case string: {
			fallback_to_stack_string:
			// TODO: deduplicate strings

			I(push_c, (s64)literal->string.count);

			auto data = allocate_data(conv.constant_data_builder, as_bytes((Span<utf8>)literal->string));
			I(push_a, data);

			literal->string_data_offset = data;
			break;
		}
		case character:
			I(push_c, literal->character);
			break;
		case Float:
			push_comment(conv, format(u8"float {}", literal->Float));

			switch (get_size(literal->type)) {
				case 4: I(push_c, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
				case 8: I(push_c, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
				default: invalid_code_path();
			}
			break;
		case boolean:
			I(push_c, (u8)literal->Bool);
			break;
		case integer: {
			if (dtype == type_u8 ||
				dtype == type_s8)
				I(push_c, (u8)literal->integer);
			else if (dtype == type_u16 ||
					 dtype == type_s16)
				I(push_c, (u16)literal->integer);
			else if (dtype == type_u32 ||
					 dtype == type_s32)
				I(push_c, (u32)literal->integer);
			else if (dtype == type_u64 ||
					 dtype == type_s64 ||
					 dtype == type_pointer_to_void)
				I(push_c, (s64)literal->integer);
			else if (dtype == type_f32) {
				auto f = (f32)(s64)literal->integer;
				I(push_c, *(s32 *)&f);
			} else if (dtype == type_f64) {
				auto f = (f64)(s64)literal->integer;
				I(push_c, *(s64 *)&f);
			}
			else if (literal->type->kind == Ast_unary_operator && ((AstUnaryOperator *)literal->type)->operation == '*')
				I(push_c, (s64)literal->integer);
			else invalid_code_path();
			break;
		}
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstUnaryOperator *unop) {
	push_comment(conv, format(u8"unary '{}'", operator_string(unop->operation)));
	switch (unop->operation) {
		case '-': {
			append_to_stack(conv, unop->expression);
			auto size = get_size(unop->type);
			if (::is_integer(unop->type)) {
				switch (size) {
					case 1: I(negi8_m,  rs); break;
					case 2: I(negi16_m, rs); break;
					case 4: I(negi32_m, rs); break;
					case 8: I(negi64_m, rs); break;
					default: invalid_code_path();
				}
			} else if (::is_float(unop->type)) {
				switch (size) {
					case 4:
						I(pop_f, x0);
						I(mov_rc, r0, (s64)0x8000'0000);
						I(mov_fr, x1, r0);
						I(xor_ff, x0, x1);
						I(push_f, x0);
						break;
					case 8:
						I(pop_f, x0);
						I(mov_rc, r0, (s64)0x8000'0000'0000'0000);
						I(mov_fr, x1, r0);
						I(xor_ff, x0, x1);
						I(push_f, x0);
						break;
					default: invalid_code_path();
				}
			} else {
				invalid_code_path();
			}

			break;
		}
		case '&': {
			// :PUSH_ADDRESS: TODO: Replace this with load_address_of
			push_address_of(conv, unop->expression);
			break;
		}
		case '*': {
			auto size = ceil(get_size(unop->type), context.stack_word_size);
			I(sub_rc, rs, size);
			I(push_r, rs);
			append_to_stack(conv, unop->expression);
			append_memory_copy(conv, size, false, unop->expression->location, u8"stack"s);
			break;
		}
		case '!': {
			append_to_stack(conv, unop->expression);
			I(pop_r, r0);
			I(toboolnot_r, r0);
			I(push_r, r0);
			break;
		}
		default: {
			invalid_code_path();
			break;
		}
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstSubscript *subscript) {
	push_comment(conv, format(u8"subscript"));
	auto element_size = get_size(subscript->type);
	assert(element_size);

	// TODO: order of evaluation matters

	if (is_power_of_2(element_size) && element_size <= 8) {
		auto index_register = r0;
		auto base_register = r1;

		append_to_stack(conv, subscript->index_expression);

		Optional<Register> addr_opt;
		if (::is_pointer(subscript->expression->type)) {
			append_to_stack(conv, subscript->expression);
			I(pop_r, base_register);
		} else {
			addr_opt = load_address_of(conv, subscript->expression);
			if (addr_opt) {
				base_register = addr_opt.value_unchecked();
				free_register(conv, base_register);
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
		append_to_stack(conv, subscript->index_expression);
		if (::is_pointer(subscript->expression->type)) {
			append_to_stack(conv, subscript->expression);
		} else {
			// :PUSH_ADDRESS: TODO: Replace this with load_address_of
			push_address_of(conv, subscript->expression);
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
		append_memory_copy(conv, element_size, false, subscript->location, u8"stack"s);
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstCast *cast) {
	push_comment(conv, format(u8"cast from '{}' to '{}'", type_to_string(cast->expression->type), type_to_string(cast->type)));

	append_to_stack(conv, cast->expression);

	if (is_pointer_internally(cast->expression->type) && is_pointer_internally(cast->type)) {
		return {};
	}

	AstStruct *from = 0;
	AstStruct *to = 0;

	if (is_pointer_internally(cast->expression->type))
		from = type_u64;
	else
		from = get_struct(cast->expression->type);

	if (is_pointer_internally(cast->type))
		to = type_u64;
	else
		to = get_struct(cast->type);


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
	} else if (from == type_u8) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) { I(and_mc, rs, 0xff); } // discard bits that could be garbage
		else if (to == type_u32) { I(and_mc, rs, 0xff); }
		else if (to == type_u64) { I(and_mc, rs, 0xff); }
		else if (to == type_s8) {}
		else if (to == type_s16) { I(and_mc, rs, 0xff); }
		else if (to == type_s32) { I(and_mc, rs, 0xff); }
		else if (to == type_s64) { I(and_mc, rs, 0xff); }
		else invalid_code_path();
	} else if (from == type_u16) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) { I(and_mc, rs, 0xffff); }
		else if (to == type_u64) { I(and_mc, rs, 0xffff); }
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) { I(and_mc, rs, 0xffff); }
		else if (to == type_s64) { I(and_mc, rs, 0xffff); }
		else invalid_code_path();
	} else if (from == type_u32) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) {}
		else if (to == type_u64) { I(and_mc, rs, 0xffffffff); }
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) {}
		else if (to == type_s64) { I(and_mc, rs, 0xffffffff); }
		else invalid_code_path();
	} else if (from == type_u64) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) {}
		else if (to == type_u64) {}
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) {}
		else if (to == type_s64) {}
		else invalid_code_path();
	} else if (from == type_s8) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); } // discard bits that could be garbage
		else if (to == type_u32) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); }
		else if (to == type_u64) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); }
		else if (to == type_s8) {}
		else if (to == type_s16) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); }
		else if (to == type_s32) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); }
		else if (to == type_s64) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); }
		else invalid_code_path();
	} else if (from == type_s16) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); }
		else if (to == type_u64) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); }
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); }
		else if (to == type_s64) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); }
		else invalid_code_path();
	} else if (from == type_s32) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) {}
		else if (to == type_u64) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); }
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) {}
		else if (to == type_s64) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); }
		else if (to == type_f32) { I(cvt_s32_f32); }
		else invalid_code_path();
	} else if (from == type_s64) {
		if (false) {}
		else if (to == type_u8) {}
		else if (to == type_u16) {}
		else if (to == type_u32) {}
		else if (to == type_u64) {}
		else if (to == type_s8) {}
		else if (to == type_s16) {}
		else if (to == type_s32) {}
		else if (to == type_s64) {}
		else if (to == type_f64) { I(cvt_s64_f64); }
		else invalid_code_path();
	} else if (from == type_f64) {
		if (false) {}
		else if (to == type_s64) { I(cvt_f64_s64); }
		else invalid_code_path();
	}
	else invalid_code_path();
	return {};
}
static ValueRegisters append(Converter &conv, AstLambda *lambda, bool push_address) {

	s64 parameter_size_accumulator = 0;
	for (auto parameter : lambda->parameters) {
		parameter->bytecode_offset = parameter_size_accumulator;
		parameter_size_accumulator += ceil(get_size(parameter->type), context.stack_word_size);
	}
	lambda->parameters_size = parameter_size_accumulator;

	if (lambda->has_body) {
		if (types_match(lambda->return_parameter->type, type_type)) {
			if (push_address) {
				// TODO: this should work at runtime in the future
				invalid_code_path("can't push address of lambda that returns a type");
			}
			return {};
		}

		LambdaState ls;
		ls.init();

		ls.current_scope = &lambda->body_scope;

		auto old_ls = conv.ls;
		conv.ls = &ls;
		defer {
			conv.ls = old_ls;
			ls.free();
		};

		auto old_lambda = conv.lambda;
		conv.lambda = lambda;
		defer { conv.lambda = old_lambda; };

		lambda->first_instruction = &conv.ls->body_builder.add(MI(noop));
		lambda->first_instruction->labeled = true;

		if (lambda->definition) {
			push_comment(conv, format(u8"lambda {}", lambda->definition->name));
		} else {
			push_comment(conv, format(u8"lambda {}", where(lambda->location.data)));
		}

		auto return_value_size = ceil(get_size(lambda->return_parameter->type), context.stack_word_size);

		//if (lambda->definition->name == u8"print_string"s)
		//	debug_break();

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
						I(push_f, stdcall_float_registers[arg_index]);
					else
						I(push_r, to_bc_register(stdcall_int_registers[arg_index]));
			};

			push_comment(conv, u8"reserve space for return value"s);
			I(sub_rc, rs, return_value_size);

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
					I(push_m, rs + offset);
					offset += 16;
				}
			}
			push_comment(conv, u8"dummy return address"s);
			I(sub_rc, rs, 8);
		}

		I(push_r, rb);
		I(mov_rr, rb, rs);

		push_comment(conv, u8"zero out the return value"s);
		append_memory_set(conv, rb + context.stack_word_size * 2 + lambda->parameters_size, 0, return_value_size);

		append(conv, lambda->body_scope);

		lambda->return_location = count_of(conv.ls->body_builder);

		for (auto i : lambda->return_jumps) {
			auto offset = lambda->return_location - i.index;
			if (offset == 1) {
				i.jmp->kind = InstructionKind::noop; // TODO: this instruction can be removed. but i don't know if it should be.
			} else {
				i.jmp->jmp.offset = offset;
			}
		}

		II(mov_rr, rs, rb)->labeled = true;
		I(pop_r, rb);

		if (lambda->convention == CallingConvention::stdcall) {
			push_comment(conv, u8"pop dummy return address and arguments"s);
			I(add_rc, rs, context.stack_word_size + (s64)lambda->parameters.count * context.stack_word_size);
			push_comment(conv, u8"put return value into rax"s);
			I(pop_r, x86_64::to_bc_register(x86_64::Register64::rax));
		}
		I(ret);

		lambda->location_in_bytecode = count_of(conv.builder);
#if 1
		add_steal(&conv.builder, &conv.ls->body_builder);
#else
		add(&conv.builder, conv.ls->body_builder);
#endif

		for (auto relocation : conv.local_relocations) {
			relocation.instruction_index += lambda->location_in_bytecode;
			conv.global_relocations.add(relocation);
		}
		conv.local_relocations.clear();
	} else if (lambda->extern_library.data) {
		conv.extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
	}

	if (push_address) {
		// :PUSH_ADDRESS: TODO: Replace this with load_address_of
		push_address_of(conv, lambda);
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstIfx *If) {
	auto condition = append(conv, If->condition);
	decltype(Instruction::jz_cr) *jz = 0;
	if (condition.count) {
		assert(condition.count == 1);
		jz = I(jz_cr, 0, condition[0]);
		free_register(conv, condition[0]);
	} else {
		I(pop_r, r0);
		jz = I(jz_cr, 0, r0);
	}

	auto initial_stack_size = conv.ls->stack_state.cursor;

	auto count_before_true = count_of(conv.ls->body_builder);

	// NOTE: Right now there is no way to force an expression to be in specific registers.
	// So true and false expressions may be in different registers and right now there
	// is no way to prevent this without pushing everyting to the stack.
	// Maybe this is not a problem if optimization is good enough.
	auto true_expression = append(conv, If->true_expression);
	for (auto r : reverse(true_expression)) {
		I(push_r, r);
		free_register(conv, r);
	}

	auto jmp = I(jmp, 0);
	auto count_after_true = count_of(conv.ls->body_builder);

	conv.ls->stack_state.cursor = initial_stack_size;

	auto false_expression = append(conv, If->false_expression);
	for (auto r : reverse(false_expression)) {
		I(push_r, r);
		free_register(conv, r);
	}

	auto count_after_false = count_of(conv.ls->body_builder);

	conv.ls->body_builder[count_after_true].labeled = true;

	// :DUMMYNOOP:
	// Next instruction after else block must be labeled,
	// but we don't have it yet. So we add a noop so we can label it.
	II(noop)->labeled = true;

	jz->offset = count_after_true - count_before_true + 1;
	jmp->offset = count_after_false - count_after_true + 1;
	return {};
}

void fix_relocations(InstructionList &instructions, List<Relocation> relocations) {
	timed_function(context.profiler);
	for (auto &r : relocations) {
		instructions[r.instruction_index].call_c.constant = r.lambda->location_in_bytecode;
	}

}

Bytecode build_bytecode() {
	timed_function(context.profiler);

	assert(context.general_purpose_register_count != 0);

	Bytecode result;

	auto _conv = new Converter;
	defer { delete _conv; };

	auto &conv = *_conv;

	for_each(global_scope.statements, [&](auto statement) {
		append(conv, statement);
	});

	result.instructions = conv.builder;
	result.constant_data = (List<u8>)to_string(conv.constant_data_builder);
	result.data = (List<u8>)to_string(conv.data_builder);
	result.zero_data_size = conv.zero_data_size;
	result.extern_libraries = conv.extern_libraries;

	fix_relocations(result.instructions, conv.global_relocations);


	// print_bytecode(result.instructions);

	return result;

	sizeof(Address);
	sizeof(Instruction);
	sizeof(Instruction::mov_re);
	sizeof(Instruction::mov8_mc);
}
