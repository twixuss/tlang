#include "bytecode.h"
#include "ast.h"
#include "x86_64.h"

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
	Optional<KnownValue> state[(u32)Register::count];
	Optional<KnownValue> &operator[](Register reg) { return state[(u8)reg]; }
};

struct StackState {
	List<Optional<KnownValue>> data;
	s64 cursor = -1;
	s64 rb_offset = -1;
	s64 max_cursor = -1;

	void init(s64 return_type_size) {
		// include return value and return address pushed by `call` instruction
		rb_offset = cursor = ceil(return_type_size, 8ll) / 8 + 1;
	}

	void push(Optional<KnownValue> v) {
		data.resize(cursor + 1);
		data[cursor] = v;
		cursor++;
		max_cursor = max(max_cursor, cursor);
	}
	void push(Optional<s64> v) {
		if (v) {
			push(KnownValue{
				.kind = KnownValueKind::constant,
				.constant = v.value_unchecked()
			});
		} else {
			push(Optional<KnownValue>{});
		}
	}
	void push_unknown() {
		push(Optional<KnownValue>{});
	}
	Optional<KnownValue> pop() {
		assert(cursor);
		return data[--cursor];
	}
	// amount in bytes
	// positive amount shrinks the stack,
	// negative grows
	void offset(s64 amount) {
		assert(amount % 8 == 0);
		amount /= 8;
		if (amount > 0) {
			assert(cursor >= amount);
		}
		cursor -= amount;
		max_cursor = max(max_cursor, cursor);
		data.resize(cursor);
	}
	Optional<KnownValue> top() {
		return data[cursor-1];
	}
	// If that address contains known value, return it.
	// Use this function to optimize reads from stack memory.
	Optional<KnownValue> *get_value_address_at(Address addr) {
		if (addr.base == rs) {
			if (addr.c % context.stack_word_size == 0) {
				auto offset = cursor - 1 - addr.c / context.stack_word_size;
				if (0 <= offset && offset < data.count) {
					return &data[offset];
				}
			}
		} else if (addr.base == rb) {
			if (addr.c % context.stack_word_size == 0) {
				auto offset = rb_offset - addr.c / context.stack_word_size;
				if (0 <= offset && offset < data.count) {
					return &data[offset];
				}
			}
		}

		return 0;
	}
	Optional<KnownValue> get_value_at(Address addr) {
		auto address = get_value_address_at(addr);
		if (address)
			return *address;
		return {};
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
	decltype(Instruction::push_used_registers) *push_used_registers = 0;
	decltype(Instruction::pop_used_registers) *pop_used_registers = 0;
	u64 used_registers_mask = 0;
	List<Instruction *> parameter_load_offsets;
	s64 temporary_size = 0;
	s64 temporary_cursor = 0;
	Instruction *temporary_reserver = 0;
	List<Instruction *> local_references;

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
		// `body_builder` may be not freed, master builder will steal it anyway.
		tl::free(stack_state.data);
	}
};

struct InstructionThatReferencesLambda {
	Instruction *instruction;
	AstLambda *lambda;
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

	List<InstructionThatReferencesLambda> instructions_that_reference_lambdas;

	String comment;

	bool dont_care_about_definition_spacing = false;
};

Optional<Register> allocate_register(Converter &conv) {
	auto r = conv.ls->available_registers.pop();
	if (r.has_value())
		conv.ls->used_registers_mask |= 1 << (u64)r.value_unchecked();
	return r;
}
void free_register(Converter &conv, Register reg) {
	conv.ls->available_registers.push(reg);
}

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

void set_comment(Instruction *i, Span<utf8> string) {
	i->comment = string;
}

void push_comment(Converter &conv, Span<utf8> string) {
	if (conv.comment.data) {
		conv.comment = (String)concatenate(as_span(conv.comment), '\n', string);
	} else {
		conv.comment = (String)string;
	}
}

#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind, .line=(u64)__LINE__,}

#else

#define set_comment(...)
#define push_comment(...)

#define MI(_kind, ...) {._kind={__VA_ARGS__}, .kind = InstructionKind::_kind}

#endif


#define II(kind, ...) add_instruction(conv, MI(kind, __VA_ARGS__))
#define I(kind, ...) (&add_instruction(conv, MI(kind, __VA_ARGS__))->kind)

#if BYTECODE_DEBUG
void remove_last_instruction(Converter &conv) {
	auto removed = conv.ls->body_builder.pop_back();
	auto &back = conv.ls->body_builder.back();
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
	conv.ls->body_builder.pop_back();
}
#endif

Instruction *add_instruction(Converter &conv, Instruction next) {
#if BYTECODE_DEBUG
	next.comment = conv.comment;
	conv.comment = {};
#endif

	auto &ls = *conv.ls;
	auto &stack_state = ls.stack_state;
	auto &register_state = ls.register_state;
	auto &body_builder = ls.body_builder;

	using enum InstructionKind;

	// keep track of the stack and registers
	switch (next.kind) {
		case jmp_label:
		case jmp: {
			for (auto &val : stack_state.data) {
				val = null_opt;
			}
			for (auto &reg : register_state.state) {
				reg = null_opt;
			}
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

			if (next.s == 0)
				return 0;

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

			if (next.s == 0)
				return 0;

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
			conv.stack_state.push_unknown();
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
				conv.stack_state.offset(next.add_rc.s);
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
				conv.stack_state.offset(-next.sub_rc.s);
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

[[nodiscard]] static ValueRegisters append(Converter &, AstCall *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstIdentifier *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstLiteral *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstBinaryOperator *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstUnaryOperator *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstSubscript *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstLambda *, bool push_address, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstIfx *, Optional<Address> = {});
[[nodiscard]] static ValueRegisters append(Converter &, AstPack*, Optional<Address> = {});

// TODO: make destination address not optional
[[nodiscard]] static ValueRegisters append(Converter &conv, AstExpression *expression, Optional<Address> destination = {}) {
	switch (expression->kind) {
		case Ast_identifier:           return append(conv, (AstIdentifier *)expression, destination);
		case Ast_literal:              return append(conv, (AstLiteral *)expression, destination);
		case Ast_call:                 return append(conv, (AstCall *)expression, destination);
		case Ast_binary_operator:      return append(conv, (AstBinaryOperator*)expression, destination);
		case Ast_unary_operator:       return append(conv, (AstUnaryOperator*)expression, destination);
		case Ast_subscript:            return append(conv, (AstSubscript*)expression, destination);
		case Ast_lambda:               return append(conv, (AstLambda*)expression, true, destination);
		case Ast_ifx:                  return append(conv, (AstIfx*)expression, destination);
		case Ast_pack:                 return append(conv, (AstPack*)expression, destination);
		default: invalid_code_path();
	}
}

[[deprecated("Don't use append_to_stack. Use append instead.")]]
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
static void append(Converter &, AstAssert *);

static void append(Converter &conv, AstStatement *statement) {
	switch (statement->kind) {
		case Ast_definition:           return append(conv, (AstDefinition *)statement);
		case Ast_return:               return append(conv, (AstReturn *)statement);
		case Ast_if:                   return append(conv, (AstIf*)statement);
		case Ast_expression_statement: return append(conv, (AstExpressionStatement*)statement);
		case Ast_while:                return append(conv, (AstWhile*)statement);
		case Ast_block:                return append(conv, (AstBlock*)statement);
		case Ast_operator_definition:      append(conv, ((AstOperatorDefinition*)statement)->lambda, false); return;
		case Ast_defer: {
			// defer is appended later, after appending a block.
			auto Defer = (AstDefer *)statement;
			Defer->scope.parent->bytecode_defers.add(Defer);
			return;
		}
		case Ast_assert: return append(conv, (AstAssert *)statement);
		case Ast_print:
		case Ast_import:
		case Ast_parse:
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

[[deprecated("Don't use push_address_of. Use load_address_of instead.")]]
static void push_address_of(Converter &conv, AstExpression *expression);

#include <optional>

static Optional<Register> load_address_of(Converter &conv, AstExpression *expression);
static Optional<Register> load_address_of(Converter &conv, AstDefinition *definition);

static Optional<Register> load_address_of(Optional<Register> destination, Converter &conv, AstDefinition *definition) {
	if (!destination)
		destination = allocate_register(conv);
	s64 definition_size = ceil(get_size(definition->type), context.stack_word_size);

	if (definition->parent_lambda_or_struct) {
		if (definition->parent_lambda_or_struct->kind == Ast_lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;

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

			assert(parent_lambda->parameters_size != -1);
			s64 const stack_base_register_size = context.stack_word_size;
			s64 const return_address_size = context.stack_word_size;
			s64 const parameters_end_offset = stack_base_register_size + return_address_size + parent_lambda->parameters_size;
			s64 const return_parameters_start_offset = parameters_end_offset;

			// if (definition->name == "uMsg")
			// 	debug_break();

			// local 0
			// ...
			// local n
			// return value space
			// temporaries
			// pointer to return value
			// argument 0
			// ...
			// argument n
			// return address
			// saved registers
			// rb

			Instruction *offset_instr = 0;

			if (definition->is_return_parameter) {
				if (destination) {
					REDECLARE_REF(destination, destination.value_unchecked());
					offset_instr = II(lea, destination, rb + return_parameters_start_offset);
				} else {
					offset_instr = II(lea, r0, rb + return_parameters_start_offset);
					I(push_r, r0);
				}
				// Due to saving caller's register BEFORE pushing rb, we don't know the offsets of parameters, because
				// we don't know beforehand how much registers will be allocated in this lambda. So we need to record every load
				// of parameter and patch the offset later.
				conv.ls->parameter_load_offsets.add(offset_instr);
				return destination;
			}
			if (definition->is_parameter) {
				auto offset = parameters_end_offset - 8 - definition->bytecode_offset;
				auto addrreg = destination ? destination.value_unchecked() : r0;

				offset_instr = II(lea, addrreg, rb + offset);
				if (get_size(definition->type) > context.stack_word_size) {
					I(mov8_rm, addrreg, addrreg);
				}
				if (!destination) {
					I(push_r, addrreg);
				}

				conv.ls->parameter_load_offsets.add(offset_instr);
				return destination;
			}

			// local definition
			auto offset = -definition_size - definition->bytecode_offset;

			if (destination) {
				offset_instr = II(lea, destination.value_unchecked(), rb + offset);
			} else {
				I(push_r, rb);
				offset_instr = II(add_mc, rs, offset);
			}
			conv.ls->local_references.add(offset_instr);
			return destination;
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
			if (definition->name == "glBindBuffer")
				debug_break();
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
				Instruction *instr = 0;
				if (destination) instr = II(mov_rt, destination.value_unchecked(), -1);
				else             instr = II(push_t, -1);
				if (lambda->has_body) {
					conv.instructions_that_reference_lambdas.add({.instruction=instr, .lambda=lambda});
				}
			} else {
				assert((s64)(s32)count_of(lambda->definition->name) == (s64)count_of(lambda->definition->name));
				if (destination) {
					I(mov_re, destination.value_unchecked(), (String)lambda->definition->name);
				} else {
					I(mov_re, r0, (String)lambda->definition->name);
					I(push_r, r0);
				}
			}
			return destination;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition();

			if (definition->expression && definition->expression->kind == Ast_lambda) {
				expression = definition->expression;
				goto push_address_of_lambda;

			} else {
				return load_address_of(conv, definition);
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			assert(binop->operation == dot);
			assert(binop->right->kind == Ast_identifier);
			auto right = (AstIdentifier *)binop->right;
			auto offset = right->definition()->offset_in_struct;
			assert(offset != INVALID_MEMBER_OFFSET);
			auto destination = is_pointer(binop->left->type)
				? (append_to_stack(conv, binop->left), Optional<Register>{})
				: load_address_of(conv, binop->left);
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
			assert(unop->operation == UnaryOperation::dereference);
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

static Optional<Register> load_address_of(Converter &conv, AstDefinition *definition) {
	return load_address_of({}, conv, definition);
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

				if (definition->parent_lambda_or_struct) {
					if (definition->parent_lambda_or_struct->kind == Ast_lambda) {
						auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;

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
static void append_memory_copy(Converter &conv, Optional<Register> dst, Optional<Register> src, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	if (bytes_to_copy == 0)
		return;

	push_comment(conv, format(u8"copy {} bytes from {} into {}, reverse={}"s, bytes_to_copy, from_name, to_name, reverse));

	// Load addresses into registers if they weren't already
	if (src) {
		if (dst) {
			// Address are passed in registers. Nothing to do.
		} else {
			// Load destination address into register different from source address register.
			dst = src.value_unchecked() == r0 ? r1 : r0;
			I(pop_r, dst.value_unchecked());
		}
	} else {
		if (dst) {
			// Load source address into register different from destination address register.
			src = dst.value_unchecked() == r0 ? r1 : r0;
			I(pop_r, src.value_unchecked());
		} else {
			// Pop both addresses into different registers.
			src = (I(pop_r, r0), r0);
			dst = (I(pop_r, r1), r1);
		}
	}

	{
		REDECLARE_REF(src, src.value());
		REDECLARE_REF(dst, dst.value());

		if (bytes_to_copy <= context.register_size) {
			constexpr auto tmp = r2;
			switch (bytes_to_copy) {
				case 1:
					I(mov1_rm, tmp, src);
					I(mov1_mr, dst, tmp);
					return;
				case 2:
					I(mov2_rm, tmp, src);
					I(mov2_mr, dst, tmp);
					return;
				case 4:
					I(mov4_rm, tmp, src);
					I(mov4_mr, dst, tmp);
					return;
				case 8:
					I(mov8_rm, tmp, src);
					I(mov8_mr, dst, tmp);
					return;
			}
		}

		if (reverse) {
			I(copyb_mmc, dst, src, bytes_to_copy);
		} else {
			I(copyf_mmc, dst, src, bytes_to_copy);
		}
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
			assert(src.base != r0);
			assert(dst.base != r1);
			if (reverse) {
				I(copyb_mmc, dst, src, bytes_to_copy);
			} else {
				I(copyf_mmc, dst, src, bytes_to_copy);
			}
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

static void append_memory_set(Converter &conv, Address d, s64 s, s64 size, bool reverse) {
	if (reverse)
		I(setb_mcc, d, (s32)s, (s32)size);
	else
		I(setf_mcc, d, (s32)s, (s32)size);
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
		append_memory_set(conv, rs, 0, size, false);
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
	scoped_replace(conv.ls->current_scope, &scope);
	for (auto statement : scope.statements) {
		// if (statement->uid() == 1826)
		// 	debug_break();
		//if (statement->location == "glGenBuffers = @ wglGetProcAddress(\"glGenBuffers\\0\".data)")
		//	debug_break();

		push_comment(conv, (Span<utf8>)format("==== {}: {} ====", where(statement->location.data), statement->location));

		auto cursor_before = conv.ls->stack_state.cursor;
		defer {
			auto cursor_after = conv.ls->stack_state.cursor;

			switch (statement->kind) {
				case Ast_definition:
				case Ast_return:
					break;
				default:
					assert(cursor_before == cursor_after);
					break;
			}
		};

		conv.ls->temporary_cursor = 0;
		defer {
			conv.ls->temporary_size = max(conv.ls->temporary_size, conv.ls->temporary_cursor);
		};

		append(conv, statement);
	}
	for (auto Defer : reverse(scope.bytecode_defers)) {
		append(conv, Defer->scope);
	}
}

static void append(Converter &conv, AstDefinition *definition) {
	if (definition->_uid == 99)
		debug_break();

	if (definition->built_in)
		return;

	// TODO_OFFSET:
	// Remove this after
	if (definition->bytecode_offset != INVALID_DATA_OFFSET) {
		return;
	}

	// don't append overload sets
	if (!definition->type)
		return;

	auto definition_size = get_size(definition->type);
	assert(definition_size != -1);

	// Don't do anything for constant definitions in lambdas
	if (definition->parent_lambda_or_struct && definition->parent_lambda_or_struct->kind != Ast_struct && definition->is_constant) {
		return;
	}

	if (definition->expression && definition->expression->kind == Ast_lambda) {
		assert_always(append(conv, (AstLambda *)definition->expression, false).count == 0);
		return;
	}

	if (definition->expression) {
		if (definition->expression->kind == Ast_struct) {
			auto Struct = (AstStruct *)definition->expression;
			for_each (Struct->scope.definitions, [&](auto, auto members) {
				auto member = members[0];
				if (member->is_constant && member->expression && member->expression->kind == Ast_lambda) {
					append(conv, member);
				}
			});
		}
	}

	if (definition->expression && is_type(definition->expression))
		return;

	if (definition->parent_lambda_or_struct) {
		if (definition->parent_lambda_or_struct->kind == Ast_lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_lambda_or_struct;
			push_comment(conv, format(u8"definition {}", definition->name));
			assert(!definition->is_parameter);

			auto size = ceil(definition_size, context.stack_word_size);

			definition->bytecode_offset = parent_lambda->offset_accumulator;
			parent_lambda->offset_accumulator += size;

			if (definition->expression) {
				if (definition->expression->type == type_noinit) {
					I(sub_rc, rs, size);
				} else {
					conv.dont_care_about_definition_spacing = true;
					defer { conv.dont_care_about_definition_spacing = false; };

					auto cursor_before = conv.ls->stack_state.cursor;
					append_to_stack(conv, definition->expression);
					auto cursor_after = conv.ls->stack_state.cursor;

					auto size_with_spacing = (cursor_after - cursor_before) * context.stack_word_size;
					auto size_diff = size_with_spacing - ceil(size, context.stack_word_size);

					// account for spacing
					assert(size_diff == 0 || size_diff == 8);
					definition->bytecode_offset += size_diff;
					parent_lambda->offset_accumulator += size_diff;
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

		auto return_parameter_address = load_address_of(conv, ret->lambda->return_parameter);
		if (!return_parameter_address) {
			return_parameter_address = r0;
			I(pop_r, return_parameter_address.value_unchecked());
		}

		{
			REDECLARE_REF(return_parameter_address, return_parameter_address.value_unchecked());

			defer {
				if (return_parameter_address != r0)
					free_register(conv, return_parameter_address);
				if (expression_registers.count && expression_registers[0] != r1) {
					for (auto r : expression_registers) {
						free_register(conv, r);
					}
				}
			};

			auto size = get_size(ret->expression->type);
			if (size <= 8) {
				if (expression_registers.count != 0) {
					assert(expression_registers.count == 1);
				} else {
					expression_registers = {r1};
					I(pop_r, expression_registers[0]);
				}

				assert(context.stack_word_size == 8); // TODO: implement for x86 32 bit
				I(mov8_mr, return_parameter_address, expression_registers[0]);
			} else {
				if (expression_registers.count) {
					// Move multiple registers to [return_parameter_address]
					invalid_code_path("not implemented");
				} else {
					append_memory_copy(conv, return_parameter_address, rs, size, false, u8"expression"s, u8"parameter"s);
				}
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
		if (conv.ls) {
			// if we are in a lambda, append statements with all checks and defers etc.
			append(conv, *scope);
		} else {
			for (auto statement : scope->statements) {
				append(conv, statement);
			}
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

	I(jmp_label);

	append(conv, If->false_scope);

	auto end_offset_false = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size_false = end_offset_false - start_offset;

	I(add_rc, rs, allocated_size_false);
	auto false_end = count_of(conv.ls->body_builder);

	I(jmp_label);

	jz->offset = false_start - true_start + 1;
	jmp->offset = false_end - false_start + 1;
}
static void append(Converter &conv, AstWhile *While) {
	auto start_offset = conv.lambda->offset_accumulator;

	auto count_before_condition = count_of(conv.ls->body_builder);
	I(jmp_label);
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

	I(jmp_label);

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
static void append(Converter &conv, AstAssert *Assert) {
	push_comment(conv, format(u8"assert {}", Assert->location));

	append_to_stack(conv, Assert->condition);
	I(pop_r, r0);
	I(jnz_cr, 2, r0);
	I(debug_break);
	I(jmp_label);
}

static Address allocate_temporary_space(Converter &conv, s64 size) {
	auto offset = rb-conv.ls->temporary_cursor-size;
	conv.ls->temporary_cursor += size;
	return offset;
}

static ValueRegisters append(Converter &conv, AstBinaryOperator *bin, Optional<Address> destination) {
	assert(!destination);
	push_comment(conv, format(u8"binary {}"s, bin->location));

	auto left = bin->left;
	auto right = bin->right;

	using enum BinaryOperation;
	if (bin->operation == dot) {
		switch (right->kind) {
			case Ast_identifier: {
				AstStruct *Struct = 0;
				bool is_pointer = false;
				if (auto pointer = as_pointer(left->type)) {
					Struct = get_struct(pointer->expression);
					is_pointer = true;
				} else {
					Struct = get_struct(left->type);
				}

				if (Struct) {
					auto struct_size = get_size(left->type);


					assert(right->kind == Ast_identifier);
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

							push_comment(conv, u8"1. Reserve space for eventual value"s);
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

								push_comment(conv, u8"2. Append destination pointer"s);
								I(push_r, rs); // destination

								push_comment(conv, u8"3. Append pointer to the struct"s);
								append_to_stack(conv, left);
								I(add_mc, rs, member->offset_in_struct);

								push_comment(conv, u8"4. Copy the member"s);
								append_memory_copy(conv, member_size, true, bin->location, u8"stack"s);
							}
						}
					} else {
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

		AstExpression *from = 0;
		AstExpression *to = 0;
		if (is_pointer_internally(cast->left->type))
			from = type_u64;
		else
			from = direct(cast->left->type);

		if (is_pointer_internally(cast->type))
			to = type_u64;
		else
			to = direct(cast->type);


		{
			auto array = as_array(from);
			auto span = as_span(to);
			if (array && span) {
				I(push_c, (s64)get_constant_integer(array->index_expression).value());
				if (is_addressable(left)) {
					push_address_of(conv, left);
				} else {
					auto size = ceil(get_size(array), context.stack_word_size);

					append_to_stack(conv, left);

					auto offset = rb-conv.ls->temporary_cursor-size;
					append_memory_copy_a(conv, offset, rs, size, false, "cast"str, "temporary"str);

					I(add_rc, rs, size);
					I(lea, r0, offset);
					I(push_r, r0);

					conv.ls->temporary_cursor += size;
				}
				return {};
			}
		}


		append_to_stack(conv, cast->left);

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
		} else if (from == type_u8) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { I(and_mc, rs, 0xff); return {}; } // discard bits that could be garbage
			else if (to == type_u32) { I(and_mc, rs, 0xff); return {}; }
			else if (to == type_u64) { I(and_mc, rs, 0xff); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { I(and_mc, rs, 0xff); return {}; }
			else if (to == type_s32) { I(and_mc, rs, 0xff); return {}; }
			else if (to == type_s64) { I(and_mc, rs, 0xff); return {}; }
		} else if (from == type_u16) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == type_u64) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { I(and_mc, rs, 0xffff); return {}; }
			else if (to == type_s64) { I(and_mc, rs, 0xffff); return {}; }
		} else if (from == type_u32) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { return {}; }
			else if (to == type_u64) { I(and_mc, rs, 0xffffffff); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { return {}; }
			else if (to == type_s64) { I(and_mc, rs, 0xffffffff); return {}; }
		} else if (from == type_u64) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { return {}; }
			else if (to == type_u64) { return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { return {}; }
			else if (to == type_s64) { return {}; }
		} else if (from == type_s8) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); return {}; } // discard bits that could be garbage
			else if (to == type_u32) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == type_u64) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); return {}; }
			else if (to == type_s32) { I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == type_s64) { I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
		} else if (from == type_s16) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == type_u64) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); return {}; }
			else if (to == type_s64) { I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
		} else if (from == type_s32) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { return {}; }
			else if (to == type_u64) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { return {}; }
			else if (to == type_s64) { I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); return {}; }
			else if (to == type_f32) { I(cvt_s32_f32); return {}; }
		} else if (from == type_s64) {
			if (false) { return {}; }
			else if (to == type_u8) { return {}; }
			else if (to == type_u16) { return {}; }
			else if (to == type_u32) { return {}; }
			else if (to == type_u64) { return {}; }
			else if (to == type_s8) { return {}; }
			else if (to == type_s16) { return {}; }
			else if (to == type_s32) { return {}; }
			else if (to == type_s64) { return {}; }
			else if (to == type_f64) { I(cvt_s64_f64); return {}; }
		} else if (from == type_f64) {
			if (false) { return {}; }
			else if (to == type_s64) { I(cvt_f64_s64); return {}; }
		}

		if (to == type_bool && as_option(from)) {
			I(mov1_rm, r0, rs);
			I(add_rc, rs, get_size(from));
			I(push_r, r0);
			return {};
		}

		{
			auto option = as_option(to);
			if (option) {
				assert(types_match(option->expression, from));
				//     string as ?string
				//      count    count
				// rs -> data    data
				//               has_value <- rs

				I(sub_rc, rs, get_align(from));
				I(mov1_mc, rs, 1);
				return {};
			}
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
static ValueRegisters append(Converter &conv, AstIdentifier *identifier, Optional<Address> destination) {
	assert(!destination);
	push_comment(conv, format(u8"load identifer {}", identifier->location));

	auto definition = identifier->definition();
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
			push_address_of(conv, identifier);
			append_memory_copy(conv, size, false, identifier->location, u8"stack"s);
			return {};
		}
	}
	invalid_code_path();
}
static ValueRegisters append(Converter &conv, AstCall *call, Optional<Address> destination) {
	//if (call->_uid == 1604)
	//	debug_break();

	assert(!destination);
	//if (where(call->location.data) == "main.tl:40:5")
	//	debug_break();

	auto dont_care_about_definition_spacing = conv.dont_care_about_definition_spacing;
	conv.dont_care_about_definition_spacing = false;

	push_comment(conv, format(u8"call {}", call->callable->location));

	auto lambda_type = get_lambda(call->lambda_type);
	assert(lambda_type);
	assert(lambda_type->parameters_size != -1);

	auto lambda = get_lambda(call->callable);
	bool is_member = lambda && lambda->is_member;

	// each argument's size is not more than context.stack_word_size.
	// bigger arguments are passed as pointers.
	s64 bytes_will_be_pushed = (tl::count(lambda_type->parameters, [](auto param){return !param->is_constant;}) + is_member) * context.stack_word_size;

	bool did_align = false;

	auto move_from_stack_to_temporary = [&](s64 size) {
		assert(lambda_type->convention != CallingConvention::stdcall);

		auto offset = allocate_temporary_space(conv, size);
		append_memory_copy_a(conv, offset, rs, size, false, "argument"str, "temporary"str);

		return offset;
	};

	auto append_arguments_to_stack = [&] {
		// TODO: implement for 32-bit
		assert(context.stack_word_size == 8);

		// Append all arguments to stack
		if (is_member) {
			assert(call->callable->kind == Ast_binary_operator);
			auto bin = (AstBinaryOperator *)call->callable;
			assert(bin->operation == BinaryOperation::dot);
			assert(is_addressable(bin->left));
			push_address_of(conv, bin->left);
		}

		for (umm i = 0; i < call->sorted_arguments.count; ++i) {
			auto arg = call->sorted_arguments[i];
			auto param = lambda_type->parameters[i];

			if (param->is_constant)
				continue;

			append_to_stack(conv, arg);

			auto size = ceil(get_size(arg->type), context.stack_word_size);
			if (size > context.stack_word_size) {
				// Put argument into temporary space and pass a pointer to it
				auto offset = move_from_stack_to_temporary(size);

				I(add_rc, rs, size);
				I(lea, r0, offset);
				I(push_r, r0);
			} else {
				// leave small argument as is.
			}
		}
	};

	auto align_stack = [&] {
		if ((((conv.ls->stack_state.cursor-conv.ls->stack_state.rb_offset+1)*context.stack_word_size + bytes_will_be_pushed) % 16) != 0){
			push_comment(conv, "align stack"str);
			I(sub_rc, rs, 8);
			did_align = true;
		}
	};

	if (lambda_type->is_intrinsic) {
		auto name = lambda_type->definition->name;
		if (name == "debug_break") {
			I(debug_break);
		} else if (name == "memcpy") {
			append_arguments_to_stack();
			auto rdst = r0;
			auto rsrc = r1;
			auto rsize = r2;
			I(pop_r, rsize);
			I(pop_r, rsrc);
			I(pop_r, rdst);
			I(copyf_mmr, rdst, rsrc, rsize);
		} else {
			invalid_code_path("Unknown intrinsic");
		}
		return {};
	}

	assert(context.stack_word_size == 8);

	auto &arguments = call->sorted_arguments;
	bool lambda_is_constant = is_constant(call->callable);

	auto start_stack_size = conv.ls->stack_state.cursor;
	auto expected_result_size = ceil(get_size(lambda_type->return_parameter->type), 8ll);
	defer {
		// ensure we have the right amount of data on the stack
		auto actual_size = (conv.ls->stack_state.cursor - start_stack_size) * 8;
		assert(expected_result_size == actual_size);

		// make these visible in debugger
		auto x = call;
		auto y = lambda_type;
	};

	auto check_alignment = [&] {
		push_comment(conv, "aligment check"str);
		I(mov_rr, r0, rs);
		I(and_rc, r0, 0xf);
		I(jz_cr, 2, r0);
		I(debug_break);
		I(jmp_label);
	};

	switch (lambda_type->convention) {
		case CallingConvention::tlang: {
			s64 return_parameters_size_on_stack = ceil(get_size(call->type), 8ll);

			push_comment(conv, u8"reserve space for return value"s);
			I(sub_rc, rs, return_parameters_size_on_stack);

			align_stack();

			append_arguments_to_stack();

			// check_alignment();

			if (lambda_is_constant || is_member) {
				if (lambda->definition) { // null if polymorphic
					assert(lambda->definition->is_constant);
				}
				conv.instructions_that_reference_lambdas.add({
					.instruction = II(call_c, -1),
					.lambda = lambda_type,
				});
			} else {
				append_to_stack(conv, call->callable);
				auto rax = x86_64::to_bc_register(x86_64::Register64::rax);
				I(pop_r, rax);
				I(call_r, rax);
			}
			I(add_rc, rs, bytes_will_be_pushed);


			// Allow local definitions to be spaced out by word size to avoid unnecessary copying.
			if (dont_care_about_definition_spacing) {
				if (did_align) {
					expected_result_size += 8;
				}
			} else {
				if (did_align) {
					append_memory_copy_a(conv, rs+8, rs, return_parameters_size_on_stack, true, u8"16 aligned stack"s, u8"16 unaligned stack"s);
					I(add_rc, rs, 8);
				}
			}

			break;
		}
		case CallingConvention::stdcall: {

			using namespace x86_64;

			s64 const shadow_space_size = 32;

			if (lambda_type->parameters.count < 4) {
				// shadow space
				bytes_will_be_pushed += 32;
			}

			for (auto argument : arguments) {
				assert(get_size(argument->type) <= 8);
			}

			align_stack();

			append_arguments_to_stack();

			// check_alignment();

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
		 		if (lambda_type->parameters.count > arg_index)
					if (::is_float(lambda_type->parameters[arg_index]->type))
						I(mov8_xm, stdcall_float_registers[arg_index], rs + (lambda_type->parameters.count*8 - stack_offset));
					else
						I(mov8_rm, to_bc_register(stdcall_int_registers[arg_index]), rs + (lambda_type->parameters.count*8 - stack_offset));
			};

		 	move_arg(0,  8);
			move_arg(1, 16);
			move_arg(2, 24);
			move_arg(3, 32);

			if (lambda_type->parameters.count > 4) {
				push_comment(conv, "Swap argument order"str);

				// these are not used in bytecode and stdcall
				auto r0 = x86_64::to_bc_register(x86_64::Register64::r10);
				auto r1 = x86_64::to_bc_register(x86_64::Register64::r11);

				for (s64 i = 0; i < lambda_type->parameters.count / 2; ++i) {
					auto m0 = rs + i * 8;
					auto m1 = rs + (lambda_type->parameters.count-i-1)*8;
					I(mov8_rm, r0, m0);
					I(xchg8_m, m1, r0);
					I(mov8_mr, m0, r0);
				}
			}

			if (lambda_type->parameters.count < 4) {
				push_comment(conv, u8"reserve shadow space"s);
				I(sub_rc, rs, 32);
			}

			auto function_address_register = to_bc_register(Register64::rax);
			if (lambda_is_constant) {
				load_address_of(function_address_register, conv, call->callable);
				I(call_r, function_address_register);
			} else {
				append_to_stack(conv, call->callable);
				I(pop_r, function_address_register);
				I(call_r, function_address_register);
			}

			push_comment(conv, u8"remove arguments"s);
			I(add_rc, rs, bytes_will_be_pushed);

			if (did_align) {
				push_comment(conv, u8"restore stack before alignment"s);
				I(add_rc, rs, 8);
			}

			if (!types_match(call->type, type_void)) {
				assert(get_size(lambda_type->return_parameter->type) > 0);
				I(push_r, to_bc_register(Register64::rax));
			}

			break;
		}
		default:
			invalid_code_path();
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstLiteral *literal, Optional<Address> destination) {
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

	if (destination) {
		REDECLARE_REF(destination, destination.value_unchecked());
		switch (literal->literal_kind) {
			case noinit:
				break;
			case string: {
				// TODO: deduplicate strings
				assert(context.stack_word_size == 8);
				I(mov8_mc, destination+8, (s64)literal->string.count);

				auto data = allocate_data(conv.constant_data_builder, as_bytes((Span<utf8>)literal->string));
				I(mov_ra, r0, data);
				I(mov8_mr, destination, r0);

				literal->string_data_offset = data;
				break;
			}
			case character:
				I(mov1_mc, destination, (s64)literal->character);
				break;
			case Float:
				push_comment(conv, format(u8"float {}", literal->Float));

				switch (get_size(literal->type)) {
					case 4: I(mov4_mc, destination, (s64)std::bit_cast<s32>((f32)literal->Float)); break;
					case 8: I(mov8_mc, destination, (s64)std::bit_cast<s64>((f64)literal->Float)); break;
					default: invalid_code_path();
				}
				break;
			case boolean:
				I(mov1_mc, destination, (s64)literal->Bool);
				break;
			case integer: {
				assert(context.stack_word_size == 8);
				if (dtype == type_u8 || dtype == type_s8)
					I(mov1_mc, destination, (s64)literal->integer);
				else if (dtype == type_u16 || dtype == type_s16)
					I(mov2_mc, destination, (s64)literal->integer);
				else if (dtype == type_u32 || dtype == type_s32)
					I(mov4_mc, destination, (s64)literal->integer);
				else if (dtype == type_u64 || dtype == type_s64 || dtype == type_pointer_to_void)
					I(mov8_mc, destination, (s64)literal->integer);
				else if (dtype == type_f32) {
					auto f = (f32)(s64)literal->integer;
					I(mov4_mc, destination, *(s32 *)&f);
				} else if (dtype == type_f64) {
					auto f = (f64)(s64)literal->integer;
					I(mov8_mc, destination, *(s64 *)&f);
				}
				else if (literal->type->kind == Ast_unary_operator && ((AstUnaryOperator *)literal->type)->operation == UnaryOperation::pointer)
					I(mov8_mc, destination, (s64)literal->integer);
				else invalid_code_path();
				break;
			}
			case null: {
				I(mov8_mc, destination, 0);
				break;
			}
			default: invalid_code_path();
		}
		return {};
	}


	switch (literal->literal_kind) {
		case noinit:
			I(sub_rc, rs, ceil(get_size(literal->type), context.stack_word_size));
			break;
		case string: {
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
			assert(context.stack_word_size == 8);
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
			else if (literal->type->kind == Ast_unary_operator && ((AstUnaryOperator *)literal->type)->operation == UnaryOperation::pointer)
				I(push_c, (s64)literal->integer);
			else invalid_code_path();
			break;
		}
		case null: {
			I(push_c, 0);
			break;
		}
		default: invalid_code_path();
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstUnaryOperator *unop, Optional<Address> destination) {
	assert(!destination);
	push_comment(conv, format(u8"unary {}", unop->location));
	switch (unop->operation) {
		using enum UnaryOperation;
		case minus: {
			auto registers = append(conv, unop->expression);
			if (registers.count == 0) {
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
			} else if (registers.count == 1) {
				auto size = get_size(unop->type);
				if (::is_integer(unop->type)) {
					I(negi_r, registers[0]);
				} else if (::is_float(unop->type)) {
					switch (size) {
						case 4:
							I(mov_fr, x0, registers[0]);
							I(mov_rc, r0, (s64)0x8000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(mov_rf, registers[0], x0);
							break;
						case 8:
							I(mov_fr, x0, registers[0]);
							I(mov_rc, r0, (s64)0x8000'0000'0000'0000);
							I(mov_fr, x1, r0);
							I(xor_ff, x0, x1);
							I(mov_rf, registers[0], x0);
							break;
						default: invalid_code_path();
					}
				} else {
					invalid_code_path();
				}
			} else {
				invalid_code_path();
			}

			return registers;
		}
		case address_of: {
			return value_registers(load_address_of(conv, unop->expression));
		}
		case dereference: {
			auto size = ceil(get_size(unop->type), context.stack_word_size);
			if (size <= 8) {
				auto registers = append(conv, unop->expression);
				assert(registers.count == 1);
				switch (size) {
					case 1: I(mov1_rm, registers[0], registers[0]); break;
					case 2: I(mov2_rm, registers[0], registers[0]); break;
					case 4: I(mov4_rm, registers[0], registers[0]); break;
					case 8: I(mov8_rm, registers[0], registers[0]); break;
				}
				return registers;
			} else {
				I(sub_rc, rs, size);
				I(push_r, rs);
				append_to_stack(conv, unop->expression);
				append_memory_copy(conv, size, false, unop->expression->location, u8"stack"s);
				return {};
			}
			break;
		}
		case bnot: {
			auto registers = append(conv, unop->expression);
			if (registers.count == 0) {
				I(not_m, rs);
			} else if (registers.count == 1) {
				I(not_r, registers[0]);
			} else {
				invalid_code_path();
			}
			return registers;
		}
		case unwrap: {
			append_to_stack(conv, unop->expression);
			I(mov1_rm, r0, rs);
			I(jnz_cr, 2, r0);
			I(debug_break);
			I(jmp_label);
			I(add_rc, rs, get_align(unop->type));
			return {};
		}
		case autocast: {
			return append(conv, unop->expression);
		}
	}
	invalid_code_path();
}
static ValueRegisters append(Converter &conv, AstSubscript *subscript, Optional<Address> destination) {
	assert(!destination);
	push_comment(conv, format(u8"subscript {}", subscript->location));
	auto element_size = get_size(subscript->type);
	assert(element_size);


	// NOTE: order of evaluation matters

	if (is_power_of_2(element_size) && element_size <= 8) {
		auto index_register = r0;
		auto base_register = r1;

		append_to_stack(conv, subscript->index_expression);

		Optional<Register> addr_opt;
		if (::is_pointer(subscript->expression->type)) {
			// pointer indexing
			append_to_stack(conv, subscript->expression);
			I(pop_r, base_register);
		} else if (auto span = as_span(subscript->expression->type)) {
			// span indexing
			append_to_stack(conv, subscript->expression);
			I(pop_r, base_register);
			I(add_rc, rs, context.stack_word_size);
		} else {
			// array indexing
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
			// pointer indexing
			append_to_stack(conv, subscript->expression);
		} else if (auto span = as_span(subscript->expression->type)) {
			// span indexing
			append_to_stack(conv, subscript->expression);

			// replace count with data
			I(pop_r, r0);
			I(mov8_mr, rs, r0);
		} else {
			// array indexing
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
static ValueRegisters append(Converter &conv, AstLambda *lambda, bool push_address, Optional<Address> destination) {
	assert(!destination);
	if (lambda->is_poly) {
		assert(!push_address);

		for (auto hardened : lambda->hardened_polys) {
			append(conv, hardened.lambda, false);
		}
		return {};
	}

	//if (lambda->original_poly)
	//	debug_break();

	lambda->return_parameter->bytecode_offset = 0;
	assert(lambda->return_parameter->parent_lambda_or_struct);
	assert(lambda->return_parameter->is_return_parameter);
	assert(!lambda->return_parameter->is_parameter);

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
		ls.stack_state.init(get_size(lambda->return_parameter->type));

		ls.current_scope = &lambda->body_scope;

		auto old_ls = conv.ls;
		conv.ls = &ls;
		defer {
			conv.ls = old_ls;
			ls.free();
		};

		scoped_replace(conv.lambda, lambda);

		auto return_value_size = ceil(get_size(lambda->return_parameter->type), context.stack_word_size);

		//if (lambda->definition->name == u8"print_string"s)
		//	debug_break();

		ls.push_used_registers = I(push_used_registers);

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
			I(push_c, 0xdeadc0d);
		}

		I(push_r, rb);
		I(mov_rr, rb, rs);

		push_comment(conv, u8"zero out the return value"s);
		auto address = load_address_of(conv, lambda->return_parameter);
		append_memory_set(conv, address.value(), 0, return_value_size, false);

		ls.temporary_reserver = II(noop);

		append(conv, lambda->body_scope);

		if (ls.temporary_size) {
			*ls.temporary_reserver = MI(sub_rc, rs, ceil(ls.temporary_size, 16ll));
			set_comment(ls.temporary_reserver, "maybe reserve space for temporary values."str);
			for (auto i : ls.local_references) {
				switch (i->kind) {
					using enum InstructionKind;
					case lea:
						i->lea.s.c -= ls.temporary_size;
						break;
					case add_mc:
						i->add_mc.s -= ls.temporary_size;
						break;
				}
			}
		}

		lambda->return_location = count_of(conv.ls->body_builder);

		I(jmp_label);
		I(mov_rr, rs, rb);
		I(pop_r, rb);

		if (lambda->convention == CallingConvention::stdcall) {
			push_comment(conv, u8"pop dummy return address and arguments"s);
			s64 const return_address_size = context.stack_word_size;
			I(add_rc, rs, return_address_size + (s64)lambda->parameters.count * context.stack_word_size);
			push_comment(conv, u8"put return value into rax"s);
			I(pop_r, x86_64::to_bc_register(x86_64::Register64::rax));
		}

		ls.pop_used_registers = I(pop_used_registers);

		s64 additional_rb_parameter_offset = 0;
		if (lambda->convention == CallingConvention::stdcall) {
			// FIXME: push and pop only necessary registers
			// HACK: -1 means push all non-volatile registers according to x64 stdcall convention.
			ls.push_used_registers->mask =
			ls. pop_used_registers->mask = -1;
			// additional_rb_parameter_offset = -8; //
			// additional_rb_parameter_offset = ceil(9u*8, 16u);
		} else {
			ls.push_used_registers->mask =
			ls. pop_used_registers->mask = ls.used_registers_mask;
			additional_rb_parameter_offset = ceil(count_bits(ls.used_registers_mask)*8, 16u);
		}

		if (additional_rb_parameter_offset) {
			for (auto instr : ls.parameter_load_offsets) {
				switch (instr->kind) {
					using enum InstructionKind;
					// These are used for loading parameter address
					case lea:
						instr->lea.s.c += additional_rb_parameter_offset;
						break;
					case add_mc:
						instr->add_mc.s += additional_rb_parameter_offset;
						break;
					// This is used to zero out return parameter.
					case mov8_mc:
						instr->mov8_mc.s += additional_rb_parameter_offset;
						break;
					case mov8_rm:
						instr->mov8_rm.s.c += additional_rb_parameter_offset;
						break;
					default:
						invalid_code_path();
				}
			}
		}


		I(ret);


		lambda->location_in_bytecode = count_of(conv.builder);

		lambda->first_instruction = &conv.builder.add(MI(jmp_label));

		if (lambda->definition) {
			lambda->first_instruction->comment = format(u8"lambda {} {}", lambda->definition->name, lambda->location);
		} else {
			lambda->first_instruction->comment = format(u8"lambda {} {}", where(lambda->location.data), lambda->location);
		}


		auto used_bytes = ls.stack_state.max_cursor*context.stack_word_size + ls.temporary_size;
		if (used_bytes >= 4096) {
			conv.builder.add(MI(prepare_stack, used_bytes));
		}

		for (auto i : lambda->return_jumps) {
			auto offset = lambda->return_location - i.index;
			if (offset == 1) {
				i.jmp->kind = InstructionKind::noop; // TODO: this instruction can be removed. but i don't know if it should be.
			} else {
				i.jmp->jmp.offset = offset;
			}
		}
#if 1
		add_steal(&conv.builder, &conv.ls->body_builder);
#else
		add(&conv.builder, conv.ls->body_builder);
#endif
	} else {
		lambda->definition->bytecode_offset = 0;
		if (lambda->extern_library.data) {
			conv.extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
		}
	}

	if (push_address) {
		// :PUSH_ADDRESS: TODO: Replace this with load_address_of
		push_address_of(conv, lambda);
	}
	return {};
}
static ValueRegisters append(Converter &conv, AstIfx *If, Optional<Address> destination) {
	assert(!destination);

	push_comment(conv, format(u8"ifx {}", If->location));

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

	I(jmp_label);

	auto false_expression = append(conv, If->false_expression);
	for (auto r : reverse(false_expression)) {
		I(push_r, r);
		free_register(conv, r);
	}

	auto count_after_false = count_of(conv.ls->body_builder);

	I(jmp_label);

	jz->offset = count_after_true - count_before_true + 1;
	jmp->offset = count_after_false - count_after_true + 1;
	return {};
}
static ValueRegisters append(Converter &conv, AstPack *pack, Optional<Address> destination) {
	assert(!destination);

	auto elem_size = ceil(get_size(pack->expressions[0]->type), context.stack_word_size);
	auto total_size = (s64)pack->expressions.count * elem_size;

	I(sub_rc, rs, total_size);

	for (umm i = 0; i < pack->expressions.count; ++i) {
		auto expression = pack->expressions[i];
		append(conv, expression);
		append_memory_copy_a(conv, rs+elem_size+i*elem_size, rs, elem_size, false, {}, {});
		I(add_rc, rs, elem_size);
	}

	return {};
}

Bytecode build_bytecode() {
	timed_function(context.profiler);

	assert(context.general_purpose_register_count != 0);

	Bytecode result;

	auto _conv = new Converter();
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

	for (auto i : conv.instructions_that_reference_lambdas) {
		assert(i.lambda->location_in_bytecode != -1);
		switch (i.instruction->kind) {
			case InstructionKind::call_c: {
				i.instruction->call_c.constant = i.lambda->location_in_bytecode;
				break;
			}
			case InstructionKind::push_t: {
				i.instruction->push_t.s = i.lambda->location_in_bytecode;
				break;
			}
			case InstructionKind::mov_rt: {
				i.instruction->mov_rt.s = i.lambda->location_in_bytecode;
				break;
			}
		}
	}

	// print_bytecode(result.instructions);

	return result;

	sizeof(Address);
	sizeof(Instruction);
	sizeof(Instruction::mov_re);
	sizeof(Instruction::mov8_mc);
}
