#define DUMBEST_BYTECODE 1
#include "bytecode.h"
#include "ast.h"
#include "extern.h"

static constexpr s64 stack_word_size = 8;
static constexpr s64 register_size = 8;

using InstructionBuilder = BlockList<Instruction>;

struct Relocation {
	umm instruction_index;
	AstLambda *lambda;
};

struct StringInfo {
	s64 data_and_size_offset;
	s64 string_offset;

	bool constant;
};

struct Converter {
	InstructionBuilder builder;
	InstructionBuilder *body_builder;
	List<Span<utf8>> extern_functions;
	StringBuilder constant_data_builder;
	StringBuilder data_builder;
	umm zero_data_size = 0;
	AstLambda *lambda = 0;

	List<Relocation> local_relocations;
	List<Relocation> global_relocations;

	List<StringInfo> constant_strings;
};


auto get_ceiled_size(AstExpression *type) {
	return ceil(get_size(type), stack_word_size);
}
s64 ceil(s64 value) {
	return ceil(value, stack_word_size);
}

using enum Register;

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
	auto &back = conv.body_builder->back();
	if (back.comment) {
		back.comment = concatenate(as_span(back.comment), ';', string, '\0').data;
	} else {
		back.comment = null_terminate(string).data;
	}
}

#define MI(_kind, ...) \
	{.kind = InstructionKind::_kind, .line=(u64)__LINE__, ._kind={__VA_ARGS__}}

#else

#define push_comment(...)

#define MI(_kind, ...) \
	{.kind = InstructionKind::_kind, ._kind={__VA_ARGS__}}

#endif


#define I(kind, ...) \
	add_instruction(conv, MI(kind, __VA_ARGS__))

Instruction *add_instruction(Converter &conv, Instruction i) {
	timed_function();

	using enum InstructionKind;
	switch (i.kind) {
#if !DUMBEST_BYTECODE
		case pop_reg: {
			auto &back = conv.body_builder->back();
			switch (back.kind) {
				case push_constant:
					conv.body_builder->pop_back();
					return I(move_constant_to_reg, .reg=i.push_r.reg, .constant=back.push_constant.constant);
				case push_r:
					conv.body_builder->pop_back();
					return I(mov_rr, i.pus.src_reg=back.push_r.reg);
				case push_mem:
					conv.body_builder->pop_back();
					return I(mov_rm, i.push_r._reg=back.push_mem.reg);
				case add_constant_to_mem:
					if (back.add_constant_to_mem.reg == rs) {
						auto add = back.add_constant_to_mem;
						conv.body_builder->pop_back();
						I(mov_rm, i.push_r._reg=rs);
						return I(add_rc, i.p, .constant=add.constant);
					}
					break;
			}
			break;
		}
		case move_mem_to_reg: {
			if (i.move_mem_to_reg.src_reg == rs) {
				auto &back = conv.body_builder->back();
				switch (back.kind) {
					case push_r: {
						return I(mov_rr, i.movreg.dst_reg, .src_reg=back.push_r.reg);
					}
					default:
						break;
				}
			}
			break;
		}
#endif
		case add_rc:
			if (i.add_rc.s == 0)
				return 0;
			break;
		case add_mc:
			if (i.add_mc.s == 0)
				return 0;
			break;
		case sub_rc:
			if (i.sub_rc.s == 0)
				return 0;
			break;
		case sub_mc:
			if (i.sub_mc.s == 0)
				return 0;
			break;
		case mul_rc: {
			if (i.mul_rc.s == 0) {
				return I(xor_rr, i.mul_rc.d, i.mul_rc.d);
			} else {
				if (is_power_of_2(i.mul_rc.s)) {
					return I(shl_rc, i.mul_rc.d, log2(i.mul_rc.s));
				}
			}
			break;
		}
		case mul_mc: {
			if (i.mul_mc.s == 0) {
				return I(mov8_mc, i.mul_mc.d, 0);
			} else {
				if (is_power_of_2(i.mul_mc.s)) {
					return I(shl_mc, i.mul_mc.d, log2(i.mul_mc.s));
				}
			}
			break;
		}
	}

	return &conv.body_builder->add(i);
}

static bool is_debug = false;
// Will be set to non zero value in debug mode.
// Stack will be offset by this value to insert padding before and after a stack frame
// which will allow to detect out of bounds writes.
static s64 debug_stack_frame_padding = 0;

static void append(Converter &, AstCall*);
static void append(Converter &, AstDefinition*);
static void append(Converter &, AstIdentifier*);
static void append(Converter &, AstLiteral*);
static void append(Converter &, AstReturn*);
static void append(Converter &, AstBinaryOperator*);
static void append(Converter &, AstIf*);
static void append(Converter &, AstExpressionStatement*);
static void append(Converter &, AstUnaryOperator*);
static void append(Converter &, AstWhile*);
static void append(Converter &, AstSubscript*);
static void append(Converter &, AstBlock*);
static void append(Converter &, AstCast*);
static void append(Converter &, AstLambda*, bool push_address = true);

static void append(Converter &conv, AstNode *node) {
	switch (node->kind) {
		case Ast_definition:           return append(conv, (AstDefinition *)node);
		case Ast_return:               return append(conv, (AstReturn *)node);
		case Ast_identifier:           return append(conv, (AstIdentifier *)node);
		case Ast_literal:              return append(conv, (AstLiteral *)node);
		case Ast_call:                 return append(conv, (AstCall *)node);
		case Ast_binary_operator:      return append(conv, (AstBinaryOperator*)node);
		case Ast_if:                   return append(conv, (AstIf*)node);
		case Ast_expression_statement: return append(conv, (AstExpressionStatement*)node);
		case Ast_unary_operator:       return append(conv, (AstUnaryOperator*)node);
		case Ast_while:                return append(conv, (AstWhile*)node);
		case Ast_subscript:            return append(conv, (AstSubscript*)node);
		case Ast_block:                return append(conv, (AstBlock*)node);
		case Ast_cast:                 return append(conv, (AstCast*)node);
		case Ast_lambda:               return append(conv, (AstLambda*)node);
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

static void push_address_of(Converter &conv, AstExpression *expression) {
	push_comment(conv, format(u8"push_address_of %", expression->location));
	switch (expression->kind) {
		case Ast_lambda: {
		push_address_of_lambda:

			auto lambda = (AstLambda *)expression;
			if (lambda->has_body) {
				// TODO: YIELD!!!!!!
				assert(lambda->location_in_bytecode != -1);
				I(pushta, lambda->location_in_bytecode);
			} else {
				I(pushextern, lambda->definition->name);
			}
			break;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition;

			if (definition->type->kind == Ast_lambda) {
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
							offset = -(definition->bytecode_offset + ceil(size) + debug_stack_frame_padding);
						}


						I(push_r, rb);
						I(add_mc, rs, offset);
					} else {
						invalid_code_path();
					}
				} else {

					// Global constants and variables

					s64 remaining_bytes = size;
					s64 offset = 0;

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
						I(pushcda, definition->bytecode_offset);
					} else {
						if (definition->expression) {
							I(pushda, definition->bytecode_offset);
						} else {
							I(pushuda, definition->bytecode_offset);
						}
					}
				}
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			assert(binop->operation == '.');
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
}

//
// Expects pointers to destination and source on the stack
// First you should push destination, then source
// Pops the addresses
static void append_memory_copy(Converter &conv, s64 bytes_to_copy, bool reverse, Span<utf8> from_name, Span<utf8> to_name) {
	push_comment(conv, format(u8"copy % bytes from % into %, reverse=%"s, bytes_to_copy, from_name, to_name, reverse));


	switch (bytes_to_copy) {
		case 1:
		case 2:
		case 4:
		case 8: {
			constexpr auto src = r0;
			constexpr auto dst = r1;
			constexpr auto tmp = r2;
			I(pop_r, src);
			I(pop_r, dst);
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
			break;
		}
		default: {
			if (reverse) {
				I(copyb_ssc, bytes_to_copy);
			} else {
				I(copyf_ssc, bytes_to_copy);
			}
			break;
		}
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

	s64 definition_size = 0;
	if (definition->type->kind != Ast_lambda) {
		definition_size = get_size(definition->type);
		if (definition->type != &type_type && definition->type != &type_unsized_integer)
			assert(definition_size);
	}

	if (definition->parent_block && definition->parent_block->kind != Ast_struct && definition->is_constant) {
		return;
	}

	if (definition->expression && definition->expression->kind == Ast_lambda) {
		append(conv, (AstLambda *)definition->expression, false);
		return;
	}

	if (definition->expression && is_type(definition->expression))
		return;

	if (definition->parent_block) {
		if (definition->parent_block->kind == Ast_lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_block;
			push_comment(conv, format(u8"definition %", definition->name));
			assert(!definition->is_parameter);


			auto size = ceil(definition_size, stack_word_size);
			definition->bytecode_offset = parent_lambda->offset_accumulator;
			parent_lambda->offset_accumulator += size;

			if (definition->expression) {
				append(conv, definition->expression);
			} else {
				auto remaining_bytes = size;
				while (remaining_bytes > 0) {
					I(push_c, 0);
					remaining_bytes -= stack_word_size;
				}
			}
		} else {
			invalid_code_path();
		}
	} else {
		if (definition->is_constant) {
			if (definition->expression) {
				assert(definition->expression->kind == Ast_literal);
				auto literal = (AstLiteral *)definition->expression;


				switch (literal->literal_kind) {
					case LiteralKind::integer: {
						definition->bytecode_offset = allocate_data(conv.constant_data_builder, value_as_bytes((s64)literal->integer));
						break;
					}
					case LiteralKind::string: {
						invalid_code_path("not implemented");
						auto offset = allocate_data(conv.constant_data_builder, 16);
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
		append(conv, ret->expression);
		auto size = get_size(ret->expression->type);

		// destination
		I(push_r, rb);
		I(add_mc, rs, 16 + lambda->parameters_size);

		// source
		I(push_r, rs);
		I(add_mc, rs, 8);

		append_memory_copy(conv, size, false, u8"expression"s, u8"parameter"s);
	}

	auto jump_index = (s64)count_of(*conv.body_builder);
	auto return_jump = I(jmp, 0);

	lambda->return_jumps.add({return_jump, jump_index});

	/*
	if (lambda->convention == CallingConvention::stdcall) {
		I(mov_rr, rs, rb);
		I(pop_r, rb);
		I(add_rc, rs, stack_word_size + lambda->parameters_size); // pop fake return address and parameters
		I(pop_r, r0);
	}
	I(mov_rr, rs, rb);
	I(pop_r, rb);
	I(ret);
	*/
}

static void append(Converter &conv, AstBinaryOperator *bin) {
	push_comment(conv, format(u8"binary %"s, operator_string(bin->operation)));

	auto left = bin->left;
	auto right = bin->right;

	if (bin->operation == '.') {
		switch (right->kind) {
			case Ast_identifier: {
				auto Struct = get_struct(left->type);
				assert(Struct);
				auto struct_size = get_size(left->type);


				assert(right->kind == Ast_identifier);
				auto ident = (AstIdentifier *)right;
				auto member = ident->definition;;
				assert(member);
				auto member_size = get_size(member->type);

				// assert(struct_size % stack_word_size == 0);
				// assert(member_size % stack_word_size == 0);

				if (member->is_constant) {
					invalid_code_path("not implemented");
					I(push_c, member->bytecode_offset);
				} else {
					assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);

					I(sub_rc, rs, ceil(member_size));

					append(conv, left);

					//if (member == Struct->members.back()) {
					//	I(add_rc, rs, struct_size - member_size); // just throw away rest of the struct
					//} else
					{
						/*

						a :: struct {
							a : u8;
							b : u8;
						}
						                    struct  result
						    30      38      40      48      50
						  0 |------||------||------||------||------| ffff
							                ab      b       ????????

						*/

						I(push_r, rs); // destination
						I(add_mc, rs, ceil(struct_size));

						I(push_r, rs); // source
						I(add_mc, rs, stack_word_size + member->offset_in_struct);

						append_memory_copy(conv, member_size, true, bin->location, u8"stack"s);

						I(add_rc, rs, ceil(struct_size));
					}
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
			case '+':
			case '-':
			case '*':
			case '/':
			case '%':
			case '|':
			case '&':
			case '>>':
			case '<<':
			case '^': {
				append(conv, left);
				append(conv, right);
				I(pop_r, r0);
				switch (bin->operation) {
					case '+': I(add_mr, rs, r0); break;
					case '-': I(sub_mr, rs, r0); break;
					case '*': I(mul_mr, rs, r0); break;
					case '/': I(div_mr, rs, r0); break;
					case '%': I(mod_mr, rs, r0); break;
					case '|': I( or_mr, rs, r0); break;
					case '&': I(and_mr, rs, r0); break;
					case '^': I(xor_mr, rs, r0); break;
					case '>>': I(shr_mr, rs, r0); break;
					case '<<': I(shl_mr, rs, r0); break;
				}
				break;
			}
			case '=': { // push destination address, then write

				append(conv, right);

				auto bytes_to_write = get_size(left->type);
				auto expr_size      = get_size(right->type);
				assert(bytes_to_write == expr_size);

				push_address_of(conv, left); // destination address
				I(push_r, rs); // source address
				I(add_mc, rs, stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(conv, bytes_to_write, false, right->location, left->location); // will pop src and dst addresses

				I(add_rc, rs, ceil(bytes_to_write));

				break;
			}
			case '<':
			case '>':
			case '<=':
			case '>=':
			case '==':
			case '!=': {
				append(conv, left);
				append(conv, right);
				auto convert_comparison = [&](BinaryOperation op) {
					switch (bin->operation) {
						case '<':   return Comparison::l;
						case '>':   return Comparison::g;
						case '<=':  return Comparison::le;
						case '>=':  return Comparison::ge;
						case '==':  return Comparison::e;
						case '!=':  return Comparison::ne;
					}
					invalid_code_path();
				};

				I(pop_r, r1); // right
				I(pop_r, r0); // left
				if (::is_signed(left->type)) {
					switch (get_size(left->type)) {
						case 1: I(cmps1, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 2: I(cmps2, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 4: I(cmps4, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 8: I(cmps8, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						default: invalid_code_path();
					}
				} else {
					switch (get_size(left->type)) {
						case 1: I(cmpu1, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 2: I(cmpu2, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 4: I(cmpu4, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						case 8: I(cmpu8, .d=r2, .a=r0, .b=r1, .c = convert_comparison(bin->operation)); break;
						default: invalid_code_path();
					}
				}
				I(push_r, r2); // left
				break;
			}
			case '+=':
			case '-=':
			case '*=':
			case '/=':
			case '%=':
			case '|=':
			case '&=':
			case '^=':
			case '<<=':
			case '>>=': {
				append(conv, right);

				push_address_of(conv, left);

				I(pop_r, r0); // destination address
				I(pop_r, r1); // value

				switch (bin->operation) {
					case '+=': I(add_mr, r0, r1); break;
					case '-=': I(sub_mr, r0, r1); break;
					case '*=': I(mul_mr, r0, r1); break;
					case '/=': I(div_mr, r0, r1); break;
					case '%=': I(mod_mr, r0, r1); break;
					case '|=': I( or_mr, r0, r1); break;
					case '&=': I(and_mr, r0, r1); break;
					case '^=': I(xor_mr, r0, r1); break;
					case '>>=': I(shr_mr, r0, r1); break;
					case '<<=': I(shl_mr, r0, r1); break;
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
		return;
	}
}
static void append(Converter &conv, AstIdentifier *identifier) {
	push_comment(conv, format(u8"load identifer %", identifier->name));

	if (identifier->type->kind == Ast_lambda) {
		push_address_of(conv, identifier);
	} else {
		auto size = get_size(identifier->type); // NOT definition->type because definition can be unsized
		assert(size);

		I(sub_rc, rs, ceil(size));
		I(push_r, rs);
		push_address_of(conv, identifier);
		append_memory_copy(conv, size, false, identifier->location, u8"stack"s);
	}
}

static void append(Converter &conv, AstCall *call) {
	push_comment(conv, format(u8"call %", call->name));

	auto lambda = call->lambda;

	switch (lambda->convention) {
		case CallingConvention::tlang: {
			s64 return_parameters_size_on_stack = ceil(get_size(call->type));
			I(sub_rc, rs, return_parameters_size_on_stack); // Reserve space for return value

			s64 arguments_size_on_stack = 0;
			for (auto argument : call->arguments) {
				arguments_size_on_stack += ceil(get_size(argument->type));
				append(conv, argument);
			}
			if (lambda->has_body) {
				conv.local_relocations.add({
					.instruction_index = count_of(*conv.body_builder),
					.lambda = lambda,
				});
				I(call_constant);
			} else {
				I(call_string, .string=call->name);
			}

			if (arguments_size_on_stack) {
				assert(arguments_size_on_stack % stack_word_size == 0);
				I(add_rc, rs, arguments_size_on_stack);
			}
			break;
		}
		case CallingConvention::stdcall: {
			s64 const shadow_space_size = 32;
			s64 arguments_size_on_stack = 0;
			for (auto argument : call->arguments) {
				auto size = get_size(argument->type);
				assert(size <= stack_word_size);
				arguments_size_on_stack += stack_word_size;
			}


			if (call->arguments.count >= 1) { arguments_size_on_stack -= stack_word_size; }
			if (call->arguments.count >= 2) { arguments_size_on_stack -= stack_word_size; }
			if (call->arguments.count >= 3) { arguments_size_on_stack -= stack_word_size; }
			if (call->arguments.count >= 4) { arguments_size_on_stack -= stack_word_size; }

			I(mov_rr, r0, rs);
			I(and_rc, rs, ~(s64)15);
			I(push_r, r0);

			if ((arguments_size_on_stack % 16) == 0) {
				I(sub_rc, rs, stack_word_size);
			}

			// TODO: argument evaluation should not be in reverse order
			for (auto argument : reverse(call->arguments)) {
				append(conv, argument);
			}

			if (call->arguments.count >= 1) { I(pop_r, r0); }
			if (call->arguments.count >= 2) { I(pop_r, r1); }
			if (call->arguments.count >= 3) { I(pop_r, r2);  }
			if (call->arguments.count >= 4) { I(pop_r, r3);  }

			// Shadow space
			// Only for microsoft 64bit
			I(sub_rc, rs, shadow_space_size);

			if (lambda->has_body) {
				conv.local_relocations.add({
					.instruction_index = count_of(*conv.body_builder),
					.lambda = lambda,
				});
				I(stdcall_constant);
			} else {
				I(stdcall_string, .string=call->name);
			}


			// this removes arguments from the stack and restore stack which was before alignment
			I(add_rc, rs, (arguments_size_on_stack + shadow_space_size) + ((((arguments_size_on_stack + shadow_space_size) % 16) == 0) ? stack_word_size : 0));

			I(pop_r, rs);

			I(push_stdcall_result);
			break;
		}
	}

}
static void append(Converter &conv, AstLiteral *literal) {
	push_comment(conv, format(u8"literal %", literal->location));

	assert(literal->type != &type_unsized_integer);

	using enum LiteralKind;

	if (literal->literal_kind == string) {
		// TODO: deduplicate strings

		I(push_c, (s64)literal->string.count);

		auto data = allocate_data(conv.constant_data_builder, as_bytes(literal->string));
		I(pushcda, data);

		literal->string_data_offset = data;
	} else if (literal->literal_kind == character) {
		I(push_c, literal->character);
	} else if (literal->literal_kind == boolean) {
		I(push_c, (u8)literal->Bool);
	} else {
		     if (types_match(literal->type, &type_u8 ) ||
				 types_match(literal->type, &type_s8 ))
															I(push_c, (u8)literal->integer);
		else if (types_match(literal->type, &type_u16) ||
				 types_match(literal->type, &type_s16))
															I(push_c, (u16)literal->integer);
		else if (types_match(literal->type, &type_u32) ||
				 types_match(literal->type, &type_s32))
															I(push_c, (u32)literal->integer);
		else if (types_match(literal->type, &type_u64) ||
				 types_match(literal->type, &type_s64) ||
				 literal->type == &type_pointer_to_void) {
			I(push_c, (s64)literal->integer);
		} else if (literal->type->kind == Ast_unary_operator && ((AstUnaryOperator *)literal->type)->operation == '*') {
			I(push_c, (s64)literal->integer);
		}
		else invalid_code_path();
	}

}
static void append(Converter &conv, AstIf *If) {
	auto start_offset = conv.lambda->offset_accumulator;

	append(conv, If->condition);

	I(pop_r, r0);
	auto jz = I(jz, .reg=r0, .offset=0);

	auto true_start = count_of(*conv.body_builder);
	for (auto statement : If->true_scope.statements) {
		append(conv, statement);
	}

	auto end_offset_true = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size_true = end_offset_true - start_offset;

	I(add_rc, rs, allocated_size_true);
	auto jmp = I(jmp, .offset=0);

	conv.lambda->offset_accumulator = start_offset;

	auto false_start = count_of(*conv.body_builder);
	for (auto statement : If->false_scope.statements) {
		append(conv, statement);
	}

	auto end_offset_false = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size_false = end_offset_false - start_offset;

	auto false_end = count_of(*conv.body_builder) + 1;

	I(add_rc, rs, allocated_size_false);

	jz->jz.offset = false_start - true_start + 1;
	jmp->jmp.offset = false_end - false_start;
}
static void append(Converter &conv, AstWhile *While) {
	auto start_offset = conv.lambda->offset_accumulator;

	auto count_before_condition = count_of(*conv.body_builder);
	append(conv, While->condition);

	I(pop_r, r0);
	auto jz = I(jz, .reg=r0, .offset=0);
	auto count_after_condition = count_of(*conv.body_builder);


	for (auto statement : While->scope.statements) {
		append(conv, statement);
	}

	auto end_offset = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size = end_offset - start_offset;

	I(add_rc, rs, allocated_size);
	auto count_after_body = count_of(*conv.body_builder);
	auto jmp = I(jmp, .offset=0);

	jmp->jmp.offset = (s64)count_before_condition - (s64)count_after_body;
	jz->jz.offset = (s64)count_after_body - (s64)count_after_condition + 2;
}
static void append(Converter &conv, AstBlock *block) {
	push_comment(conv, u8"block"s);

	auto start_offset = conv.lambda->offset_accumulator;

	for (auto statement : block->scope.statements) {
		append(conv, statement);
	}

	auto end_offset = conv.lambda->offset_accumulator;
	conv.lambda->offset_accumulator = start_offset;
	auto allocated_size = end_offset - start_offset;

	I(add_rc, rs, allocated_size);
}
static void append(Converter &conv, AstExpressionStatement *es) {
	append(conv, es->expression);
	switch (es->expression->kind) {
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)es->expression;
			if (
				bin->operation == '=' ||
				bin->operation == '+=' ||
				bin->operation == '-=' ||
				bin->operation == '*=' ||
				bin->operation == '/=' ||
				bin->operation == '%=' ||
				bin->operation == '|=' ||
				bin->operation == '&=' ||
				bin->operation == '^=' ||
				bin->operation == '>>=' ||
				bin->operation == '<<='
			) {
				// these do not push anyting
				return;
			}
			break;
		}

		case Ast_call:
			// discard return value
			auto call = (AstCall *)es->expression;
			auto size = get_ceiled_size(call->type);
			if (size) {
				I(add_rc, rs, size);
			}
			return;
	}

	invalid_code_path();
}
static void append(Converter &conv, AstUnaryOperator *unop) {
	switch (unop->operation) {
		case '-': {
			assert(types_match(unop->expression->type, &type_u8) ||
			       types_match(unop->expression->type, &type_u16) ||
			       types_match(unop->expression->type, &type_u32) ||
			       types_match(unop->expression->type, &type_u64) ||
			       types_match(unop->expression->type, &type_s8) ||
			       types_match(unop->expression->type, &type_s16) ||
			       types_match(unop->expression->type, &type_s32) ||
			       types_match(unop->expression->type, &type_s64));

			append(conv, unop->expression);
			I(pop_r, r1);
			I(xor_rr, r0, r0);
			I(sub_rr, r0, r1);
			I(push_r, r0);
			break;
		}
		case '&': {
			push_address_of(conv, unop->expression);
			break;
		}
		default: {
			invalid_code_path();
			break;
		}
	}
}
static void append(Converter &conv, AstSubscript *subscript) {
	push_comment(conv, format(u8"subscript"));
	append(conv, subscript->index_expression);
	push_address_of(conv, subscript->expression);
	I(pop_r, r0); // array address
	I(pop_r, r1); // index

	auto element_size = get_size(subscript->type);
	assert(element_size);

	I(mul_rc, r1, element_size);

	I(add_rr, r0, r1);
	// now r0 contains element's address

	// reserve space on stack
	I(sub_rc, rs, element_size);

	I(push_r, rs);// destination
	I(push_r, r0);// source
	append_memory_copy(conv, element_size, false, subscript->location, u8"stack"s);

}
static void append(Converter &conv, AstCast *cast) {
	push_comment(conv, format(u8"cast from '%' to '%'", type_to_string(cast->expression->type), type_to_string(cast->type)));

	append(conv, cast->expression);

	switch (cast->cast_kind) {
		using enum CastKind;
			// Same size, different signs
		case u8_s8:
		case u16_s16:
		case u32_s32:
		case u64_s64:
		case s8_u8:
		case s16_u16:
		case s32_u32:
		case s64_u64:
			break;

			// Unsigned to unsigned smaller
		case u16_u8:
		case u32_u8:
		case u32_u16:
		case u64_u8:
		case u64_u16:
		case u64_u32:
			break;

			// Unsigned to signed smaller
		case u16_s8:
		case u32_s8:
		case u32_s16:
		case u64_s8:
		case u64_s16:
		case u64_s32:
			break;

			// Signed to signed smaller
		case s16_s8:
		case s32_s8:
		case s32_s16:
		case s64_s8:
		case s64_s16:
		case s64_s32:
			break;

			// Signed to unsigned smaller
		case s16_u8:
		case s32_u8:
		case s32_u16:
		case s64_u8:
		case s64_u16:
		case s64_u32:
			break;

			// Unsigned to unsigned, unsigned to signed, bigger
			// Remove bits that are not part of the value
		case u8_u16:  case u8_s16: ;
		case u8_u32:  case u8_s32: ;
		case u8_u64:  case u8_s64: I(and_mc, rs, 0xff); break;
		case u16_u32: case u16_s32:;
		case u16_u64: case u16_s64:I(and_mc, rs, 0xffff); break;
		case u32_u64: case u32_s64:I(and_mc, rs, 0xffffffff); break;
			break;

			// Signed to unsigned, signed to signed, bigger
			// sign extension
		case s8_u16:  case s8_s16:  I(movsx21_rm, r0, rs); I(mov2_mr, rs, r0); break;
		case s8_u32:  case s8_s32:  I(movsx41_rm, r0, rs); I(mov4_mr, rs, r0); break;
		case s8_u64:  case s8_s64:  I(movsx81_rm, r0, rs); I(mov8_mr, rs, r0); break;
		case s16_u32: case s16_s32: I(movsx42_rm, r0, rs); I(mov4_mr, rs, r0); break;
		case s16_u64: case s16_s64: I(movsx82_rm, r0, rs); I(mov8_mr, rs, r0); break;
		case s32_u64: case s32_s64: I(movsx84_rm, r0, rs); I(mov8_mr, rs, r0); break;
			break;

		case pointer:
			break;

		default:
			invalid_code_path("not implemented");
	}
}
static void append(Converter &conv, AstLambda *lambda, bool push_address) {
	if (lambda->has_body) {
		InstructionBuilder body_builder;

		auto prev_body_builder = conv.body_builder;
		conv.body_builder = &body_builder;
		defer { conv.body_builder = prev_body_builder; };

		lambda->first_instruction = &
		conv.body_builder->add(MI(push_r, rb));
		conv.body_builder->add(MI(mov_rr, rb, rs));

		if (lambda->definition) {
			push_comment(conv, format(u8"lambda %", lambda->definition->name));
		} else {
			push_comment(conv, format(u8"lambda %", where(lambda->location.data)));
		}

		auto old_lambda = conv.lambda;
		conv.lambda = lambda;
		defer { conv.lambda = old_lambda; };

		auto calc_parameter_offsets_and_append_body = [&] {
			s64 parameter_size_accumulator = 0;
			for (auto parameter : lambda->parameters) {
				parameter->bytecode_offset = parameter_size_accumulator;
				parameter_size_accumulator += ceil(get_size(parameter->type));
			}
			lambda->parameters_size = parameter_size_accumulator;


#if 0
			if (is_debug) {
				I(sub_rc, rs, debug_stack_frame_padding);

				I(mov_rr, rdi, rs);
				I(mov_rc, r2, debug_stack_frame_padding);
				I(mov_rc, r0, 0xcc);
				I(cld);
				// Fill (E)CX bytes at ES:[(E)DI] with AL.
				I(repstosb);
			}
#endif

			for (auto statement : lambda->body_scope.statements) {
				append(conv, statement);
			}

			lambda->return_location = count_of(*conv.body_builder);

			for (auto i : lambda->return_jumps) {
				i.jmp->jmp.offset = lambda->return_location - i.index;
			}

#if 0
			if (is_debug) {
				I(mov_rr, rdi, rb);
				I(sub_rc, rdi, debug_stack_frame_padding);
				I(mov_rc, r0, 0xcc);
				I(mov_rc, r2, debug_stack_frame_padding);
				I(repescasb);
				I(jz_, +3); // skip jz and int3
				I(int3);


				// mov edi, a
				// mov al, 0xcc
				// mov ecx, 4
				// repe scasb
				// setz bl
			}
#endif
			I(mov_rr, rs, rb);
			I(pop_r, rb);
		};


		switch (lambda->convention) {
			case CallingConvention::tlang: {

				calc_parameter_offsets_and_append_body();

				break;
			}
			case CallingConvention::stdcall: {
				I(stdcall_begin_lambda, lambda);

				calc_parameter_offsets_and_append_body();

				I(stdcall_end_lambda, lambda);
				break;
			}
			default: {
				invalid_code_path();
			}
		}
		I(ret);

		lambda->location_in_bytecode = count_of(conv.builder);
		add(&conv.builder, *conv.body_builder);

		for (auto relocation : conv.local_relocations) {
			relocation.instruction_index += lambda->location_in_bytecode;
			conv.global_relocations.add(relocation);
		}
		conv.local_relocations.clear();
	} else {
		conv.extern_functions.add(lambda->definition->name);
	}

	if (push_address) {
		push_address_of(conv, lambda);
	}
}

void print_bytecode(List<Instruction> instructions) {
	timed_function();
	using enum InstructionKind;
	for (auto i : instructions) {
		switch (i.kind) {
			/*
			case mov_rr:		                  print("mov         %, %    \n", i.mov_rr     .dst_reg , i.mov_rr     .src_reg  ); break;
			case move_constant_to_reg:	          print("mov         %, %    \n", i.move_constant_to_reg.reg     , i.move_constant_to_reg.constant ); break;
			case move_mem_to_reg:		          print("mov         %, [%]  \n", i.move_mem_to_reg     .dst_reg , i.move_mem_to_reg     .src_reg  ); break;
			case mov_mr:		                  print("mov         [%], %  \n", i.mov_mr     .dst_reg , i.mov_mr     .src_reg  ); break;
			case push_r:				          print("push        %       \n", i.push_r            .reg                                       ); break;
			case push_constant:			          print("push        %       \n", i.push_constant       .constant                                  ); break;
			case push_mem:				          print("push        [%]     \n", i.push_mem            .reg                                       ); break;
			case pushcda:                         print("push        c addr %\n", i.pushcda.address                             ); break;
			case pushda:                          print("push        d addr %\n", i.pushda.address                                      ); break;
			case pushuda:                         print("push        z addr %\n", i.pushuda.address                        ); break;
			case pop_reg:				          print("pop         %       \n", i.pop_reg             .reg                                       ); break;
			case ret:					          print("ret                 \n"                                                                   ); break;
			case add_constant_to_reg:	          print("add         %, %    \n", i.add_constant_to_reg.reg      , i.add_constant_to_reg.constant  ); break;
			case add_constant_to_mem:	          print("add         [%], %  \n", i.add_constant_to_mem.reg      , i.add_constant_to_mem.constant  ); break;
			case add_reg_to_mem:		          print("add         [%], %  \n", i.add_reg_to_mem     .dst_reg  , i.add_reg_to_mem     .src_reg   ); break;
			case add_reg_to_reg:		          print("add         %, %    \n", i.add_reg_to_reg     .dst_reg  , i.add_reg_to_reg     .src_reg   ); break;
			case sub_constant_to_reg:	          print("sub         %, %    \n", i.sub_constant_to_reg.reg      , i.sub_constant_to_reg.constant  ); break;
			case sub_reg_to_reg:		          print("sub         %, %    \n", i.sub_reg_to_reg     .dst_reg  , i.sub_reg_to_reg     .src_reg   ); break;
			case sub_reg_to_mem:		          print("sub         [%], %  \n", i.sub_reg_to_mem     .dst_reg  , i.sub_reg_to_mem     .src_reg   ); break;
			case mul_reg_to_mem:		          print("mul         [%], %  \n", i.mul_reg_to_mem     .dst_reg  , i.mul_reg_to_mem     .src_reg   ); break;
			case div_reg_to_mem:		          print("div         [%], %  \n", i.div_reg_to_mem     .dst_reg  , i.div_reg_to_mem     .src_reg   ); break;
			case mod_reg_to_mem:		          print("mod         [%], %  \n", i.mod_reg_to_mem     .dst_reg  , i.mod_reg_to_mem     .src_reg   ); break;
			case or_reg_to_mem:			          print(" or         [%], %  \n", i.or_reg_to_mem      .dst_reg  , i.or_reg_to_mem      .src_reg   ); break;
			case and_constant_to_reg:	          print("and         %, %    \n", i.and_constant_to_reg.reg      , i.and_constant_to_reg.constant  ); break;
			case and_reg_to_mem:		          print("and         [%], %  \n", i.and_reg_to_mem     .dst_reg  , i.and_reg_to_mem     .src_reg   ); break;
			case xor_reg_to_reg:		          print("xor         %, %    \n", i.xor_reg_to_reg     .dst_reg  , i.xor_reg_to_reg     .src_reg   ); break;
			case xor_reg_to_mem:		          print("xor         [%], %  \n", i.xor_reg_to_mem     .dst_reg  , i.xor_reg_to_mem     .src_reg   ); break;
			case cmp_r0_r1:			          print("cmp_r0_r1 %, %    \n", i.cmp_r0_r1        .dst_reg  , i.cmp_r0_r1        .comparison); break;
			case call_constant:			          print("call        %       \n", i.call_constant      .constant                                   ); break;
			case call_string:			          print("call        %       \n", i.call_string        .string                                     ); break;
			case jmp:					          print("jmp         %       \n", i.jmp                .offset                                     ); break;
			case jz:					          print("jz          %, %    \n", i.jz                 .reg      , i.jz                 .offset    ); break;
			default:invalid_code_path();
			*/
		}
	}
}

void fix_relocations(List<Instruction> &instructions, List<Relocation> relocations) {
	timed_function();
	for (auto &r : relocations) {
		instructions[r.instruction_index].call_constant.constant = r.lambda->location_in_bytecode;
	}

}

Bytecode build_bytecode() {
	timed_function();

	if (is_debug) {
		debug_stack_frame_padding = 256;
	}

	Bytecode result;

	auto _conv = new Converter;
	defer { delete _conv; };

	auto &conv = *_conv;

	LinearSet<AstStatement *> test;
	for_each(global_scope.statements, [&](auto statement) {
		append(conv, statement);

	});

	result.instructions = to_list(conv.builder);
	result.constant_data = (List<u8>)to_string(conv.constant_data_builder);
	result.data = (List<u8>)to_string(conv.data_builder);
	result.zero_data_size = conv.zero_data_size;
	result.extern_functions = conv.extern_functions;

	fix_relocations(result.instructions, conv.global_relocations);


	// print_bytecode(result.instructions);

	return result;
}
