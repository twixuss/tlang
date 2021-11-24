#define DUMBEST_BYTECODE 1
#include "bytecode.h"
#include "ast.h"
#include "extern.h"

static constexpr s64 stack_word_size = 8;
static constexpr s64 register_size = 8;

using InstructionBuilder = BlockList<Instruction, 4096>;

struct Converter {
	InstructionBuilder builder;
	InstructionBuilder *body_builder;
	List<Span<utf8>> extern_functions;
	StringBuilder constant_data_builder;
	StringBuilder data_builder;
	StringBuilder zero_data_builder;
	u32 tab_count;
	AstLambda *lambda = 0;
};


#define DEFINE_EXTRA(name) \
static name##ExtraData *ex(name *node) { \
	if (!node->user_data) \
		node->user_data = default_allocator.allocate<name##ExtraData>(); \
	return (name##ExtraData *)node->user_data; \
}

#define INVALID_DATA_OFFSET -1
struct AstDefinitionExtraData {
	s64 offset = INVALID_DATA_OFFSET;
};
DEFINE_EXTRA(AstDefinition)


struct AstLambdaExtraData {
	s64 offset_accumulator = 0;
	s64 parameters_size = 0;
};
DEFINE_EXTRA(AstLambda)

using enum Register;

s64 allocate_data(StringBuilder &conv, Span<u8> string) {
	auto result = conv.count();
	append_bytes(conv, string);
	return result;
}

s64 allocate_zero_data(Converter &conv, s64 byte_count) {
	auto result = conv.zero_data_builder.count();
	while (byte_count--)
		append_bytes(conv.zero_data_builder, '\0');
	return result;
}

void push_comment(Converter &conv, Span<utf8> string) {
	auto &back = conv.body_builder->back();
	if (back.comment) {
		back.comment = concatenate(as_span(back.comment), ';', string, '\0').data;
	} else {
		back.comment = null_terminate(string).data;
	}
}

#define MI(_kind, ...) \
	{.kind = InstructionKind::_kind, ._kind={__VA_ARGS__}}

#define I(kind, ...) \
	add_instruction(conv, MI(kind, __VA_ARGS__))

Instruction &add_instruction(Converter &conv, Instruction i) {
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
					if (back.add_constant_to_mem.reg == rsp) {
						auto add = back.add_constant_to_mem;
						conv.body_builder->pop_back();
						I(mov_rm, i.push_r._reg=rsp);
						return I(add_rc, i.p, .constant=add.constant);
					}
					break;
			}
			break;
		}
		case move_mem_to_reg: {
			if (i.move_mem_to_reg.src_reg == rsp) {
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
		case mul_rc: {
			if (is_power_of_2(i.mul_rc.s)) {
				return I(shl_rc, i.mul_rc.d, log2(i.mul_rc.s));
			}
			break;
		}
	}
	return conv.body_builder->add(i);
}

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

static void append(Converter &conv, AstNode *node) {
	switch (node->kind) {
		case Ast_definition: return append(conv, (AstDefinition *)node);
		case Ast_return:     return append(conv, (AstReturn *)node);
		case Ast_identifier: return append(conv, (AstIdentifier *)node);
		case Ast_literal:    return append(conv, (AstLiteral *)node);
		case Ast_call:       return append(conv, (AstCall *)node);
		case Ast_binary_operator: return append(conv, (AstBinaryOperator*)node);
		case Ast_if:         return append(conv, (AstIf*)node);
		case Ast_expression_statement: return append(conv, (AstExpressionStatement*)node);
		case Ast_unary_operator: return append(conv, (AstUnaryOperator*)node);
		case Ast_while: return append(conv, (AstWhile*)node);
		case Ast_subscript: return append(conv, (AstSubscript*)node);
		default: invalid_code_path();
	}
}

static void append(Converter &conv, AstDefinition *definition) {
	if (definition->built_in)
		return;

	// TODO_OFFSET:
	// Remove this after
	if (ex(definition)->offset != INVALID_DATA_OFFSET) {
		return;
	}

	s64 definition_size = 0;
	if (definition->type->kind != Ast_lambda) {
		definition_size = get_size(definition->type);
		if (definition->type != &type_type)
			assert(definition_size);
	}

	if (definition->expression && definition->expression->kind == Ast_lambda) {
		auto lambda = (AstLambda *)definition->expression;
		if (lambda->has_body) {
			InstructionBuilder body_builder;

			auto prev_body_builder = conv.body_builder;
			conv.body_builder = &body_builder;
			defer { conv.body_builder = prev_body_builder; };

			lambda->first_instruction = &
			conv.body_builder->add(MI(push_r, rbp));
			conv.body_builder->add(MI(mov_rr, rbp, rsp));


			auto old_tab_count = conv.tab_count;
			conv.tab_count = 1;
			defer { conv.tab_count = old_tab_count; };

			auto old_lambda = conv.lambda;
			conv.lambda = lambda;
			defer { conv.lambda = old_lambda; };

			s64 parameter_size_accumulator = 0;
			for (auto parameter : lambda->parameters) {
				ex(parameter)->offset = parameter_size_accumulator;
				parameter_size_accumulator += get_size(parameter->type);
			}
			ex(lambda)->parameters_size = parameter_size_accumulator;

			for (auto statement : lambda->statements) {
				append(conv, statement);
			}

			if (types_match(lambda->return_type, &type_void)) {
				I(mov_rr, rsp, rbp);
				I(pop_r, rbp);
				I(ret);
				//append(*conv.body_builder, "mov rsp, rbp\npop rbp\nret\n");
			}

			lambda->location_in_bytecode = count_of(conv.builder);
			add(&conv.builder, *conv.body_builder);

			// append(conv.builder, to_string(*conv.body_builder));
		} else {
			conv.extern_functions.add(lambda->name);
		}
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
			ex(definition)->offset = ex(parent_lambda)->offset_accumulator;
			ex(parent_lambda)->offset_accumulator += size;
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
			ex(definition)->offset = allocate_data(conv.constant_data_builder, value_as_bytes((s64)get_constant_integer(definition->expression).value()));
		} else {
			if (definition->expression) {
				ex(definition)->offset = allocate_data(conv.data_builder, value_as_bytes((s64)get_constant_integer(definition->expression).value()));
			} else {
				ex(definition)->offset = allocate_zero_data(conv, definition_size);
			}
		}
	}
}
static void append(Converter &conv, AstReturn *ret) {
	push_comment(conv, u8"return"s);

	append(conv, ret->expression);

	I(pop_r, rax);
	I(mov_rr, rsp, rbp);
	I(pop_r, rbp);
	I(ret);
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
	local2->data <- rsp (grows ^^^)
			count
	local1->data
			count
	local0->data
			count
			old rbp <- rbp
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
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition;
			s64 size = get_size(definition->type);

			if (definition->parent_block) {
				if (definition->parent_block->kind == Ast_lambda) {
					auto parent_lambda = (AstLambda *)definition->parent_block;

					s64 offset = 0;

					if (definition->is_parameter) {
						// Function Parameter
						offset += ex(definition)->offset + 16; // skip 16 bytes of rbp and return address
					} else {
						// Local
						offset += -(ex(definition)->offset + size);
					}


					I(push_r, rbp);
					I(add_mc, rsp, offset);
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
				auto e = ex(definition);
				if (e->offset == INVALID_DATA_OFFSET) {
					append(conv, definition);
					assert(e->offset != INVALID_DATA_OFFSET);
				}
				if (definition->is_constant) {
					I(pushcda, e->offset);
				} else {
					if (definition->expression) {
						I(pushda, e->offset);
					} else {
						I(pushuda, e->offset);
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
				I(add_mc, rsp, offset);
			}
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			push_address_of(conv, subscript->expression);

			append(conv, subscript->index_expression);
			I(pop_r, rax);

			auto element_size = get_size(subscript->type);
			assert(element_size);
			I(mul_rc, rax, element_size);

			I(add_mr, rsp, rax);

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
static void append_memory_copy(Converter &conv, s64 bytes_to_copy, bool reverse) {
	push_comment(conv, format(u8"copy % bytes, reverse=%"s, bytes_to_copy, reverse));

	constexpr auto src_reg = rax;
	constexpr auto dst_reg = rbx;
	constexpr auto tmp_reg = rcx;

	I(pop_r, src_reg);
	I(pop_r, dst_reg);

	if (reverse) {
		auto offset = bytes_to_copy - stack_word_size;
		if (offset) {
			I(add_rc, src_reg, offset);
			I(add_rc, dst_reg, offset);
		}
		while (bytes_to_copy > 0) {
			I(mov_rm, tmp_reg, src_reg);
			I(mov_mr, dst_reg, tmp_reg);
			bytes_to_copy -= stack_word_size;
			if (bytes_to_copy > 0) {
				I(sub_rc, src_reg, stack_word_size);
				I(sub_rc, dst_reg, stack_word_size);
			}
		}
	} else {
		while (bytes_to_copy > 0) {
			I(mov_rm, tmp_reg, src_reg);
			I(mov_mr, dst_reg, tmp_reg);
			bytes_to_copy -= stack_word_size;
			if (bytes_to_copy > 0) {
				I(add_rc, src_reg, stack_word_size);
				I(add_rc, dst_reg, stack_word_size);
			}
		}
	}
}

static void append(Converter &conv, AstBinaryOperator *bin) {
	push_comment(conv, format(u8"binary %"s, binary_operator_string(bin->operation)));

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
				if (member->is_constant) {
					invalid_code_path("not implemented");
					I(push_c, ex(member)->offset);
				} else {
					assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);
					append(conv, left);

					if (member == Struct->members.back()) {
						I(add_rc, rsp, struct_size - member_size); // just throw away rest of the struct
					} else {
						I(push_r, rsp); // destination
						I(add_mc, rsp, struct_size - member_size);

						I(push_r, rsp); // source
						I(add_mc, rsp, stack_word_size + member->offset_in_struct);

						append_memory_copy(conv, member_size, true);

						I(add_rc, rsp, struct_size - member_size);
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
		if (bin->operation != '=') {
			append(conv, left);
			append(conv, right);
		}
		switch (bin->operation) {
			case '+': I(pop_r, rax); I(add_mr, rsp, rax); break;
			case '-': I(pop_r, rax); I(sub_mr, rsp, rax); break;
			case '*': I(pop_r, rax); I(mul_mr, rsp, rax); break;
			case '/': I(pop_r, rax); I(div_mr, rsp, rax); break;
			case '|': I(pop_r, rax); I( or_mr, rsp, rax); break;
			case '&': I(pop_r, rax); I(and_mr, rsp, rax); break;
			case '^': I(pop_r, rax); I(xor_mr, rsp, rax); break;
			case '=': { // push destination address, then write

				append(conv, right);

				auto bytes_to_write = get_size(left->type);
				auto expr_size = get_size(right->type);
				assert(bytes_to_write == expr_size);

				push_address_of(conv, left); // destination address
				I(push_r, rsp); // source address
				I(add_mc, rsp, stack_word_size);

				assert(bytes_to_write);

				append_memory_copy(conv, bytes_to_write, false); // will pop src and dst addresses

				I(add_rc, rsp, bytes_to_write);

				break;
			}
			case '<':
			case '>':
			case '<=':
			case '>=':
			case '==':
			case '!=': {
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

				I(pop_r, rbx); // right
				I(pop_r, rax); // left
				I(cmp_rax_rbx, .dst_reg=rcx, .comparison = convert_comparison(bin->operation));
				I(push_r, rcx); // left
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
	auto definition = identifier->definition;
	assert(definition);

	push_comment(conv, format(u8"load identifer %", identifier->name));

	auto size = get_size(definition->type);


	I(sub_rc, rsp, ceil(size, (s64)8));
	I(push_r, rsp);
	push_address_of(conv, identifier);
	append_memory_copy(conv, size, false);
}

enum class RelocationKind {
	call,
};

struct Relocation {
	RelocationKind kind;
	Instruction *instruction;
	AstLambda *lambda;
};

List<Relocation> relocations;

static void append(Converter &conv, AstCall *call) {
	push_comment(conv, format(u8"call %", call->name));

	if (call->lambda->has_body) {
		s64 arguments_size_on_stack = 0;
		for (auto argument : call->arguments) {
			arguments_size_on_stack += get_size(argument->type);
			append(conv, argument);
		}
		auto &call_instr =
		I(call_constant);
		relocations.add({
			.kind = RelocationKind::call,
			.instruction = &call_instr,
			.lambda = call->lambda,
		});

		if (arguments_size_on_stack) {
			I(add_rc, rsp, arguments_size_on_stack);
		}
		I(push_r, rax);
	} else {
		assert(call->lambda->extern_language == u8"C"s);

		u32 const shadow_space_size = 32;
		u32 arguments_size_on_stack = shadow_space_size;
		for (auto argument : call->arguments) {
			auto size = get_size(argument->type);
			assert(size <= stack_word_size);
			arguments_size_on_stack += stack_word_size;
		}


		if (call->arguments.count >= 1) { arguments_size_on_stack -= stack_word_size; }
		if (call->arguments.count >= 2) { arguments_size_on_stack -= stack_word_size; }
		if (call->arguments.count >= 3) { arguments_size_on_stack -= stack_word_size; }
		if (call->arguments.count >= 4) { arguments_size_on_stack -= stack_word_size; }

		I(mov_rr, rax, rsp);
		I(and_rc, rsp, ~(s64)15);
		I(push_r, rax);

		if ((arguments_size_on_stack % 16) == 0) {
			I(sub_rc, rsp, stack_word_size);
		}

		for (auto argument : reverse(call->arguments)) {
			append(conv, argument);
		}

		if (call->arguments.count >= 1) { I(pop_r, rcx); }
		if (call->arguments.count >= 2) { I(pop_r, rdx); }
		if (call->arguments.count >= 3) { I(pop_r, r8);  }
		if (call->arguments.count >= 4) { I(pop_r, r9);  }

		// Shadow space
		// Only for microsoft 64bit
		I(sub_rc, rsp, shadow_space_size);


		I(call_string, .string=format(u8"%", call->name));


		if (arguments_size_on_stack > 0) {
			// this removes arguments from the stack
			I(add_rc, rsp, arguments_size_on_stack);
		}

		// restore stack before alignment
		if ((arguments_size_on_stack % 16) == 0) {
			I(add_rc, rsp, stack_word_size);
		}
		I(pop_r, rsp);

		I(push_r, rax);
	}
}
static void append(Converter &conv, AstLiteral *literal) {
	push_comment(conv, format(u8"literal %", literal->location));

	assert(literal->type != &type_unsized_integer);
	     if (types_match(literal->type, &type_bool))
														I(push_c, (u8)literal->Bool);
	else if (types_match(literal->type, &type_u8 ) ||
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
	}
	else if (types_match(literal->type, &type_string)) {

		// TODO: deduplicate strings

		I(push_c, (s64)literal->string.count);
		I(pushcda, allocate_data(conv.constant_data_builder, as_bytes(literal->string)));
	}
	else invalid_code_path();

}
static void append(Converter &conv, AstIf *If) {
	auto start_offset = ex(conv.lambda)->offset_accumulator;

	append(conv, If->condition);

	I(pop_r, rax);
	auto &jz = I(jz, .reg=rax, .offset=0);

	auto count_before_true = count_of(*conv.body_builder);

	for (auto statement : If->true_statements) {
		append(conv, statement);
	}

	auto end_offset_true = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack_true = end_offset_true - start_offset;

	I(add_rc, rsp, allocated_size_on_stack_true);
	auto &jmp = I(jmp, .offset=0);

	auto count_before_false = count_of(*conv.body_builder);

	for (auto statement : If->false_statements) {
		append(conv, statement);
	}

	auto end_offset_false = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack_false = end_offset_false - start_offset;

	I(add_rc, rsp, allocated_size_on_stack_false);
	auto count_after = count_of(*conv.body_builder);

	jz.jz.offset = count_before_false - count_before_true + 1;
	jmp.jmp.offset = count_after - count_before_false;
}
static void append(Converter &conv, AstWhile *While) {
	auto start_offset = ex(conv.lambda)->offset_accumulator;


	auto count_before_condition = count_of(*conv.body_builder);
	append(conv, While->condition);

	I(pop_r, rax);
	auto &jz = I(jz, .reg=rax, .offset=0);
	auto count_after_condition = count_of(*conv.body_builder);


	for (auto statement : While->statements) {
		append(conv, statement);
	}

	auto end_offset = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack = end_offset - start_offset;

	I(add_rc, rsp, allocated_size_on_stack);
	auto count_after_body = count_of(*conv.body_builder);
	auto &jmp = I(jmp, .offset=0);

	jmp.jmp.offset = (s64)count_before_condition - (s64)count_after_body;
	jz.jz.offset = (s64)count_after_body - (s64)count_after_condition + 2;
}
static void append(Converter &conv, AstExpressionStatement *es) {
	append(conv, es->expression);
	if (es->expression->kind == Ast_binary_operator && ((AstBinaryOperator *)es->expression)->operation == '=') {
		// assignment does not push anything
	} else {
		I(add_rc, rsp, stack_word_size);
	}
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
			I(pop_r, rbx);
			I(xor_rr, rax, rax);
			I(sub_rr, rax, rbx);
			I(push_r, rax);
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
	I(pop_r, rax); // array address
	I(pop_r, rbx); // index

	auto element_size = get_size(subscript->type);
	assert(element_size);

	I(mul_rc, rbx, element_size);

	I(add_rr, rax, rbx);
	// now rax contains element's address

	// reserve space on stack
	I(sub_rc, rsp, element_size);

	I(push_r, rsp);// destination
	I(push_r, rax);// source
	append_memory_copy(conv, element_size, false);

}

void print_bytecode(List<Instruction> instructions) {
	timed_function();
	using enum InstructionKind;
	for (auto i : instructions) {
		switch (i.kind) {
			/*
			case mov_rr:		          print("mov         %, %    \n", i.mov_rr     .dst_reg , i.mov_rr     .src_reg  ); break;
			case move_constant_to_reg:	          print("mov         %, %    \n", i.move_constant_to_reg.reg     , i.move_constant_to_reg.constant ); break;
			case move_mem_to_reg:		          print("mov         %, [%]  \n", i.move_mem_to_reg     .dst_reg , i.move_mem_to_reg     .src_reg  ); break;
			case mov_mr:		          print("mov         [%], %  \n", i.mov_mr     .dst_reg , i.mov_mr     .src_reg  ); break;
			case push_r:				          print("push        %       \n", i.push_r            .reg                                       ); break;
			case push_constant:			          print("push        %       \n", i.push_constant       .constant                                  ); break;
			case push_mem:				          print("push        [%]     \n", i.push_mem            .reg                                       ); break;
			case pushcda:      print("push        c addr %\n", i.pushcda.address                             ); break;
			case pushda:               print("push        d addr %\n", i.pushda.address                                      ); break;
			case pushuda: print("push        z addr %\n", i.pushuda.address                        ); break;
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
			case cmp_rax_rbx:			          print("cmp_rax_rbx %, %    \n", i.cmp_rax_rbx        .dst_reg  , i.cmp_rax_rbx        .comparison); break;
			case call_constant:			          print("call        %       \n", i.call_constant      .constant                                   ); break;
			case call_string:			          print("call        %       \n", i.call_string        .string                                     ); break;
			case jmp:					          print("jmp         %       \n", i.jmp                .offset                                     ); break;
			case jz:					          print("jz          %, %    \n", i.jz                 .reg      , i.jz                 .offset    ); break;
			default:invalid_code_path();
			*/
		}
	}
}

void fix_relocations() {
	timed_function();
	for (auto &r : relocations) {
		switch (r.kind) {
			case RelocationKind::call: {
				r.instruction->call_constant.constant = r.lambda->location_in_bytecode;
				break;
			}
			default:
				invalid_code_path();
		}
	}

}

Bytecode build_bytecode() {
	timed_function();

	Bytecode result;

	relocations.allocator = default_allocator;

	Converter conv = {};

	for_each(global_statements, [&](auto key, auto statement) {
		append(conv, statement);
	});

	result.instructions = to_list(conv.builder);
	result.constant_data = (List<u8>)to_string(conv.constant_data_builder);
	result.data = (List<u8>)to_string(conv.data_builder);
	result.zero_data = (List<u8>)to_string(conv.zero_data_builder);
	result.extern_functions = conv.extern_functions;

	fix_relocations();

	// print_bytecode(result.instructions);

	return result;
}
