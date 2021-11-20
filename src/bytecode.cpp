#define DUMBEST_BYTECODE 0
#include "bytecode.h"
#include "ast.h"
#include "extern.h"

using InstructionBuilder = BlockList<Instruction, 4096>;

struct Converter {
	InstructionBuilder builder;
	InstructionBuilder *body_builder;
	StringBuilder extern_builder;
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

}

#define make_instruction(_kind, ...) \
	{.kind = InstructionKind::_kind, ._kind={__VA_ARGS__}}

#define push_instruction(conv, kind, ...) \
	add_instruction(conv, make_instruction(kind, __VA_ARGS__))

Instruction &add_instruction(Converter &conv, Instruction i) {
	using enum InstructionKind;
#if !DUMBEST_BYTECODE
	switch (i.kind) {
		case pop_reg: {
			auto &back = conv.body_builder->back();
			switch (back.kind) {
				case push_constant:
					conv.body_builder->pop_back();
					return push_instruction(conv, move_constant_to_reg, .reg=i.push_reg.reg, .constant=back.push_constant.constant);
				case push_reg:
					conv.body_builder->pop_back();
					return push_instruction(conv, move_reg_to_reg, .dst_reg=i.push_reg.reg, .src_reg=back.push_reg.reg);
				case push_mem:
					conv.body_builder->pop_back();
					return push_instruction(conv, move_mem_to_reg, .dst_reg=i.push_reg.reg, .src_reg=back.push_mem.reg);
				case add_constant_to_mem:
					if (back.add_constant_to_mem.reg == rsp) {
						auto add = back.add_constant_to_mem;
						conv.body_builder->pop_back();
						push_instruction(conv, move_mem_to_reg, .dst_reg=i.push_reg.reg, .src_reg=rsp);
						return push_instruction(conv, add_constant_to_reg, .reg=i.push_reg.reg, .constant=add.constant);
					}
					break;
			}
			break;
		}
		case move_mem_to_reg: {
			if (i.move_mem_to_reg.src_reg == rsp) {
				auto &back = conv.body_builder->back();
				switch (back.kind) {
					case push_reg: {
						return push_instruction(conv, move_reg_to_reg, .dst_reg=i.move_mem_to_reg.dst_reg, .src_reg=back.push_reg.reg);
					}
					default:
						break;
				}
			}
			break;
		}
	}
#endif
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

	u32 definition_size = 0;
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

			// append_format(*conv.body_builder, "push rbp\nmov rbp, rsp\n", lambda->uid, lambda->name);
			lambda->first_instruction = &
			conv.body_builder->add(make_instruction(push_reg,        .reg=rbp));
			conv.body_builder->add(make_instruction(move_reg_to_reg, .dst_reg=rbp, .src_reg=rsp));


			auto old_tab_count = conv.tab_count;
			conv.tab_count = 1;
			defer { conv.tab_count = old_tab_count; };

			auto old_lambda = conv.lambda;
			conv.lambda = lambda;
			defer { conv.lambda = old_lambda; };

			u32 parameter_size_accumulator = 0;
			for (auto parameter : lambda->parameters) {
				ex(parameter)->offset = parameter_size_accumulator;
				parameter_size_accumulator += get_size(parameter->type);
			}
			ex(lambda)->parameters_size = parameter_size_accumulator;

			for (auto statement : lambda->statements) {
				append(conv, statement);
			}

			if (types_match(lambda->return_type, &type_void)) {
				push_instruction(conv, move_reg_to_reg, .dst_reg=rsp, .src_reg=rbp);
				push_instruction(conv, pop_reg, .reg=rbp);
				push_instruction(conv, ret);
				//append(*conv.body_builder, "mov rsp, rbp\npop rbp\nret\n");
			}

			lambda->location_in_bytecode = count_of(conv.builder);
			add(&conv.builder, *conv.body_builder);

			// append(conv.builder, to_string(*conv.body_builder));
		} else {
			append_format(conv.extern_builder, "extern %\n", lambda->name);
		}
		return;
	}

	if (definition->expression && is_type(definition->expression))
		return;

	if (definition->parent_block) {
		if (definition->parent_block->kind == Ast_lambda) {
			auto parent_lambda = (AstLambda *)definition->parent_block;
			push_comment(conv, format(u8"definition %", definition->name));
			//append_format(*conv.body_builder, "; - definition %\n", definition->name);
			assert(!definition->is_parameter);


			auto size = ceil(definition_size, 8u); // TODO: hardcoded 8
			ex(definition)->offset = ex(parent_lambda)->offset_accumulator;
			ex(parent_lambda)->offset_accumulator += size;
			if (definition->expression) {
				append(conv, definition->expression);
			} else {
				auto remaining_bytes = size;
				while (remaining_bytes > 0) {
					push_instruction(conv, push_constant, .constant = 0);
					//append_format(*conv.body_builder, "push 0\n");
					remaining_bytes -= 8;
				}
			}
		} else {
			invalid_code_path();
		}
	} else {
		if (definition->is_constant) {
			ex(definition)->offset = allocate_data(conv.constant_data_builder, value_as_bytes((s64)get_constant_integer(definition->expression).value()));
			// append_format(conv.rodata_builder, "%: dq %\n", definition->name, get_constant_integer(definition->expression).value());
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
	//append(*conv.body_builder, "; - return\n");

	append(conv, ret->expression);

	push_instruction(conv, pop_reg, .reg=rax);
	push_instruction(conv, move_reg_to_reg, .dst_reg=rsp, .src_reg=rbp);
	push_instruction(conv, pop_reg, .reg=rbp);
	push_instruction(conv, ret);
	//append(*conv.body_builder, "pop rax\nmov rsp, rbp\npop rbp\nret\n");
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


					//append_format(*conv.body_builder, "lea rax, qword [rbp + %]\npush rax\n", offset);
					push_instruction(conv, push_reg, .reg=rbp);
					push_instruction(conv, add_constant_to_mem, .reg=rsp, .constant=offset);
				} else {
					invalid_code_path();
				}
			} else {
				// Global constants and variables

				s64 remaining_bytes = size;
				s64 offset = 0;

				//append_format(*conv.body_builder, "mov rax, %\npush rax\n", definition->name);
				//
				// TODO_OFFSET: Remove this after AND PIECE ABOVE
				// It would be better to get rid of append here
				// by calculating global variables' offsets at typecheck time
				//
				auto e = ex(definition);
				if (e->offset == INVALID_DATA_OFFSET) {
					append(conv, definition);
					assert(e->offset != INVALID_DATA_OFFSET);
				}
				if (definition->is_constant) {
					push_instruction(conv, push_constant_data_address, .address=e->offset);
				} else {
					if (definition->expression) {
						push_instruction(conv, push_data_address, .address=e->offset);
					} else {
						push_instruction(conv, push_uninitialized_data_address, .address=e->offset);
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
				//append_format(*conv.body_builder, "add qword[rsp], %\n", offset);
				push_instruction(conv, add_constant_to_mem, .reg=rsp, .constant=offset);
			}
			break;
		}
		default:
			invalid_code_path();
	}
}

//
// Expects pointers to destination and source on the stack
// First you should push destination, then source
//
static void append_memory_copy(Converter &conv, s64 bytes_to_copy) {
	//append_format(*conv.body_builder, "; copy % bytes\npop rsi\npop rdi\n", bytes_to_copy);
	push_comment(conv, format(u8"copy % bytes"s, bytes_to_copy));

	constexpr auto src_reg = rax;
	constexpr auto dst_reg = rbx;
	constexpr auto tmp_reg = rcx;

	push_instruction(conv, pop_reg, .reg=src_reg);
	push_instruction(conv, pop_reg, .reg=dst_reg);
	//s64 offset = 0;
	while (bytes_to_copy > 0) {
		//append_format(*conv.body_builder, "mov rax, qword [rsi + %]\nmov qword[rdi + %], rax\n", offset, offset);
		push_instruction(conv, move_mem_to_reg, .dst_reg=tmp_reg, .src_reg=src_reg);
		push_instruction(conv, move_reg_to_mem, .dst_reg=dst_reg, .src_reg=tmp_reg);
		bytes_to_copy -= 8;
		if (bytes_to_copy > 0) {
			push_instruction(conv, add_constant_to_reg, .reg=src_reg, .constant=8);
			push_instruction(conv, add_constant_to_reg, .reg=dst_reg, .constant=8);
		}
		//offset += 8;
	}
}

static void append(Converter &conv, AstBinaryOperator *bin) {
	//append_format(*conv.body_builder, "; - binary %\n", Span((char *)&bin->operation, 1));
	push_comment(conv, format(u8"binary %"s, Span((char *)&bin->operation, 1)));

	auto left = bin->left;
	auto right = bin->right;

	if (bin->operation == '.') {
		switch (right->kind) {
			case Ast_identifier: {
				auto Struct = get_struct(left->type);
				assert(Struct);

				assert(right->kind == Ast_identifier);
				auto ident = (AstIdentifier *)right;
				auto member = ident->definition;;
				assert(member);
				if (member->is_constant) {
					invalid_code_path("not implemented");
					//append_format(*conv.body_builder, "mov rax, %\npush qword[rax]\n", member->name);
					push_instruction(conv, push_constant, .constant=ex(member)->offset);
				} else {
					assert(member->offset_in_struct != INVALID_MEMBER_OFFSET);
					append(conv, left);
					auto struct_size = get_size(left->type);
					//append_format(*conv.body_builder, "add rsp, %\n", struct_size);
					//append_format(*conv.body_builder, "push qword[rsp + %]\n", member->offset_in_struct - struct_size);
					push_instruction(conv, move_reg_to_reg, .dst_reg=rax, .src_reg=rsp);
					push_instruction(conv, add_constant_to_reg, .reg=rax, .constant=member->offset_in_struct - struct_size);
					push_instruction(conv, push_reg, .reg=rax);
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
			//case '+': append(*conv.body_builder, "pop rax\nadd qword[rsp], rax\n"); break;
			//case '-': append(*conv.body_builder, "pop rax\nsub qword[rsp], rax\n"); break;
			//case '*': append(*conv.body_builder, "pop rax\npop rbx\nimul rax, rbx\npush rax\n"); break;
			//case '/': append(*conv.body_builder, "pop rbx\npop rax\nxor rdx, rdx\nidiv rbx\npush rax\n"); break;
			//case '|': append(*conv.body_builder, "pop rax\nor qword[rsp], rax\n"); break;
			//case '&': append(*conv.body_builder, "pop rax\nand qword[rsp], rax\n"); break;
			//case '^': append(*conv.body_builder, "pop rax\nxor qword[rsp], rax\n"); break;
			case '+':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, add_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '-':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, sub_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '*':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, mul_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '/':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, div_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '|':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, or_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '&':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, and_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '^':
				push_instruction(conv, pop_reg, .reg=rax);
				push_instruction(conv, xor_reg_to_mem, .dst_reg=rsp, .src_reg=rax);
				break;
			case '=': {
				// push destination address, then write

				append(conv, right);

				//append_format(*conv.body_builder, "; - load address of %\n", bin->left->combined_location);
				push_comment(conv, format(u8"load address of %", bin->left->combined_location));

				auto bytes_to_write = get_size(left->type);
				auto expr_size = get_size(right->type);
				assert(bytes_to_write == expr_size);

				push_address_of(conv, left); // destination address
				// append(*conv.body_builder, "lea rax, [rsp + 8]\npush rax\n"); // source address
				push_instruction(conv, push_reg, .reg=rsp);
				push_instruction(conv, add_constant_to_mem, .reg=rsp, .constant=8);

				assert(bytes_to_write);

				append_memory_copy(conv, bytes_to_write); // will pop src and dst addresses

				// append_format(*conv.body_builder, "add rsp, %\n", bytes_to_write); // pop source expression
				push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=bytes_to_write);

				break;
			}
			case '<':
			case '>':
			case '<=':
			case '>=':
			case '==':
			case '!=': {
				/*
				append(*conv.body_builder, "pop rax\npop rbx\nmov rcx, 0\nmov rdx, 1\ncmp rbx, rax\ncmov");
				switch (bin->operation) {
					case '<':   append(*conv.body_builder, "l"); break;
					case '>':   append(*conv.body_builder, "g"); break;
					case '<=':  append(*conv.body_builder, "le"); break;
					case '>=':  append(*conv.body_builder, "ge"); break;
					case '==':  append(*conv.body_builder, "e"); break;
					case '!=':  append(*conv.body_builder, "ne"); break;
				}
				append(*conv.body_builder, " rcx, rdx\npush rcx\n");
				*/

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

				push_instruction(conv, pop_reg, .reg=rbx); // right
				push_instruction(conv, pop_reg, .reg=rax); // left
				push_instruction(conv, cmp_rax_rbx, .dst_reg=rcx, .comparison = convert_comparison(bin->operation));
				push_instruction(conv, push_reg, .reg=rcx); // left
				break;
			}
			default: {
				invalid_code_path();
				break;
			}
		}
		//append_format(*conv.body_builder, "pop rax\npop rbx\nadd rax, rbx\npush rax\n");
		return;
	}
}
static void append(Converter &conv, AstIdentifier *identifier) {
	auto definition = identifier->definition;
	assert(definition);

	//append_format(*conv.body_builder, "; - load identifier %\n", identifier->name);
	push_comment(conv, format(u8"load identifer %", identifier->name));

	auto size = get_size(definition->type);


	push_instruction(conv, sub_constant_to_reg, .reg=rsp, .constant=size);
	push_instruction(conv, push_reg, .reg=rsp);
	push_address_of(conv, identifier);
	append_memory_copy(conv, size);
}

enum class RelocationKind {
	call,
	constant_data,
};

struct Relocation {
	RelocationKind kind;
	Instruction *instruction;
	union {
		AstLambda *lambda;
	};
};

List<Relocation> relocations;

static void append(Converter &conv, AstCall *call) {
	//append_format(*conv.body_builder, "; - call %\n", call->name);
	push_comment(conv, format(u8"call %", call->name));

	if (call->lambda->has_body) {
		u32 arguments_size_on_stack = 0;
		for (auto argument : call->arguments) {
			arguments_size_on_stack += get_size(argument->type);
			append(conv, argument);
		}
		// append_format(*conv.body_builder, "call l%\n", call->lambda->uid);
		auto &call_instr =
		push_instruction(conv, call_constant);
		relocations.add({
			.kind = RelocationKind::call,
			.instruction = &call_instr,
			.lambda = call->lambda,
		});

		if (arguments_size_on_stack) {
			//append_format(*conv.body_builder, "add rsp, %\n", arguments_size_on_stack);
			push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=arguments_size_on_stack);
		}
		//append_format(*conv.body_builder, "push rax\n");
		push_instruction(conv, push_reg, .reg=rax);
	} else {
		assert(call->lambda->extern_language == u8"C"s);

		u32 const shadow_space_size = 32;
		u32 arguments_size_on_stack = shadow_space_size;
		for (auto argument : call->arguments) {
			auto size = get_size(argument->type);
			assert(size <= 8);
			arguments_size_on_stack += 8;
		}


		if (call->arguments.count >= 1) { arguments_size_on_stack -= 8; }
		if (call->arguments.count >= 2) { arguments_size_on_stack -= 8; }
		if (call->arguments.count >= 3) { arguments_size_on_stack -= 8; }
		if (call->arguments.count >= 4) { arguments_size_on_stack -= 8; }

		//print("%\n", arguments_size_on_stack);

		// append(*conv.body_builder, "mov rax, rsp\nand rsp, ~15\npush rax\n");
		push_instruction(conv, move_reg_to_reg, .dst_reg=rax, .src_reg=rsp);
		push_instruction(conv, and_constant_to_reg, .reg=rsp, .constant= ~(s64)15);
		push_instruction(conv, push_reg, .reg=rax);

		if ((arguments_size_on_stack % 16) == 0) {
			// append(*conv.body_builder, "sub rsp, 8\n");
			push_instruction(conv, sub_constant_to_reg, .reg=rsp, .constant=8);
		}

		for (auto argument : reverse(call->arguments)) {
			append(conv, argument);
		}

		/*
		if (call->arguments.count >= 1) { append(*conv.body_builder, "pop rcx\n"); }
		if (call->arguments.count >= 2) { append(*conv.body_builder, "pop rdx\n"); }
		if (call->arguments.count >= 3) { append(*conv.body_builder, "pop r8\n");  }
		if (call->arguments.count >= 4) { append(*conv.body_builder, "pop r9\n");  }
		*/
		if (call->arguments.count >= 1) { push_instruction(conv, pop_reg, .reg=rcx); }
		if (call->arguments.count >= 2) { push_instruction(conv, pop_reg, .reg=rdx); }
		if (call->arguments.count >= 3) { push_instruction(conv, pop_reg, .reg=r8);  }
		if (call->arguments.count >= 4) { push_instruction(conv, pop_reg, .reg=r9);  }

		// Shadow space
		// Only for microsoft 64bit
		// append_format(*conv.body_builder, "sub rsp, %\n", shadow_space_size);
		push_instruction(conv, sub_constant_to_reg, .reg=rsp, .constant=shadow_space_size);


		// append_format(*conv.body_builder, "call %\n", call->lambda->name);
		push_instruction(conv, call_string, .string=format(u8"fn%", call->lambda->uid));


		if (arguments_size_on_stack > 0) {
			// this removes arguments from the stack
			// append_format(*conv.body_builder, "add rsp, %\n", arguments_size_on_stack);
			push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=arguments_size_on_stack);
		}

		// restore stack before alignment
		if ((arguments_size_on_stack % 16) == 0) {
			// append(*conv.body_builder, "add rsp, 8\n");
			push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=8);
		}
		//append_format(*conv.body_builder, "pop rsp\n\n");
		push_instruction(conv, pop_reg, .reg=rsp);

		//append_format(*conv.body_builder, "push rax\n");
		push_instruction(conv, push_reg, .reg=rax);
	}
}
static void append(Converter &conv, AstLiteral *literal) {
	//append_format(*conv.body_builder, "; - literal %\n", literal->literal_kind);

	assert(literal->type != &type_unsized_integer);
	     if (types_match(literal->type, &type_u8 ) ||
	         types_match(literal->type, &type_s8 ) ||
	         types_match(literal->type, &type_bool)) // append_format(*conv.body_builder, "push byte %\n", (u8)literal->integer );
														push_instruction(conv, push_constant, .constant=(u8)literal->integer);
	else if (types_match(literal->type, &type_u16) ||
	         types_match(literal->type, &type_s16)) //append_format(*conv.body_builder, "push word %\n", (u16)literal->integer);
														push_instruction(conv, push_constant, .constant=(u16)literal->integer);
	else if (types_match(literal->type, &type_u32) ||
	         types_match(literal->type, &type_s32)) //append_format(*conv.body_builder, "push dword %\n", (u32)literal->integer);
														push_instruction(conv, push_constant, .constant=(u32)literal->integer);
	else if (types_match(literal->type, &type_u64) ||
	         types_match(literal->type, &type_s64)) {
		push_instruction(conv, push_constant, .constant=(s64)literal->integer);
	}
	else if (types_match(literal->type, &type_string)) {
		//append_format(conv.rodata_builder, "string_literal_%:db ", literal->uid);
		//for (auto c : literal->string) {
		//	append_format(conv.rodata_builder, "%, ", (u8)c);
		//}
		//append(conv.rodata_builder, '\n');
		// append_format(*conv.body_builder, "push qword %\nmov rax, qword string_literal_%\npush rax\n", literal->string.count, literal->uid);

		// TODO: deduplicate strings

		push_instruction(conv, push_constant, .constant=(s64)literal->string.count);
		auto &inst = push_instruction(conv, push_constant, .constant=allocate_data(conv.constant_data_builder, as_bytes(literal->string)));
		relocations.add({
			.kind = RelocationKind::constant_data,
			.instruction = &inst,
		});
	}
	else invalid_code_path();

}
static void append(Converter &conv, AstIf *If) {
	auto start_offset = ex(conv.lambda)->offset_accumulator;

	append(conv, If->condition);

	//append_format(*conv.body_builder, "pop rax\ntest rax, rax\njz .f%\n", If->uid);
	push_instruction(conv, pop_reg, .reg=rax);
	auto &jz = push_instruction(conv, jz, .reg=rax, .offset=0);

	auto count_before_true = count_of(*conv.body_builder);

	for (auto statement : If->true_statements) {
		append(conv, statement);
	}

	auto end_offset_true = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack_true = end_offset_true - start_offset;

	//append_format(*conv.body_builder, "add rsp, %\njmp .e%\n.f%:\n", allocated_size_on_stack_true, If->uid, If->uid);
	push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=allocated_size_on_stack_true);
	auto &jmp = push_instruction(conv, jmp, .offset=0);

	auto count_before_false = count_of(*conv.body_builder);

	for (auto statement : If->false_statements) {
		append(conv, statement);
	}

	auto end_offset_false = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack_false = end_offset_false - start_offset;

	//append_format(*conv.body_builder, "add rsp, %\n.e%:\n", allocated_size_on_stack_false, If->uid);
	push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=allocated_size_on_stack_false);
	auto count_after = count_of(*conv.body_builder);

	jz.jz.offset = count_before_false - count_before_true;
	jmp.jmp.offset = count_after - count_before_false;
}
static void append(Converter &conv, AstWhile *While) {
	auto start_offset = ex(conv.lambda)->offset_accumulator;

	// append_format(*conv.body_builder, "; - while\n.n%:\n", While->uid);

	append(conv, While->condition);

	//append_format(*conv.body_builder, "pop rax\ntest rax, rax\njz .e%\n", While->uid);
	push_instruction(conv, pop_reg, .reg=rax);
	auto &jz = push_instruction(conv, jz, .reg=rax, .offset=0);
	auto count_before = count_of(*conv.body_builder);


	for (auto statement : While->statements) {
		append(conv, statement);
	}

	auto end_offset = ex(conv.lambda)->offset_accumulator;
	ex(conv.lambda)->offset_accumulator = start_offset;
	auto allocated_size_on_stack = end_offset - start_offset;

	//append_format(*conv.body_builder, "add rsp, %\njmp .n%\n.e%:\n", allocated_size_on_stack, While->uid, While->uid);
	push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=allocated_size_on_stack);
	auto count_after = count_of(*conv.body_builder);
	auto &jmp = push_instruction(conv, jmp, .offset=0);

	jmp.jmp.offset = (s64)count_before - (s64)count_after;
	jz.jz.offset = (s64)count_after - (s64)count_before + 1;
}
static void append(Converter &conv, AstExpressionStatement *es) {
	append(conv, es->expression);
	if (es->expression->kind == Ast_binary_operator && ((AstBinaryOperator *)es->expression)->operation == '=') {
		// assignment does not push anything
	} else {
		//append(*conv.body_builder, "add rsp,8\n");
		push_instruction(conv, add_constant_to_reg, .reg=rsp, .constant=8);
	}
}
static void append(Converter &conv, AstUnaryOperator *unop) {
	switch (unop->operation) {
		using enum UnaryOperation;
		case minus: {
			assert(types_match(unop->expression->type, &type_u8) ||
			       types_match(unop->expression->type, &type_u16) ||
			       types_match(unop->expression->type, &type_u32) ||
			       types_match(unop->expression->type, &type_u64) ||
			       types_match(unop->expression->type, &type_s8) ||
			       types_match(unop->expression->type, &type_s16) ||
			       types_match(unop->expression->type, &type_s32) ||
			       types_match(unop->expression->type, &type_s64));

			append(conv, unop->expression);
			//append(*conv.body_builder, "pop rbx\nxor rax, rax\nsub rax, rbx\npush rax\n");
			push_instruction(conv, pop_reg, .reg=rbx);
			push_instruction(conv, xor_reg_to_reg, .dst_reg=rax, .src_reg=rax);
			push_instruction(conv, sub_reg_to_reg, .dst_reg=rax, .src_reg=rbx);
			push_instruction(conv, push_reg, .reg=rax);
			break;
		}
		default: {
			invalid_code_path();
			break;
		}
	}
}

void print_bytecode(List<Instruction> instructions) {
	using enum InstructionKind;
	for (auto i : instructions) {
		switch (i.kind) {
			case move_reg_to_reg:		          print("mov         %, %    \n", i.move_reg_to_reg     .dst_reg , i.move_reg_to_reg     .src_reg  ); break;
			case move_constant_to_reg:	          print("mov         %, %    \n", i.move_constant_to_reg.reg     , i.move_constant_to_reg.constant ); break;
			case move_mem_to_reg:		          print("mov         %, [%]  \n", i.move_mem_to_reg     .dst_reg , i.move_mem_to_reg     .src_reg  ); break;
			case move_reg_to_mem:		          print("mov         [%], %  \n", i.move_reg_to_mem     .dst_reg , i.move_reg_to_mem     .src_reg  ); break;
			case push_reg:				          print("push        %       \n", i.push_reg            .reg                                       ); break;
			case push_constant:			          print("push        %       \n", i.push_constant       .constant                                  ); break;
			case push_mem:				          print("push        [%]     \n", i.push_mem            .reg                                       ); break;
			case push_constant_data_address:      print("push        c addr %\n", i.push_constant_data_address.address                             ); break;
			case push_data_address:               print("push        d addr %\n", i.push_data_address.address                                      ); break;
			case push_uninitialized_data_address: print("push        z addr %\n", i.push_uninitialized_data_address.address                        ); break;
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
		}
	}
}

Bytecode build_bytecode() {
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

	for (auto &r : relocations) {
		switch (r.kind) {
			case RelocationKind::call: {
				r.instruction->call_constant.constant = r.lambda->location_in_bytecode;
				break;
			}
			case RelocationKind::constant_data:
				break;
			default:
				break;
		}
	}

	print_bytecode(result.instructions);

	return result;
}
