#include "nasm.h"
#include "ast.h"
#include "extern.h"

struct Converter {
	StringBuilder builder;
	StringBuilder extern_builder;
	StringBuilder *body_builder;
	StringBuilder rodata_builder;
	StringBuilder bss_builder;
	u32 tab_count;
	List<utf8> parent_lambda_name;
};


#define DEFINE_EXTRA(name) \
name##ExtraData *ex(name *node) { \
	if (!node->user_data) \
		node->user_data = default_allocator.allocate<name##ExtraData>(); \
	return (name##ExtraData *)node->user_data; \
}


struct AstDefinitionExtraData {
	u32 stack_offset = 0;
	u32 bss_offset = 0;
};
DEFINE_EXTRA(AstDefinition)


struct AstLambdaExtraData {
	u32 stack_offset_accumulator = 0;
	u32 parameters_size = 0;
};
DEFINE_EXTRA(AstLambda)


static void append(Converter &, AstCall*);
static void append(Converter &, AstDefinition*);
static void append(Converter &, AstIdentifier*);
static void append(Converter &, AstLiteral*);
static void append(Converter &, AstReturn*);
static void append(Converter &, AstBinaryOperator*);
static void append(Converter &, AstIf*);
static void append(Converter &, AstExpressionStatement*);
static void append(Converter &, AstUnaryOperator*);

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
		default: invalid_code_path();
	}
}

static void append(Converter &conv, AstDefinition *definition) {
	if (definition->built_in)
		return;

	if (definition->expression) {
		switch (definition->expression->kind) {
			case Ast_lambda: {
				auto lambda = (AstLambda *)definition->expression;
				if (lambda->has_body) {
					auto full_name = conv.parent_lambda_name.count == 0 ? definition->name : concatenate(conv.parent_lambda_name, u8"$"s, definition->name);

					StringBuilder body_builder;

					auto prev_body_builder = conv.body_builder;
					conv.body_builder = &body_builder;
					defer { conv.body_builder = prev_body_builder; };

					append_format(*conv.body_builder, "%:;----------------\npush rbp\nmov rbp, rsp\n", full_name);

					auto old_parent_lambda_name = copy(conv.parent_lambda_name);
					conv.parent_lambda_name.set(full_name);
					defer { conv.parent_lambda_name.set(old_parent_lambda_name); };

					auto old_tab_count = conv.tab_count;
					conv.tab_count = 1;
					defer { conv.tab_count = old_tab_count; };

					u32 parameter_size_accumulator = 0;
					for (auto parameter : lambda->parameters) {
						ex(parameter)->stack_offset = parameter_size_accumulator;
						parameter_size_accumulator += get_size(parameter->type);
					}
					ex(lambda)->parameters_size = parameter_size_accumulator;

					for (auto statement : lambda->statements) {
						append(conv, statement);
					}

					if (types_match(lambda->return_type, &type_void)) {
						append(*conv.body_builder, "mov rsp, rbp\npop rbp\nret\n");
					}

					append(conv.builder, to_string(*conv.body_builder));
				} else {
					append_format(conv.extern_builder, "extern %\n", lambda->name);
				}
				break;
			}
			default: {
				if (is_type(definition->expression))
					break;

				if (definition->parent_lambda) {
					append_format(*conv.body_builder, "; - definition %\n", definition->name);
					if (!definition->is_parameter) {
						auto size = get_size(definition->type);
						ex(definition)->stack_offset = ex(definition->parent_lambda)->stack_offset_accumulator;
						ex(definition->parent_lambda)->stack_offset_accumulator += ceil(size, 8u); // TODO: hardcoded 8

						append(conv, definition->expression);
					}
				} else {
					auto size = get_size(definition->type);
					assert(size);
					if (definition->is_constant) {
						append_format(conv.rodata_builder, "%: dq %\n", definition->name, get_constant_integer(definition->expression).value());
					} else {
						append_format(conv.bss_builder, "%: resb %\n", definition->name, size);
					}
				}
				break;
			}
		}
	} else {
		append_format(*conv.body_builder, "; - definition %\n", definition->name);
		assert(!definition->is_parameter);
		auto size = ceil(get_size(definition->type), 8u); // TODO: hardcoded 8
		ex(definition->parent_lambda)->stack_offset_accumulator += size;
		ex(definition)->stack_offset = ex(definition->parent_lambda)->stack_offset_accumulator;
		append_format(*conv.body_builder, "sub rsp, %\n", size);
	}
}
static void append(Converter &conv, AstReturn *ret) {
	append(*conv.body_builder, "; - return\n");

	append(conv, ret->expression);
	append(*conv.body_builder, "pop rax\nmov rsp, rbp\npop rbp\nret\n");
}

u32 offset_of(AstStruct *Struct, AstDefinition *member) {
	u32 offset = 0;
	for (auto m : Struct->members) {
		if (m == member) {
			return offset;
		}
		offset += get_size(m->type);
	}
	invalid_code_path();
}

static void append(Converter &conv, AstBinaryOperator *bin) {
	append_format(*conv.body_builder, "; - binary %\n", bin->operation);

	auto left = bin->left;
	auto right = bin->right;

	if (bin->operation == BinaryOperation::member_access) {
		switch (right->kind) {
			case Ast_identifier: {
				auto Struct = get_struct(left->type);
				assert(Struct);

				assert(right->kind == Ast_identifier);
				auto ident = (AstIdentifier *)right;
				auto member = ident->definition;;
				assert(member);
				if (member->is_constant) {
					append_format(*conv.body_builder, "mov rax, %\npush qword[rax]\n", member->name);
				} else {
					append(conv, left);
					append_format(*conv.body_builder, "add rsp, %\n", get_size(left->type));
					append_format(*conv.body_builder, "push qword[rsp - %]\n", offset_of(Struct, member) + 8);
				}
				break;
			}
			default: {
				invalid_code_path();
				break;
			}
		}
	} else {
		append(conv, left);
		append(conv, right);
		switch (bin->operation) {
			using enum BinaryOperation;
			case add:      append_format(*conv.body_builder, "pop rax\nadd qword[rsp], rax\n"); break;
			case subtract: append_format(*conv.body_builder, "pop rax\nsub qword[rsp], rax\n"); break;
			case multiply: append_format(*conv.body_builder, "pop rax\npop rbx\nimul rax, rbx\npush rax\n"); break;
			case divide:   append_format(*conv.body_builder, "pop rbx\npop rax\nxor rdx, rdx\nidiv rbx\npush rax\n"); break;
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

	append_format(*conv.body_builder, "; - load identifier %\n", identifier->name);

	if (definition->parent_lambda) {
		if (definition->is_parameter) {
			// Function Parameter

			s64 size = get_size(definition->type);
			s64 offset = 8 + ex(definition->parent_lambda)->parameters_size - ex(definition)->stack_offset;

			while (size > 0) {
				append_format(*conv.body_builder, "push qword[rbp + %]\n", offset);
				size -= 8;
				offset -= 8;
			}
		} else {
			// Local

			s64 size = get_size(definition->type);
			s64 offset = ex(definition)->stack_offset + 8;

			while (size > 0) {
				append_format(*conv.body_builder, "push qword[rbp - %]\n", offset);
				size -= 8;
				offset += 8;
			}
		}
	} else {
		// Global constants and variables

		append_format(*conv.body_builder, "mov rax, %\npush qword [rax]\n", definition->name);
	}
}

List<utf8> get_full_name(AstLambda *lambda) {
	List<utf8> full_name;
	while (lambda) {
		if (full_name.count) {
			full_name.insert(u8'$', full_name.begin());
		}
		full_name.insert(lambda->name, full_name.begin());
		lambda = lambda->parent_lambda;
	}
	return full_name;
}

static void append(Converter &conv, AstCall *call) {
	append_format(*conv.body_builder, "; - call %\n", call->name);

	if (call->lambda->has_body) {
		u32 arguments_size_on_stack = 0;
		for (auto argument : call->arguments) {
			arguments_size_on_stack += get_size(argument->type);
			append(conv, argument);
		}
		append_format(*conv.body_builder, "call %\n", get_full_name(call->lambda));
		if (arguments_size_on_stack) {
			append_format(*conv.body_builder, "add rsp, %\n", arguments_size_on_stack);
		}
		append_format(*conv.body_builder, "push rax\n");
	} else {
		assert(call->lambda->extern_language == u8"C"s);

		u32 arguments_size_on_stack = 0;
		for (auto argument : reverse(call->arguments)) {
			arguments_size_on_stack += get_size(argument->type);
			append(conv, argument);
		}

		if (call->arguments.count >= 1) { arguments_size_on_stack -= get_size(call->arguments[0]->type); append(*conv.body_builder, "pop rcx\n"); }
		if (call->arguments.count >= 2) { arguments_size_on_stack -= get_size(call->arguments[1]->type); append(*conv.body_builder, "pop rdx\n"); }
		if (call->arguments.count >= 3) { arguments_size_on_stack -= get_size(call->arguments[2]->type); append(*conv.body_builder, "pop r8\n");  }
		if (call->arguments.count >= 4) { arguments_size_on_stack -= get_size(call->arguments[3]->type); append(*conv.body_builder, "pop r9\n");  }

		append_format(*conv.body_builder, "call %\n", call->lambda->name);

		if (arguments_size_on_stack > 0) {
			// this removes arguments from the stack
			append_format(*conv.body_builder, "add rsp, %\n", arguments_size_on_stack);
		}

		append_format(*conv.body_builder, "push rax\n");
	}
}
static void append(Converter &conv, AstLiteral *literal) {
	//append_format(*conv.body_builder, "; - literal %\n", literal->literal_kind);

	assert(literal->type != &type_unsized_integer);
	     if (types_match(literal->type, &type_u8 ) ||
	         types_match(literal->type, &type_s8 ) ||
	         types_match(literal->type, &type_bool)) append_format(*conv.body_builder, "push byte %\n", (u8)literal->integer );
	else if (types_match(literal->type, &type_u16) ||
	         types_match(literal->type, &type_s16)) append_format(*conv.body_builder, "push word %\n", (u16)literal->integer);
	else if (types_match(literal->type, &type_u32) ||
	         types_match(literal->type, &type_s32)) append_format(*conv.body_builder, "push dword %\n", (u32)literal->integer);
	else if (types_match(literal->type, &type_u64) ||
	         types_match(literal->type, &type_s64)) append_format(*conv.body_builder, "push qword %\n", (u64)literal->integer);
	else if (types_match(literal->type, &type_string)) {
		append_format(conv.rodata_builder, "string_literal_%:db\"%\"\n", literal->uid, literal->string);
		append_format(*conv.body_builder, "mov rax, qword string_literal_%\npush rax\npush qword %\n", literal->uid, literal->string.count);
	}
	else invalid_code_path();

}
static void append(Converter &conv, AstIf *If) {
	append_format(*conv.body_builder, "; - if\n");

	append(conv, If->condition);
	append_format(*conv.body_builder, "pop rax\ntest rax, rax\njz .false%\n", If->uid);
	for (auto statement : If->true_statements) {
		append(conv, statement);
	}
	append_format(*conv.body_builder, "jmp .end%\n.false%:\n", If->uid, If->uid);
	for (auto statement : If->false_statements) {
		append(conv, statement);
	}
	append_format(*conv.body_builder, ".end%:\n", If->uid);
}
static void append(Converter &conv, AstExpressionStatement *es) {
	append(conv, es->expression);
	append(*conv.body_builder, "add rsp,8\n");
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
			append(*conv.body_builder, "pop rbx\nxor rax, rax\nsub rax, rbx\npush rax\n");
			break;
		}
		default: {
			invalid_code_path();
			break;
		}
	}
}

void output_nasm() {
	scoped_allocator(temporary_allocator);
	timed_function();

	Converter conv = {};

	for_each(global_statements, [&](auto key, auto statement) {
		append(conv, statement);
	});

	auto output_path = to_pathchars(format(u8"%.asm", source_path_without_extension));

	{
		auto file = open_file(output_path, {.write = true});
		defer { close(file); };

		write(file, R"(
bits 64
)"b);
		write(file, as_bytes(to_string(conv.extern_builder)));
		write(file, R"(
section .bss
)"b);
		write(file, as_bytes(to_string(conv.bss_builder)));
		write(file, R"(
section .rodata
)"b);
		write(file, as_bytes(to_string(conv.rodata_builder)));
		write(file, R"(
section .text

global main

)"b);
		write(file, as_bytes(to_string(conv.builder)));
	}


	StringBuilder bat_builder;
	append_format(bat_builder, u8R"(
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
nasm -f win64 -g "%.asm" -o "%.obj"
)", source_path_without_extension, source_path_without_extension);

	append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
	append_format(bat_builder, R"(link "%.obj" /out:"%.exe" /entry:"main" /subsystem:console)", source_path_without_extension, source_path_without_extension);
	for (auto library : extern_libraries) {
		append_format(bat_builder, " %", library);
	}

	auto bat_path = to_pathchars(concatenate(executable_directory, "\\nasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

	timed_block("nasm + link"s);

	auto process = start_process(bat_path);
	if (!process.handle) {
		print(Print_error, "Cannot execute file '%'\n", bat_path);
		return;
	}

	defer { free(process); };

	StringBuilder compile_log;

	while (1) {
		u8 buf[256];
		auto bytes_read = process.standard_out->read(array_as_span(buf));

		if (bytes_read == 0)
			break;

		auto string = Span((utf8 *)buf, bytes_read);
		print(string);
		append(compile_log, string);
	}

	write_entire_file("compile_log.txt"s, as_bytes(to_string(compile_log)));

	wait(process);
	auto exit_code = get_exit_code(process);
	if (exit_code != 0) {
		print(Print_error, "Build command failed\n");
		return;
	}

	print("Build succeeded\n");
}
