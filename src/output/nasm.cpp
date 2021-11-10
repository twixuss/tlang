#include "nasm.h"
#include <ast.h>

struct Converter {
	StringBuilder builder;
	StringBuilder *body_builder;
	StringBuilder rodata_builder;
	StringBuilder bss_builder;
	u32 tab_count;
	List<utf8> parent_lambda_name;
	bool last_instruction_is_push;
};


#define DEFINE_EXTRA(name) \
name##ExtraData *ex(name *node) { \
	if (!node->user_data) \
		node->user_data = default_allocator.allocate<name##ExtraData>(); \
	return (name##ExtraData *)node->user_data; \
}


struct AstDefinitionExtraData {
	u32 rbp_offset;
	u32 bss_offset;
};
DEFINE_EXTRA(AstDefinition)


struct AstLambdaExtraData {
	u32 rbp_offset_accumulator;
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

					for (auto statement : lambda->statements) {
						append(conv, statement);
					}

					append(conv.builder, to_string(*conv.body_builder));
				}
				break;
			}
			default: {
				if (is_type(definition->expression))
					break;

				if (definition->parent_lambda) {
					if (definition->is_parameter) {
						ex(definition)->rbp_offset = find_index_of(definition->parent_lambda->parameters, definition);
					} else {
						auto size = get_size(definition->type);
						ex(definition->parent_lambda)->rbp_offset_accumulator += ceil(size, 8u);
						ex(definition)->rbp_offset = ex(definition->parent_lambda)->rbp_offset_accumulator;

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
		assert(!definition->is_parameter);
		auto size = ceil(get_size(definition->type), 8u);
		ex(definition->parent_lambda)->rbp_offset_accumulator += size;
		ex(definition)->rbp_offset = ex(definition->parent_lambda)->rbp_offset_accumulator;
		append_format(*conv.body_builder, "sub rsp, %\n", size);
	}
}
static void append(Converter &conv, AstReturn *ret) {
	append(conv, ret->expression);
	append_format(*conv.body_builder, "pop rax\nmov rsp, rbp\npop rbp\nret\n");
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
	if (bin->operation == BinaryOperation::member_access) {
		switch (bin->right->kind) {
			case Ast_identifier: {
				auto Struct = get_struct(bin->left->type);
				assert(Struct);

				assert(bin->left->kind == Ast_identifier);
				auto variable = ((AstIdentifier *)bin->left)->definition;
				assert(variable);

				auto ident = (AstIdentifier *)bin->right;
				auto member = ident->definition;;
				assert(member);
				if (member->is_constant) {
					append_format(*conv.body_builder, "mov rax, %\npush qword[rax]\n", member->name);
				} else {
					append(conv, bin->left);
					print("rbp_offset is %, member offset is %\n", ex(variable)->rbp_offset, offset_of(Struct, member));
					append_format(*conv.body_builder, "push qword[rbp - %]\n", ex(variable)->rbp_offset + offset_of(Struct, member));
				}
				break;
			}
			default: {
				invalid_code_path();
				break;
			}
		}
	} else {
		append(conv, bin->left);
		append(conv, bin->right);
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
	if (definition->parent_lambda) {
		if (definition->is_parameter) {
			// Function Parameter
			append_format(*conv.body_builder, "push qword[rbp + %]\n", 16 + ex(definition)->rbp_offset);
		} else {
			// Local
			append_format(*conv.body_builder, "push qword[rbp - %]\n", ex(definition)->rbp_offset);
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
	for (auto argument : call->arguments) {
		append(conv, argument);
	}
	append_format(*conv.body_builder, "call %\n", get_full_name(call->lambda));
	if (call->arguments.count) {
		append_format(*conv.body_builder, "add rsp, %\n", call->arguments.count * 8);
	}
	append_format(*conv.body_builder, "push rax\n");
	conv.last_instruction_is_push = true;
}
static void append(Converter &conv, AstLiteral *literal) {
	assert(literal->type != &type_unsized_integer);
	     if (types_match(literal->type, &type_u8 ) ||
	         types_match(literal->type, &type_s8 ) ||
	         types_match(literal->type, &type_bool)) append_format(*conv.body_builder, "push byte %\n", literal->u8 );
	else if (types_match(literal->type, &type_u16) ||
	         types_match(literal->type, &type_s16)) append_format(*conv.body_builder, "push word %\n", literal->u16);
	else if (types_match(literal->type, &type_u32) ||
	         types_match(literal->type, &type_s32)) append_format(*conv.body_builder, "push dword %\n", literal->u32);
	else if (types_match(literal->type, &type_u64) ||
	         types_match(literal->type, &type_s64)) append_format(*conv.body_builder, "push qword %\n", literal->u64);
	else if (types_match(literal->type, &type_string)) {
		append_format(conv.rodata_builder, "string_literal_%:db\"%\"\n", literal->uid, literal->string);
		append_format(*conv.body_builder, "mov rax, qword string_literal_%\npush rax\npush qword %\n", literal->uid, literal->string.count);
	}
	else invalid_code_path();

}
static void append(Converter &conv, AstIf *If) {
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

	auto output_path = to_pathchars(format(u8"%.nasm", source_path_without_extension));

	{
		auto file = open_file(output_path, {.write = true});
		defer { close(file); };

		write(file, R"(
bits 64

section .bss
print_buffer: resb 64
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
nasm -g -f win64 "%.nasm" -o "%.obj"
)", source_path_without_extension, source_path_without_extension);

	append(bat_builder, "if %errorlevel% neq 0 exit /b %errorlevel%\n");
	append_format(bat_builder, R"(link kernel32.lib "%.obj" /out:"%.exe" /entry:"main" /subsystem:console)", source_path_without_extension, source_path_without_extension);

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
