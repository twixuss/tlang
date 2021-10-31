#include "nasm.h"
#include <ast.h>

struct Converter {
	StringBuilder builder;
	StringBuilder *body_builder;
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
};
DEFINE_EXTRA(AstDefinition)


struct AstLambdaExtraData {
	u32 rbp_offset_accumulator;
};
DEFINE_EXTRA(AstLambda)


static umm append_type(StringBuilder &b, AstDefinition *type) {
	if (type == &definition_unsized_integer) {
		print("(unsized_integer)");
	} else if (type == nullptr) {
		print("(null)");
	} else {
		return append(b, type->name);
	}
	invalid_code_path();
}

static void append_tabs(Converter &conv) {
	for (auto i = conv.tab_count; i--;) {
		append(*conv.body_builder, '\t');
	}
}

static void append(Converter &, AstCall*);
static void append(Converter &, AstDefinition*);
static void append(Converter &, AstIdentifier*);
static void append(Converter &, AstInteger*);
static void append(Converter &, AstReturn*);
static void append(Converter &, AstBinaryOperator*);

static void append(Converter &conv, AstNode *node) {
	switch (node->kind) {
		case Ast_definition: return append(conv, (AstDefinition *)node);
		case Ast_return:	 return append(conv, (AstReturn *)node);
		case Ast_identifier: return append(conv, (AstIdentifier *)node);
		case Ast_integer:	 return append(conv, (AstInteger *)node);
		case Ast_call:       return append(conv, (AstCall *)node);
		case Ast_binary_operator: return append(conv, (AstBinaryOperator*)node);
		default: invalid_code_path();
	}
}

static void append(Converter &conv, AstDefinition *definition) {
	switch (definition->expression->kind) {
		case Ast_lambda: {
			auto lambda = (AstLambda *)definition->expression;

			auto full_name = conv.parent_lambda_name.count == 0 ? definition->name : concatenate(conv.parent_lambda_name, u8"$"s, definition->name);

			StringBuilder body_builder;

			auto prev_body_builder = conv.body_builder;
			conv.body_builder = &body_builder;
			defer { conv.body_builder = prev_body_builder; };

			append_format(*conv.body_builder, "%:\n\tpush rbp\n\tmov rbp, rsp\n", full_name);

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
			break;
		}
		default: {
			if (definition->parent_lambda) {
				ex(definition->parent_lambda)->rbp_offset_accumulator += 8;
				ex(definition)->rbp_offset = ex(definition->parent_lambda)->rbp_offset_accumulator;

				append(conv, definition->expression);


				//append_type(*conv.body_builder, definition->type);
				//append_format(*conv.body_builder, " % = ", definition->name);
				//append_format(*conv.body_builder, ";\n");
			} else {
				invalid_code_path("definition no parent");
			}
			break;
		}
	}
}
static void append(Converter &conv, AstReturn *ret) {
	append(conv, ret->expression);
	append_format(*conv.body_builder, "\tpop rax\n\tmov rsp, rbp\n\tpop rbp\n\tret\n");
}
static void append(Converter &conv, AstBinaryOperator *bin) {
	append(conv, bin->left);
	append(conv, bin->right);
	append_format(*conv.body_builder, "\tpop rax\n\tadd qword[rsp], rax\n");
	//append_format(*conv.body_builder, "\tpop rax\n\tpop rbx\n\tadd rax, rbx\n\tpush rax\n");
}
static void append(Converter &conv, AstIdentifier *identifier) {
	append_format(*conv.body_builder, "\tpush qword[rbp - %]\n", ex(identifier->definition)->rbp_offset);
}
static void append(Converter &conv, AstCall *call) {
	List<utf8> full_name;

	auto lambda = call->lambda;
	while (lambda) {
		if (full_name.count) {
			full_name.insert(u8'$', full_name.begin());
		}
		full_name.insert(lambda->name, full_name.begin());
		lambda = lambda->parent_lambda;
	}
	append_format(*conv.body_builder, "\tcall %\n\tpush rax\n", full_name);
	conv.last_instruction_is_push = true;
}
static void append(Converter &conv, AstInteger *integer) {
	append_format(*conv.body_builder, "\tpush qword %\n", integer->value);
}

void output_nasm() {
	scoped_allocator(temporary_allocator);

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
	append_format(bat_builder, R"(link "%.obj" /out:"%.exe" /entry:"main" /subsystem:console)", source_path_without_extension, source_path_without_extension);

	auto bat_path = to_pathchars(concatenate(executable_directory, "\\nasm_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

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
