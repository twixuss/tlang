#include "c.h"
#include <ast.h>

struct Converter {
	StringBuilder declaration_builder;
	StringBuilder definition_builder;
	StringBuilder *body_builder;
	u32 tab_count;
	List<utf8> parent_lambda_name;
};

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

static void append(Converter &conv, AstNode *node) {
	switch (node->kind) {
		case Ast_definition: return append(conv, (AstDefinition *)node);
		case Ast_return:	 return append(conv, (AstReturn *)node);
		case Ast_identifier: return append(conv, (AstIdentifier *)node);
		case Ast_integer:	 return append(conv, (AstInteger *)node);
		case Ast_call:       return append(conv, (AstCall *)node);
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

			append_type(conv.declaration_builder, lambda->return_type);
			append_type(*conv.body_builder, lambda->return_type);
			append_format(conv.declaration_builder, " %();\n", full_name);
			append_format(*conv.body_builder, " %() {\n", full_name);

			auto old_parent_lambda_name = copy(conv.parent_lambda_name);
			conv.parent_lambda_name.set(full_name);
			defer { conv.parent_lambda_name.set(old_parent_lambda_name); };

			auto old_tab_count = conv.tab_count;
			conv.tab_count = 1;
			defer { conv.tab_count = old_tab_count; };

			for (auto statement : lambda->statements) {
				append(conv, statement);
			}

			append_format(*conv.body_builder, "}\n");
			append(conv.definition_builder, to_string(*conv.body_builder));
			break;
		}
		default: {
			append_tabs(conv);
			append_type(*conv.body_builder, definition->type);
			append_format(*conv.body_builder, " % = ", definition->name);
			append(conv, definition->expression);
			append_format(*conv.body_builder, ";\n");
			break;
		}
	}
}
static void append(Converter &conv, AstReturn *ret) {
	append_tabs(conv);
	append(*conv.body_builder, "return ");
	append(conv, ret->expression);
	append(*conv.body_builder, ";\n");
}
static void append(Converter &conv, AstIdentifier *identifier) {
	append(*conv.body_builder, identifier->name);
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
	append(*conv.body_builder, full_name);
	append(*conv.body_builder, "()");
}
static void append(Converter &conv, AstInteger *integer) {
	append(*conv.body_builder, integer->value);
}

void output_c() {
	scoped_allocator(temporary_allocator);

	Converter conv = {};

	for_each(global_statements, [&](auto key, auto statement) {
		append(conv, statement);
	});

	auto c_path = to_pathchars(format(u8"%\\%.c", executable_directory, source_path));

	{
		auto file = open_file(c_path, {.write = true});
		defer { close(file); };

		write(file, R"(
#define _CONCAT(a, b) a ## b
#define CONCAT(a, b) _CONCAT(a, b)
#define static_assert(c) const char CONCAT(_static_assert_, __COUNTER__)[(c) ? 1 : 0]

typedef signed char s8;
typedef signed short s16;
typedef signed s32;
typedef signed long long s64;
typedef unsigned char u8;
typedef unsigned short u16;
typedef unsigned u32;
typedef unsigned long long u64;
typedef u8 bool;

#define true 1
#define false 0

static_assert(sizeof(s8)  == 1);
static_assert(sizeof(s16) == 2);
static_assert(sizeof(s32) == 4);
static_assert(sizeof(s64) == 8);
static_assert(sizeof(u8)  == 1);
static_assert(sizeof(u16) == 2);
static_assert(sizeof(u32) == 4);
static_assert(sizeof(u64) == 8);
)"b);
		write(file, as_bytes(to_string(conv.declaration_builder)));
		write(file, as_bytes(to_string(conv.definition_builder)));
	}


	constexpr auto cl_path = "C:\\Program Files (x86)\\Microsoft Visual Studio\\2019\\Community\\VC\\Tools\\MSVC\\14.29.30037\\bin\\Hostx64\\x64\\cl.exe"s;

	StringBuilder bat_builder;
	append(bat_builder, u8R"(
@echo off
call "C:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" x64
cl )");
	append(bat_builder, c_path);
	append(bat_builder, R"( /ZI > compile_log.txt)");

	auto bat_path = to_pathchars(concatenate(executable_directory, "\\c_build.bat"s));
	write_entire_file(bat_path, as_bytes(to_string(bat_builder)));

	auto process = execute(bat_path);
	if (!process.handle) {
		print(Print_error, "Cannot execute file '%'\n", bat_path);
		return;
	}

	defer { free(process); };

	wait(process);
	auto exit_code = get_exit_code(process);
	if (exit_code != 0) {
		print(Print_error, "Build command failed\n");
		print(as_utf8(read_entire_file(tl_file_string("compile_log.txt"))));
		return;
	}

	print("Build succeeded\n");
}
