#include "compiler.h"

#pragma warning(push, 0)
#include <tl/ram.h>
#include <tl/process.h>
#define NOMINMAX
#include <Windows.h>
#pragma warning(pop)


bool type_is_built_in(AstExpression *type) {
	switch (type->kind) {
		case Ast_Struct: return struct_is_built_in((AstStruct *)type);
		case Ast_UnaryOperator: return true;
		case Ast_Subscript: return true;
	}
	invalid_code_path();
	return {};
}

void lock(Scope *scope) {
	if (scope == &compiler->global_scope) {
		lock(compiler->global_scope_mutex);
	}
}
void unlock(Scope *scope) {
	if (scope == &compiler->global_scope) {
		unlock(compiler->global_scope_mutex);
	}
}

void append_type(StringBuilder &builder, AstExpression *type, bool silent_error) {
	if (!type) {
		append(builder, "(null)");
		return;
	}

#define ensure(x) \
	if (silent_error) { \
		if (!(x)) { \
			append(builder, u8"!error!"s); \
			return; \
		} \
	} else { \
		assert(x); \
	}

	// ensure(is_type(type));
	switch (type->kind) {
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;
			if (Struct->definition)
				append(builder, Struct->definition->name);
			else
				append(builder, "<unnamed>");
			break;
		}
		case Ast_LambdaType: {
			auto lambda = ((AstLambdaType *)type)->lambda;

			append(builder, "(");
			for (auto &parameter : lambda->parameters) {
				if (&parameter != lambda->parameters.data) {
					append(builder, ", ");
				}
				append_type(builder, parameter->type, silent_error);
			}
			if (lambda->return_parameter) {
				append(builder, "): ");
				append_type(builder, lambda->return_parameter->type, silent_error);
			} else {
				append(builder, ")");
			}
			switch (lambda->convention) {
				case CallingConvention::stdcall: append(builder, " #stdcall"); break;
			}
			break;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)type;
			// ensure(identifier->definition);
			// ensure(types_match(identifier->definition->expression->type, type_type));
			append(builder, identifier->name);
			break;
		}
		case Ast_UnaryOperator: {
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)type;
			append(builder, unop->operation);
			append_type(builder, unop->expression, silent_error);
			break;
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)type;
			append(builder, '[');
			append(builder, get_constant_integer(subscript->index_expression).map<s64>().value_or(-1));
			append(builder, ']');
			append_type(builder, subscript->expression, silent_error);
			break;
		}
		case Ast_Span: {
			auto subscript = (AstSpan *)type;
			append(builder, "[]");
			append_type(builder, subscript->expression, silent_error);
			break;
		}
		case Ast_Enum: {
			auto Enum = (AstEnum *)type;
			ensure(Enum->definition);
			append(builder, Enum->definition->name);
			break;
		}
		case Ast_Call: {
			auto call = (AstCall *)type;
			append_type(builder, call->callable, silent_error);

			append(builder, '(');
			for (auto &argument : call->unsorted_arguments) {
				if (&argument != call->unsorted_arguments.data)
					append(builder, ", ");
				append_type(builder, argument.expression, silent_error);
			}
			append(builder, ')');
			break;
		}
		default: {
			ensure(false);
		}
	}
#undef ensure
}

// TODO FIXME extremely inefficient on allocations.
HeapString type_to_string(AstExpression *type, bool silent_error) {
	// Is this 'aka' thing really useful?
	// return type_name(type, silent_error);


	if (!type)
		return (HeapString)to_list<MyAllocator>(u8"null"s);

	StringBuilder builder;
	append_type(builder, type, silent_error);
	auto type_str = to_string(builder);
	builder.clear();

	auto d = direct(type);
	if (d) {
		append_type(builder, d, silent_error);
		auto d_str = to_string(builder);
		builder.clear();

		if (d_str != type_str) {
			append(builder, type_str);
			append(builder, " aka "s);
			append(builder, d_str);
			return (HeapString)to_string<HeapString::Allocator>(builder);
		}
	}
	append(builder, type_str);
	return (HeapString)to_string<HeapString::Allocator>(builder);
}

HeapString type_name(AstExpression *type, bool silent_error) {
	if (!type)
		return (HeapString)to_list<HeapString::Allocator>(u8"null"s);

	StringBuilder builder;
	append_type(builder, type, silent_error);
	return (HeapString)to_string<HeapString::Allocator>(builder);
}

AstExpression *get_definition_expression(AstExpression *expression) {
	while (expression->kind == Ast_Identifier)
		expression = ((AstIdentifier *)expression)->definition()->expression;
	return expression;
}

Optional<BinaryOperation> as_binary_operation(TokenKind kind) {
	auto as_char = [](char const *str) {
		u32 c = str[0];
		if (str[1]) {
			c <<= 8; c |= str[1];
			if (str[2]) {
				c <<= 8; c |= str[2];
				if (str[3]) { c <<= 8; c |= str[3]; }
			}
		}
		return c;
	};

	switch (kind) {
		using enum BinaryOperation;
#define e(name, token) case as_char(#token): return name;
	ENUMERATE_BINARY_OPERATIONS
#undef e
	}
	return {};
}
String as_string(BinaryOperation op) {
	switch (op) {
		using enum BinaryOperation;
#define e(name, token) case name: return #token##str;
	ENUMERATE_BINARY_OPERATIONS
#undef e
	}
	invalid_code_path();
	return {};
}

bool is_integer(AstExpression *type) {
	return
		types_match(type, compiler->builtin_unsized_integer) ||
		types_match(type, compiler->builtin_u8) ||
		types_match(type, compiler->builtin_u16) ||
		types_match(type, compiler->builtin_u32) ||
		types_match(type, compiler->builtin_u64) ||
		types_match(type, compiler->builtin_s8) ||
		types_match(type, compiler->builtin_s16) ||
		types_match(type, compiler->builtin_s32) ||
		types_match(type, compiler->builtin_s64);
}

bool is_integer_or_pointer(AstExpression *type) {
	return
		::is_integer(type) ||
		is_pointer(type);
}

bool is_integer_internally(AstExpression *type) {
	return
		::is_integer(type) ||
		is_pointer_internally(type) ||
		direct_as<AstEnum>(type);
}

bool is_signed(AstExpression *type) {
	return
		types_match(type, compiler->builtin_s8) ||
		types_match(type, compiler->builtin_s16) ||
		types_match(type, compiler->builtin_s32) ||
		types_match(type, compiler->builtin_s64);
}

bool is_integer(AstStruct *type) {
	return
		type == compiler->builtin_unsized_integer.Struct ||
		type == compiler->builtin_u8 .Struct||
		type == compiler->builtin_u16 .Struct||
		type == compiler->builtin_u32 .Struct||
		type == compiler->builtin_u64 .Struct||
		type == compiler->builtin_s8 .Struct||
		type == compiler->builtin_s16 .Struct||
		type == compiler->builtin_s32 .Struct||
		type == compiler->builtin_s64.Struct;
}

bool is_signed(AstStruct *type) {
	return
		type == compiler->builtin_s8 .Struct||
		type == compiler->builtin_s16 .Struct||
		type == compiler->builtin_s32 .Struct||
		type == compiler->builtin_s64.Struct;
}

bool is_float(AstExpression *type) {
	return
		types_match(type, compiler->builtin_unsized_float) ||
		types_match(type, compiler->builtin_f32) ||
		types_match(type, compiler->builtin_f64);
}

bool is_float(AstStruct *type) {
	return
		type == compiler->builtin_unsized_float.Struct ||
		type == compiler->builtin_f32 .Struct||
		type == compiler->builtin_f64.Struct;
}

#if OVERLOAD_NEW
void *operator new(umm size) {
	return my_allocate(size, __STDCPP_DEFAULT_NEW_ALIGNMENT__);
}
void *operator new(umm size, std::align_val_t align) {
	return my_allocate(size, (umm)align);
}
void operator delete(void *) {
}
void operator delete(void *data, umm size) {
	my_deallocate(data, size);
}
#endif

bool is_pointer_internally(AstExpression *type) {
	return is_lambda_type(type) || is_pointer(type);
}
bool is_pointer_to(AstExpression *pointer_type, AstExpression *pointee_type) {
	auto p = as_pointer(pointer_type);
	if (!p)
		return false;
	return types_match(p->expression, pointee_type);

}

AstLiteral *get_literal(AstExpression *expression) {
    switch (expression->kind) {
        case Ast_Literal: return (AstLiteral *)expression;
        case Ast_Identifier: {
            auto identifier = (AstIdentifier *)expression;
			if (identifier->definition()) {
				auto definition = identifier->definition();
				if (definition->is_constant) {
					return get_literal(definition->expression);
				}
            }
            break;
        }
    }
    return 0;
}

bool is_constant(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Literal:
		case Ast_Lambda:
		//case Ast_Import:
			return true;
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			return is_constant(binop->right);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			return is_constant(unop->expression);
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;

			if (identifier->possible_definitions.count) {
				// HACK: TODO: FIXME: this is to make passing overloaded functions working.
				for (auto definition : identifier->possible_definitions) {
					if (!definition->is_constant)
						return false;
				}
				return true;
			}

			assert(identifier->definition());
			if (identifier->definition())
				return identifier->definition()->is_constant;
			return false;
		}
		case Ast_Call: {
			auto call = (AstCall *)expression;
			if (auto Struct = direct_as<AstStruct>(call->callable)) {
				for (auto arg : call->unsorted_arguments) {
					if (!is_constant(arg.expression))
						return false;
				}
				return true;
			}
			break;
		}
		case Ast_ArrayInitializer: {
			auto ArrayInitializer = (AstArrayInitializer *)expression;
			for (auto &element : ArrayInitializer->elements) {
				if (!is_constant(element))
					return false;
			}
			return true;
		}
	}



    if (is_type(expression))
        return true;


    return false;
}

AstLambda *get_lambda(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Lambda:
			return (AstLambda *)expression;
		case Ast_LambdaType:
			return ((AstLambdaType *)expression)->lambda;
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)expression;
			if (!ident->definition())
				return 0;
			return get_lambda(ident->definition()->expression);
		}
		case Ast_BinaryOperator: {
			auto bin = (AstBinaryOperator *)expression;
			if (bin->operation == BinaryOperation::dot)
				return get_lambda(bin->right);
			break;
		}
	}
	return 0;
}

AstLambdaType *get_lambda_type(AstExpression *type) {
	switch (type->kind) {
		case Ast_LambdaType:
			return (AstLambdaType *)type;
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)type;
			if (!ident->definition()->expression)
				return 0;
			return get_lambda_type(ident->definition()->expression);
		}
	}
	return 0;
}

Comparison comparison_from_binary_operation(BinaryOperation operation) {
	switch (operation) {
		case BinaryOperation::lt: return Comparison::l;
		case BinaryOperation::gt: return Comparison::g;
		case BinaryOperation::le: return Comparison::le;
		case BinaryOperation::ge: return Comparison::ge;
		case BinaryOperation::eq: return Comparison::e;
		case BinaryOperation::ne: return Comparison::ne;
	}
	invalid_code_path();
}
/*
List<Expression<> *> get_arguments_addresses(AstCall *call) {
	switch (call->argument->kind) {
		case Ast_Tuple:
			return map(((AstTuple *)call->argument)->expressions, [](auto &e) { return &e; });
		default: invalid_code_path();
	}
}

List<Expression<>> get_arguments(AstCall *call) {
	switch (call->argument->kind) {
		case Ast_Tuple: return ((AstTuple *)call->argument)->expressions;
		default: invalid_code_path();
	}
}
*/

bool is_sized_array(AstExpression *type) {
	return type->kind == Ast_Subscript;
}

AstSubscript *as_array(AstExpression *type) {
	auto d = direct(type);
	if (!d)
		return 0;
	if (d->kind == Ast_Subscript)
		return (AstSubscript *)d;
	return 0;
}
AstSpan *as_span(AstExpression *type) {
	auto d = direct(type);
	if (!d)
		return 0;
	if (d->kind == Ast_Span)
		return (AstSpan *)d;
	return 0;
}

bool is_addressable(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Identifier: {
			return true;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != BinaryOperation::dot)
				break;

			return is_addressable(binop->right);
		}
		case Ast_Subscript: {
			return is_addressable(((AstSubscript *)expression)->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			if (unop->operation == UnaryOperation::dereference)
				return true;
			break;
		}
	}
	return false;
}


void Scope::add(AstStatement *statement TL_LPD) {
	if (!statement->parent_scope) {
		statement->parent_scope = this;
	}
	statement_list.add(statement TL_LA);
	if (statement->kind == Ast_Definition) {
		auto definition = (AstDefinition *)statement;
		definition_list.add(definition TL_LA);
		definition_map.get_or_insert(definition->name TL_LA).add(definition TL_LA);
	}
}
void Scope::add(AstDefinition *definition TL_LPD) {
	if (!definition->parent_scope) {
		definition->parent_scope = this;
	}
	statement_list.add(definition TL_LA);
	definition_list.add(definition TL_LA);
	definition_map.get_or_insert(definition->name TL_LA).add(definition TL_LA);
}

const Strings strings_en = {
	.usage = u8R"(Usage:
    {} <path> [options]
Option                             Description
--print-ast <when>                 Print the abstract syntax tree of the program
    parse                          After parsing
    type                           After typechecking
--keep-temp                        Keep temporary files (build.bat, *.asm, etc)
--output <path>                    Specify pathname of resulting executable
--target <toolchain>               Generate the executable using specified toolchain
    none
    fasm_x86_64_windows (default)
    nasm_x86_64_windows
--debug-path                       Print paths
--debug-poly                       Show polymorphic functions instantiations
)",
	.no_source_path_received = u8"No source path received.",
	.error = u8"Error",
	.warning = u8"Warning",

	.info = u8"Info",
};
const Strings strings_ru = {
	.usage =u8R"(Использование:
    {} <путь> [опции]
Опция                                  Описание
--print-ast <когда>                    Вывести дерево программы
    parse                              После парсинга
    type                               После проверки типов
--keep-temp                            Сохранить временные файлы (build.bat, *.asm, etc)
--output <путь>                        Путь к выходному исполняемому файлу
--target <генератор>                   Генератор исполняемого файла
    none
    fasm_x86_64_windows (по умолчанию)
    nasm_x86_64_windows
--debug-path                           Вывести пути
--debug-poly                           Показать создание полиморфных функций
)",
	.no_source_path_received = u8"Не указан путь к исходному файлу.",
	.error = u8"Ошибка",
	.warning = u8"Предупреждение",
	.info = u8"Информация",
};

void init_strings() {
	compiler->strings = strings_en;
	{
		utf16 buffer[256];
		GetUserDefaultLocaleName((wchar *)buffer, sizeof(buffer));
		if (as_span(buffer) == u"ru-RU"s) {
			compiler->strings = strings_ru;
		}
		for (auto i = &compiler->strings._start_marker + 1; i != &compiler->strings._end_marker; ++i) {
			if (*i == 0) {
				*i = *(&strings_en._start_marker + (i - &compiler->strings._start_marker));
			}
		}
	}
}
