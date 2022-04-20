#include <ast.h>
#include <tl/ram.h>
#include <tl/process.h>

umm append(StringBuilder &builder, AstKind kind) {
	switch (kind) {
#define e(name) case Ast_ ## name: return append(builder, u8#name##s);
		ENUMERATE_AST_KIND(e)
#undef e
	}
	return append(builder, "(unknown AstKind)");
}

s32 ast_node_uid_counter;

BlockList<AstDefinition> ast_definitions;

AstStruct *type_bool;
AstStruct *type_u8;
AstStruct *type_u16;
AstStruct *type_u32;
AstStruct *type_u64;
AstStruct *type_s8;
AstStruct *type_s16;
AstStruct *type_s32;
AstStruct *type_s64;
AstStruct *type_f32;
AstStruct *type_f64;
AstStruct *type_type;
AstStruct *type_string;
AstStruct *type_noinit;
AstStruct *type_unsized_integer;
AstStruct *type_unsized_float;
AstStruct *type_void;
AstStruct *type_default_signed_integer;
AstStruct *type_default_unsigned_integer;
AstStruct *type_default_integer;
AstStruct *type_default_float;
AstUnaryOperator *type_pointer_to_void;

AstIdentifier *type_int;
AstIdentifier *type_sint;
AstIdentifier *type_uint;

bool struct_is_built_in(AstStruct *type) {
	return
		type == type_bool   ||
		type == type_u8     ||
		type == type_u16    ||
		type == type_u32    ||
		type == type_u64    ||
		type == type_s8     ||
		type == type_s16    ||
		type == type_s32    ||
		type == type_s64    ||
		type == type_f32    ||
		type == type_f64    ||
		type == type_type   ||
		type == type_string ||
		type == type_noinit ||
		type == type_unsized_integer ||
		type == type_unsized_float ||
		type == type_void;
}
bool type_is_built_in(AstExpression *type) {
	switch (type->kind) {
		case Ast_struct: return struct_is_built_in((AstStruct *)type);
		case Ast_unary_operator: return true;
		case Ast_subscript: return true;
	}
	invalid_code_path();
	return {};
}

Scope global_scope;

Mutex global_scope_mutex;
void lock(Scope *scope) {
	if (scope == &global_scope) {
		lock(global_scope_mutex);
	}
}
void unlock(Scope *scope) {
	if (scope == &global_scope) {
		unlock(global_scope_mutex);
	}
}

// HashMap<String, AstDefinition *> names_not_available_for_globals;

bool needs_semicolon(AstExpression *node) {
	// if (node->kind == Ast_unary_operator)
	// 	return needs_semicolon(((AstUnaryOperator *)node)->expression);

	if (node->kind == Ast_lambda && ((AstLambda *)node)->has_body == false)
		return true;

	return node->kind != Ast_lambda && node->kind != Ast_struct;
}

bool can_be_global(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_expression_statement: {
			auto expression = ((AstExpressionStatement *)statement)->expression;
			switch (expression->kind) {
				case Ast_import:
					return true;
			}
			return false;
		}
		case Ast_if: {
			auto If = (AstIf *)statement;
			return If->is_constant;
		}
		case Ast_definition:
		case Ast_operator_definition:
		case Ast_assert:
			return true;

		default:
			return false;
	}
}

bool is_type(AstExpression *expression) {
	if (types_match(expression->type, type_type))
		return true;

	if (expression->kind == Ast_lambda)
		return ((AstLambda *)expression)->is_type;

	return false;
}

void append_type(StringBuilder &builder, AstExpression *type, bool silent_error) {
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
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			ensure(Struct->definition);
			append(builder, Struct->definition->name);
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)type;

			append(builder, "fn ");
			switch (lambda->convention) {
				case CallingConvention::stdcall: append(builder, "#stdcall "); break;
			}
			append(builder, "(");
			for (auto &parameter : lambda->parameters) {
				if (&parameter != lambda->parameters.data) {
					append(builder, ", ");
				}
				append_type(builder, parameter->type, silent_error);
			}
			append(builder, ") -> ");
			append_type(builder, lambda->return_parameter->type, silent_error);
			break;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			ensure(identifier->definition);
			ensure(types_match(identifier->definition->expression->type, type_type));
			append(builder, identifier->name);
			break;
		}
		case Ast_unary_operator: {
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)type;
			ensure(unop->operation == pointer);
			append(builder, as_string(unop->operation));
			append_type(builder, unop->expression, silent_error);
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;
			append(builder, '[');
			append(builder, (s64)get_constant_integer(subscript->index_expression).value());
			append(builder, ']');
			append_type(builder, subscript->expression, silent_error);
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
			append(builder, " (aka "s);
			append(builder, d_str);
			append(builder, ')');
			return (HeapString)to_string<MyAllocator>(builder);
		}
	}
	append(builder, type_str);
	return (HeapString)to_string<MyAllocator>(builder);
}

HeapString type_name(AstExpression *type, bool silent_error) {
	if (!type)
		return (HeapString)to_list<MyAllocator>(u8"null"s);

	StringBuilder builder;
	append_type(builder, type, silent_error);
	return (HeapString)to_string<MyAllocator>(builder);
}


s64 get_align(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			return Struct->alignment;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				using enum UnaryOperation;
				case pointer: return 8;
				default: invalid_code_path();
			}
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;
			return get_align(subscript->expression);
		}
		case Ast_lambda: {
			return 8;
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

bool same_argument_and_return_types(AstLambda *a, AstLambda *b) {
	if (a->parameters.count != b->parameters.count)
		return false;

	if (!types_match(a->return_parameter->type, b->return_parameter->type))
		return false;

	for (umm i = 0; i < a->parameters.count; ++i) {
		if (!types_match(a->parameters[i]->type, b->parameters[i]->type))
			return false;
	}
	return true;
}

bool types_match_ns(AstExpression *a, AstExpression *b) {
	while (a->kind == Ast_identifier) {
		a = ((AstIdentifier *)a)->definition->expression;
	}
	while (b->kind == Ast_identifier) {
		b = ((AstIdentifier *)b)->definition->expression;
	}

	if (a->kind != b->kind) {
		return false;
	}

	if (a->kind == Ast_struct) {
		return a == b;
	}

	if (a->kind == Ast_unary_operator) {
		return types_match(((AstUnaryOperator *)a)->expression, ((AstUnaryOperator *)b)->expression);
	}

	auto eq = [](auto a, auto b) {
		if (!a.has_value()) return false;
		if (!b.has_value()) return false;
		return a.value() == b.value();
	};

	if (a->kind == Ast_subscript) {
		return types_match(((AstSubscript *)a)->expression, ((AstSubscript *)b)->expression) &&
			eq(get_constant_integer(((AstSubscript *)a)->index_expression),
			get_constant_integer(((AstSubscript *)b)->index_expression));
	}

	if (a->kind == Ast_lambda) {
		auto al = (AstLambda *)a;
		auto bl = (AstLambda *)b;

		if (!same_argument_and_return_types(al, bl))
			return false;

		if (al->convention != bl->convention)
			return false;

		return true;
	}

	return false;
}

bool types_match(AstExpression *type_a, AstExpression *type_b) {
	if (type_a == type_b)
		return true;

	return types_match_ns(type_a, type_b) || types_match_ns(type_b, type_a);
}

AstExpression *direct(AstExpression *type) {
	switch (type->kind) {
		case Ast_identifier: {
			do {
				auto identifier = (AstIdentifier *)type;
				if (!identifier->definition)
					return 0;
				type = identifier->definition->expression;
			} while (type->kind == Ast_identifier);
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)type;
			return direct(binop->right);
		}
		case Ast_unary_operator:
		case Ast_subscript:
		case Ast_struct:
		case Ast_lambda:
			break;
		default: invalid_code_path();
	}

	return type;
}

AstStruct *get_struct(AstExpression *type) {
	type = direct(type);
	if (type->kind == Ast_struct)
		return (AstStruct *)type;

	return 0;
}

AstExpression *get_definition_expression(AstExpression *expression) {
	while (expression->kind == Ast_identifier)
		expression = ((AstIdentifier *)expression)->definition->expression;
	return expression;
}

String operator_string(u64 op) {
	switch (op) {
		case '!': return u8"!"s;
		case '+': return u8"+"s;
		case '-': return u8"-"s;
		case '*': return u8"*"s;
		case '/': return u8"/"s;
		case '%': return u8"%"s;
		case '|': return u8"|"s;
		case '&': return u8"&"s;
		case '^': return u8"^"s;
		case '.': return u8"."s;
		case '>': return u8">"s;
		case '<': return u8"<"s;
		case '>=': return u8">="s;
		case '<=': return u8"<="s;
		case '==': return u8"=="s;
		case '!=': return u8"!="s;
		case '=': return u8"="s;
		case '+=': return u8"+="s;
		case '-=': return u8"-="s;
		case '*=': return u8"*="s;
		case '/=': return u8"/="s;
		case '%=': return u8"%="s;
		case '|=': return u8"|="s;
		case '&=': return u8"&="s;
		case '^=': return u8"^="s;
		case '>>': return u8">>"s;
		case '<<': return u8"<<"s;
		case '>>=': return u8">>="s;
		case '<<=': return u8"<<="s;
	}
	invalid_code_path();
	return {};
}
String operator_string(BinaryOperation op) {
	using enum BinaryOperation;
	switch (op) {
		case add: return u8"+"s;
		case sub: return u8"-"s;
		case mul: return u8"*"s;
		case div: return u8"/"s;
		case mod: return u8"%"s;
		case bor: return u8"|"s;
		case band: return u8"&"s;
		case bxor: return u8"^"s;
		case bsr: return u8">>"s;
		case bsl: return u8"<<"s;
		case dot: return u8"."s;
		case gt: return u8">"s;
		case lt: return u8"<"s;
		case ge: return u8">="s;
		case le: return u8"<="s;
		case eq: return u8"=="s;
		case ne: return u8"!="s;
		case ass: return u8"="s;
		case addass: return u8"+="s;
		case subass: return u8"-="s;
		case mulass: return u8"*="s;
		case divass: return u8"/="s;
		case modass: return u8"%="s;
		case borass: return u8"|="s;
		case bandass: return u8"&="s;
		case bxorass: return u8"^="s;
		case bsrass: return u8">>="s;
		case bslass: return u8"<<="s;
		case as: return u8"as"s;
	}
	invalid_code_path();
	return {};
}

bool is_integer(AstExpression *type) {
	return
		types_match(type, type_unsized_integer) ||
		types_match(type, type_u8) ||
		types_match(type, type_u16) ||
		types_match(type, type_u32) ||
		types_match(type, type_u64) ||
		types_match(type, type_s8) ||
		types_match(type, type_s16) ||
		types_match(type, type_s32) ||
		types_match(type, type_s64);
}

bool is_signed(AstExpression *type) {
	return
		types_match(type, type_s8) ||
		types_match(type, type_s16) ||
		types_match(type, type_s32) ||
		types_match(type, type_s64);
}

bool is_integer(AstStruct *type) {
	return
		type == type_unsized_integer ||
		type == type_u8 ||
		type == type_u16 ||
		type == type_u32 ||
		type == type_u64 ||
		type == type_s8 ||
		type == type_s16 ||
		type == type_s32 ||
		type == type_s64;
}

bool is_signed(AstStruct *type) {
	return
		type == type_s8 ||
		type == type_s16 ||
		type == type_s32 ||
		type == type_s64;
}

bool is_float(AstExpression *type) {
	return
		types_match(type, type_unsized_float) ||
		types_match(type, type_f32) ||
		types_match(type, type_f64);
}

bool is_float(AstStruct *type) {
	return
		type == type_unsized_float ||
		type == type_f32 ||
		type == type_f64;
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

bool is_pointer(AstExpression *type) {
	switch (type->kind) {
		case Ast_identifier: {
			auto ident = (AstIdentifier *)type;
			return is_pointer(ident->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			if (unop->operation == UnaryOperation::pointer_or_dereference) {
				// This fails at parse time.
				// assert(is_type(unop->expression));
				return true;
			}
			return unop->operation == UnaryOperation::pointer;
		}
	}
	return false;
}
bool is_pointer_internally(AstExpression *type) {
	return type->kind == Ast_lambda || is_pointer(type);
}

AstLiteral *get_literal(AstExpression *expression) {
    switch (expression->kind) {
        case Ast_literal: return (AstLiteral *)expression;
        case Ast_identifier: {
            auto identifier = (AstIdentifier *)expression;
			if (identifier->definition) {
				auto definition = identifier->definition;
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
    if (expression->kind == Ast_literal)
        return true;

    if (expression->kind == Ast_identifier) {
        auto identifier = (AstIdentifier *)expression;
        if (identifier->definition)
            return identifier->definition->is_constant;
        return false;
    }

    if (expression->kind == Ast_binary_operator) {
        auto binop = (AstBinaryOperator *)expression;
        return is_constant(binop->left) && is_constant(binop->right);
    }

    if (expression->kind == Ast_lambda) {
        return true;
    }

    if (expression->kind == Ast_import) {
        return true;
    }

    if (is_type(expression))
        return true;

    return false;
}

AstLambda *get_lambda(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_lambda:
			return (AstLambda *)expression;
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			assert(ident->definition->expression);
			return get_lambda(ident->definition->expression);
		}
	}
	return 0;
}

bool is_lambda(AstExpression *expression) {
	return direct(expression)->kind == Ast_lambda;
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
		case Ast_tuple:
			return map(((AstTuple *)call->argument)->expressions, [](auto &e) { return &e; });
		default: invalid_code_path();
	}
}

List<Expression<>> get_arguments(AstCall *call) {
	switch (call->argument->kind) {
		case Ast_tuple: return ((AstTuple *)call->argument)->expressions;
		default: invalid_code_path();
	}
}
*/

bool is_sized_array(AstExpression *type) {
	return type->kind == Ast_subscript;
}
