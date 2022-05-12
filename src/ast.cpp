#include <ast.h>
#include <tl/ram.h>
#include <tl/process.h>

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
AstStruct *type_unknown;
AstStruct *type_unknown_enum;
AstStruct *type_poly;
AstStruct *type_void;
AstStruct *type_default_signed_integer;
AstStruct *type_default_unsigned_integer;
AstStruct *type_default_integer;
AstStruct *type_default_float;
AstStruct *type_overload_set;
AstStruct *type_typeinfo;
AstUnaryOperator *type_pointer_to_void;
AstUnaryOperator *type_pointer_to_typeinfo;

AstIdentifier *type_int;
AstIdentifier *type_sint;
AstIdentifier *type_uint;
AstIdentifier *type_float;

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
		case Ast_Struct: return struct_is_built_in((AstStruct *)type);
		case Ast_UnaryOperator: return true;
		case Ast_Subscript: return true;
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
	// if (node->kind == Ast_UnaryOperator)
	// 	return needs_semicolon(((AstUnaryOperator *)node)->expression);

	if (node->kind == Ast_Lambda && ((AstLambda *)node)->has_body == false)
		return true;

	return node->kind != Ast_Lambda && node->kind != Ast_Struct && node->kind != Ast_Enum;
}

bool can_be_global(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_ExpressionStatement: {
			auto expression = ((AstExpressionStatement *)statement)->expression;
			switch (expression->kind) {
				case Ast_Import:
					return true;
			}
			return false;
		}
		case Ast_If: {
			auto If = (AstIf *)statement;
			return If->is_constant;
		}
		case Ast_Definition:
		case Ast_OperatorDefinition:
		case Ast_Assert:
		case Ast_Print:
			return true;

		default:
			return false;
	}
}

bool is_type(AstExpression *expression) {
	assert(expression->type);
	return types_match(expression->type, type_type);
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
			ensure(Struct->definition);
			append(builder, Struct->definition->name);
			break;
		}
		case Ast_LambdaType: {
			auto lambda = ((AstLambdaType *)type)->lambda;

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
			if (lambda->return_parameter) {
				append(builder, "): ");
				append_type(builder, lambda->return_parameter->type, silent_error);
			} else {
				append(builder, ")");
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
			append(builder, (s64)get_constant_integer(subscript->index_expression).value());
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
			append(builder, " aka "s);
			append(builder, d_str);
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

s64 get_size(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;
			return Struct->size;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition()->expression);
		}
		case Ast_UnaryOperator: {
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case pointer:  return context.stack_word_size;
				case typeof:   return get_size(unop->expression->type);
				case option:   return get_size(unop->expression) + get_align(unop->expression);
				default: invalid_code_path();
			}
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)type;
			auto count = get_constant_integer(subscript->index_expression);
			assert(count.has_value());

			return get_size(subscript->expression) * (s64)count.value();
		}
		case Ast_Span: {
			return context.stack_word_size*2; // pointer+count;
		}
		case Ast_LambdaType: {
			return context.stack_word_size;
		}
		case Ast_Enum: {
			assert(!((AstEnum *)type)->underlying_type, "not implemented");
			return context.stack_word_size;
		}
		default: {
			invalid_code_path();
			return -1;
		}
	}
}

s64 get_align(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;
			return Struct->alignment;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_align(identifier->definition()->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				using enum UnaryOperation;
				case pointer: return 8;
				default: invalid_code_path();
			}
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)type;
			return get_align(subscript->expression);
		}
		case Ast_LambdaType: {
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

bool types_match(AstExpression *a, AstExpression *b) {
	if (a == b)
		return true;

	a = direct(a);
	b = direct(b);

	if (a->kind != b->kind) {
		return false;
	}

	switch (a->kind) {
		case Ast_Struct:
			return a == b;
		case Ast_UnaryOperator: {
			auto au = (AstUnaryOperator *)a;
			auto bu = (AstUnaryOperator *)b;
			return types_match(au->expression, bu->expression);
		}


		case Ast_Subscript: {
			auto eq = [](auto a, auto b) {
				if (!a.has_value()) return false;
				if (!b.has_value()) return false;
				return a.value() == b.value();
			};
			auto as = (AstSubscript *)a;
			auto bs = (AstSubscript *)b;
			return types_match(as->expression, bs->expression) &&
				eq(get_constant_integer(as->index_expression), get_constant_integer(bs->index_expression));
		}
		case Ast_Span: {
			auto as = (AstSpan *)a;
			auto bs = (AstSpan *)b;
			return types_match(as->expression, bs->expression);
		}
		case Ast_LambdaType: {
			auto al = ((AstLambdaType *)a)->lambda;
			auto bl = ((AstLambdaType *)b)->lambda;

			assert(al);
			assert(bl);

			if (!same_argument_and_return_types(al, bl))
				return false;

			if (al->convention != bl->convention)
				return false;

			return true;
		}
	}
	invalid_code_path();
}

AstExpression *direct(AstExpression *type) {
	switch (type->kind) {
		case Ast_Identifier: {
			do {
				auto identifier = (AstIdentifier *)type;
				if (!identifier->definition())
					return 0;
				type = identifier->definition()->expression;
				if (!type)
					return 0;
			} while (type->kind == Ast_Identifier);
			break;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)type;
			return direct(binop->right);
		}
		default:
			break;
	}

	return type;
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
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)type;
			return is_pointer(ident->definition()->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			if (unop->operation == UnaryOperation::pointer_or_dereference_or_unwrap) {
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
	return is_lambda_type(type) || is_pointer(type);
}
AstUnaryOperator *as_pointer(AstExpression *type) {
	switch (type->kind) {
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)type;
			return as_pointer(ident->definition()->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case UnaryOperation::pointer_or_dereference_or_unwrap:
				// This fails at parse time.
				// assert(is_type(unop->expression));
				case UnaryOperation::pointer:
					return unop;
			}
			break;
		}
	}
	return 0;
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
		case Ast_Import:
			return true;
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			return is_constant(binop->left) && is_constant(binop->right);
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
			assert(ident->definition()->expression);
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
	if (d->kind == Ast_Subscript)
		return (AstSubscript *)d;
	return 0;
}
AstSpan *as_span(AstExpression *type) {
	auto d = direct(type);
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
			auto subscript = (AstSubscript *)expression;
			assert(subscript->expression->kind == Ast_Identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return is_addressable(identifier);
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
