#include <ast.h>

umm append(StringBuilder &builder, AstKind kind) {
	switch (kind) {
#define e(name) case Ast_ ## name: return append(builder, u8#name##s);
		ENUMERATE_AST_KIND(e)
#undef e
	}
	return append(builder, "(unknown AstKind)");
}

umm append(StringBuilder &builder, BinaryOperation op) {
	switch (op) {
		using enum BinaryOperation;
#define e(name) case name: return append(builder, u8#name##s);
		ENUMERATE_BINARY_OPERATIONS(e)
#undef e
	}
	return append(builder, "(unknown BinaryOperation)");
}
umm append(StringBuilder &builder, UnaryOperation op) {
	switch (op) {
		using enum UnaryOperation;
#define e(name) case name: return append(builder, u8#name##s);
		ENUMERATE_UNARY_OPERATIONS(e)
#undef e
	}
	return append(builder, "(unknown UnaryOperation)");
}

s32 ast_node_uid_counter;

AstStruct type_bool;

AstStruct type_u8;
AstStruct type_u16;
AstStruct type_u32;
AstStruct type_u64;
AstStruct type_s8;
AstStruct type_s16;
AstStruct type_s32;
AstStruct type_s64;
AstStruct type_type;
AstStruct type_string;

AstStruct type_unsized_integer;
AstStruct type_void;
AstStruct *type_default_integer;

HashMap<Span<utf8>, AstStatement *> global_statements;
RecursiveMutex global_statements_mutex;

bool needs_semicolon(AstNode *node) {
	return node->kind != Ast_lambda;
}

bool can_be_global(AstStatement *statement) {
	if (statement->kind == Ast_definition)
		return true;
	return false;
}

Optional<s64> get_constant_integer(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::integer) {
				return literal->u64;
			}
			break;
		}
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			if (ident->definition && ident->definition->is_constant) {
				return get_constant_integer(ident->definition->expression);
			}
			break;
		}
	}
	return {};
}

bool is_type(AstExpression *expression) {
	if (expression->type == &type_type)
		return true;

	if (expression->kind == Ast_lambda)
		return true;

	return false;
}

void append_type(StringBuilder &builder, AstExpression *type) {
	assert(is_type(type));
	switch (type->kind) {
		case Ast_struct: {
			append(builder, ((AstStruct *)type)->name);
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)type;

			append(builder, "fn (");
			for (auto &parameter : lambda->parameters) {
				if (&parameter != lambda->parameters.data) {
					append(builder, ", ");
				}
				append_type(builder, parameter->type);
			}
			append(builder, ") ");
			append_type(builder, lambda->return_type);
			break;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			assert(identifier->definition);
			assert(identifier->definition->expression->type == &type_type);
			append(builder, identifier->name);
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			assert(unop->operation == UnaryOperation::star);
			append(builder, '*');
			append_type(builder, unop->expression);
			break;
		}
		default: {
			invalid_code_path();
		}
	}
}

List<utf8> type_to_string(AstExpression *type) {
	if (!type)
		return to_list(u8"***null***"s);
	StringBuilder builder;
	append_type(builder, type);
	return (List<utf8>)to_string(builder);
}

u32 get_size(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			return Struct->size;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				using enum UnaryOperation;
				case star: return 8;
				default: invalid_code_path();
			}
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

bool types_match_ns(AstExpression *a, AstExpression *b) {
	while (a->kind == Ast_identifier) {
		a = ((AstIdentifier *)a)->definition->expression;
	}
	while (b->kind == Ast_identifier) {
		b = ((AstIdentifier *)b)->definition->expression;
	}

	if (a->kind == Ast_struct && b->kind == Ast_struct) {
		return a == b;
	}

	return false;
}

bool types_match(AstExpression *type_a, AstExpression *type_b) {
	assert(is_type(type_a));
	assert(is_type(type_b));

	if (type_a == type_b)
		return true;

	return types_match_ns(type_a, type_b) || types_match_ns(type_b, type_a);
}

AstStruct *get_struct(AstExpression *type) {
	while (type->kind == Ast_identifier) {
		type = ((AstIdentifier *)type)->definition->expression;
	}

	if (type->kind == Ast_struct)
		return (AstStruct *)type;

	return 0;
}
