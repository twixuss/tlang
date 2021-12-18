#include "print_ast.h"

u32 tab_count = 0;

extern bool typecheck_finished;

void print_tabs() {
	for (u32 i = tab_count; i--;) {
		print("  ");
	}
}

void print_ast(AstBinaryOperator *node);
void print_ast(AstDefinition *node);
void print_ast(AstLambda *node);
void print_ast(AstIdentifier *node);
void print_ast(AstLiteral *node);
void print_ast(AstReturn *node);
void print_ast(AstCall *node);
void print_ast(AstStruct *node);
void print_ast(AstIf *node);
void print_ast(AstWhile *node);
void print_ast(AstExpressionStatement *node);
void print_ast(AstUnaryOperator *node);
void print_ast(AstSubscript *node);
void print_ast(AstCast*node);
void print_ast(AstNode *node) {
	print_tabs();
	print("'%'\n", node->location);
	switch (node->kind) {
		case Ast_definition: return print_ast((AstDefinition *)node);
		case Ast_lambda:     return print_ast((AstLambda     *)node);
		case Ast_identifier: return print_ast((AstIdentifier *)node);
		case Ast_literal:    return print_ast((AstLiteral    *)node);
		case Ast_return:     return print_ast((AstReturn     *)node);
		case Ast_call:       return print_ast((AstCall       *)node);
		case Ast_if:         return print_ast((AstIf         *)node);
		case Ast_while:      return print_ast((AstWhile      *)node);
		case Ast_expression_statement: return print_ast((AstExpressionStatement *)node);
		case Ast_binary_operator: return print_ast((AstBinaryOperator *)node);
		case Ast_struct: return print_ast((AstStruct *)node);
		case Ast_unary_operator: return print_ast((AstUnaryOperator *)node);
		case Ast_subscript: return print_ast((AstSubscript *)node);
		case Ast_cast : return print_ast((AstCast*)node);
		default:
			print_tabs();
			print("unknown - uid: %\n", node->uid);
			break;
	}
}
void print_ast(AstDefinition *node) {
	if (node->built_in)
		return;

	print_tabs();
	print("definition - name: %, type: %, uid: %\n", node->name, type_to_string(node->type, true), node->uid);
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstLambda *node) {
	print_tabs();
	print("lambda - return_type: %, uid: %\n", "type_to_string(node->return_type, true)", node->uid);
	tab_count += 1;
	for (auto statement : node->body_scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
}
void print_ast(AstIdentifier *node) {
	print_tabs();
	print("identifier - type: %, uid: %, definition.uid: %\n", type_to_string(node->type, true), node->uid, node->definition ? node->definition->uid : -1);
}
void print_ast(AstCall *node) {
	print_tabs();
	print("call - name: %, type: %, uid: %, definition.uid: %\n", node->name, type_to_string(node->type, true), node->uid, node->definition ? node->definition->uid : -1);
	tab_count += 1;
	print_tabs();
	print("arguments:\n");
	tab_count += 1;
	for (auto argument : node->arguments) {
		print_ast(argument);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstBinaryOperator *node) {
	print_tabs();
	print("binary - operation: %, type: %, uid: %\n", binary_operator_string(node->operation), type_to_string(node->type, true), node->uid);
	tab_count += 1;

	print_tabs();
	print("left:\n");
	tab_count += 1;
	print_ast(node->left);
	tab_count -= 1;

	print_tabs();
	print("right:\n");
	tab_count += 1;
	print_ast(node->right);
	tab_count -= 1;

	tab_count -= 1;
}
void print_ast(AstUnaryOperator *node) {
	print_tabs();
	print("unary - operation: %, type: %, uid: %\n", node->operation, type_to_string(node->type, true), node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstLiteral *node) {
	print_tabs();
	if (typecheck_finished) {
			 if (types_match(node->type, &type_u8  )) print("u8  literal - value: %, uid: %\n", (u8 )node->integer, node->uid);
		else if (types_match(node->type, &type_u16 )) print("u16 literal - value: %, uid: %\n", (u16)node->integer, node->uid);
		else if (types_match(node->type, &type_u32 )) print("u32 literal - value: %, uid: %\n", (u32)node->integer, node->uid);
		else if (types_match(node->type, &type_u64 )) print("u64 literal - value: %, uid: %\n", (u64)node->integer, node->uid);
		else if (types_match(node->type, &type_s8  )) print("s8  literal - value: %, uid: %\n", (s8 )node->integer, node->uid);
		else if (types_match(node->type, &type_s16 )) print("s16 literal - value: %, uid: %\n", (s16)node->integer, node->uid);
		else if (types_match(node->type, &type_s32 )) print("s32 literal - value: %, uid: %\n", (s32)node->integer, node->uid);
		else if (types_match(node->type, &type_s64 )) print("s64 literal - value: %, uid: %\n", (s64)node->integer, node->uid);
		else if (types_match(node->type, &type_unsized_integer)) print("unsized integer literal - value: %, uid: %\n", (u64)node->integer, node->uid);
		else if (types_match(node->type, &type_bool)) print("bool literal - value: %, uid: %\n", node->Bool, node->uid);
		else if (types_match(node->type, &type_string)) print("string literal - value: %, uid: %\n", node->location, node->uid);
		else if (node->type == &type_pointer_to_void) print("null literal - uid: %\n", node->uid);
		else invalid_code_path();
	} else {
		switch (node->literal_kind) {
			case LiteralKind::integer: print("integer literal - value: %, uid: %\n", (s64)node->integer, node->uid); break;
			case LiteralKind::boolean: print("boolean literal - value: %, uid: %\n", node->Bool, node->uid); break;
			case LiteralKind::string : print( "string literal - value: %, uid: %\n", node->location, node->uid); break;
			default: invalid_code_path();
		}
	}
}
void print_ast(AstReturn *node) {
	print_tabs();
	print("return - uid: %\n", node->uid);
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstStruct *node) {
	print_tabs();
	if (node->definition)
		print("struct - name: %, uid: %\n", node->definition->name, node->uid);
	else
		print("struct - unnamed, uid: %\n", node->uid);
	tab_count += 1;
	for (auto member : node->members) {
		print_ast(member);
	}
	tab_count -= 1;
}
void print_ast(AstIf *node) {
	print_tabs(); print("if - uid: %\n", node->uid);
	tab_count += 1;
	print_tabs(); print("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_tabs(); print("true statements:\n");
	tab_count += 1;
	for (auto statement : node->true_scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	print_tabs(); print("false statements:\n");
	tab_count += 1;
	for (auto statement : node->false_scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstExpressionStatement *node) {
	print_ast(node->expression);
}
void print_ast(AstSubscript *node) {
	print_tabs();
	print("subscript - type: %, uid: %\n", type_to_string(node->type, true), node->uid);
	tab_count += 1;
	print_tabs();
	print("index expression:\n");
	tab_count += 1;
	print_ast(node->index_expression);
	tab_count -= 1;
	print_tabs();
	print("expression:\n");
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstWhile *node) {
	print_tabs(); print("while - uid: %\n", node->uid);
	tab_count += 1;
	print_tabs(); print("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_tabs(); print("statements:\n");
	tab_count += 1;
	for (auto statement : node->scope.statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstCast*cast) {
	print_tabs(); print("cast - type: %, uid: %\n", type_to_string(cast->type), cast->uid);
	print_ast(cast->expression);
}

void print_ast() {
	timed_function();
	for (auto statement : global_scope.statements) {
		print_ast(statement);
	}
}
