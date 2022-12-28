#pragma once
#include "compiler.h"

void visit(AstNode *node, auto &&visitor);
void visit(Scope *scope, auto &&visitor) {
	for (auto statement : scope->statement_list) {
		visit(statement, visitor);
	}
}

void visit_children(AstNode *node, auto &&visitor) {}
void visit_children(AstDefinition *node, auto &&visitor) {
	if (node->type)
		visit(node->type, visitor);
	if (node->expression)
		visit(node->expression, visitor);
}
void visit_children(AstReturn *node, auto &&visitor) {
	if (node->expression)
		visit(node->expression, visitor);
}
void visit_children(AstLambda *node, auto &&visitor) {
	visit(node->constant_scope, visitor);
	visit(node->parameter_scope, visitor);
	visit(node->body, visitor);
}
void visit_children(AstLambdaType *node, auto &&visitor) {}
void visit_children(AstIdentifier *node, auto &&visitor) {}
void visit_children(AstLiteral *node, auto &&visitor) {}
void visit_children(AstCall *node, auto &&visitor) {
	visit(node->callable, visitor);
	for (auto arg : node->sorted_arguments) {
		visit(arg, visitor);
	}
}
void visit_children(AstBinaryOperator *node, auto &&visitor) {
	visit(node->left, visitor);
	visit(node->right, visitor);
}
void visit_children(AstStruct *node, auto &&visitor) {
	visit(node->parameter_scope, visitor);
	visit(node->member_scope, visitor);
}
void visit_children(AstIf *node, auto &&visitor) {
	visit(node->condition, visitor);
	visit(node->true_block, visitor);
	visit(node->false_block, visitor);
}
void visit_children(AstExpressionStatement *node, auto &&visitor) {
	visit(node->expression, visitor);
}
void visit_children(AstUnaryOperator *node, auto &&visitor) {
	visit(node->expression, visitor);
}
void visit_children(AstWhile *node, auto &&visitor) {
	visit(node->condition, visitor);
	visit(node->scope, visitor);
}
void visit_children(AstSubscript *node, auto &&visitor) {
	visit(node->index_expression, visitor);
	visit(node->expression, visitor);
}
void visit_children(AstSpan *node, auto &&visitor) {
	visit(node->expression, visitor);
}
void visit_children(AstBlock *node, auto &&visitor) {
	visit(node->scope, visitor);
}
void visit_children(AstTuple *node, auto &&visitor) {}
void visit_children(AstTest *node, auto &&visitor) {
	visit(node->scope, visitor);
}
void visit_children(AstAssert *node, auto &&visitor) {
	visit(node->condition, visitor);
}
void visit_children(AstDefer *node, auto &&visitor) {
	visit(node->scope, visitor);
}
void visit_children(AstPrint *node, auto &&visitor) {
	visit(node->expression, visitor);
}
void visit_children(AstOperatorDefinition *node, auto &&visitor) {
	visit(node->definition, visitor);
}
void visit_children(AstParse *node, auto &&visitor) {
	visit(node->expression, visitor);
}
void visit_children(AstEnum *node, auto &&visitor) {
	visit(node->scope, visitor);
}
void visit_children(AstEmptyStatement *node, auto &&visitor) {}
void visit_children(AstFor *node, auto &&visitor) {}
void visit_children(AstLoopControl *node, auto &&visitor) {}
void visit_children(AstMatch *node, auto &&visitor) {}
void visit_children(AstUsing *node, auto &&visitor) {}
void visit_children(AstArrayInitializer *node, auto &&visitor) {}

void visit(AstNode *node, auto &&visitor) {
	switch (node->kind) {
#define e(name) \
		case Ast_##name: { auto name = (Ast##name *)node; visitor(name); visit_children(name, visitor); break;}
		ENUMERATE_AST_KIND
#undef e
	}
}
