#pragma once
#include "compiler.h"

#define VISIT(x) (visitor(&x), visit(x, visitor))

void visit(AstNode *node, auto &&visitor);
void visit(Scope *scope, auto &&visitor) {
	visitor(scope);
	for (auto &statement : scope->statement_list) {
		VISIT(statement);
	}
}

void visit_children(AstDefinition *node, auto &&visitor) {
	if (node->parsed_type)
		VISIT(node->parsed_type);
	if (node->expression)
		VISIT(node->expression);
}
void visit_children(AstReturn *node, auto &&visitor) {
	if (node->expression)
		VISIT(node->expression);
}
void visit_children(AstLambda *node, auto &&visitor) {
	VISIT(node->constant_scope);
	VISIT(node->parameter_scope);
	VISIT(node->body);
}
void visit_children(AstLambdaType *node, auto &&visitor) {}
void visit_children(AstIdentifier *node, auto &&visitor) {}
void visit_children(AstLiteral *node, auto &&visitor) {}
void visit_children(AstCall *node, auto &&visitor) {
	VISIT(node->callable);
	for (auto &arg : node->sorted_arguments) {
		if (arg)
			VISIT(arg);
	}
}
void visit_children(AstBinaryOperator *node, auto &&visitor) {
	VISIT(node->left);
	VISIT(node->right);
}
void visit_children(AstStruct *node, auto &&visitor) {
	VISIT(node->parameter_scope);
	VISIT(node->member_scope);
}
void visit_children(AstIf *node, auto &&visitor) {
	VISIT(node->condition);
	VISIT(node->true_block);
	VISIT(node->false_block);
}
void visit_children(AstExpressionStatement *node, auto &&visitor) {
	VISIT(node->expression);
}
void visit_children(AstUnaryOperator *node, auto &&visitor) {
	VISIT(node->expression);
}
void visit_children(AstWhile *node, auto &&visitor) {
	VISIT(node->condition);
	VISIT(node->scope);
}
void visit_children(AstSubscript *node, auto &&visitor) {
	VISIT(node->index_expression);
	VISIT(node->expression);
}
void visit_children(AstArray *node, auto &&visitor) {
	VISIT(node->count_expression);
	VISIT(node->element_type);
}
void visit_children(AstSpan *node, auto &&visitor) {
	VISIT(node->expression);
}
void visit_children(AstBlock *node, auto &&visitor) {
	VISIT(node->scope);
}
void visit_children(AstTuple *node, auto &&visitor) {}
void visit_children(AstTest *node, auto &&visitor) {
	VISIT(node->scope);
}
void visit_children(AstAssert *node, auto &&visitor) {
	VISIT(node->condition);
}
void visit_children(AstDefer *node, auto &&visitor) {
	VISIT(node->scope);
}
void visit_children(AstPrint *node, auto &&visitor) {
	VISIT(node->expression);
}
void visit_children(AstOperatorDefinition *node, auto &&visitor) {
	VISIT(node->definition);
}
void visit_children(AstParse *node, auto &&visitor) {
	VISIT(node->expression);
}
void visit_children(AstEnum *node, auto &&visitor) {
	VISIT(node->scope);
}
void visit_children(AstEmptyStatement *node, auto &&visitor) {}
void visit_children(AstFor *node, auto &&visitor) {
	VISIT(node->range);
	VISIT(node->scope);
}
void visit_children(AstLoopControl *node, auto &&visitor) {}
void visit_children(AstMatch *node, auto &&visitor) {
	VISIT(node->expression);
	for (auto &Case : node->cases) {
		if (Case.expression) {
			VISIT(Case.expression);
		}
		VISIT(Case.block);
	}
}
void visit_children(AstUsing *node, auto &&visitor) { VISIT(node->expression); }
void visit_children(AstArrayInitializer *node, auto &&visitor) {
	for (auto &e : node->elements) {
		VISIT(e);
	}
}
void visit_children(AstYield *node, auto &&visitor) { VISIT(node->expression); }
void visit_children(AstDistinct *node, auto &&visitor) { VISIT(node->expression); }
void visit_children(AstForMarker *node, auto &&visitor) { if (node->expression) VISIT(node->expression); }

void visit(AstNode *node, auto &&visitor) {
	switch (node->kind) {
#define e(name) \
		case Ast_##name: { auto name = (Ast##name *)node; visitor(name); visit_children(name, visitor); break;}
		ENUMERATE_AST_KIND
#undef e
	}
}

#undef VISIT
