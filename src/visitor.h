#pragma once
#include "compiler.h"

struct EnterScope {
	Scope *scope;
};

struct ExitScope {
	Scope *scope;
};

#define VISIT(x) \
	do { \
		if constexpr (is_same<decltype(visitor(&x)), void>) { \
			visitor(&x); \
		} else { \
			if (!visitor(&x)) \
				return false; \
		} \
		if (!visit(x, visitor)) \
			return false; \
	} while (0)

// Returns true if iteration was not stopped.
// Iteration is stopped when visitor returns false.
bool visit(AstNode *node, auto &&visitor);
bool visit(Scope *scope, auto &&visitor) {
	visitor(EnterScope{scope});
	for (auto &statement : scope->statement_list) {
		VISIT(statement);
	}
	visitor(ExitScope{scope});
	return true;
}

bool visit_children(AstDefinition *node, auto &&visitor) {
	if (node->parsed_type)
		VISIT(node->parsed_type);
	if (node->expression)
		VISIT(node->expression);
	return true;
}
bool visit_children(AstReturn *node, auto &&visitor) {
	if (node->expression)
		VISIT(node->expression);
	return true;
}
bool visit_children(AstLambda *node, auto &&visitor) {
	VISIT(node->constant_scope);
	VISIT(node->parameter_scope);
	VISIT(node->body);
	return true;
}
bool visit_children(AstLambdaType *node, auto &&visitor) {
	return true;
}
bool visit_children(AstIdentifier *node, auto &&visitor) {
	return true;
}
bool visit_children(AstLiteral *node, auto &&visitor) {
	return true;
}
bool visit_children(AstCall *node, auto &&visitor) {
	VISIT(node->callable);
	for (auto &arg : node->sorted_arguments) {
		if (arg)
			VISIT(arg);
	}
	return true;
}
bool visit_children(AstBinaryOperator *node, auto &&visitor) {
	VISIT(node->left);
	VISIT(node->right);
	return true;
}
bool visit_children(AstStruct *node, auto &&visitor) {
	VISIT(node->parameter_scope);
	VISIT(node->member_scope);
	return true;
}
bool visit_children(AstIf *node, auto &&visitor) {
	VISIT(node->condition);
	VISIT(node->true_block);
	VISIT(node->false_block);
	return true;
}
bool visit_children(AstExpressionStatement *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstUnaryOperator *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstWhile *node, auto &&visitor) {
	VISIT(node->condition);
	VISIT(node->scope);
	return true;
}
bool visit_children(AstSubscript *node, auto &&visitor) {
	VISIT(node->index_expression);
	VISIT(node->expression);
	return true;
}
bool visit_children(AstArray *node, auto &&visitor) {
	VISIT(node->count_expression);
	VISIT(node->element_type);
	return true;
}
bool visit_children(AstSpan *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstBlock *node, auto &&visitor) {
	VISIT(node->scope);
	return true;
}
bool visit_children(AstTuple *node, auto &&visitor) {
	return true;
}
bool visit_children(AstTest *node, auto &&visitor) {
	VISIT(node->scope);
	return true;
}
bool visit_children(AstAssert *node, auto &&visitor) {
	VISIT(node->condition);
	return true;
}
bool visit_children(AstDefer *node, auto &&visitor) {
	VISIT(node->scope);
	return true;
}
bool visit_children(AstPrint *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstOperatorDefinition *node, auto &&visitor) {
	VISIT(node->definition);
	return true;
}
bool visit_children(AstParse *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstEnum *node, auto &&visitor) {
	VISIT(node->scope);
	return true;
}
bool visit_children(AstEmptyStatement *node, auto &&visitor) {
	return true;
}
bool visit_children(AstFor *node, auto &&visitor) {
	VISIT(node->range);
	VISIT(node->scope);
	return true;
}
bool visit_children(AstLoopControl *node, auto &&visitor) {
	return true;
}
bool visit_children(AstMatch *node, auto &&visitor) {
	VISIT(node->expression);
	for (auto &Case : node->cases) {
		if (Case.expression) {
			VISIT(Case.expression);
		}
		VISIT(Case.body);
	}
	return true;
}
bool visit_children(AstUsing *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstArrayInitializer *node, auto &&visitor) {
	for (auto &e : node->elements) {
		VISIT(e);
	}
	return true;
}
bool visit_children(AstYield *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstDistinct *node, auto &&visitor) {
	VISIT(node->expression);
	return true;
}
bool visit_children(AstForMarker *node, auto &&visitor) {
	if (node->expression)
		VISIT(node->expression);
	return true;
}

bool visit(AstNode *node, auto &&visitor) {
	switch (node->kind) {
#define e(name) \
	case Ast_##name: { \
		auto x = (Ast##name *)node; \
		if constexpr (is_same<decltype(visitor(x)), void>) { \
			visitor(x); \
		} else { \
			if (!visitor(x)) \
				return false; \
		} \
		if (!visit_children(x, visitor)) \
			return false; \
		break; \
	}
		ENUMERATE_AST_KIND
#undef e
	}
	return true;
}

#undef VISIT
