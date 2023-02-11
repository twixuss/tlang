#pragma once
#include "compiler.h"

void print_lowered();
void print_lowered(AstStatement *);
void print_lowered(AstExpression *);

inline void print_lowered(AstNode *node) {
	switch (node->kind) {
#define e(name) case Ast_##name: return print_lowered((AstExpression *)node);
ENUMERATE_EXPRESSION_KIND
#undef e
	}
	return print_lowered((AstStatement *)node);
}
