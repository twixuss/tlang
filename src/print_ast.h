#pragma once
#include "ast.h"

void print_ast(AstBinaryOperator *node);
void print_ast(AstDefinition *node);
void print_ast(AstLambda *node);
void print_ast(AstIdentifier *node);
void print_ast(AstLiteral *node);
void print_ast(AstReturn *node);
void print_ast(AstCall *node);
void print_ast(AstStruct *node);
void print_ast(AstIf *node);
void print_ast(AstIfx *node);
void print_ast(AstWhile *node);
void print_ast(AstExpressionStatement *node);
void print_ast(AstUnaryOperator *node);
void print_ast(AstSubscript *node);
void print_ast(AstTuple*node);
void print_ast(AstAssert*node);
void print_ast(AstParse* parse);
void print_ast(AstMatch* match);
void print_ast(AstNode *node);
void print_ast();

void print_lowered();

inline void print_ast(Span<AstStatement *> statements) {
    for (auto statement : statements) {
        print_ast(statement);
    }
}
