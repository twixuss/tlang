#pragma once
#include <common.h>
#include <token.h>

#define ENUMERATE_AST_KIND(e) \
e(null) \
e(definition) \
e(return) \
e(lambda) \
e(identifier) \
e(integer) \
e(call) \
e(binary_operator) \

enum AstKind {
#define e(name) Ast_ ## name,
	ENUMERATE_AST_KIND(e)
#undef e
};

umm append(StringBuilder &builder, AstKind kind);

extern u32 ast_node_uid_counter;

struct AstNode {
	AstKind kind = Ast_null;
	u32 uid = atomic_increment(&ast_node_uid_counter);
	Span<utf8> location;

	void *user_data = 0;
};

struct AstExpression;
struct AstDefinition;
struct AstLambda;

struct AstStatement : AstNode {
};

struct AstDefinition : AstStatement {
	AstDefinition() { kind = Ast_definition; }

	Span<utf8> name = {};
	AstExpression *expression = 0;
	AstExpression *type_expression = 0;
	AstDefinition *type = 0;

	AstLambda *parent_lambda = 0;

	bool is_type : 1 = false;
};

struct AstReturn : AstStatement {
	AstReturn() { kind = Ast_return; }

	AstExpression *expression = 0;
};

struct AstExpression : AstNode {
	AstDefinition *type = 0;
};

struct AstInteger : AstExpression {
	AstInteger() { kind = Ast_integer; }

	u64 value = 0;
};

struct AstIdentifier : AstExpression {
	AstIdentifier() { kind = Ast_identifier; }

	AstDefinition *definition = 0;

	Span<utf8> name;
};

struct AstLambda : AstExpression {
	AstLambda() { kind = Ast_lambda; }

	List<AstDefinition *> return_types;
	AstDefinition *return_type = 0;
	AstExpression *return_type_expression = 0;

	List<AstStatement *> statements;

	AstLambda *parent_lambda = 0;

	List<utf8> name;

	HashMap<Span<utf8>, AstDefinition *> local_definitions;
};

struct AstCall : AstExpression {
	AstCall() { kind = Ast_call; }

	AstDefinition *definition = 0;

	AstLambda *lambda = 0;

	Span<utf8> name;
};

enum class BinaryOperation {
	none,
	add,
};

struct AstBinaryOperator : AstExpression {
	AstBinaryOperator() { kind = Ast_binary_operator; }

	BinaryOperation operation = {};

	AstExpression *left = 0;
	AstExpression *right = 0;
};

extern HashMap<u32, AstDefinition> built_in_definitions;
extern AstDefinition definition_unsized_integer;
extern AstDefinition definition_void;
extern AstDefinition *definition_default_integer;

extern HashMap<Span<utf8>, AstStatement *> global_statements;
extern RecursiveMutex global_statements_mutex;

bool needs_semicolon(AstNode *node);
bool can_be_global(AstStatement *statement);

AstDefinition &get_built_in_definition_from_token(TokenKind t);
AstDefinition *find_built_in_definition_from_token(TokenKind t);

template <class T>
T *new_ast() {
	return default_allocator.allocate<T>();
}
