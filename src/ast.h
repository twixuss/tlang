#pragma once
#include <common.h>
#include <token.h>

#define ENUMERATE_AST_KIND(e) \
e(null) \
e(definition) \
e(return) \
e(lambda) \
e(identifier) \
e(literal) \
e(call) \
e(binary_operator) \
e(struct) \
e(if) \
e(expression_statement) \
e(unary_operator) \

enum AstKind {
#define e(name) Ast_ ## name,
	ENUMERATE_AST_KIND(e)
#undef e
};

umm append(StringBuilder &builder, AstKind kind);

extern s32 ast_node_uid_counter;

struct AstNode {
	AstKind kind = Ast_null;
	s32 uid = atomic_increment(&ast_node_uid_counter);
	Span<utf8> location;

	void *user_data = 0;
};

struct AstExpression;
struct AstDefinition;
struct AstLambda;
struct AstStruct;

struct AstStatement : AstNode {
};

struct AstExpressionStatement : AstStatement {
	AstExpressionStatement() { kind = Ast_expression_statement; }
	AstExpression *expression = 0;
};

struct AstDefinition : AstStatement {
	AstDefinition() { kind = Ast_definition; }

	Span<utf8> name = {};
	AstExpression *expression = 0;
	AstExpression *type = 0;

	AstLambda *parent_lambda = 0;

	bool is_constant  : 1 = false;
	bool is_parameter : 1 = false;
	bool built_in     : 1 = false;
};

struct AstReturn : AstStatement {
	AstReturn() { kind = Ast_return; }

	AstExpression *expression = 0;
};

struct AstExpression : AstNode {
	AstExpression *type = 0;
};

enum class LiteralKind : u8 {
	none,
	integer,
	boolean,
	string,
};

struct AstLiteral: AstExpression {
	AstLiteral() { kind = Ast_literal; memset(value_bytes, 0, sizeof(value_bytes)); }

	LiteralKind literal_kind = {};
	union {
		u8 value_bytes[16];
		u64 u64;
		u32 u32;
		u16 u16;
		u8 u8;
		s64 s64;
		s32 s32;
		s16 s16;
		s8 s8;
		bool Bool;
		Span<utf8> string;
	};
};

struct AstIdentifier : AstExpression {
	AstIdentifier() { kind = Ast_identifier; }

	AstDefinition *definition = 0;

	Span<utf8> name;
};

struct AstLambda : AstExpression {
	AstLambda() { kind = Ast_lambda; }

	List<AstReturn *> return_statements;
	AstExpression *return_type = 0;

	List<AstDefinition *> parameters;

	List<AstStatement *> statements;

	AstLambda *parent_lambda = 0;

	List<utf8> name;

	HashMap<Span<utf8>, AstDefinition *> local_definitions;

	bool has_body = true;
};

struct AstCall : AstExpression {
	AstCall() { kind = Ast_call; }

	AstDefinition *definition = 0;

	AstLambda *lambda = 0;

	List<AstExpression *> arguments;

	Span<utf8> name;
};

struct AstStruct : AstExpression {
	AstStruct() { kind = Ast_struct; }

	Span<utf8> name = {};
	List<AstDefinition *> members;
	List<AstDefinition *> constants;

	AstLambda *parent_lambda = 0;

	u32 size = 0;
};

struct AstIf : AstStatement {
	AstIf() { kind = Ast_if; }

	AstExpression *condition = 0;

	List<AstStatement *> true_statements;
	List<AstStatement *> false_statements;
};

#define ENUMERATE_BINARY_OPERATIONS(e) \
e(none) \
e(add) \
e(subtract) \
e(multiply) \
e(divide) \
e(member_access) \
e(_and) \
e(_or) \
e(_xor) \

enum class BinaryOperation {
#define e(name) name,
	ENUMERATE_BINARY_OPERATIONS(e)
#undef e
};

umm append(StringBuilder &builder, BinaryOperation op);

struct AstBinaryOperator : AstExpression {
	AstBinaryOperator() { kind = Ast_binary_operator; }

	BinaryOperation operation = {};

	AstExpression *left = 0;
	AstExpression *right = 0;
};

#define ENUMERATE_UNARY_OPERATIONS(e) \
e(none) \
e(plus) \
e(minus) \
e(star) \
e(_and) \
e(_not) \

enum class UnaryOperation {
#define e(name) name,
	ENUMERATE_UNARY_OPERATIONS(e)
#undef e
};

umm append(StringBuilder &builder, UnaryOperation op);

struct AstUnaryOperator : AstExpression {
	AstUnaryOperator() { kind = Ast_unary_operator; }

	UnaryOperation operation = {};

	AstExpression *expression = 0;
};

extern AstStruct type_type;
extern AstStruct type_void;
extern AstStruct type_bool;
extern AstStruct type_u8;
extern AstStruct type_u16;
extern AstStruct type_u32;
extern AstStruct type_u64;
extern AstStruct type_s8;
extern AstStruct type_s16;
extern AstStruct type_s32;
extern AstStruct type_s64;
extern AstStruct type_string;

extern AstStruct type_unsized_integer;
extern AstStruct *type_default_integer;

extern HashMap<Span<utf8>, AstStatement *> global_statements;
extern RecursiveMutex global_statements_mutex;

bool needs_semicolon(AstNode *node);
bool can_be_global(AstStatement *statement);

AstStruct &get_built_in_type_from_token(TokenKind t);
AstStruct *find_built_in_type_from_token(TokenKind t);

template <class T>
T *new_ast() {
	return default_allocator.allocate<T>();
}

Optional<s64> get_constant_integer(AstExpression *expression);

List<utf8> type_to_string(AstExpression *type);

u32 get_size(AstExpression *type);

bool types_match(AstExpression *type_a, AstExpression *type_b);

bool is_type(AstExpression *expression);

AstStruct *get_struct(AstExpression *type);
