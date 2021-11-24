#pragma once
#include <common.h>
#include <token.h>
#include <tl/big_int.h>

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
e(while) \
e(subscript) \

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

#define INVALID_MEMBER_OFFSET (-1)

struct AstDefinition : AstStatement {
	AstDefinition() { kind = Ast_definition; }

	Span<utf8> name = {};
	AstExpression *expression = 0;
	AstExpression *type = 0;

	AstExpression *parent_block = 0;

	s64 offset_in_struct = INVALID_MEMBER_OFFSET;

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
	AstLiteral() {
		kind = Ast_literal;
		memset(&integer, 0, max(sizeof(integer), sizeof(Bool), sizeof(string)));
	}
	~AstLiteral() {

	}

	LiteralKind literal_kind = {};
	union {
		BigInt integer;
		bool Bool;
		Span<utf8> string;
		// TODO: If you add new things here, make sure to also change the constructor
	};
};

struct AstIdentifier : AstExpression {
	AstIdentifier() { kind = Ast_identifier; }

	AstDefinition *definition = 0;

	Span<utf8> name;
};

struct Instruction;

struct AstLambda : AstExpression {
	AstLambda() { kind = Ast_lambda; }

	List<AstReturn *> return_statements;
	AstExpression *return_type = 0;

	List<AstDefinition *> parameters; // This will be small most of the time, so no need in hashmap

	List<AstStatement *> statements;

	AstLambda *parent_lambda = 0;

	List<utf8> name;

	HashMap<Span<utf8>, AstDefinition *> local_definitions;

	bool has_body = true;

	Span<utf8> extern_language;
	Span<utf8> extern_library;

	Instruction *first_instruction = 0;
	s64 location_in_bytecode = -1;
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

	AstDefinition *definition = 0;
	List<AstDefinition *> members;
	List<AstDefinition *> constants;

	u32 size = 0;
};

struct AstIf : AstStatement {
	AstIf() { kind = Ast_if; }

	AstExpression *condition = 0;

	List<AstStatement *> true_statements;
	List<AstStatement *> false_statements;
};

struct AstWhile : AstStatement {
	AstWhile() { kind = Ast_while; }

	AstExpression *condition = 0;

	List<AstStatement *> statements;
};

using BinaryOperation = u32;

struct AstBinaryOperator : AstExpression {
	AstBinaryOperator() { kind = Ast_binary_operator; }

	BinaryOperation operation = {};

	AstExpression *left = 0;
	AstExpression *right = 0;
};

using UnaryOperation = u32;

struct AstUnaryOperator : AstExpression {
	AstUnaryOperator() { kind = Ast_unary_operator; }

	UnaryOperation operation = {};

	AstExpression *expression = 0;
};

struct AstSubscript : AstExpression {
	AstSubscript() { kind = Ast_subscript; }
	AstExpression *expression = 0;
	AstExpression *index_expression = 0;
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
extern AstUnaryOperator type_pointer_to_void;

extern AstStruct type_unsized_integer;
extern AstStruct *type_default_integer;

extern HashMap<Span<utf8>, AstStatement *> global_statements;
extern RecursiveMutex global_statements_mutex;

bool needs_semicolon(AstExpression *node);
bool can_be_global(AstStatement *statement);

AstStruct &get_built_in_type_from_token(TokenKind t);
AstStruct *find_built_in_type_from_token(TokenKind t);

void *my_allocate(umm size, umm align);

template <class T>
T *new_ast() {
	return default_allocator.allocate<T>();
}

Optional<BigInt> get_constant_integer(AstExpression *expression);

List<utf8> type_to_string(AstExpression *type, bool silent_error = false);

s64 get_size(AstExpression *type);

bool types_match(AstExpression *type_a, AstExpression *type_b);

bool is_type(AstExpression *expression);

AstStruct *get_struct(AstExpression *type);

void init_ast_allocator();

extern LinearSet<Span<utf8>> extern_libraries;

extern AstLambda *main_lambda;

inline Span<utf8> binary_operator_string(BinaryOperation op) {
	switch (op) {
		case '+': return u8"+"s;
		case '-': return u8"-"s;
		case '*': return u8"*"s;
		case '/': return u8"/"s;
		case '%': return u8"%"s;
		case '|': return u8"|"s;
		case '&': return u8"&"s;
		case '^': return u8"^"s;
		case '.': return u8"."s;
		case '>': return u8">"s;
		case '<': return u8"<"s;
		case '>=': return u8">="s;
		case '<=': return u8"<="s;
		case '==': return u8"=="s;
		case '!=': return u8"!="s;
		case '=': return u8"="s;
	}
	invalid_code_path();
}
