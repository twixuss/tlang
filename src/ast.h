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
e(block) \
e(tuple) \
e(cast) \
e(sizeof) \

enum AstKind {
#define e(name) Ast_ ## name,
	ENUMERATE_AST_KIND(e)
#undef e
};

umm append(StringBuilder &builder, AstKind kind);

struct AstNode;
struct AstStatement;
struct AstExpression;
struct AstDefinition;
struct AstLambda;
struct AstStruct;

struct Scope {
	Scope *parent = 0;
	u32 level = 0;
	List<Scope *> children;
	List<AstStatement *> statements;
	HashMap<Span<utf8>, AstDefinition *> definitions;
	HashMap<Span<utf8>, AstLambda *> functions;
	AstNode *node = 0;
};

extern s32 ast_node_uid_counter;

struct AstNode {
	AstKind kind = Ast_null;
	s32 uid = atomic_increment(&ast_node_uid_counter);
	Span<utf8> location;

	void *user_data = 0;
};

struct AstStatement : AstNode {
};

struct AstBlock : AstStatement {
	AstBlock() {
		kind = Ast_block;
		scope.node = this;
	}

	Scope scope;
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
	Scope *parent_scope = 0;

	s64 offset_in_struct = INVALID_MEMBER_OFFSET;

	bool is_constant  : 1 = false;
	bool is_parameter : 1 = false;
	bool built_in     : 1 = false;
	bool is_return_parameter : 1 = false;
};

struct AstReturn : AstStatement {
	AstReturn() { kind = Ast_return; }

	AstExpression *expression = 0;
	AstLambda *lambda = 0;
};

struct AstExpression : AstNode {
	AstExpression *type = 0;
};

enum class LiteralKind : u8 {
	none,
	integer,
	boolean,
	string,
	character,
};

struct AstLiteral : AstExpression {

	LiteralKind literal_kind = {};

	union {
		BigInt integer;
		bool Bool;
		struct {
			Span<utf8> string;
			s64 string_data_offset;
		};
		utf8 character;
	};

	AstLiteral() {
		memset(this, 0, sizeof(*this));
		kind = Ast_literal;
	}
	~AstLiteral() {

	}
};

struct AstIdentifier : AstExpression {
	AstIdentifier() { kind = Ast_identifier; }

	AstDefinition *definition = 0;

	Span<utf8> name;
};

struct Instruction;

enum class ExternLanguage {
	none,
	c,
};

enum class CallingConvention {
	none,
	tlang,
	stdcall,
};

struct AstLambda : AstExpression {
	AstLambda() {
		kind = Ast_lambda;
		parameter_scope.node = this;
		body_scope.node = this;
	}

	AstDefinition *definition = 0; // not null if lambda is named

	List<AstReturn *> return_statements;
	AstDefinition *return_parameter = 0;

	// body_scope is a child of parameter_scope to avoid parameter redefinition
	Scope parameter_scope;
	Scope body_scope;

	List<AstDefinition *> parameters; // This will be small most of the time, so no need in hashmap

	//List<AstDefinition *> return_parameters;

	// List<AstStatement *> statements;

	AstLambda *parent_lambda = 0;

	List<utf8> name;

	// HashMap<Span<utf8>, AstDefinition *> local_definitions;

	bool has_body = true;

	bool finished_typechecking_head = false;

	ExternLanguage extern_language = {};
	Span<utf8> extern_library;

	Instruction *first_instruction = 0;
	s64 location_in_bytecode = -1;

	CallingConvention convention = CallingConvention::none;
};

struct AstCall : AstExpression {
	AstCall() { kind = Ast_call; }

	AstDefinition *definition = 0;

	AstLambda *lambda = 0;

	List<AstExpression *> arguments;

	Span<utf8> name;
};

enum class StructLayout {
	none,
	tlang,
	c,
};

struct AstStruct : AstExpression {
	AstStruct() {
		kind = Ast_struct;
		scope.node = this;
	}

	AstDefinition *definition = 0;

	// TODO: this is redundant
	List<AstDefinition *> members;
	List<AstDefinition *> constants;

	Scope scope;

	s64 size = 0;
	s64 alignment = 0;

	StructLayout layout = StructLayout::none;
};

struct AstIf : AstStatement {
	AstIf() {
		kind = Ast_if;
		true_scope.node = this;
		false_scope.node = this;
	}

	AstExpression *condition = 0;

	Scope true_scope;
	Scope false_scope;
};

struct AstWhile : AstStatement {
	AstWhile() {
		kind = Ast_while;
		scope.node = this;
	}

	AstExpression *condition = 0;

	Scope scope;
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

struct AstTuple : AstExpression {
	AstTuple() { kind = Ast_tuple; }

	List<AstExpression *> expressions;
};

enum class CastKind {
	none,

	u8_s8,
	u8_s16,
	u8_s32,
	u8_s64,
	u8_u16,
	u8_u32,
	u8_u64,

	u16_s8,
	u16_s16,
	u16_s32,
	u16_s64,
	u16_u8,
	u16_u32,
	u16_u64,

	u32_s8,
	u32_s16,
	u32_s32,
	u32_s64,
	u32_u8,
	u32_u16,
	u32_u64,

	u64_s8,
	u64_s16,
	u64_s32,
	u64_s64,
	u64_u8,
	u64_u16,
	u64_u32,

	s8_s16,
	s8_s32,
	s8_s64,
	s8_u8,
	s8_u16,
	s8_u32,
	s8_u64,

	s16_s8,
	s16_s32,
	s16_s64,
	s16_u8,
	s16_u16,
	s16_u32,
	s16_u64,

	s32_s8,
	s32_s16,
	s32_s64,
	s32_u8,
	s32_u16,
	s32_u32,
	s32_u64,

	s64_s8,
	s64_s16,
	s64_s32,
	s64_u8,
	s64_u16,
	s64_u32,
	s64_u64,

	pointer,
};

struct AstCast : AstExpression {
	AstCast() { kind = Ast_cast; }

	AstExpression * expression = 0;

	CastKind cast_kind = {};
};

struct AstSizeof : AstExpression {
	AstSizeof() { kind = Ast_sizeof; }

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
extern AstUnaryOperator type_pointer_to_void;

extern AstStruct type_unsized_integer;
extern AstStruct *type_default_integer;

extern Scope global_scope;

extern Mutex global_scope_mutex;
void lock(Scope *scope);
void unlock(Scope *scope);

extern HashMap<Span<utf8>, AstDefinition *> names_not_available_for_globals;

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
s64 get_align(AstExpression *type);

bool types_match(AstExpression *type_a, AstExpression *type_b);

bool is_type(AstExpression *expression);

AstStruct *get_struct(AstExpression *type);

void init_ast_allocator();

extern LinearSet<Span<utf8>> extern_libraries;

extern AstLambda *main_lambda;

Span<utf8> binary_operator_string(BinaryOperation op);
