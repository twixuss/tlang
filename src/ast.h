#pragma once
#include <common.h>
#include <token.h>
#include <tl/big_int.h>

// TODO: maybe this can be merged into tl
// This structure allows to store data either in-place or reference it somewhere else
// NOTE: Copying Box may or may not copy it's contents. So don't expect changes in Box's copy to reflect in original instance.
template <class T>
struct Box {
    T *pointer = 0;
    union {
        T value;
    };

    inline Box() : pointer(0) {}
    inline Box(std::nullptr_t) : pointer(0) {}
    inline Box(T *that) : pointer(that) {}
    inline Box(T const &that) : pointer(&value), value(that) {}
    inline Box(Box const &that) {
		if (that.owning()) {
			pointer = &value;
			value = that.value;
		} else {
			pointer = that.pointer;
		}
	}
    inline ~Box() {}

	inline Box &operator=(Box const &that) {
		return *new(this) Box(that);
	}

    inline explicit operator bool() { return pointer != 0; }

	inline bool owning() const { return pointer == &value; }

    inline T &operator*() { return *pointer; }
    inline T *operator->() { return pointer; }
};

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
e(test) \
e(ifx) \
e(assert) \
e(typeof) \
e(print) \
e(import) \

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
struct AstLiteral;

struct Scope {
	Scope *parent = 0;
	u32 level = 0;
	List<Scope *> children;
	List<AstStatement *> statements;
	HashMap<Span<utf8>, List<AstDefinition *>> definitions; // multiple definitions for a single name in case of function overloading
	AstNode *node = 0;

	void append(Scope &that) {
		children.add(that.children);
		statements.add(that.statements);
		for_each(that.definitions, [&](auto &key, auto &value) {
			definitions.get_or_insert(key).add(value);
		});
	}
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

struct AstExpression : AstNode {
	AstExpression *type = 0;
};

enum class LiteralKind : u8 {
	none,
	integer,
	boolean,
	string,
	character,
	noinit,
	Float,
	type,
};

struct AstLiteral : AstExpression {

	LiteralKind literal_kind = {};

	union {
		BigInt integer;
		bool Bool;
		struct {
			Span<utf8> string;
			s64 string_data_offset;
#pragma warning(suppress: 4201)
		};
		u32 character;
		f64 Float;
		AstExpression *type_value;
	};

	AstLiteral() {
		memset(this, 0, sizeof(*this));
		kind = Ast_literal;
	}
	~AstLiteral() {

	}
};

#define INVALID_MEMBER_OFFSET (-1)
#define INVALID_DATA_OFFSET (-1)

struct AstDefinition : AstStatement {
	AstDefinition() { kind = Ast_definition; }

	Span<utf8> name = {};
	AstExpression *expression = 0;
	AstExpression *type = 0;

	Box<AstLiteral> evaluated;

	AstExpression *parent_block = 0;
	Scope *parent_scope = 0;

	s64 offset_in_struct = INVALID_MEMBER_OFFSET;

	bool is_constant         : 1 = false;
	bool is_parameter        : 1 = false;
	bool built_in            : 1 = false;
	bool is_return_parameter : 1 = false;

	s64 bytecode_offset = INVALID_DATA_OFFSET;
};

struct AstReturn : AstStatement {
	AstReturn() { kind = Ast_return; }

	AstExpression *expression = 0;
	AstLambda *lambda = 0;
};

struct AstIdentifier : AstExpression {
	AstIdentifier() { kind = Ast_identifier; }

	AstDefinition *definition = 0;
	List<AstDefinition *> possible_definitions;

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

// MYTYPEISME
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

	// HashMap<Span<utf8>, AstDefinition *> local_definitions;

	bool has_body                   : 1 = true;
	bool is_type                    : 1 = false;
	bool finished_typechecking_head : 1 = false;
	bool is_intrinsic               : 1 = false;

	ExternLanguage extern_language = {};
	Span<utf8> extern_library;

	Instruction *first_instruction = 0;
	s64 location_in_bytecode = -1;
	s64 return_location = -1;

	CallingConvention convention = CallingConvention::none;


	// For bytecode generation

	s64 offset_accumulator = 0;
	s64 parameters_size = 0; // Sum of (parameters' size ceiled to 8 byte boundary)
	struct ReturnInfo {
		Instruction *jmp;
		s64 index;
	};
	List<ReturnInfo> return_jumps;
};

struct AstCall : AstExpression {
	AstCall() { kind = Ast_call; }

	AstExpression *expression = 0;
	List<AstExpression *> arguments;

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

enum class BinaryOperation {
    add,     // +
    sub,     // -
    mul,     // *
    div,     // /
    mod,     // %
    bxor,    // ^
    band,    // &
    bor,     // |
    bsl,     // <<
    bsr,     // >>
    eq,      // ==
    ne,      // !=
    gt,      // >
    lt,      // <
    ge,      // >=
    le,      // <=
    land,    // &&
    lor,     // ||
	dot,     // .
    ass,     // =
    addass,  // +=
    subass,  // -=
    mulass,  // *=
    divass,  // /=
    modass,  // %=
    bxorass, // ^=
    bandass, // &=
    borass,  // |=
    bslass,  // <<=
    bsrass,  // >>=
    count,
};

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

	// u64 simd_size = 0;

	bool is_prefix : 1 = false;
	// bool is_simd   : 1 = false;
};

struct AstTuple : AstExpression {
	AstTuple() { kind = Ast_tuple; }

	List<AstExpression *> expressions;
};

struct AstPrint : AstStatement {
	AstPrint() {kind = Ast_print;}
	AstExpression *expression = 0;
};

struct AstCast : AstExpression {
	AstCast() { kind = Ast_cast; }

	AstExpression * expression = 0;
	// TODO: user defined casts
	// AstLambda *lambda = 0; // non null for user defined casts
};

struct AstSizeof : AstExpression {
	AstSizeof() { kind = Ast_sizeof; }

	AstExpression *expression = 0;
};

struct AstTypeof : AstExpression {
	AstTypeof() { kind = Ast_typeof; }

	AstExpression *expression = 0;
};

struct AstTest : AstStatement {
	AstTest() {
		kind = Ast_test;
		scope.node = this;
	}

	bool should_compile = false;

	Scope scope;
};

struct AstIfx : AstExpression {
	AstIfx() {
		kind = Ast_ifx;
	}
	AstExpression *condition = 0;
	AstExpression *true_expression = 0;
	AstExpression *false_expression = 0;
};

struct AstAssert : AstStatement {
	AstAssert() {
		kind = Ast_assert;
	}
	AstExpression *condition = 0;
};

// MYTYPEISME
struct AstImport : AstExpression {
	AstImport() {
		kind = Ast_import;
	}

	Span<utf8> path;
	Scope *scope = 0;
};

extern AstStruct type_type; // These can be referenced by user programmer
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
extern AstStruct type_f32;
extern AstStruct type_f64;
extern AstStruct type_string;
extern AstStruct type_noinit; // These are special, user can't use them directly. typeof'ing them should do what? i don't know. TODO
extern AstStruct type_unsized_integer;
extern AstStruct type_unsized_float;
// inline constexpr u32 built_in_struct_count = 14;

extern AstUnaryOperator type_pointer_to_void;
extern AstStruct *type_default_integer;
extern AstStruct *type_default_float;

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
void *my_reallocate(void *data, umm old_size, umm new_size, umm align);
void my_deallocate(void *data, umm size);

template <class T>
T *new_ast(TL_LPC) {
	return default_allocator.allocate<T>(TL_LAC);
}

inline Optional<BigInt> get_constant_integer(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::integer) {
				return literal->integer;
			}
			break;
		}
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			if (ident->definition && ident->definition->is_constant) {
				return get_constant_integer(ident->definition->expression);
			}
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;
			auto got_value = get_constant_integer(unop->expression);
			if (!got_value)
				return got_value;

			auto value = got_value.value_unchecked();

			switch (unop->operation) {
				case '-': return -value;
			}
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			using enum BinaryOperation;
			switch (binop->operation) {
				case add: return get_constant_integer(binop->left) + get_constant_integer(binop->right);
				case sub: return get_constant_integer(binop->left) - get_constant_integer(binop->right);
				case mul: return get_constant_integer(binop->left) * get_constant_integer(binop->right);
				case div: print("constexpr / not implemented\n"); return {};// return get_constant_integer(binop->left) / get_constant_integer(binop->right);
				case mod: print("constexpr % not implemented\n"); return {};// return get_constant_integer(binop->left) % get_constant_integer(binop->right);
				case bxor: return get_constant_integer(binop->left) ^ get_constant_integer(binop->right);
				case band: return get_constant_integer(binop->left) & get_constant_integer(binop->right);
				case bor: return get_constant_integer(binop->left) | get_constant_integer(binop->right);
				case bsl: print("constexpr << not implemented\n"); return {};// return get_constant_integer(binop->left) << get_constant_integer(binop->right);
				case bsr: print("constexpr >> not implemented\n"); return {};// return get_constant_integer(binop->left) >> get_constant_integer(binop->right);
				case eq:
				case ne:
				case dot:
					return {};
				default: invalid_code_path();
			}
			break;
		}
	}
	return {};
}

// Returns a human readable string.
// For example for type `int` it will return "int (aka s64)"
List<utf8> type_to_string(AstExpression *type, bool silent_error = false);

// Returns just the name of the type without synonyms
List<utf8> type_name     (AstExpression *type, bool silent_error = false);

inline s64 get_size(AstExpression *type) {
	assert(type);
	switch (type->kind) {
		case Ast_struct: {
			auto Struct = (AstStruct *)type;
			return Struct->size;
		}
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition->expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case '*': return 8;
				default: invalid_code_path();
			}
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;

			auto count = get_constant_integer(subscript->index_expression);
			assert(count.has_value());

			return get_size(subscript->expression) * (s64)count.value();
		}
		case Ast_lambda: {
			return 8;
		}
		case Ast_typeof: {
			auto typeof = (AstTypeof *)type;
			return get_size(typeof->expression->type);
		}
		case Ast_call: {
			auto call = (AstCall *)type;
			return get_size(call->type);
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

s64 get_align(AstExpression *type);

bool types_match(AstExpression *type_a, AstExpression *type_b);

bool is_type(AstExpression *expression);

AstStruct *get_struct(AstExpression *type);
AstExpression *direct(AstExpression *type);
AstExpression *get_definition_expression(AstExpression *expression);

void init_ast_allocator();

Span<utf8> operator_string(u64 op);
Span<utf8> operator_string(BinaryOperation op);

bool is_integer(AstExpression *type);
bool is_signed(AstExpression *type);
bool is_float(AstExpression *type);

bool is_integer(AstStruct *type);
bool is_signed(AstStruct *type);
bool is_float(AstStruct *type);

#define OVERLOAD_NEW 0
#if OVERLOAD_NEW
void *operator new(umm size);
void *operator new(umm size, std::align_val_t align);
void operator delete(void *);
void operator delete(void *, umm size);
#endif

bool is_pointer(AstExpression *type);

AstLiteral *get_literal(AstExpression *expression);
bool is_constant(AstExpression *expression);
AstLambda *get_lambda(AstExpression *expression);
bool is_lambda(AstExpression *expression);

bool struct_is_built_in(AstStruct *type);
bool type_is_built_in(AstExpression *type);

Comparison comparison_from_binary_operation(BinaryOperation operation);
