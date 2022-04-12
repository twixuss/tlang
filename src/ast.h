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
e(test) \
e(ifx) \
e(assert) \
e(import) \
e(defer) \
e(print) \

enum AstKind : u8 {
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
struct AstDefer;

#pragma pack(push, 1)
template <class T>
struct Ref {
	T *pointer = 0;
	u32 index = -1;

	Ref() = default;
	Ref(T *pointer, u32 index) : pointer(pointer), index(index) {
	}
	Ref(T *pointer) : pointer(pointer) {
		auto index = index_of(T::storage, pointer);
		assert(index <= max_value<u32>);
		assert(index < count_of(T::storage));
		this->index = (u32)index;
	}

	Ref &operator=(T *pointer) {
		auto index = index_of(T::storage, pointer);
		assert(index <= max_value<u32>);
		assert(index < count_of(T::storage));
		this->pointer = pointer;
		this->index = (u32)index;
		return *this;
	}

	T *operator->() {
		return pointer;
	}
	operator T*() {
		return pointer;
	}

	template <class U>
	explicit operator U*() const {
		return (U *)pointer;
	}
};
#pragma pack(pop)

#define REFERENCE32(type, name) \
	u32 _##name##_index = (u32)~0; \
	type *##name##() { return &type::storage[_##name##_index]; } \
	void set_##name##(type *value) { auto idx = index_of(type::storage, value); assert((umm)(u32)idx == idx); _##name##_index = (u32)idx; } \
	bool has_##name##() { return _##name##_index != (u32)~0; }

template <class T>
struct DefaultAllocatable {
	inline static T *create(TL_LPC) {
		return default_allocator.allocate<T>(TL_LAC);
	}
};

template <class T>
struct StorageAllocatable {
	inline static BlockList<T> storage;
	inline static Ref<T> create(TL_LPC) {
		auto index = T::storage.count;
		assert((u32)index == index);
		auto pointer = &T::storage.add(TL_LAC);
		return {pointer, (u32)index};
	}
};

#if TL_DEBUG
template <class Tag, class T>
struct Pool32Allocatable {
	inline static T *create(TL_LPC) {
		return MyAllocator{}.allocate<T>();
	}
};

template <class T>
using ExpressionPool = DefaultAllocatable<T>;
template <class T = AstExpression>
using Expression = T *;

template <class T>
using StatementPool = DefaultAllocatable<T>;
template <class T>
using Statement = T *;

template <class T>
forceinline T *raw(T *pointer) { return pointer; }

#else
template <class Tag, class T>
struct Pool32Allocatable {
	inline static typename Pool32<Tag>::template Ptr<T> create(TL_LPC) {
		return Pool32<Tag>{}.allocate<T>();
	}
};

template <class T>
using ExpressionPool = Pool32Allocatable<AstExpression, T>;
template <class T = AstExpression>
using Expression = Pool32<AstExpression>::Ptr<T>;

template <class T>
using StatementPool = Pool32Allocatable<AstStatement, T>;
template <class T>
using Statement = Pool32<AstStatement>::Ptr<T>;

template <class T>
forceinline T *raw(Expression<T> expression) { return expression.raw(); }

template <class T>
forceinline T *raw(Statement<T> statement) { return statement.raw(); }
#endif

using DefinitionList = List<AstDefinition *, MyAllocator, u32>;

struct Scope : DefaultAllocatable<Scope> {
	AstNode *node = 0;
	Scope *parent = 0;
	u32 level = 0;
	List<Scope *> children;
	List<AstStatement *> statements;
	HashMap<String, DefinitionList> definitions; // multiple definitions for a single name in case of function overloading
	List<AstDefer *> bytecode_defers;

	void append(Scope &that) {
		children.add(that.children);
		statements.add(that.statements);
		for_each(that.definitions, [&](auto &key, auto &value) {
			definitions.get_or_insert(key).add(value);
		});
	}
};

template <class T>
T *NEW(TL_LPC) {
	return default_allocator.allocate<T>(TL_LAC);
}

extern s32 ast_node_uid_counter;

struct AstNode {
	AstKind kind = Ast_null;
	Span<utf8, u32> location;

#if TL_DEBUG
	s32 _uid = atomic_increment(&ast_node_uid_counter);
	s32 uid() { return _uid; }
#else
	s32 uid() { return 0; }
#endif
};
inline static constexpr auto sizeof_AstNode = sizeof AstNode;

struct AstStatement : AstNode {
};

struct AstBlock : AstStatement, StatementPool<AstBlock> {
	AstBlock() {
		kind = Ast_block;
		scope.node = this;
	}

	Scope scope;
};

struct AstExpressionStatement : AstStatement, StatementPool<AstExpressionStatement> {
	AstExpressionStatement() { kind = Ast_expression_statement; }
	Expression<> expression = {};
};

struct AstExpression : AstNode {
	Expression<> type = {};
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

using BigInteger = tl::impl::BigInt<List<u64, MyAllocator, u32>>;

struct AstLiteral : AstExpression, ExpressionPool<AstLiteral> {

	union {
		BigInteger integer;
		bool Bool;
		struct {
			HeapString string;
			s32 string_data_offset;
#pragma warning(suppress: 4201)
		};
		u32 character;
		f64 Float;
		Expression<> type_value;
	};

	LiteralKind literal_kind = {};

	AstLiteral() {
		memset(this, 0, sizeof(*this));
		kind = Ast_literal;
	}
	~AstLiteral() {

	}
};

inline static constexpr auto sizeof_AstLiteral = sizeof AstLiteral;

#define INVALID_MEMBER_OFFSET (-1)
#define INVALID_DATA_OFFSET (-1)

struct AstDefinition : AstStatement, StatementPool<AstDefinition> {
	AstDefinition() { kind = Ast_definition; }

	Expression<> expression = {};
	Expression<> type = {};
	Expression<> parent_block = {};

	Box<AstLiteral> evaluated;

	// REFERENCE32(Scope, parent_scope);

	Span<utf8, u32> name = {};
	s32 offset_in_struct = INVALID_MEMBER_OFFSET;
	s32 bytecode_offset = INVALID_DATA_OFFSET;

	bool is_constant         : 1 = false;
	bool is_parameter        : 1 = false;
	bool built_in            : 1 = false;
	bool is_return_parameter : 1 = false;
};

// Started with 176
inline static constexpr auto sizeof_AstDefinition = sizeof AstDefinition;

struct AstReturn : AstStatement, StatementPool<AstReturn> {
	AstReturn() { kind = Ast_return; }

	Expression<> expression = {};
	Expression<AstLambda> lambda = {};
};

struct AstIdentifier : AstExpression, ExpressionPool<AstIdentifier> {
	AstIdentifier() { kind = Ast_identifier; }

	Span<utf8, u32> name;

	Statement<AstDefinition> definition;

	DefinitionList possible_definitions;

};

inline static constexpr auto sizeof_AstIdentifier = sizeof AstIdentifier;
inline static constexpr auto sizeof_Span32 = sizeof Span<AstIdentifier, u32>;
inline static constexpr auto sizeof_List32 = sizeof List<AstIdentifier, MyAllocator, u32>;

struct Instruction;

enum class ExternLanguage {
	none,
	c,
};

enum class CallingConvention : u8 {
	none,
	tlang,
	stdcall,
};

// MYTYPEISME
struct AstLambda : AstExpression, ExpressionPool<AstLambda> {
	AstLambda() {
		kind = Ast_lambda;
		parameter_scope.node = this;
		body_scope.node = this;
	}

	Statement<AstDefinition> definition = {}; // not null if lambda is named

	List<AstReturn *> return_statements;
	Statement<AstDefinition> return_parameter = {};

	// body_scope is a child of parameter_scope to avoid parameter redefinition
	Scope parameter_scope;
	Scope body_scope;

	DefinitionList parameters; // This will be small most of the time, so no need in hashmap

	//DefinitionList return_parameters;

	// List<AstStatement *> statements;

	Expression<AstLambda> parent_lambda = {};

	// HashMap<String, AstDefinition *> local_definitions;

	bool has_body                   : 1 = true;
	bool is_type                    : 1 = false;
	bool finished_typechecking_head : 1 = false;
	bool is_intrinsic               : 1 = false;

	ExternLanguage extern_language = {};
	String extern_library;

	Instruction *first_instruction = 0;
	s64 location_in_bytecode = -1;
	s64 return_location = -1;

	CallingConvention convention = CallingConvention::none;


	// For bytecode generation

	s64 offset_accumulator = 0;
	s64 parameters_size = 0; // Sum of (parameters' size ceiled to context.stack_word_size)
	struct ReturnInfo {
		Instruction *jmp;
		s64 index;
	};
	List<ReturnInfo> return_jumps;
};

struct AstTuple : AstExpression, ExpressionPool<AstTuple> {
	AstTuple() { kind = Ast_tuple; }

	List<Expression<>> expressions;
};

struct AstCall : AstExpression, ExpressionPool<AstCall> {
	AstCall() { kind = Ast_call; }

	Expression<> callable = {};
	Expression<> argument = {};

};

enum class StructLayout {
	none,
	tlang,
	c,
};

struct AstStruct : AstExpression, ExpressionPool<AstStruct> {
	AstStruct() {
		kind = Ast_struct;
		scope.node = this;
	}

	Statement<AstDefinition> definition = {};

	// TODO: this is redundant
	DefinitionList members;
	DefinitionList constants;

	Scope scope;

	s64 size = 0;
	s64 alignment = 0;

	StructLayout layout = StructLayout::none;

	bool is_union : 1 = false;
};

struct AstIf : AstStatement, StatementPool<AstIf> {
	AstIf() {
		kind = Ast_if;
		true_scope.node = this;
		false_scope.node = this;
	}

	Expression<> condition = {};

	Scope true_scope;
	Scope false_scope;

	bool is_constant : 1 = false;
	bool true_branch_was_taken : 1 = false;
};

struct AstWhile : AstStatement, StatementPool<AstWhile> {
	AstWhile() {
		kind = Ast_while;
		scope.node = this;
	}

	Expression<> condition = {};

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
	as,      // as
    count,
};

// NOTE: Cast operator's right expression is it's type. So right expression pointer is wasted space...
struct AstBinaryOperator : AstExpression, ExpressionPool<AstBinaryOperator> {
	AstBinaryOperator() { kind = Ast_binary_operator; }

	BinaryOperation operation = {};

	Expression<> left = {};
	Expression<> right = {};
};

enum class UnaryOperation : u8 {
	plus,        // +
	minus,       // -
	bnot,        // !
	address_of,  // &
	dereference, // *
	pointer,     // *
	autocast,    // autocast
	Sizeof,      // #sizeof
	typeof,      // #typeof
	count,
	pointer_or_dereference,
};

inline String as_string(UnaryOperation unop) {
	switch (unop) {
		using enum UnaryOperation;
		case plus:        return "+"str;
		case minus:       return "-"str;
		case bnot:        return "!"str;
		case address_of:  return "&"str;
		case pointer:     return "*"str;
		case dereference: return "*"str;
		case autocast:    return "autocast"str;
		case Sizeof:      return "#sizeof"str;
		case typeof:      return "#typeof"str;
		case pointer_or_dereference: return "pointer_or_dereference"str;
	}
	invalid_code_path();
}

inline Optional<UnaryOperation> as_unary_operation(Token token) {
	switch (token.kind) {
		using enum UnaryOperation;
		case '+': return plus;
		case '-': return minus;
		case '!': return bnot;
		case '&': return address_of;
		case '*': return pointer_or_dereference;
		case Token_autocast:
			return autocast;
		case Token_directive:
			if (token.string == "#sizeof") return Sizeof;
			if (token.string == "#typeof") return typeof;
			break;
	}
	return {};
}

struct AstUnaryOperator : AstExpression, ExpressionPool<AstUnaryOperator> {
	AstUnaryOperator() { kind = Ast_unary_operator; }

	UnaryOperation operation = {};

	Expression<> expression = {};
};

struct AstSubscript : AstExpression, ExpressionPool<AstSubscript> {
	AstSubscript() { kind = Ast_subscript; }
	Expression<> expression = {};
	Expression<> index_expression = {};

	// u64 simd_size = 0;

	bool is_prefix : 1 = false;
	// bool is_simd   : 1 = false;
};

struct AstTest : AstStatement, StatementPool<AstTest> {
	AstTest() {
		kind = Ast_test;
		scope.node = this;
	}

	bool should_compile = false;

	Scope scope;
};

struct AstIfx : AstExpression, ExpressionPool<AstIfx> {
	AstIfx() {
		kind = Ast_ifx;
	}
	Expression<> condition = {};
	Expression<> true_expression = {};
	Expression<> false_expression = {};
};

struct AstAssert : AstStatement, StatementPool<AstAssert> {
	AstAssert() {
		kind = Ast_assert;
	}
	Expression<> condition = {};
};

struct AstPrint : AstStatement, StatementPool<AstPrint> {
	AstPrint() {
		kind = Ast_print;
	}
	Expression<> expression = {};
};

/*
// MYTYPEISME
struct AstImport : AstExpression, ExpressionPool<AstImport> {
	AstImport() {
		kind = Ast_import;
	}

	String path;
	Scope *scope = 0;
};
*/

struct AstDefer : AstStatement, StatementPool<AstDefer> {
	AstDefer() {
		kind = Ast_defer;
		scope.node = this;
	}

	Scope scope;
};

extern AstStruct *type_type; // These can be referenced by user programmer
extern AstStruct *type_void;
extern AstStruct *type_bool;
extern AstStruct *type_u8;
extern AstStruct *type_u16;
extern AstStruct *type_u32;
extern AstStruct *type_u64;
extern AstStruct *type_s8;
extern AstStruct *type_s16;
extern AstStruct *type_s32;
extern AstStruct *type_s64;
extern AstStruct *type_f32;
extern AstStruct *type_f64;
extern AstStruct *type_string;
extern AstStruct *type_noinit; // These are special, user can't use them directly. typeof'ing them should do what? i don't know. TODO
extern AstStruct *type_unsized_integer;
extern AstStruct *type_unsized_float;
// inline constexpr u32 built_in_struct_count = 14;

extern AstUnaryOperator *type_pointer_to_void;
extern AstStruct *type_default_signed_integer;
extern AstStruct *type_default_unsigned_integer;
extern AstStruct *type_default_integer;
extern AstStruct *type_default_float;

extern Scope global_scope;

extern Mutex global_scope_mutex;
void lock(Scope *scope);
void unlock(Scope *scope);

extern HashMap<String, AstDefinition *> names_not_available_for_globals;

bool needs_semicolon(AstExpression *node);
bool can_be_global(AstStatement *statement);

AstStruct &get_built_in_type_from_token(TokenKind t);
AstStruct *find_built_in_type_from_token(TokenKind t);


inline Optional<BigInteger> get_constant_integer(AstExpression *expression) {
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
				using enum UnaryOperation;
				case minus: return -value;
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
				case div: return get_constant_integer(binop->left) / get_constant_integer(binop->right);
				case mod: return get_constant_integer(binop->left) % get_constant_integer(binop->right);
				case bxor: return get_constant_integer(binop->left) ^ get_constant_integer(binop->right);
				case band: return get_constant_integer(binop->left) & get_constant_integer(binop->right);
				case bor: return get_constant_integer(binop->left) | get_constant_integer(binop->right);
				case bsl: return get_constant_integer(binop->left) << get_constant_integer(binop->right);
				case bsr: return {}; //return get_constant_integer(binop->left) >> get_constant_integer(binop->right);
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
HeapString type_to_string(AstExpression *type, bool silent_error = false);

// Returns just the name of the type without synonyms
HeapString type_name     (AstExpression *type, bool silent_error = false);

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
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case pointer: return context.stack_word_size;
				case typeof:  return get_size(unop->expression->type);
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
			return context.stack_word_size;
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

String operator_string(u64 op);
String operator_string(BinaryOperation op);

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
bool is_pointer_internally(AstExpression *type);

AstLiteral *get_literal(AstExpression *expression);
bool is_constant(AstExpression *expression);
AstLambda *get_lambda(AstExpression *expression);
bool is_lambda(AstExpression *expression);

bool struct_is_built_in(AstStruct *type);
bool type_is_built_in(AstExpression *type);

Comparison comparison_from_binary_operation(BinaryOperation operation);

List<Expression<> *> get_arguments_addresses(AstCall *call);
List<Expression<>> get_arguments(AstCall *call);

bool is_sized_array(AstExpression *type);
bool same_argument_and_return_types(AstLambda *a, AstLambda *b);
