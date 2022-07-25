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

// #define e(name)
// ENUMERATE_AST_KIND
// #undef e
#define ENUMERATE_AST_KIND \
e(Definition) \
e(Return) \
e(Lambda) \
e(LambdaType) \
e(Identifier) \
e(Literal) \
e(Call) \
e(BinaryOperator) \
e(Struct) \
e(If) \
e(ExpressionStatement) \
e(UnaryOperator) \
e(While) \
e(Subscript) \
e(Span) \
e(Block) \
e(Tuple) \
e(Test) \
e(Ifx) \
e(Assert) \
e(Import) \
e(Defer) \
e(Print) \
e(OperatorDefinition) \
e(Parse) \
e(Pack) \
e(Enum) \
e(EmptyStatement) \
e(For) \
e(LoopControl) \
e(Match) \
e(Using) \

enum AstKind : u8 {
	Ast_Unknown = 0,
#define e(name) Ast_ ## name,
	ENUMERATE_AST_KIND
#undef e
};

inline String as_string(AstKind kind) {
	switch (kind) {
#define e(name) case Ast_ ## name: return #name##str;
	ENUMERATE_AST_KIND
#undef e
	}
	invalid_code_path();
}

inline umm append(StringBuilder &builder, AstKind kind) {
	return append(builder, as_string(kind));
}

#define e(name) struct Ast ## name;
	ENUMERATE_AST_KIND
#undef e

template <class T>
inline static constexpr AstKind kind_of = Ast_Unknown;
#define e(name) template <> inline static constexpr AstKind kind_of<Ast##name> = Ast_##name;
	ENUMERATE_AST_KIND
#undef e

struct AstNode;
struct AstStatement;
struct AstExpression;
#define e(name) struct Ast##name;
	ENUMERATE_AST_KIND
#undef e

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

template <class A, class B>
std::tuple<A *, B *> create_expressions() {
	return MyAllocator{}.allocate_merge<A, B>();
}

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

using DefinitionList = SmallList<AstDefinition *>;

struct Scope : DefaultAllocatable<Scope> {
	AstNode *node = 0;
	Scope *parent = 0;
	u32 level = 0;
	SmallList<Scope *> children;
	SmallList<AstStatement *> statements;
	SmallList<AstUsing *> usings;
	Map<KeyString, DefinitionList> definitions; // multiple definitions for a single name in case of function overloading
	SmallList<AstDefer *> bytecode_defers;
	u32 defers_start_index = 0;

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
	AstKind kind = Ast_Unknown;
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
	Scope *parent_scope = 0;
};

struct AstEmptyStatement : AstStatement, StatementPool<AstEmptyStatement> {
	AstEmptyStatement() { kind = Ast_EmptyStatement; }
};

struct AstBlock : AstStatement, StatementPool<AstBlock> {
	AstBlock() {
		kind = Ast_Block;
		scope = Scope::create();
		scope->node = this;
	}

	Scope *scope;
};

struct AstExpressionStatement : AstStatement, StatementPool<AstExpressionStatement> {
	AstExpressionStatement() { kind = Ast_ExpressionStatement; }
	Expression<> expression = {};
};

struct AstExpression : AstNode {
	Expression<> type = {};
	Expression<> directed = {};
	// Expression<AstLiteral> evaluated = {};
	bool is_parenthesized : 1 = false;
	AstExpression() {
		directed = this;
	}
};

#define ENUMERATE_LITERAL_KINDS \
	e(none) \
	e(null) \
	e(integer) \
	e(boolean) \
	e(string) \
	e(character) \
	e(noinit) \
	e(Float) \
	e(type) \
	e(lambda_name) \
	e(Struct) \
	e(pack) \
	e(pointer) \

enum class LiteralKind : u8 {
#define e(name) name,
	ENUMERATE_LITERAL_KINDS
#undef e
};

inline String as_string(LiteralKind kind) {
	switch (kind) {
#define e(name) case LiteralKind::name: return #name##str;
	ENUMERATE_LITERAL_KINDS
#undef e
	}
	invalid_code_path();
}

inline umm append(StringBuilder &builder, LiteralKind kind) {
	return append(builder, as_string(kind));
}

using BigInteger = tl::impl::BigInt<List<u64, MyAllocator, u32>>;

enum SectionKind : u8 {
	constant,
};

struct AstLiteral : AstExpression, ExpressionPool<AstLiteral> {
	union {
		BigInteger integer;
		bool Bool;
		struct {
			s64 offset;
			s64 count;

			String get() const {
				return (String)Span(compiler.constant_section.buffer.subspan(offset, count).value());
			}
			void set(String string) {
				count = string.count;
				if (auto found = compiler.string_set.find(string)) {
					offset = found->value;
				} else {
					compiler.constant_section.buffer.ensure_capacity(string.count + 1);

					auto data = compiler.constant_section.buffer.last->end();

					offset = compiler.constant_section.buffer.count;
					for (auto ch : string)
						compiler.constant_section.w1(ch);

					compiler.constant_section.w1(0);

					compiler.string_set.get_or_insert({(utf8 *)data, (u32)count}) = offset;
				}
			}

		} string;
		u32 character;
		f64 Float;
		Expression<> type_value;
		struct {
			SmallList<AstLiteral *> struct_values;
			s64 struct_offset;
		};
		struct {
			SmallList<AstLiteral *> pack_values;
			s64 pack_offset;
		};
		struct {
			SectionKind section;
			s64 offset;
		} pointer;
	};

	LiteralKind literal_kind = {};

	AstLiteral() {
		memset(this, 0, sizeof(*this));
		kind = Ast_Literal;
	}
	~AstLiteral() {}
};

inline static constexpr auto sizeof_AstLiteral = sizeof AstLiteral;

#define INVALID_MEMBER_OFFSET (-1)
#define INVALID_DATA_OFFSET (-1)

enum class LambdaDefinitionLocation : u8 {
	body,
	parameter,
	return_parameter,
};

struct AstDefinition : AstStatement, StatementPool<AstDefinition> {
	AstDefinition() { kind = Ast_Definition; }

	Expression<> expression = {};
	Expression<> type = {};
	AstNode *container_node = {};
	Expression<AstIdentifier> poly_ident = {};

	AstLiteral *evaluated = {};

	// REFERENCE32(Scope, parent_scope);

	KeyString name = {};
	// s32 bytecode_offset = INVALID_DATA_OFFSET;

	LambdaDefinitionLocation definition_location = {};
	s32 offset = -1;

	bool is_constant         : 1 = false;
	bool typechecked         : 1 = false;
	bool is_poly             : 1 = false;
	bool depends_on_poly     : 1 = false;
	bool is_pack             : 1 = false;
};

// Started with 176
inline static constexpr auto sizeof_AstDefinition = sizeof AstDefinition;

struct AstReturn : AstStatement, StatementPool<AstReturn> {
	AstReturn() { kind = Ast_Return; }

	Expression<> expression = {};
	Expression<AstLambda> lambda = {};
};

struct AstIdentifier : AstExpression, ExpressionPool<AstIdentifier> {
	AstIdentifier() { kind = Ast_Identifier; }

	KeyString name;

	AstDefinition *definition() { return possible_definitions.count == 1 ? possible_definitions.data[0] : 0; }

	DefinitionList possible_definitions;

	bool is_poly : 1 = false;

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

struct AstLambdaType : AstExpression, ExpressionPool<AstLambdaType> {
	AstLambdaType() { kind = Ast_LambdaType; }
	Expression<AstLambda> lambda;
};

struct AstLambda : AstExpression, ExpressionPool<AstLambda> {
	AstLambda() {
		kind = Ast_Lambda;

		type_scope      = Scope::create();
		parameter_scope = Scope::create();
		body_scope      = Scope::create();

		type_scope->node      = this;
		parameter_scope->node = this;
		body_scope->node      = this;

		body_scope->parent = parameter_scope;
		parameter_scope->parent = type_scope;
	}

	Statement<AstDefinition> definition = {}; // not null if lambda is named

	SmallList<AstReturn *> return_statements;
	Statement<AstDefinition> return_parameter = {};

	Statement<AstReturn> return_statement_type_deduced_from = {};

	Scope *type_scope;
	Scope *parameter_scope;
	Scope *body_scope;
	Scope *outer_scope() { return type_scope; }

	DefinitionList parameters;

	//DefinitionList return_parameters;

	// SmallList<AstStatement *> statements;

	Expression<AstLambda> parent_lambda = {};

	// Map<String, AstDefinition *> local_definitions;

	struct HardenedPoly {
		AstLambda *lambda;
		AstDefinition *definition;
		AstCall *call;
	};

	SmallList<HardenedPoly> hardened_polys;
	Expression<AstLambda> original_poly = {};
	Expression<> insert_into = {};

	bool has_body                   : 1 = true;
	bool is_type                    : 1 = false;
	bool finished_typechecking_head : 1 = false;
	bool is_intrinsic               : 1 = false;
	bool is_poly                    : 1 = false;
	bool is_member                  : 1 = false;
	bool has_pack                   : 1 = false;
	bool print_bytecode             : 1 = false;
	bool is_evaluated_at_compile_time : 1 = false;

	ExternLanguage extern_language = {};
	String extern_library;

	s64 location_in_bytecode = -1;

	CallingConvention convention = CallingConvention::none;

	SmallList<AstLiteral *> function_directives;
	String type_name;

	s64 locals_size = 0;

	// For bytecode generation

	RegisterSet used_registers;

	s64 temporary_size = 0;
	s64 max_stack_space_used_for_call = 0;

	// s64 offset_accumulator = 0;
	s64 parameters_size = -1; // Sum of (parameters' size ceiled to compiler.stack_word_size)
	struct ReturnInfo {
		Instruction *jmp;
		s64 index;
	};
	SmallList<ReturnInfo> return_jumps;
};

struct AstTuple : AstExpression, ExpressionPool<AstTuple> {
	AstTuple() { kind = Ast_Tuple; }

	SmallList<Expression<>> expressions;
};

struct NamedArgument {
	KeyString name;
	AstExpression *expression;
};

struct AstCall : AstExpression, ExpressionPool<AstCall> {
	AstCall() { kind = Ast_Call; }

	Expression<> callable = {};
	Expression<> resolved = {};
	SmallList<NamedArgument> unsorted_arguments = {};
	SmallList<AstExpression *> sorted_arguments = {};

	Expression<AstLambdaType> lambda_type = {};
};

enum class StructLayout {
	none,
	tlang,
	c,
};

struct AstStruct : AstExpression, ExpressionPool<AstStruct> {
	AstStruct() {
		kind = Ast_Struct;

		parameter_scope = Scope::create();
		member_scope = Scope::create();

		parameter_scope->node = this;
		member_scope->node = this;

		member_scope->parent = parameter_scope;
	}

	Statement<AstDefinition> definition = {};
	DefinitionList data_members; // TODO: redundant with scope

	Scope *parameter_scope;
	Scope *member_scope;

	DefinitionList parameters; // TODO: redundant with scope

	Expression<AstStruct> instantiated_from = {};

	s64 size = -1;
	s64 alignment = -1;

	StructLayout layout = StructLayout::none;

	bool is_union    : 1 = false;
	bool is_template : 1 = false;
};

struct AstIf : AstStatement, StatementPool<AstIf> {
	AstIf() {
		kind = Ast_If;

		true_scope = Scope::create();
		false_scope = Scope::create();

		true_scope->node = this;
		false_scope->node = this;
	}

	Expression<> condition = {};

	Scope *true_scope;
	Scope *false_scope;

	bool is_constant : 1 = false;
	bool true_branch_was_taken : 1 = false;
};

struct AstWhile : AstStatement, StatementPool<AstWhile> {
	AstWhile() {
		kind = Ast_While;
		scope = Scope::create();
		scope->node = this;
	}

	Expression<> condition = {};

	Scope *scope;
};

struct AstFor : AstStatement, StatementPool<AstFor> {
	AstFor() {
		kind = Ast_For;
		scope = Scope::create();
		scope->node = this;
	}

	KeyString iterator_name = {};
	Expression<> range = {};

	Scope *scope;
};

//#define e(name, token)
#define ENUMERATE_BINARY_OPERATIONS \
e(add,     +) \
e(sub,     -) \
e(mul,     *) \
e(div,     /) \
e(mod,     %) \
e(bxor,    ^) \
e(band,    &) \
e(bor,     |) \
e(bsl,     <<) \
e(bsr,     >>) \
e(eq,      ==) \
e(ne,      !=) \
e(gt,      >) \
e(lt,      <) \
e(ge,      >=) \
e(le,      <=) \
e(land,    &&) \
e(lor,     ||) \
e(dot,     .) \
e(ass,     =) \
e(addass,  +=) \
e(subass,  -=) \
e(mulass,  *=) \
e(divass,  /=) \
e(modass,  %=) \
e(bxorass, ^=) \
e(bandass, &=) \
e(borass,  |=) \
e(bslass,  <<=) \
e(bsrass,  >>=) \
e(as,      as) \
e(range,   ..) \

enum class BinaryOperation {
#define e(name, token) name,
	ENUMERATE_BINARY_OPERATIONS
#undef e
    count,
};

Optional<BinaryOperation> as_binary_operation(TokenKind kind);
String as_string(BinaryOperation op);

inline umm append(StringBuilder &builder, BinaryOperation op) {
	return append(builder, as_string(op));
}

inline s32 get_precedence(BinaryOperation op) {
	using enum BinaryOperation;

	switch (op) {
		case dot:
			return 100;

		case as:
			return 90;

		case mul:
		case div:
		case mod:
			return 20;

		case add:
		case sub:
			return 10;

		case band:
		case bor:
		case bxor:
		case bsl:
		case bsr:
			return 5;

		case gt:
		case lt:
		case eq:
		case ne:
		case ge:
		case le:
			return 4;

		case lor:
		case land:
			return 3;

		case range:
			return 2;

		case ass:
		case addass:
		case subass:
		case mulass:
		case divass:
		case modass:
		case bxorass:
		case bandass:
		case borass:
		case bslass:
		case bsrass:
			return 1;
	}

	invalid_code_path();
	return 0;
}

// NOTE: Cast operator's right expression is it's type. So right expression pointer is wasted space...
struct AstBinaryOperator : AstExpression, ExpressionPool<AstBinaryOperator> {
	AstBinaryOperator() { kind = Ast_BinaryOperator; }

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
	unwrap,      // *
	autocast,    // @
	Sizeof,      // #sizeof
	typeof,      // #typeof
	option,      // ?
	poly,        // $
	typeinfo,    // #typeinfo
	dot,         // .
	pack,        // ..
	count,
	pointer_or_dereference_or_unwrap,
	internal_move_to_temporary, // move the value into temporary space, result is a pointer to that space.
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
		case unwrap:      return "*"str;
		case autocast:    return "@"str;
		case Sizeof:      return "#sizeof"str;
		case typeof:      return "#typeof"str;
		case option:      return "?"str;
		case poly:        return "$"str;
		case typeinfo:    return "#typeinfo"str;
		case dot:         return "."str;
		case pack:        return ".."str;
		case pointer_or_dereference_or_unwrap: return "<*>"str;
		case internal_move_to_temporary: return "<tmp>"str;
	}
	invalid_code_path();
}
inline umm append(StringBuilder &builder, UnaryOperation unop) {
	return append(builder, as_string(unop));
}

inline Optional<UnaryOperation> as_unary_operation(Token token) {
	switch (token.kind) {
		using enum UnaryOperation;
		case '+': return plus;
		case '-': return minus;
		case '!': return bnot;
		case '&': return address_of;
		case '*': return pointer_or_dereference_or_unwrap;
		case '?': return option;
		case '@': return autocast;
		case '$': return poly;
		case '.': return dot;
		case '..': return pack;
		case Token_directive:
			if (token.string == "#sizeof")   return Sizeof;
			if (token.string == "#typeof")   return typeof;
			if (token.string == "#typeinfo") return typeinfo;
			break;
	}
	return {};
}

struct AstUnaryOperator : AstExpression, ExpressionPool<AstUnaryOperator> {
	AstUnaryOperator() { kind = Ast_UnaryOperator; }

	UnaryOperation operation = {};

	Expression<> expression = {};
};

struct AstSubscript : AstExpression, ExpressionPool<AstSubscript> {
	AstSubscript() { kind = Ast_Subscript; }
	Expression<> expression = {};
	Expression<> index_expression = {};

	bool is_prefix : 1 = false;
};

struct AstSpan : AstExpression, ExpressionPool<AstSpan> {
	AstSpan() { kind = Ast_Span; }
	Expression<> expression = {};
};

struct AstTest : AstExpression, ExpressionPool<AstTest> {
	AstTest() {
		kind = Ast_Test;
		scope = Scope::create();
		scope->node = this;
	}

	Scope *scope;
};

struct AstIfx : AstExpression, ExpressionPool<AstIfx> {
	AstIfx() {
		kind = Ast_Ifx;
	}
	Expression<> condition = {};
	Expression<> true_expression = {};
	Expression<> false_expression = {};
};

struct AstAssert : AstStatement, StatementPool<AstAssert> {
	AstAssert() {
		kind = Ast_Assert;
	}
	Expression<> condition = {};
	bool is_constant : 1 = false;
};

struct AstPrint : AstStatement, StatementPool<AstPrint> {
	AstPrint() {
		kind = Ast_Print;
	}
	Expression<> expression = {};
};

struct AstParse : AstStatement, StatementPool<AstParse> {
	AstParse() {
		kind = Ast_Parse;
	}
	Expression<> expression = {};
};

/*
// MYTYPEISME
struct AstImport : AstExpression, ExpressionPool<AstImport> {
	AstImport() {
		kind = Ast_Import;
	}

	String path;
	Scope *scope = 0;
};
*/

struct AstDefer : AstStatement, StatementPool<AstDefer> {
	AstDefer() {
		kind = Ast_Defer;
		scope = Scope::create();
		scope->node = this;
	}

	Scope *scope;
};

struct AstOperatorDefinition : AstStatement, StatementPool<AstOperatorDefinition> {
	AstOperatorDefinition() { kind = Ast_OperatorDefinition; }

	Expression<AstLambda> lambda = {};

	TokenKind operation = {};
	bool is_implicit : 1 = false;
};

struct AstPack : AstExpression, ExpressionPool<AstPack> {
	AstPack() { kind = Ast_Pack; }
	SmallList<AstExpression *> expressions = {};
};

struct AstEnum : AstExpression, ExpressionPool<AstEnum> {
	AstEnum() {
		kind = Ast_Enum;
		scope = Scope::create();
		scope->node = this;
	}

	Statement<AstDefinition> definition = {};

	Scope *scope;

	Expression<> underlying_type = {};
};

enum class LoopControl {
	Break,
	Continue,
};

struct AstLoopControl : AstStatement, StatementPool<AstLoopControl> {
	AstLoopControl() {
		kind = Ast_LoopControl;
	}
	LoopControl control;
	AstWhile *loop;
};

struct MatchCase {
	AstExpression *expression = 0; // will be null for default case
	Scope *scope = Scope::create();
};

struct AstMatch : AstStatement, StatementPool<AstMatch> {
	AstMatch() {
		kind = Ast_Match;
	}

	Expression<> expression = {};
	BlockList<MatchCase> cases; // elements need to be persistent in memory cause they contain Scope.
	MatchCase *default_case = 0;
	String default_case_location = {};
};

struct AstUsing : AstStatement, StatementPool<AstUsing> {
	AstUsing() {
		kind = Ast_Using;
	}

	Expression<> expression = {};
	Statement<AstDefinition> definition = {};
};

struct BuiltinStruct {
	// actual thing
	AstStruct        *Struct;  // string :: struct

	// reusable things
	AstIdentifier    *ident;   // string
	AstUnaryOperator *pointer; // *string
	AstSpan          *span;	   // []string
};

struct BuiltinEnum {
	// actual thing
	AstEnum          *Enum;    // type_kind :: enum

	// reusable things
	AstIdentifier    *ident;   // type_kind
	AstUnaryOperator *pointer; // *type_kind
	AstSpan          *span;	   // []type_kind
};

extern BuiltinStruct builtin_void;
extern BuiltinStruct builtin_bool;
extern BuiltinStruct builtin_u8;
extern BuiltinStruct builtin_u16;
extern BuiltinStruct builtin_u32;
extern BuiltinStruct builtin_u64;
extern BuiltinStruct builtin_s8;
extern BuiltinStruct builtin_s16;
extern BuiltinStruct builtin_s32;
extern BuiltinStruct builtin_s64;
extern BuiltinStruct builtin_f32;
extern BuiltinStruct builtin_f64;

extern BuiltinStruct builtin_type;
extern BuiltinStruct builtin_string;
extern BuiltinStruct builtin_struct_member;
extern BuiltinStruct builtin_typeinfo;
extern BuiltinStruct builtin_any;
extern BuiltinStruct builtin_range;
extern BuiltinStruct builtin_enum_member;

extern BuiltinEnum builtin_type_kind;

extern BuiltinStruct builtin_unsized_integer;
extern BuiltinStruct builtin_unsized_float;
extern BuiltinStruct builtin_noinit;
extern BuiltinStruct builtin_unknown;
extern BuiltinStruct builtin_unknown_enum;
extern BuiltinStruct builtin_poly;
extern BuiltinStruct builtin_overload_set;

extern BuiltinStruct *builtin_default_signed_integer;
extern BuiltinStruct *builtin_default_unsigned_integer;
extern BuiltinStruct *builtin_default_integer;
extern BuiltinStruct *builtin_default_float;

extern AstIdentifier *type_int;
extern AstIdentifier *type_sint;
extern AstIdentifier *type_uint;
extern AstIdentifier *type_float;

extern Scope global_scope;

extern Mutex global_scope_mutex;
void lock(Scope *scope);
void unlock(Scope *scope);

extern Map<String, AstDefinition *> names_not_available_for_globals;

bool needs_semicolon(AstExpression *node);
bool can_be_global(AstStatement *statement);

AstStruct &get_built_in_type_from_token(TokenKind t);
AstStruct *find_built_in_type_from_token(TokenKind t);


inline Optional<BigInteger> get_constant_integer(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::integer) {
				return literal->integer;
			}
			break;
		}
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)expression;
			if (ident->definition() && ident->definition()->is_constant) {
				return get_constant_integer(ident->definition()->expression);
			}
			break;
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			auto got_value = get_constant_integer(unop->expression);
			if (!got_value)
				return got_value;

			auto value = got_value.value_unchecked();

			switch (unop->operation) {
				using enum UnaryOperation;
				case minus: return -value;
				default: invalid_code_path();
			}
			break;
		}
		case Ast_BinaryOperator: {
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
				case bsr: return get_constant_integer(binop->left) >> get_constant_integer(binop->right);
				case eq: // return get_constant_integer(binop->left) == get_constant_integer(binop->right);
				case ne: // return get_constant_integer(binop->left) != get_constant_integer(binop->right);
				case dot:
					return {};
				default: invalid_code_path();
			}
			break;
		}
	}
	return {};
}

void append_type(StringBuilder &builder, AstExpression *type, bool silent_error);

// Returns a human readable string.
// For example for type `int` it will return "int (aka s64)"
HeapString type_to_string(AstExpression *type, bool silent_error = false);

// Returns just the name of the type without synonyms
HeapString type_name     (AstExpression *type, bool silent_error = false);

s64 get_size(AstExpression *type, bool check_struct = true);
s64 get_align(AstExpression *type, bool check_struct = true);

bool types_match(AstExpression *type_a, AstExpression *type_b);
inline bool types_match(AstStruct *type_a, AstStruct *type_b) { return type_a == type_b; }

inline bool types_match(AstExpression *type_a, BuiltinStruct &type_b) { return types_match(type_a, type_b.Struct); }
inline bool types_match(BuiltinStruct &type_a, AstExpression *type_b) { return types_match(type_a.Struct, type_b); }

inline bool types_match(AstStruct *type_a, BuiltinStruct &type_b) { return types_match(type_a, type_b.Struct); }
inline bool types_match(BuiltinStruct &type_a, AstStruct *type_b) { return types_match(type_a.Struct, type_b); }

bool is_type(AstExpression *expression);

AstExpression *get_definition_expression(AstExpression *expression);

/*
Returns the first expression assigned to an alias.
For example:
myint :: int;
myint2 :: myint;

direct(identifier myint2) returns struct s64
*/
AstExpression *direct(AstExpression *type);

template <class T>
T *as(AstExpression *expression) {
	if (expression->kind == kind_of<T>) {
		return (T *)expression;
	}
	return 0;
}

template <class T>
T *direct_as(AstExpression *expression) {
	auto directed = direct(expression);
	if (!directed)
		return 0;
	if (directed->kind == kind_of<T>) {
		return (T *)directed;
	}
	return 0;
}

inline bool is_struct     (AstExpression *type) { return direct_as<AstStruct    >(type) != 0; }
inline bool is_lambda_type(AstExpression *type) { return direct_as<AstLambdaType>(type) != 0; }
inline bool is_lambda     (AstExpression *expression) { return direct_as<AstLambda>(expression) != 0; }

bool is_integer_or_pointer(AstExpression *type);
bool is_integer_internally(AstExpression *type);

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

AstUnaryOperator *as_pointer(AstExpression *type);

bool is_constant(AstExpression *expression);
AstLambda *get_lambda(AstExpression *expression);
AstLambdaType *get_lambda_type(AstExpression *expression);

bool struct_is_built_in(AstStruct *type);
bool type_is_built_in(AstExpression *type);

Comparison comparison_from_binary_operation(BinaryOperation operation);

/*
List<Expression<> *> get_arguments_addresses(AstCall *call);
List<Expression<>> get_arguments(AstCall *call);
*/

bool is_sized_array(AstExpression *type);
bool same_argument_and_return_types(AstLambda *a, AstLambda *b);

inline AstUnaryOperator *as_option(AstExpression *expression) {
	auto d = direct_as<AstUnaryOperator>(expression);
	if (d && d->operation == UnaryOperation::option)
		return d;
	return 0;
}
AstLiteral *get_literal(AstExpression *expression);

AstSubscript *as_array(AstExpression *type);
AstSpan *as_span(AstExpression *type);

bool is_addressable(AstExpression *expression);
