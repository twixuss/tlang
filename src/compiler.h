#pragma once
// TODO: Store nodes inline if they have to be always present.


#include <common.h>
#include <token.h>
#include <tl/big_int.h>


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
e(Assert) \
e(Defer) \
e(Print) \
e(OperatorDefinition) \
e(Parse) \
e(Enum) \
e(EmptyStatement) \
e(For) \
e(LoopControl) \
e(Match) \
e(Using) \
e(ArrayInitializer) \
e(Yield) \

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

template <class T>
struct DefaultAllocatable {
	inline static T *create(TL_LPC) {
		return default_allocator.allocate<T>(TL_LAC);
	}
};

template <class Tag, class T>
struct Pool32Allocatable {
	inline static typename Pool32<Tag>::template Ptr<T> create(TL_LPC) {
		return Pool32<Tag>{}.allocate<T>();
	}
};

#define SMALL_AST 0//!TL_DEBUG
#if SMALL_AST

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
#else
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
#endif

using DefinitionList = LinearSet<AstDefinition *>;

struct UsingAndDefinition {
	AstUsing *Using;
	AstDefinition *definition;
};

struct Scope : DefaultAllocatable<Scope> {
	AstNode *node = 0;
	Scope *parent = 0;
	u32 level = 0;
	SmallList<Scope *> children;
	SmallList<UsingAndDefinition> usings;

	// Following members are redundant, but provide easy access.
	// Maybe there's a better way to organize this.
	SmallList<AstStatement *> statement_list;
	SmallList<AstDefinition *> definition_list;
	Map<KeyString, DefinitionList> definition_map; // multiple definitions for a single name in case of function overloading

	SmallList<AstDefer *> bytecode_defers;
	u32 defers_start_index = 0;

	void add(AstStatement *statement TL_LP);
	void add(AstDefinition *definition TL_LP);

	void append(Scope &that) {
		children.add(that.children);
		statement_list.add(that.statement_list);
		definition_list.add(that.definition_list);
		for_each(that.definition_map, [&](auto &name, auto &list) {
			for (auto definition : list) {
				definition_map.get_or_insert(name).add(definition);
			}
		});
	}
};

template <class T>
T *NEW(TL_LPC) {
	return default_allocator.allocate<T>(TL_LAC);
}

struct AstNode {
	AstKind kind = Ast_Unknown;
	String location = {};

	s32 uid = 0;

	AstNode();
};
inline static constexpr auto sizeof_AstNode = sizeof AstNode;

struct AstStatement : AstNode {
	Scope *parent_scope = 0;
};

struct AstEmptyStatement : AstStatement, StatementPool<AstEmptyStatement> {
	AstEmptyStatement() { kind = Ast_EmptyStatement; }
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
	bool typechecked : 1 = false;
};

struct AstBlock : AstExpression, ExpressionPool<AstBlock> {
	AstBlock() {
		kind = Ast_Block;
		scope = Scope::create();
		scope->node = this;
	}

	Scope *scope;
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
	e(array) \
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

// FIXME: use custom allocator
using BigInteger = tl::impl::BigInt<List<u64, Allocator, u32>>;

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

			String get() const;
			void set(String string);
		} string;
		u32 character;
		f64 Float;
		Expression<> type_value;
		struct {
			SmallList<AstLiteral *> struct_values;
			s64 struct_offset;
		};
		struct {
			SmallList<AstLiteral *> array_elements;
			s64 array_offset;
		};
		struct {
			SectionKind section;
			s64 offset;
		} pointer;
	};
	// It has to be done.
	[[no_unique_address]] struct {} union_end;

	LiteralKind literal_kind = {};

	AstLiteral() {
		// Zero out that big union. MAN THIS IS BAD
		memset(&integer, 0, offsetof(AstLiteral, union_end) - offsetof(AstLiteral, integer));

		kind = Ast_Literal;
		directed = this;
	}
	~AstLiteral() {}

	template <LiteralKind request>
	inline auto as() {
		/**/ if constexpr (request == LiteralKind::integer  ) { return literal_kind == LiteralKind::integer   ? integer    : Optional<decltype(integer   )>{}; }
		else if constexpr (request == LiteralKind::boolean  ) { return literal_kind == LiteralKind::boolean   ? Bool       : Optional<decltype(Bool      )>{}; }
		else if constexpr (request == LiteralKind::string   ) { return literal_kind == LiteralKind::string    ? string     : Optional<decltype(string    )>{}; }
		else if constexpr (request == LiteralKind::character) { return literal_kind == LiteralKind::character ? character  : Optional<decltype(character )>{}; }
		else if constexpr (request == LiteralKind::Float    ) { return literal_kind == LiteralKind::Float     ? Float      : Optional<decltype(Float     )>{}; }
		else if constexpr (request == LiteralKind::type     ) { return literal_kind == LiteralKind::type      ? type_value : Optional<decltype(type_value)>{}; }
		else if constexpr (request == LiteralKind::pointer  ) { return literal_kind == LiteralKind::pointer   ? pointer    : Optional<decltype(pointer   )>{}; }
		else {
			static_assert(false);
		}
	}
};

inline static constexpr auto sizeof_AstLiteral = sizeof AstLiteral;

#define INVALID_MEMBER_OFFSET (-1)
#define INVALID_DATA_OFFSET (-1)

struct AstDefinition : AstStatement, StatementPool<AstDefinition> {
	AstDefinition() { kind = Ast_Definition; }

	Expression<> expression = {};
	Expression<> type = {};
	Expression<> container_node = {};
	Expression<AstIdentifier> poly_ident = {};

	Expression<AstLiteral> evaluated = {};

	KeyString placed_at = {};

	KeyString name = {};

	s32 offset = -1;

	bool is_constant     : 1 = false;
	bool typechecked     : 1 = false;
	bool is_poly         : 1 = false;
	bool depends_on_poly : 1 = false;
	bool is_pack         : 1 = false;
	bool has_using       : 1 = false;
	bool is_referenced   : 1 = false;
};

// Started with 176
inline static constexpr auto sizeof_AstDefinition = sizeof AstDefinition;

struct AstReturn : AstStatement, StatementPool<AstReturn> {
	AstReturn() { kind = Ast_Return; }

	Expression<> expression = {};
	Expression<AstLambda> lambda = {};
};

struct AstYield : AstStatement, StatementPool<AstYield> {
	AstYield() { kind = Ast_Yield; }

	Expression<> expression = {};
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
	AstLambdaType() {
		kind = Ast_LambdaType;
		directed = this;
	}
	Expression<AstLambda> lambda;
};

struct AstLambda : AstExpression, ExpressionPool<AstLambda> {
	AstLambda() {
		kind = Ast_Lambda;
		directed = this;

		constant_scope  = Scope::create();
		parameter_scope = Scope::create();

		constant_scope->node  = this;
		parameter_scope->node = this;

		parameter_scope->parent = constant_scope;
	}

	Scope *outer_scope() { return constant_scope; }

	Statement<AstDefinition> definition = {}; // not null if lambda is named

	// FIXME: I think everybody needs only expresssion, not the whole statement
	SmallList<AstReturn *> return_statements;

	Statement<AstDefinition> return_parameter = {};

	Expression<> return_statement_type_deduced_from = {};

	Scope *constant_scope = {};
	Scope *parameter_scope = {};
	AstExpression *body = {};

	DefinitionList parameters;

	HashMap<AstDefinition *, AstIdentifier *> parameter_to_default_value;

	//DefinitionList return_parameters;

	// SmallList<AstStatement *> statements;

	// Map<String, AstDefinition *> local_definitions;

	struct HardenedPoly {
		Expression<AstLambda> lambda = {};
		Statement<AstDefinition> definition = {};
	};

	SmallList<HardenedPoly> cached_instantiations;
	Expression<AstLambda> original_poly = {};

	bool is_type                      : 1 = false;
	bool finished_typechecking_head   : 1 = false;
	bool is_intrinsic                 : 1 = false;
	bool is_poly                      : 1 = false;
	bool is_member                    : 1 = false;
	bool has_pack                     : 1 = false;
	bool print_bytecode               : 1 = false;
	bool is_evaluated_at_compile_time : 1 = false;
	bool was_instantiated             : 1 = false;
	bool is_macro                     : 1 = false;

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
	AstTuple() {
		kind = Ast_Tuple;
		directed = this;
	}

	SmallList<Expression<>> expressions;
};

struct NamedArgument {
	KeyString name;
	AstExpression *expression;
};

struct AstCall : AstExpression, ExpressionPool<AstCall> {
	AstCall() {
		kind = Ast_Call;
		directed = this;
	}

	Expression<> callable = {};
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
		directed = this;

		parameter_scope = Scope::create();
		member_scope = Scope::create();

		parameter_scope->node = this;
		member_scope->node = this;

		member_scope->parent = parameter_scope;
	}

	Statement<AstDefinition> definition = {};

	Scope *parameter_scope;
	Scope *member_scope;

	DefinitionList data_members;

	Expression<AstStruct> instantiated_from = {};
	struct Instantiation {
		AstStruct *Struct;
		AstIdentifier *ident;
		SmallList<AstExpression *> arguments;
	};

	// NOTE: caching code relies on this to be non-moving
	BlockList<Instantiation> instantiations;

	Expression<AstLiteral> default_value = {};
	u32 default_value_offset = 0;

	s64 size = -1;
	s64 alignment = -1;

	StructLayout layout = StructLayout::none;

	bool is_union    : 1 = false;
	bool is_template : 1 = false;
	// :span hack:
	bool is_span     : 1 = false;
};

struct AstIf : AstExpression, ExpressionPool<AstIf> {
	AstIf() {
		kind = Ast_If;
	}

	Expression<> condition = {};

	Expression<AstBlock> true_block = {};
	Expression<AstBlock> false_block = {};

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

	bool by_pointer : 1 = false;
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
e(adds,    +|) \
e(subs,    -|) \

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

inline static constexpr s32 custom_precedence = 8;
inline s32 get_precedence(BinaryOperation op) {
	using enum BinaryOperation;

	switch (op) {
		case dot:
			return 10;

		case as:
			return 9;

		case mul:
		case div:
		case mod:
			return 7;

		case add:
		case sub:
		case adds:
		case subs:
			return 6;

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
	AstBinaryOperator() {
		kind = Ast_BinaryOperator;
		directed = this;
	}

	BinaryOperation operation = {};

	Expression<> left = {};
	Expression<> right = {};
};

#define ENUMERATE_UNARY_OPERATIONS \
e(plus,        +)         /*   +           */\
e(minus,       -)         /*   -           */\
e(lnot,        !)         /*   !           */\
e(bnot,        ~)         /*   ~           */\
e(address_of,  &)         /*   &           */\
e(dereference, *)         /*   *           */\
e(pointer,     *)         /*   *           */\
e(unwrap,      *)         /*   *           */\
e(autocast,    @)         /*   @           */\
e(Sizeof,      #sizeof)   /*   #sizeof     */\
e(typeof,      #typeof)   /*   #typeof     */\
e(option,      ?)         /*   ?           */\
e(poly,        $)         /*   $           */\
e(typeinfo,    #typeinfo) /*   #typeinfo   */\
e(dot,         .)         /*   .           */\
e(pack,        ..)        /*   ..          */\
e(pointer_or_dereference_or_unwrap, *) \
e(internal_move_to_temporary, XXX) // move the value into temporary space, result is a pointer to that space.


enum class UnaryOperation : u8 {
#define e(x, op) x,
	ENUMERATE_UNARY_OPERATIONS
#undef e
	count,
};

inline String as_string(UnaryOperation unop) {
	switch (unop) {
		using enum UnaryOperation;
		case plus:        return "+"str;
		case minus:       return "-"str;
		case lnot:        return "!"str;
		case bnot:        return "~"str;
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
		case '!': return lnot;
		case '~': return bnot;
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
	AstUnaryOperator() {
		kind = Ast_UnaryOperator;
		directed = this;
	}

	UnaryOperation operation = {};

	Expression<> expression = {};
};

struct AstSubscript : AstExpression, ExpressionPool<AstSubscript> {
	AstSubscript() {
		kind = Ast_Subscript;
		directed = this;
	}
	Expression<> expression = {};
	Expression<> index_expression = {};

	bool is_prefix : 1 = false;
};

struct AstSpan : AstExpression, ExpressionPool<AstSpan> {
	AstSpan() {
		kind = Ast_Span;
		directed = this;
	}
	Expression<> expression = {};
};

struct AstTest : AstExpression, ExpressionPool<AstTest> {
	AstTest() {
		kind = Ast_Test;
		directed = this;
		scope = Scope::create();
		scope->node = this;
	}

	Scope *scope;
};

struct AstAssert : AstStatement, StatementPool<AstAssert> {
	AstAssert() {
		kind = Ast_Assert;
	}
	Expression<> condition = {};
	String message = {};
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

	Statement<AstDefinition> definition = {};
	Expression<AstLambda> lambda = {};

	TokenKind operation = {};
	bool is_implicit : 1 = false;
};

struct AstEnum : AstExpression, ExpressionPool<AstEnum> {
	AstEnum() {
		kind = Ast_Enum;
		directed = this;
		scope = Scope::create();
		scope->node = this;
	}

	Statement<AstDefinition> definition = {};

	Scope *scope;

	Expression<> underlying_type = {};
};

enum class LoopControl : u8 {
	Break,
	Continue,
};

struct AstLoopControl : AstStatement, StatementPool<AstLoopControl> {
	AstLoopControl() {
		kind = Ast_LoopControl;
	}
	Statement<AstWhile> loop;
	LoopControl control;
};

struct MatchCase {
	AstExpression *expression = 0; // will be null for default case
	Expression<AstBlock> block = AstBlock::create();
};

struct AstMatch : AstExpression, ExpressionPool<AstMatch> {
	AstMatch() {
		kind = Ast_Match;
	}

	Expression<> expression = {};
	BlockList<MatchCase> cases;
	MatchCase *default_case = 0;
	String default_case_location = {};
};

struct AstUsing : AstStatement, StatementPool<AstUsing> {
	AstUsing() {
		kind = Ast_Using;
	}

	Expression<> expression = {};
};

struct AstArrayInitializer : AstExpression, ExpressionPool<AstArrayInitializer> {
	AstArrayInitializer() {
		kind = Ast_ArrayInitializer;
		directed = this;
	}

	SmallList<AstExpression *> elements = {};
};

struct BuiltinStruct {
	AstDefinition *definition;

	// actual thing
	AstStruct        *Struct;  // string :: struct

	// reusable things
	AstIdentifier    *ident;   // string
	AstUnaryOperator *pointer; // *string
	AstIdentifier    *span;	   // []string
};

struct BuiltinEnum {
	AstDefinition *definition;

	// actual thing
	AstEnum          *Enum;    // type_kind :: enum

	// reusable things
	AstIdentifier    *ident;   // type_kind
	AstUnaryOperator *pointer; // *type_kind
	AstIdentifier    *span;	   // []type_kind
};

void lock(Scope *scope);
void unlock(Scope *scope);

AstStruct &get_built_in_type_from_token(TokenKind t);
AstStruct *find_built_in_type_from_token(TokenKind t);


inline Optional<BigInteger> get_constant_integer(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::integer) {
				return literal->integer;
			}
			if (literal->literal_kind == LiteralKind::character) {
				return make_big_int<BigInteger>((BigInteger::Part) literal->character);
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
inline AstExpression *direct(AstExpression *type) {
	while (1) {
		switch (type->kind) {
			case Ast_Identifier: {
				auto identifier = (AstIdentifier *)type;
				if (!identifier->definition())
					return 0;
				type = identifier->definition()->expression;
				if (!type)
					return 0;
				break;
			}
			case Ast_BinaryOperator: {
				auto binop = (AstBinaryOperator *)type;
				type = binop->right;
				break;
			}
			case Ast_UnaryOperator: {
				auto unop = (AstUnaryOperator *)type;
				if (unop->operation == UnaryOperation::typeof) {
					type = unop->expression->type;
				} else {
					return type;
				}
				break;
			}
			default:
				return type;
		}
	}
}


template <class T>
T *as(AstNode *node) {
	if (node->kind == kind_of<T>) {
		return (T *)node;
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
bool is_pointer_to(AstExpression *pointer_type, AstExpression *pointee_type);

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

AstExpression *get_span_subtype(AstExpression *span);







enum class BuildFrom {
	bytecode,
	ast,
};

struct Compiler {
	String source_path;
	String source_path_without_extension;
	String output_path;
	String compiler_path;
	String compiler_name;
	String compiler_directory;
	String current_directory;
	AstLambda *main_lambda;
	AstLambda *build_lambda;
	AstLambda *init_runtime_lambda;
	Profiler profiler;
    List<PreciseTimer> phase_timers;
	int tabs = 0;

	// Need pointer stability.
	// Maybe use different data structure.
	LinkedList<SourceFileInfo> sources;

	s64 stack_word_size = 0;
	s64 register_size = 0;
	s64 general_purpose_register_count = 0;
	bool do_profile = false;
	bool keep_temp = false;
	bool debug_template = false;
	bool debug_overload = false;
	bool print_lowered = false;
	bool optimize = false;
	bool print_yields = false;
	bool enable_dce = false;

	String print_lowered_filter = {};

	BuildFrom build_from = BuildFrom::bytecode;

	u8 optimization_pass_count = 4;

	List<AstLambda *> lambdas_with_body;
	List<AstLambda *> lambdas_without_body;

	Section constant_section;
	Section data_section;
	s64 zero_section_size = 0;

	List<RelativeString> string_set;

	Strings strings;

	struct BytecodeBuilder *bytecode;

	ExternLibraries extern_libraries;

	s32 ast_node_uid_counter;

	BuiltinStruct builtin_type;
	BuiltinStruct builtin_void;
	BuiltinStruct builtin_bool;
	BuiltinStruct builtin_u8;
	BuiltinStruct builtin_u16;
	BuiltinStruct builtin_u32;
	BuiltinStruct builtin_u64;
	BuiltinStruct builtin_s8;
	BuiltinStruct builtin_s16;
	BuiltinStruct builtin_s32;
	BuiltinStruct builtin_s64;
	BuiltinStruct builtin_f32;
	BuiltinStruct builtin_f64;

	BuiltinStruct builtin_string;
	BuiltinStruct builtin_struct_member;
	BuiltinStruct builtin_enum_member;
	BuiltinStruct builtin_typeinfo;
	BuiltinStruct builtin_any;
	BuiltinStruct builtin_range;

	BuiltinEnum builtin_type_kind;

	BuiltinStruct builtin_unsized_integer;
	BuiltinStruct builtin_unsized_float;
	BuiltinStruct builtin_noinit;
	BuiltinStruct builtin_unknown;
	BuiltinStruct builtin_unknown_enum;
	BuiltinStruct builtin_poly;
	BuiltinStruct builtin_overload_set;
	BuiltinStruct builtin_unreachable; // NOTE: this type is assigned to a block that always returns

	HashMap<AstExpression *, AstStruct *> span_instantiations;

	BuiltinStruct *builtin_default_signed_integer;
	BuiltinStruct *builtin_default_unsigned_integer;
	BuiltinStruct *builtin_default_integer;
	BuiltinStruct *builtin_default_float;

	AstIdentifier *type_int;
	AstIdentifier *type_sint;
	AstIdentifier *type_uint;
	AstIdentifier *type_float;

	Scope global_scope;

	Mutex global_scope_mutex;

	SourceFileInfo *get_source_info(utf8 *location) {
		for (auto &source : sources) {
			if (source.source.begin() <= location && location < source.source.end()) {
				return &source;
			}
		}
		return 0;
	}

	u32 get_line_number(Span<String> lines, utf8 *from) {
		// lines will be empty at lexing time.
		// So if an error occurs at lexing time,
		// slower algorithm is executed.
		if (lines.count) {
	#if 1
			// binary search
			auto begin = lines.data;
			auto end = lines.data + lines.count;
			while (1) {
				if (begin == end)
					return begin - lines.data + 1;
				assert(begin < end);
				auto line = begin + (end - begin) / 2;
				if (line->data <= from && from < line->data + line->count) {
					return line - lines.data + 1;
				}
				if (from < line->data) {
					end = line;
				} else {
					begin = line + 1;
				}
			}
			invalid_code_path();
	#else
			for (auto &line : lines) {
				if (line.begin() <= from && from < line.end()) {
					return &line - lines.data;
				}
			}
			invalid_code_path();
	#endif
		} else {
			u32 result = 1;
			while (*--from != 0)
				result += (*from == '\n');
			return result;
		}
	}
	u32 get_line_number(utf8 *from) {
		auto info = get_source_info(from);
		return info ? get_line_number(info->lines, from) : 0;
	}

	u32 get_column_number(utf8 *from) {
		u32 result = 0;
		while (1) {
			if (*from == '\n' || *from == '\0')
				break;

			if (*from == '\t')
				result += 4;
			else
				result += 1;

			from -= 1;
		}
		return result;
	}

	void print_replacing_tabs_with_4_spaces(Span<utf8> string) {
		for (auto c : string) {
			if (c == '\t') {
				print("    ");
			} else {
				print(c);
			}
		}
	}
	ConsoleColor get_console_color(ReportKind kind) {
		switch (kind) {
			using enum ReportKind;
			using enum ConsoleColor;
			case info: return cyan;
			case warning: return yellow;
			case error: return red;
		}
		invalid_code_path();
	}

	void print_source_line(SourceFileInfo *info, ReportKind kind, Span<utf8> location) {

		if (!location.data) {
			// print("(null location)\n\n");
			return;
		}
		if (!info) {
			return;
		}

		if (location == "\n"str) {
			auto error_line_begin = location.begin();
			if (*error_line_begin != 0) {
				while (1) {
					error_line_begin--;
					if (*error_line_begin == 0 || *error_line_begin == '\n') {
						error_line_begin++;
						break;
					}
				}
			}

			auto error_line_end = location.end();
			while (1) {
				if (*error_line_end == 0 || *error_line_end == '\n') {
					break;
				}
				error_line_end++;
			}


			auto error_line = Span(error_line_begin, error_line_end);
			auto error_line_number = get_line_number(info->lines, error_line_begin);

			auto format_line = [&](auto line) {
				return format("{} | ", Format{line, align_right(5, ' ')});
			};

			auto line_start = Span(error_line.begin(), location.begin());
			auto line_end   = Span(location.end(), error_line.end());
			auto line = format_line(error_line_number);

			for (u32 i = 0; i < line.count; ++i) {
				print(' ');
			}
			for (auto c : line_start) {
				if (c == '\t') {
					print("    ");
				} else {
					print(' ');
				}
			}
			for (auto c : location) {
				if (c == '\t') {
					print("VVVV");
				} else {
					print('V');
				}
			}
			print('\n');
			print(line);

			print_replacing_tabs_with_4_spaces(line_start);
			with(get_console_color(kind), print_replacing_tabs_with_4_spaces(location));
			print_replacing_tabs_with_4_spaces(line_end);

			print("\n");
		} else {
			auto error_line_begin = location.begin();
			if (*error_line_begin != 0) {
				while (1) {
					error_line_begin--;
					if (*error_line_begin == 0 || *error_line_begin == '\n') {
						error_line_begin++;
						break;
					}
				}
			}

			auto error_line_end = location.end();
			while (1) {
				if (*error_line_end == 0 || *error_line_end == '\n') {
					break;
				}
				error_line_end++;
			}


			auto error_line = Span(error_line_begin, error_line_end);
			auto error_line_number = get_line_number(info->lines, error_line_begin);

			auto print_line = [&](auto line) {
				return print("{} | ", Format{line, align_right(5, ' ')});
			};

			// I don't know if previous line is really useful
		#if 0
			if (error_line.data[-1] != 0) {
				auto prev_line_end = error_line.data - 1;
				auto prev_line_begin = prev_line_end - 1;

				while (1) {
					if (*prev_line_begin == 0) {
						prev_line_begin += 1;
						break;
					}

					if (*prev_line_begin == '\n') {
						++prev_line_begin;
						break;
					}

					--prev_line_begin;
				}
				auto prev_line = Span(prev_line_begin, prev_line_end);
				auto prev_line_number = get_line_number(prev_line_begin);

				print_line(prev_line_number);
				print_replacing_tabs_with_4_spaces(Print_info, prev_line);
				print('\n');
			}
		#endif

			auto line_start = Span(error_line.begin(), location.begin());
			auto line_end   = Span(location.end(), error_line.end());
			auto offset = print_line(error_line_number);
			print_replacing_tabs_with_4_spaces(line_start);
			with(get_console_color(kind), print_replacing_tabs_with_4_spaces(location));
			print_replacing_tabs_with_4_spaces(line_end);
			print('\n');

			if (!find(location, u8'\n')) {
				for (u32 i = 0; i < offset; ++i) {
					print(' ');
				}
				for (auto c : line_start) {
					if (c == '\t') {
						print("    ");
					} else {
						print(' ');
					}
				}

				withs(get_console_color(kind)) {
					for (auto c : location) {
						if (c == '\t') {
							print("~~~~");
						} else {
							print('~');
						}
					}
				};
				print("\n");
			}
		}
	}

	List<utf8> where(SourceFileInfo *info, utf8 *location) {
		if (location) {
			if (info) {
				return format(u8"{}:{}:{}", info->path, get_line_number(info->lines, location), get_column_number(location));
			}
		}
		return {};
	}
	List<utf8> where(utf8 *location) {
		return where(get_source_info(location), location);
	}

	void print_report(Report r) {
		auto source_info = r.location.data ? get_source_info(r.location.data) : 0;
		if (source_info) {
			if (r.location.data) {
				print("{}: ", where(source_info, r.location.data));
			} else {
				print(" ================ ");
			}
			withs(get_console_color(r.kind)) {
				switch (r.kind) {
					case ReportKind::info:    print(strings.info   ); break;
					case ReportKind::warning: print(strings.warning); break;
					case ReportKind::error:	  print(strings.error  ); break;
					default: invalid_code_path();
				}
			};
			print(": {}\n", r.message);
			print_source_line(source_info, r.kind, r.location);
		} else {
			print(" ================ ");
			withs(get_console_color(r.kind)) {
				switch (r.kind) {
					case ReportKind::info:    print(strings.info   ); break;
					case ReportKind::warning: print(strings.warning); break;
					case ReportKind::error:	  print(strings.error  ); break;
					default: invalid_code_path();
				}
			};
			print(": {}\n", r.message);
		}
	}

	template <class ...Args, class Char>
	void immediate_info(String location, Char const *format_string, Args const &...args) {
		print_report(make_report(ReportKind::info, location, format_string, args...));
	}
	template <class ...Args, class Char>
	void immediate_info(Char const *format_string, Args const &...args) {
		immediate_info(String{}, format_string, args...);
	}

	template <class ...Args, class Char>
	void immediate_warning(String location, Char const *format_string, Args const &...args) {
		print_report(make_report(ReportKind::warning, location, format_string, args...));
	}
	template <class ...Args, class Char>
	void immediate_warning(Char const *format_string, Args const &...args) {
		immediate_warning(String{}, format_string, args...);
	}

	template <class ...Args, class Char>
	void immediate_error(String location, Char const *format_string, Args const &...args) {
		print_report(make_report(ReportKind::error, location, format_string, args...));
	}
	template <class ...Args, class Char>
	void immediate_error(Char const *format_string, Args const &...args) {
		immediate_error(String{}, format_string, args...);
	}

};

inline Compiler *compiler;

#define scoped_phase(message) \
		/*sleep_milliseconds(1000);*/ \
		timed_block(compiler->profiler, as_utf8(as_span(message))); \
        compiler->phase_timers.add(create_precise_timer()); \
		++compiler->tabs; \
        defer { if(!compiler->do_profile) return; --compiler->tabs; for (int i = 0; i < compiler->tabs;++i) print("  "); print("{} done in {} ms.\n", message, get_time(compiler->phase_timers.pop().value()) * 1000); }

inline u32 get_line_number(utf8 *from) {
	return compiler->get_line_number(from);
}
inline u32 get_column_number(utf8 *from) {
	return compiler->get_column_number(from);
}
inline List<utf8> where(utf8 *location) { return compiler->where(location); }


template <class ...Args, class Char>
void immediate_info(String location, Char const *format_string, Args const &...args) {
	compiler->immediate_info(location, format_string, args...);
}
template <class ...Args, class Char>
void immediate_info(Char const *format_string, Args const &...args) {
	compiler->immediate_info(String{}, format_string, args...);
}

template <class ...Args, class Char>
void immediate_warning(String location, Char const *format_string, Args const &...args) {
	compiler->immediate_warning(location, format_string, args...);
}
template <class ...Args, class Char>
void immediate_warning(Char const *format_string, Args const &...args) {
	compiler->immediate_warning(String{}, format_string, args...);
}

template <class ...Args, class Char>
void immediate_error(String location, Char const *format_string, Args const &...args) {
	compiler->immediate_error(location, format_string, args...);
}
template <class ...Args, class Char>
void immediate_error(Char const *format_string, Args const &...args) {
	compiler->immediate_error(String{}, format_string, args...);
}




String decltype(AstLiteral::string)::get() const {
	return (String)Span(compiler->constant_section.buffer.subspan(offset, count));
}
void decltype(AstLiteral::string)::set(String string) {
	count = string.count;

	for (auto &existing : compiler->string_set) {
		if (String((utf8 *)compiler->constant_section.buffer.data + existing.offset, existing.count) == string) {
			offset = existing.offset;
			return;
		}
	}

	compiler->constant_section.buffer.ensure_capacity(string.count + 1);

	offset = compiler->constant_section.buffer.count;
	for (auto ch : string)
		compiler->constant_section.w1(ch);

	compiler->constant_section.w1(0);

	compiler->string_set.add({(u32)offset, (u32)count});
}

inline umm append(StringBuilder &b, AstNode *node) {
	return append(b, node->location);
}

inline AstUnaryOperator *make_pointer_type(AstExpression *type) {
	using enum UnaryOperation;
	auto unop = AstUnaryOperator::create();
	unop->expression = type;
	unop->type = compiler->builtin_type.ident;
	unop->operation = pointer;
	return unop;
}

inline AstNode::AstNode() {
	uid = atomic_increment(&compiler->ast_node_uid_counter);
	int x = 5;
}

inline bool struct_is_built_in(AstStruct *type) {
	return
		type == compiler->builtin_bool           .Struct ||
		type == compiler->builtin_u8             .Struct ||
		type == compiler->builtin_u16            .Struct ||
		type == compiler->builtin_u32            .Struct ||
		type == compiler->builtin_u64            .Struct ||
		type == compiler->builtin_s8             .Struct ||
		type == compiler->builtin_s16            .Struct ||
		type == compiler->builtin_s32            .Struct ||
		type == compiler->builtin_s64            .Struct ||
		type == compiler->builtin_f32            .Struct ||
		type == compiler->builtin_f64            .Struct ||
		type == compiler->builtin_type           .Struct ||
		type == compiler->builtin_string         .Struct ||
		type == compiler->builtin_noinit         .Struct ||
		type == compiler->builtin_unsized_integer.Struct ||
		type == compiler->builtin_unsized_float  .Struct ||
		type == compiler->builtin_void           .Struct;
}

inline bool is_type(AstExpression *expression) {
	assert(expression->type);
	return types_match(expression->type, compiler->builtin_type.Struct);
}


inline bool types_match(AstExpression *a, AstExpression *b) {
	if (a == b)
		return true;

	// TODO: direct() is way too slow.
	a = direct(a);
	b = direct(b);
	//a = a->directed ? (AstExpression *)a->directed : direct(a);
	//b = b->directed ? (AstExpression *)b->directed : direct(b);

	if (a->kind != b->kind) {
		return false;
	}

	switch (a->kind) {
		case Ast_Struct:
			return a == b;
		case Ast_Enum:
			return a == b;
		case Ast_UnaryOperator: {
			auto au = (AstUnaryOperator *)a;
			auto bu = (AstUnaryOperator *)b;
			return types_match(au->expression, bu->expression);
		}


		case Ast_Subscript: {
			auto eq = [](auto a, auto b) {
				if (!a.has_value()) return false;
				if (!b.has_value()) return false;
				return a.value() == b.value();
			};
			auto as = (AstSubscript *)a;
			auto bs = (AstSubscript *)b;
			return types_match(as->expression, bs->expression) &&
				eq(get_constant_integer(as->index_expression), get_constant_integer(bs->index_expression));
		}
		case Ast_Span: {
			auto as = (AstSpan *)a;
			auto bs = (AstSpan *)b;
			return types_match(as->expression, bs->expression);
		}
		case Ast_LambdaType: {
			auto al = ((AstLambdaType *)a)->lambda;
			auto bl = ((AstLambdaType *)b)->lambda;

			assert(al);
			assert(bl);

			if (!same_argument_and_return_types(al, bl))
				return false;

			if (al->convention != bl->convention)
				return false;

			return true;
		}
	}
	invalid_code_path();
}


inline bool same_argument_and_return_types(AstLambda *a, AstLambda *b) {
	if (a->parameters.count != b->parameters.count)
		return false;

	for (umm i = 0; i < a->parameters.count; ++i) {
		if (!types_match(a->parameters[i]->type, b->parameters[i]->type))
			return false;
	}

	if (!types_match(a->return_parameter->type, b->return_parameter->type))
		return false;

	return true;
}

inline bool is_pointer(AstExpression *type) {
	switch (type->kind) {
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)type;
			return is_pointer(ident->definition()->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			if (unop->operation == UnaryOperation::pointer_or_dereference_or_unwrap) {
				// This fails at parse time.
				// assert(is_type(unop->expression));
				return true;
			}
			return unop->operation == UnaryOperation::pointer;
		}
	}
	return false;
}

inline AstExpression *get_span_subtype(AstExpression *span) {
	if (auto Struct = direct_as<AstStruct>(span)) {
		if (!Struct->is_span)
			return 0;

		auto def = (AstDefinition *)Struct->member_scope->statement_list[0];
		assert(def->kind == Ast_Definition);

		auto pointer = as_pointer(def->type);

		return pointer->expression;
	}
	return 0;
}

inline AstUnaryOperator *as_pointer(AstExpression *type) {
	switch (type->kind) {
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)type;
			auto definition = ident->definition();
			if (!definition)
				return 0;
			return as_pointer(definition->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case UnaryOperation::pointer_or_dereference_or_unwrap:
				// This fails at parse time.
				// assert(is_type(unop->expression));
				case UnaryOperation::pointer:
					return unop;
			}
			break;
		}
	}
	return 0;
}

inline s64 get_size(AstExpression *_type, bool check_struct) {
	auto type = _type->directed ? (AstExpression *)_type->directed : direct(_type);

	assert(type);
	assert(types_match(type->type, compiler->builtin_type), "attemt to get_size of an expression, not a type!");
	switch (type->kind) {
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;
			if (check_struct)
				assert(Struct->size != -1, Struct->location, "Size for {} is not computed yet. If this happens when typechecking, use an overload that accepts TypecheckState. When generating bytecode this assert should not fire.", type_to_string(Struct));
			return Struct->size;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_size(identifier->definition()->expression);
		}
		case Ast_UnaryOperator: {
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				case pointer:  return compiler->stack_word_size;
				case typeof:   return get_size(unop->expression->type);
				case option:   return get_size(unop->expression) + get_align(unop->expression);
				case pack:     return compiler->stack_word_size*2; // pointer+count;
				default: invalid_code_path();
			}
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)type;
			auto count = get_constant_integer(subscript->index_expression);
			if (!count.has_value()) {
				immediate_error(subscript->index_expression->location, "Can't get the value of this expression. FIXME: This error is immediate, figure out a way to add it to report list.");
				exit(-1);
			}

			return get_size(subscript->expression) * (s64)count.value();
		}
		case Ast_Span: {
			return compiler->stack_word_size*2; // pointer+count;
		}
		case Ast_LambdaType: {
			return compiler->stack_word_size;
		}
		case Ast_Enum: {
			assert(!((AstEnum *)type)->underlying_type, "not implemented");
			return compiler->stack_word_size;
		}
		default: {
			invalid_code_path();
			return -1;
		}
	}
}

inline s64 get_align(AstExpression *type, bool check_struct) {
	assert(type);
	switch (type->kind) {
		case Ast_Struct: {
			auto Struct = (AstStruct *)type;
			if (check_struct)
				assert(Struct->alignment != -1, "Alignment for {} is not computed yet. If this happens when typechecking, use an overload that accepts TypecheckState. When generating bytecode this assert should not fire.");
			return Struct->alignment;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)type;
			return get_align(identifier->definition()->expression, check_struct);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)type;
			switch (unop->operation) {
				using enum UnaryOperation;
				case pointer:
					return compiler->stack_word_size;
				case option:
					return get_align(unop->expression, check_struct);
				default: invalid_code_path();
			}
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)type;
			return get_align(subscript->expression, check_struct);
		}
		case Ast_LambdaType: {
			return compiler->stack_word_size;
		}
		case Ast_Enum: {
			assert(!((AstEnum *)type)->underlying_type);
			return compiler->stack_word_size;
		}
		case Ast_Span: {
			return compiler->stack_word_size;
		}
		default: {
			invalid_code_path();
			return 0;
		}
	}
}

#define ENUMERATE_DEFINITION_ORIGIN \
x(unknown) \
x(constants) \
x(rwdata) \
x(zeros) \
x(return_parameter) \
x(parameter) \
x(local) \

enum class DefinitionOrigin {
#define x(name) name,
	ENUMERATE_DEFINITION_ORIGIN
#undef x
};

inline DefinitionOrigin get_definition_origin(AstDefinition *definition) {
	if (definition->is_constant) {
		return DefinitionOrigin::constants;
	}

	if (!definition->container_node) {
		// Global
		if (definition->expression) {
			return DefinitionOrigin::rwdata;
		} else {
			return DefinitionOrigin::zeros;
		}
	}

	assert(definition->parent_scope, definition->location, "definition->container_node is not null, so should be definition->parent_scope");


	if (auto Lambda = as<AstLambda>(definition->container_node)) {
		if (definition->parent_scope == Lambda->constant_scope) {
			return DefinitionOrigin::constants;
		}

		if (definition->parent_scope == Lambda->parameter_scope) {
			if (definition == Lambda->return_parameter)
				return DefinitionOrigin::return_parameter;
			else
				return DefinitionOrigin::parameter;
		}

		return DefinitionOrigin::local;
	}

	return DefinitionOrigin::unknown;
}

inline umm append(StringBuilder &b, DefinitionOrigin d) {
	switch (d) {
		using enum DefinitionOrigin;
#define x(name) case name: return append(b, #name);
	ENUMERATE_DEFINITION_ORIGIN
#undef x
	}
	return 0;
}

inline AstExpression *get_container_node(Scope *scope) {
	if (!scope->node)
		return 0;

	switch (scope->node->kind) {
		case Ast_Lambda:
		case Ast_Struct:
		case Ast_Enum:
			return (AstExpression *)scope->node;
	}

	return get_container_node(scope->parent);
}

inline AstExpression *get_container_node(AstStatement *statement) { return get_container_node(statement->parent_scope); }
