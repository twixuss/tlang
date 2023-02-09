// TODO: default struct member values

// TODO: unsized floats use only 64 bits

#define TL_IMPL
#include <common.h>
#include <tl/main.h>
#include <tl/cpu.h>
#include <tl/time.h>
#include <tl/hash_set.h>
#include <tl/tracking_allocator.h>
#include "compiler.h"
#include "visitor.h"
#include "bytecode.h"
#include "print_ast.h"
#include <string>
#include <algorithm>
#include <charconv>

#define CORO_IMPL
#pragma push_macro("assert")
#include <coro.h>
#pragma pop_macro("assert")

#define USE_FIBERS 1
#define SWITCH_TO_FIBER t_SwitchToFiber

extern "C" void t_SwitchToFiber(void *);

enum class TypecheckResult {
	wait,
	fail,
	success,
};

u32 yield_wait_count;

#if USE_FIBERS
TypecheckResult fiber_result;
bool throwing = false;
#define yield(x) (\
	pre_yield(state, x),\
	SWITCH_TO_FIBER(state->parent_fiber),\
	post_yield(state, x) \
)
#else
#define YIELD_STATE state->coro
// #define yield(x) (_ReadWriteBarrier(), ::tl::print("remaining stack size: {}\n", (u8 *)YIELD_STATE->sp - (u8 *)YIELD_STATE->buffer_base), ::coro_yield(YIELD_STATE, (size_t)x))
#define yield(x) (_ReadWriteBarrier(), ::coro_yield(YIELD_STATE, (size_t)x))
#endif

#define USE_SLABS 0

struct Parser;
struct Reporter;
struct SourceFileContext;
struct TypecheckState;

static List<String> import_paths;

bool ensure_addressable(Reporter *reporter, AstExpression *expression);
AstDefinition *parse_definition(String name, String location, Parser *parser, bool make_current_parameter = false);
AstDefinition *parse_definition(Parser *parser, bool make_current_parameter = false);

struct ParseBlockParams {
	bool allow_no_braces = false;
};
bool parse_scope(Parser *parser, Scope *scope, ParseBlockParams params = {});
AstBlock *parse_block(Parser *parser, ParseBlockParams params = {});

AstIdentifier *instantiate_span(AstExpression *subtype, String location);
bool is_statement(AstExpression *expression);
bool verify_block(Reporter *reporter, Scope *scope);
AstIf *parse_if_expression_starting_with_condition(Parser *parser, String if_token);

void print_help() {
	print(compiler->strings.usage, compiler->compiler_name);
}

u64 printed_reports_count = 0;

struct Reporter {
	List<Report> reports;

	void print_all() {
		printed_reports_count += reports.count;
		for (auto report : reports) {
			compiler->print_report(report);
		}
	}

	template <class Size, class ...Args>
	void info(Span<utf8, Size> location, char const *format_string, Args const &...args) {
		reports.add(make_report(ReportKind::info, location, format_string, args...));
	}
	template <class ...Args>
	void info(char const *format_string, Args const &...args) {
		info(String{}, format_string, args...);
	}

	template <class Size, class ...Args>
	void warning(Span<utf8, Size> location, char const *format_string, Args const &...args) {
		reports.add(make_report(ReportKind::warning, location, format_string, args...));
	}
	template <class ...Args>
	void warning(char const *format_string, Args const &...args) {
		warning(String{}, format_string, args...);
	}

	template <class Size, class ...Args>
	void error(Span<utf8, Size> location, char const *format_string, Args const &...args) {
		//if (debugger_attached())
		//	debug_break();
		reports.add(make_report(ReportKind::error, location, format_string, args...));
	}
	template <class ...Args>
	void error(char const *format_string, Args const &...args) {
		error(String{}, format_string, args...);
	}
};


struct Lexer {
	Token *tokens_start;
	Token *tokens_end;
	Token *token_cursor;

	void add(Token token) {
		*token_cursor++ = token;
	}

	auto begin() { return tokens_start; }
	auto end()   { return token_cursor;   }

	umm tokens_lexed() { return token_cursor - tokens_start; }

	bool finished = false;
	bool success = false;
	Reporter *reporter;

	SourceFileInfo *source_info;
	Buffer source_buffer;
	String source;
};

HashSet<String> double_char_tokens;
HashSet<String> triple_char_tokens;
Map<String, TokenKind> keywords;

f32 lexer_time;
bool lexer_function(Lexer *lexer) {
	timed_function(compiler->profiler);

	auto timer = create_precise_timer();
	defer {
		lexer_time = reset(timer);
	};


	defer { lexer->finished = true; };

	auto current_p = lexer->source.begin();
	auto next_p    = lexer->source.begin();
	utf32 c;

	auto next_char = [&]() {
		current_p = next_p;
		auto got_char = get_char_and_advance_utf8(&next_p);
		if (got_char) {
			c = got_char.value();
			return true;
		}
		return false;
	};

	if (!next_char()) {
		return false;
	}

	Token token = {};

	auto push_token = [&] {
		lexer->add(token);
	};

	auto skip_identifier_chars = [&] {
		while (1) {
			if (!next_char()) {
				return;
			}
			switch (c) {
				case '`':  case '-':  case '=':  case '\\':
				case '[':  case ']':  case ';':  case '\'':
				case ',':  case '.':  case '/':  case '~':
				case '!':  case '@':  case '#':  case '$':
				case '%':  case '^':  case '&':  case '*':
				case '(':  case ')':  case '+':  case '|':
				case '{':  case '}':  case ':':  case '"':
				case '<':  case '>':  case '?':  case ' ':
				case '\n': case '\t': case '\r': case '\0': {
					return;
				}
				default: {
					continue;
				}
			}
		}
	};

	while (1) {
	_continue:
		token.string.data = current_p;
		token.string.count = 0;

	nc:
		switch (c) {
			case '\0':
				goto lexer_success;
			case ' ':
			case '\r':
			case '\t':
				token.string.data += 1;
				if (!next_char()) {
					goto lexer_success;
				}
				goto nc;
			case '\n':
			case '`':
			case '[':
			case ']':
			case '@':
			case '$':
			case '?':
			case '~':
			case ':':
			case '(':
			case ')':
			case '{':
			case '}':
			case ',':
			case ';': {
				token.kind = (TokenKind)c;
				token.string.count = 1;
				if (!next_char()) {
					goto lexer_success;
				}
				push_token();
				break;
			}
			case '=':
			case '!':
			case '>':
			case '<':
			case '+':
			case '-':
			case '*':
			case '%':
			case '|':
			case '&':
			case '^': {
				auto found = find(triple_char_tokens, token.string = Span(current_p, (u32)3));
				if (found) {
					int k = 3;
				} else {
					found = find(double_char_tokens, token.string = Span(current_p, (u32)2));
					if (!found) {
						token.string = Span(current_p, (u32)1);
					}
				}

				token.kind = 0;

				for (auto c : token.string) {
					token.kind <<= 8;
					token.kind |= c;
				}

				next_p = token.string.end();
				next_char();

				push_token();
				break;
			}
			case '"': {
				while (1) {
					auto prev = c;
					next_char();
					if (c == '"') {
						if (prev != '\\') {
							next_char();
							break;
						}
					}
				}

				token.kind = Token_string_literal;
				token.string.count = current_p - token.string.data;
				push_token();
				break;
			}
			case '\'': {
				while (1) {
					auto prev = c;
					next_char();
					if (c == '\'') {
						next_char();
						if (prev != '\\') {
							break;
						}
					}
				}

				token.kind = Token_character_literal;
				token.string.count = current_p - token.string.data;
				push_token();
				break;
			}
			case '/': {
				if (next_char()) {
					if (c == '/') {
						while (next_char()) {
							if (c == '\n' || c == '\0') {
								break;
							}
						}
						continue;
					} else if (c == '*') {
						auto closed = [&] {
							if (!next_char()) {
								token.string.count = 2;
								lexer->reporter->error(token.string, "Unclosed comment block (end of file).");
								return false;
							}
							return true;
						};
#define check_closed \
	if (!closed()) { \
		return false; \
	}

						u32 deepness = 0;

						check_closed;

						while (1) {
							if (c == '*') {
								check_closed;
								if (c == '/') {
									next_char();
									if (deepness == 0) {
										goto end_block_comment;
									}
									deepness -= 1;
									if (c == '\0') {
										lexer->reporter->error(token.string, "Unclosed comment block (end of file).");
										return false;
									}
								}
							} else if (c == '/') {
								check_closed;
								if (c == '*') {
									deepness += 1;
								}
							} else {
								check_closed;
							}
						}
					end_block_comment:
						continue;



						// Old method that does not fill `lines` list.

					continue_search:
						auto comment_begin_or_end = find(Span(current_p, lexer->source.end()), {u8"*/"s, u8"/*"s});
						if (!comment_begin_or_end) {
							token.string.count = 2;
							lexer->reporter->error(token.string, "Unclosed comment block.");
							return false;
						}

						if (*comment_begin_or_end == '*') {
							// Closed
							if (--deepness) {
								current_p = comment_begin_or_end + 2;
								goto continue_search;
							}
						} else {
							// Opened
							++deepness;
							current_p = comment_begin_or_end + 2;
							goto continue_search;
						}

						next_p = comment_begin_or_end + 2;
						next_char();
						continue;
					} else if (c == '=') {
						next_char();
						token.kind = '/=';
						token.string.count = 2;
					} else {
						token.kind = '/';
						token.string.count = 1;
					}
				} else {
					token.kind = '/';
					token.string.count = 1;
				}
				push_token();
				break;
			}
			case '.': {
				next_char();
				switch (c) {
					case '.':
						token.kind = '..';
						token.string.count = 2;
						push_token();
						next_char();
						break;
					case '0': case '1':
					case '2': case '3':
					case '4': case '5':
					case '6': case '7':
					case '8': case '9': {
						while (1) {
							next_char();
							if (c < '0' || c > '9')
								break;
						}
						token.string.set_end(current_p);
						token.kind = Token_float_literal;
						push_token();
						break;
					}
					default:
						token.kind = '.';
						token.string.count = 1;
						push_token();
						break;
				}
				break;
			}
			case '0': case '1':
			case '2': case '3':
			case '4': case '5':
			case '6': case '7':
			case '8': case '9': {
				enum class NumberKind {
					decimal,
					hex,
					binary,
				};
				using enum NumberKind;
				NumberKind number_kind = decimal;
				if (c == '0') {
					if (next_char()) {
						if (c == 'x') {
							number_kind = hex;
							if (!next_char()) {
								lexer->reporter->error(token.string, "Unexpected end when parsing hex number.");
								return false;
							}

							while (1) {
								switch (c) {
									case '0': case '1': case '2': case '3':
									case '4': case '5': case '6': case '7':
									case '8': case '9': case 'a': case 'b':
									case 'c': case 'd': case 'e': case 'f':
									case 'A': case 'B': case 'C': case 'D':
									case 'E': case 'F': case '_':
										if (next_char()) {
											continue;
										}
										break;
								}
								break;
							}

							if (current_p - token.string.data <= 2) {
								lexer->reporter->error(token.string, "Invalid hex number.");
								return false;
							}
						} else if (c == 'b') {
							number_kind = binary;
							if (!next_char()) {
								lexer->reporter->error(token.string, "Unexpected end when parsing binary number.");
								return false;
							}

							while (1) {
								switch (c) {
									case '0': case '1': case '_': {
										if (next_char()) {
											continue;
										}
										break;
									}
								}
								break;
							}

							if (current_p - token.string.data <= 2) {
								lexer->reporter->error(token.string, "Invalid hex number.");
								return false;
							}
						} else {
							// Octal?
							goto decimal;
						}
					}
				} else {
					decimal:
					while (is_digit(c) || c == '_') {
						if (!next_char()) {
							break;
						}
					}
				}

				if (c == '.') {
					next_char();
					if (c == '.') {
						token.string.count = current_p - token.string.data - 1;
						token.kind = Token_integer_literal;
						push_token();

						next_char();
						token.string.data = current_p - 2;
						token.string.count = 2;
						token.kind = '..';
						push_token();
						break;
					}

					if (number_kind != decimal) {
						lexer->reporter->error(token.string, "Float literal are supported only in decimal form.");
						return false;
					}
					token.kind = Token_float_literal;

					switch (c) {
						case '0': case '1':
						case '2': case '3':
						case '4': case '5':
						case '6': case '7':
						case '8': case '9': {
							while (1) {
								switch (c) {
									case '0': case '1':
									case '2': case '3':
									case '4': case '5':
									case '6': case '7':
									case '8': case '9':
										next_char();
										continue;
								}
								break;
							}
							break;
						}
						default: {
							token.string.count = current_p - token.string.data - 1;
							token.kind = Token_integer_literal;
							push_token();

							token.string.data = current_p - 1;
							token.string.count = 1;
							token.kind = '.';
							push_token();
							continue;
						}
					}

				} else {
					token.kind = Token_integer_literal;
				}
				token.string.count = current_p - token.string.data;
				push_token();
				break;
			}
			case '#': {
				if (!next_char()) {
					lexer->reporter->error(token.string, "Unexpected end when parsing directive.");
					return false;
				}
				while (1) {
					if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_') {
						if (!next_char()) {
							break;
						}
					} else {
						break;
					}
				}
				token.kind = Token_directive;
				token.string.count = current_p - token.string.data;
				push_token();
				break;
			}
			case '\\': {
				next_char();
				while (is_whitespace(c)) {
					next_char();
				}
				skip_identifier_chars();
				token.string.count = current_p - token.string.data;
				if (token.string.count == 0) {
					lexer->reporter->error(token.string, "Expected an identifier after \\.");
					return false;
				}
				token.kind = Token_split_identifier;

				auto &back = lexer->token_cursor[-1];

				if (back.kind == Token_identifier || back.kind == Token_split_identifier) {
					back.string.set_end(token.string.end());
					back.kind = Token_split_identifier;
				} else {
					push_token();
				}
				break;
			}
			default: {
				skip_identifier_chars();

				token.string.count = current_p - token.string.data;

				if (token.string.count == 0) {
					lexer->reporter->error(String{current_p,1}, "Bad character.");
					return false;
				}

				auto found_keyword = keywords.find(token.string);
				if (found_keyword) {
					token.kind = found_keyword->value;
				} else {
					token.kind = Token_identifier;
				}

				push_token();
				break;
			}
		}
	}

lexer_success:
	if (lexer->token_cursor != lexer->tokens_start) {
		token = lexer->token_cursor[-1];
	}
	token.kind = 'eof';
	push_token();

	lexer->success = true;
	return true;
}

u32 main_return_value = 0;

struct Parser {
	Lexer *lexer = 0;
	Token *token;
	u32 token_index = 0;
	AstExpression *container_node = 0;
	Reporter *reporter;
	String extern_language;
	String extern_library;
	Scope *current_scope = &compiler->global_scope;
	u32 scope_count = 0;
	CallingConvention current_convention = CallingConvention::tlang;
	StructLayout current_struct_layout = StructLayout::tlang;
	AstLambda *current_lambda_for_parameters = 0;
	AstDefinition *current_parameter = 0;
	String current_directory = {};

#define ENABLE_PARSER_BREAKS 1
	bool next() {
	retry:
		assert(token->kind != 'eof');
		++token;
		if (token->kind == 'eof') {
			return false;
		}

#if ENABLE_PARSER_BREAKS
		if (token->string == "#pb") {
			debug_break();
			goto retry;
		}
#endif
		return true;
	}
	bool next_solid() {
	retry:
		assert(token->kind != 'eof');
		while (1) {
			++token;
			switch (token->kind) {
				case 'eof':
					return false;
				case '\n':
					continue;
				default:
#if ENABLE_PARSER_BREAKS
					if (token->string == "#pb") {
						debug_break();
						goto retry;
					}
#endif
					return true;
			}
		}
		return true;
	}

	bool expect(TokenKind expected_kind) {
		if (token->kind != expected_kind) {
			reporter->error(token->string, "Expected '{}', but got {}", token_kind_to_string(expected_kind), token_kind_to_string(token->kind));
			return false;
		}
		return true;
	}
	bool next_expect(TokenKind expected_kind) {
		if (!next()) {
			reporter->error(token->string, "Unexpected end of file.");
			return false;
		}
		if (!expect(expected_kind))
			return false;
		return true;
	}
	//bool expect_next(TokenKind expected_kind) {
	//	if (!expect(expected_kind))
	//		return false;
	//	if (!next()) {
	//		reporter->error(token->string, "Unexpected end of file.");
	//		return false;
	//	}
	//	return true;
	//}

	// NOTE:
	// After parsing this:    []Int
	// We get:                AstSpan { expression = AstIdentifier{"Int"} }
	//
	// AstSpan is replaced with instantiations of builtin "Span" struct after typechecking. There is no reason to not do this.
	//
	// AstSpan must not be used after that. That's why this function is defined here and not available at typechecking time.
	// For that case use instantiate_span instead.
};

enum class ParseResult {
	ok,
	read_error,
	syntax_error,
	alloc_error,
};

struct SourceFileContext {
	Reporter reporter;
	Lexer lexer;
	// Scope scope;
	Parser parser;
	WorkQueue work_queue;
	ParseResult result;
};

// Incremeted when an implicit cast is parsed.
// Decremented when an implicit cast is fully typechecked.
// Knowing this count, we can skip unnecessary waiting in `implicitly_cast`
u32 not_typechecked_implicit_casts_count;
u32 not_typechecked_binary_operators_count;
u32 not_typechecked_unary_operators_count;
u32 not_typechecked_has_value_overloads_count;

List<AstLambda *> implicit_casts;
List<AstLambda *> typechecked_explicit_casts;

struct OeratorOverload {
	AstLambda *lambda;
	AstOperatorDefinition *definition;
};
Map<BinaryOperation, List<OeratorOverload>> binary_operators;
Map<UnaryOperation, List<OeratorOverload>> unary_operators;

List<AstLambda *> has_value_overloads;

AstSpan *make_span(AstExpression *type) {
	auto span = AstSpan::create();
	span->location = type->location;
	span->expression = type;
	return span;
}
AstLiteral *make_bool(bool val, String location = {}) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::boolean;
	i->location = location;
	i->Bool = val;
	return i;
}
AstLiteral *make_string(String value, String location = {}) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::string;
	i->location = location;
	i->string.set(value);
	return i;
}
AstLiteral *make_integer(BigInteger value, String location = {}) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::integer;
	i->location = location;
	i->integer = value;
	return i;
}
AstLiteral *make_integer(u64 value, String location = {}) {
	return make_integer(make_big_int<BigInteger>(value), location);
}
AstLiteral *make_integer(s64 value, String location = {}) {
	return make_integer(make_big_int<BigInteger>(value), location);
}
AstLiteral *make_float(f64 value, String location = {}) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::Float;
	i->location = location;
	i->Float = value;
	return i;
}
AstLiteral *make_null(String location = {}) {
	auto null = AstLiteral::create();
	null->literal_kind = LiteralKind::null;
	null->location = location;
	return null;
}
AstBinaryOperator *make_binop(BinaryOperation op, AstExpression *left, AstExpression *right) {
	auto bin = AstBinaryOperator::create();
	bin->left = left;
	bin->right = right;
	bin->operation = op;
	return bin;
}
AstIdentifier *make_identifier(KeyString name) {
	auto i = AstIdentifier::create();
	i->name = name;
	return i;
}
AstIdentifier *make_identifier(AstDefinition *definition, String location = {}) {
	auto i = AstIdentifier::create();
	i->name = definition->name;
	i->possible_definitions.set(definition);
	i->type = definition->type;
	i->location = location;
	return i;
}
AstSubscript *make_subscript(AstExpression *expression, AstExpression *index_expression) {
	auto s = AstSubscript::create();
	s->expression = expression;
	s->index_expression = index_expression;
	return s;
}
AstArray *make_array(AstExpression *element_type, u64 count) {
	auto s = AstArray::create();
	s->element_type = element_type;
	s->count = count;

	// Is this necessary?
	s->count_expression = make_integer(count);
	return s;
}

bool signedness_matches(AstStruct *a, AstStruct *b) {
	assert(::is_integer(a));
	assert(::is_integer(b));
	if (types_match(a, compiler->builtin_unsized_integer) ||
		types_match(b, compiler->builtin_unsized_integer))
		return true;

	return ::is_signed(a) == ::is_signed(b);
}

AstUnaryOperator *make_address_of(Reporter *reporter, AstExpression *expression) {
	if (!ensure_addressable(reporter, expression))
		return 0;

	using enum UnaryOperation;

	auto result = AstUnaryOperator::create();
	result->expression = expression;
	result->operation = address_of;
	result->location = expression->location;
	if (expression->type)
		result->type = make_pointer_type(expression->type);
	return result;
}

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser, bool whitespace_is_skippable_before_binary_operator = false, int right_precedence = 0);
AstExpression *parse_expression_1(Parser *parser);
AstDefinition *parse_definition(Parser *parser);

bool skip_newlines(Parser *parser) {
	while (parser->token->kind == '\n') {
		if (!parser->next()) {
			return false;
		}
	}
	return true;
}

void push_scope_check(Scope *scope, Parser *parser) {
	assert(scope);
	assert(scope != parser->current_scope, "Pushed the same scope twice");
	//switch (scope->node->kind) {
	//	case Ast_Lambda: {
	//		auto lambda = (AstLambda *)scope->node;
	//		if (scope == lambda->body_scope || scope == lambda->parameter_scope) {
	//			// these already have their parents set.
	//			return;
	//		}
	//		break;
	//	}
	//	case Ast_Struct: {
	//		auto Struct = (AstStruct *)scope->node;
	//		if (scope == Struct->member_scope) {
	//			// these already have their parents set.
	//			return;
	//		}
	//		break;
	//	}
	//}
	//assert(!scope->parent);
}

#define push_scope(scope) \
	auto CONCAT(new_scope, __LINE__) = scope; \
	push_scope_check(CONCAT(new_scope, __LINE__), parser); \
	auto CONCAT(old_scope, __LINE__) = (CONCAT(new_scope, __LINE__)->parent = parser->current_scope); \
	parser->current_scope->children.add(CONCAT(new_scope, __LINE__)); \
	parser->current_scope = CONCAT(new_scope, __LINE__); \
	parser->current_scope->level = CONCAT(old_scope, __LINE__)->level + 1; \
	defer { parser->current_scope = CONCAT(old_scope, __LINE__); }; \
	parser->scope_count += 1;

ExternLanguage extern_language_from_string(String string) {
	if (string == "C"str) return ExternLanguage::c;

	return ExternLanguage::none;
}

bool can_be_global(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_ExpressionStatement: {
			auto expression = ((AstExpressionStatement *)statement)->expression;
			switch (expression->kind) {
				//case Ast_Import:
				//	return true;
				case Ast_Lambda: {
					auto Lambda = (AstLambda *)expression;
					return Lambda->is_evaluated_at_compile_time;
				}
				case Ast_If: {
					auto If = (AstIf *)expression;
					return If->is_constant;
				}
			}
			return false;
		}
		case Ast_Definition:
		case Ast_OperatorDefinition:
		case Ast_Assert:
		case Ast_Print:
		case Ast_EmptyStatement:
			return true;

		default:
			return false;
	}
}

AstDefinition *make_retparam(AstExpression *type, AstLambda *parent) {
	auto retparam = AstDefinition::create();

	retparam->parsed_type = type;
	retparam->container_node = parent;
	retparam->location = type->location;

	parent->parameter_scope->add(retparam);

	return retparam;
}

bool ensure_not_in_defer(Parser *parser, String location) {
	auto scope = parser->current_scope;
	// Global scope will have `node` set to null
	while (scope->node && scope->node->kind != Ast_Lambda) {
		if (scope->node->kind == Ast_Defer) {
			parser->reporter->error(location, "Return statement can not be inside a defer statement.");
			return false;
		}
		scope = scope->parent;
	}
	return true;
}

KeyString parse_identifier(Parser *parser) {
	if (parser->token->kind == Token_split_identifier) {
		auto remaining = parser->token->string;
		List<utf8> name;
		name.allocator = temporary_allocator;
		while (1) {
			auto slash = find(remaining, u8'\\');
			if (!slash) {
				name.add(remaining);
				break;
			}

			auto first_part_last = slash - 1;
			while (first_part_last > remaining.begin() && is_whitespace((ascii)*first_part_last)) { // NOTE: if non-ascii whitespace is used, this will not work.
				--first_part_last;
			}

			name.add(Span(remaining.begin(), first_part_last + 1));

			auto second_part_first = slash + 1;
			while (is_whitespace((ascii)*second_part_first)) { // NOTE: if non-ascii whitespace is used, this will not work.
				++second_part_first;
			}

			remaining.set_begin(second_part_first);
		}
		return name;
	} else if (parser->token->kind == Token_identifier) {
		return parser->token->string;
	} else {
		parser->reporter->error(parser->token->string, "Expected an identifier.");
		return {};
	}
}

AstDefinition *create_definition(Parser *parser, String name, AstExpression *type) {

	auto definition = AstDefinition::create();

	definition->name = name;
	definition->parsed_type = type;
	definition->container_node = parser->container_node;

	return definition;
}

AstDefinition *create_definition_in_current_scope(Parser *parser, String name, AstExpression *type TL_LP) {

	auto definition = create_definition(parser, name, type);

	parser->current_scope->add(definition TL_LA);

	return definition;
}

List<AstDefinition *> parse_one_or_more_definitions(Parser *parser) {
	List<AstDefinition *> definitions;

	while (1) {
		if (parser->token->kind == Token_identifier || parser->token->kind == Token_split_identifier) {
			auto name = parse_identifier(parser);

			auto definition = create_definition_in_current_scope(parser, name, 0);
			definition->location = parser->token->string;
			definitions.add(definition);

			parser->next();
			if (parser->token->kind == ':') {
				parser->next_solid();

				auto type = parse_expression_1(parser);
				if (!type)
					return {};

				// FIXME: this code is similar to one in parse_definition
				if (parser->token->kind == Token_directive) {
					if (parser->token->string == "#at") {
						parser->next_solid();

						auto target_definition = definitions[0];

						target_definition->placed_at = parse_identifier(parser);
						if (target_definition->placed_at.is_empty()) {
							parser->reporter->error(parser->token->string, "Expected a member name after #at");
							return {};
						}

						parser->next();
					}
				}

				for (auto definition : definitions) {
					// NOTE: Maybe deep copy?
					definition->parsed_type = type;
				}

				break;
			} else if (parser->token->kind == ',') {
				parser->next_solid();
				continue;
			}
		} else if (parser->token->kind == '}') {
			break;
		}
		parser->reporter->error(parser->token->string, "Unexpected token. Only definitions are allowed inside structs.");
		return {};
	}

	if (parser->token->kind == '=') {
		parser->reporter->error(parser->token->string, "Initialization of multiple definitions is not allowed.");
		return {};
	}

	return definitions;
}

AstStruct *parse_struct(Parser *parser, String token) {
	auto Struct = AstStruct::create();
	Struct->location = token;

	scoped_replace(parser->container_node, Struct);

	if (parser->token->kind == '(') {
		parser->next();

		Struct->is_template = true;

		push_scope(Struct->parameter_scope);

		if (parser->token->kind != ')') {
			for (;;) {
				auto name = parse_identifier(parser);
				if (!name.data)
					return 0;

				auto name_location = parser->token->string;

				parser->next();

				if (!parser->expect(':'))
					return 0;

				parser->next();

				if (name != "_"str) {
					if (auto found = Struct->parameter_scope->definition_map.find(name)){
						parser->reporter->error(name_location, "Can't use identical names for different parameters.");
						return 0;
					}
				}

				auto definition = create_definition_in_current_scope(parser, name, 0);
				// NOTE: don't mark definition as constant yet because we don't know the expression.

				definition->parsed_type = parse_expression(parser);
				if (!definition->parsed_type)
					return 0;

				definition->location = {name_location.begin(), definition->parsed_type->location.end()};

				if (parser->token->kind == ')') {
					break;
				}

				if (!parser->expect(','))
					return 0;

				parser->next();
			}
		}

		parser->next();

	}

	if (!parser->expect('{'))
		return 0;

	parser->next_solid();

	push_scope(Struct->member_scope);

	while (parser->token->kind != '}') {
		skip_newlines(parser);
		if (parser->token->kind == Token_identifier || parser->token->kind == Token_split_identifier) {
			if (parser->token[1].kind == ',') {
				auto definitions = parse_one_or_more_definitions(parser);
			} else {
				auto statement = parse_statement(parser);
				if (!statement)
					return 0;

				switch (statement->kind) {
					case Ast_EmptyStatement:
						break;
					case Ast_Definition: {
						break;
					}
					default:
						parser->reporter->error(statement->location, "Only definitions are allowed inside a struct");
						return 0;
				}
			}
		} else if (parser->token->kind == '}') {
			break;
		} else {
			parser->reporter->error(parser->token->string, "Unexpected token. Only definitions are allowed here.");
			return 0;
		}
	}

	Struct->layout = parser->current_struct_layout;

	parser->next();
	return Struct;
}

inline AstUnaryOperator *make_unary(UnaryOperation operation, AstExpression *expression, AstExpression *type = 0) {
	auto unop = AstUnaryOperator::create();
	unop->operation = operation;
	unop->expression = expression;
	unop->type = type;
	unop->location = expression->location;
	return unop;
}
inline AstUnaryOperator *make_sizeof() { return make_unary(UnaryOperation::Sizeof, 0); }
inline AstUnaryOperator *make_typeof() { return make_unary(UnaryOperation::typeof, 0); }

AstLambdaType *create_lambda_type(AstLambda *lambda) {
	auto type = AstLambdaType::create();
	type->location = lambda->location;
	type->lambda = lambda;
	return type;
}

AstExpression *parse_lambda(Parser *parser) {
	auto lambda = AstLambda::create();

	lambda->location = parser->token->string;

	if (!parser->expect('('))
		return 0;

	parser->next_solid();

	lambda->is_member = parser->container_node && parser->container_node->kind == Ast_Struct;

	scoped_replace(parser->container_node, lambda);

	push_scope(lambda->constant_scope);
	push_scope(lambda->parameter_scope);

	{
		scoped_replace(parser->current_lambda_for_parameters, lambda);

		List<AstDefinition *> queued_parameters;

		if (parser->token->kind != ')') {
			for (;;) {
				bool has_using = false;
				if (parser->token->kind == Token_using) {
					has_using = true;

					parser->next_solid();
				}

				auto name = parse_identifier(parser);
				if (!name.data)
					return 0;

				auto name_location = parser->token->string;

				parser->next_solid();

				AstDefinition *parameter = create_definition_in_current_scope(parser, name, 0);
				scoped_replace(parser->current_parameter, parameter);

				parameter->name = name;
				parameter->has_using = has_using;
				parameter->location = name_location;

				if (parser->token->kind == ':') {
					parser->next_solid();

					if (parser->token->kind != '=') {
						if (parser->token->kind == '%') {
							parameter->is_constant = true;
							lambda->is_poly = true;
							parser->next_solid();
						}

						// NOTE: Use parse_expression_1 instead of parse_expression because '=' should not be included in the type.
						parameter->parsed_type = parse_expression_1(parser);
						if (!parameter->parsed_type)
							return 0;

						if (auto unop = as<AstUnaryOperator>(parameter->parsed_type)) {
							if (unop->operation == UnaryOperation::pack) {
								parameter->is_pack = true;
								parameter->parsed_type = make_span(unop->expression);
								lambda->has_pack = true;
							}
						}

						if (auto ident = as<AstIdentifier>(parameter->parsed_type)) {
							// FIXME: sketchy comparison
							if (ident->definition() == compiler->builtin_ast.definition) {
								not_implemented();
								parameter->is_poly = true;
								parameter->is_constant = true;
								lambda->is_poly = true;
							}
						}

						parameter->location = {name_location.begin(), parameter->parsed_type->location.end()};
					}

					if (parser->token->kind == '=') {
						parser->next_solid();

						parameter->expression = parse_expression(parser);
						if (!parameter->expression)
							return 0;

						parameter->location = {name_location.begin(), parameter->expression->location.end()};
					}


					if (parameter->is_pack && parameter->expression) {
						//parser->reporter->error(parameter->location, "Parameter packs can not have default values.");
						//return 0;
					}
				} else if (parser->token->kind == ',') {
					queued_parameters.add(parameter);
					parser->next_solid();
					continue;
				} else {
					parser->reporter->error(parser->token->string, "Expected ':' or ','.");
					return 0;
				}

				for (auto &other_parameter : lambda->parameters) {
					if (parameter->name != "_"str && parameter->name == other_parameter->name) {
						parser->reporter->error(parameter->location, "Can't use identical names for different parameters.");
						return 0;
					}
				}

				// TODO: fix poly with multiple parameters, e.g.  foo :: (a, b: $T) {}
				if (queued_parameters.count) {
					if (parameter->is_poly) {
						queued_parameters[0]->is_poly = true;

						for (auto other : queued_parameters.skip(1)) {
							other->is_poly = false;
						}

						parameter->is_poly = false;
					}
				}

				for (auto &queued : queued_parameters) {
					queued->parsed_type = parameter->parsed_type;
					queued->is_pack = parameter->is_pack;
					lambda->parameters.add(queued);
				}
				queued_parameters.clear();

				lambda->parameters.add(parameter);

				skip_newlines(parser);

				if (parser->token->kind == ')') {
					break;
				}

				if (!parser->expect(','))
					return 0;

				parser->next_solid();
			}
		}
		parser->next();

		auto first_retparam_token = parser->token;
		switch (parser->token->kind) {
			case '\n':
			case '{':
			case '=>':
			case Token_where:
			case Token_directive:
				break;
			case Token_identifier: {
				auto retname_location = parser->token->string;
				auto retname = parser->token->string;
				parser->next();

				if (parser->token->kind == ':') {
					lambda->return_parameter = parse_definition(retname, retname_location, parser, true);
					if (!lambda->return_parameter) {
						return 0;
					}
				} else {
					parser->token = first_retparam_token;
					goto parse_retparam_expression;
				}
				break;
			}
			case Token_using: {
				parser->next_solid();
				lambda->return_parameter = parse_definition(parser, true);
				if (!lambda->return_parameter) {
					return 0;
				}
				lambda->return_parameter->has_using = true;
				break;
			}
			default: {
			parse_retparam_expression:
				lambda->return_parameter = AstDefinition::create();
				lambda->return_parameter->container_node = lambda;
				lambda->parameter_scope->add(lambda->return_parameter);
				scoped_replace(parser->current_parameter, lambda->return_parameter);


				auto return_type = parse_expression(parser);
				if (!return_type)
					return 0;
				lambda->return_parameter->parsed_type = return_type;
				lambda->return_parameter->location = return_type->location;
				break;
			}
		}
	}

	while (parser->token->kind == Token_directive) {
		if (parser->token->string == "#stdcall"str) {
			lambda->convention = CallingConvention::stdcall;
		} else if (parser->token->string == "#tlangcall"str) {
			lambda->convention = CallingConvention::tlang;
		} else if (parser->token->string == "#intrinsic"str) {
			lambda->is_intrinsic = true;
		} else if (parser->token->string == "#print_bytecode"str) {
			lambda->print_bytecode = true;
		} else if (parser->token->string == "#type"str) {
			lambda->is_type = true;
		} else if (parser->token->string == "#macro"str) {
			lambda->is_macro = true;
		} else {
			parser->reporter->error(parser->token->string, "Unknown lambda directive.");
			return 0;
		}
		parser->next();
	}

	if (lambda->is_type && lambda->is_macro) {
		parser->reporter->error(lambda->location, "Lambda can not have #type and #macro directives at the same time.");
		return 0;
	}

	if (lambda->convention == CallingConvention::none) {
		lambda->convention = parser->current_convention;
	}

	lambda->location = {lambda->location.begin(), parser->token[-1].string.end()};

	if (!lambda->is_type) {
		if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';' && parser->token->kind != '\n' && parser->token->kind != 'eof') {
			parser->reporter->error(parser->token->string, "Unexpected token: '{}'.", parser->token->string);
			parser->reporter->info(R"(Here's how you can write a lambda:
This is the simplest lambda:
	()

Next you can optionally specify the type:
	() ReturnType

Return value can be optionally named:
	() name: ReturnType

If a name is provided, a `using` can be used:
	() using name: ReturnType

Then the body may follow:
	()                   // no body
	() { statements... }
	() => expression
)");
			return 0;
		}
	}

	bool is_short = false;
	bool has_body = true;

	if (lambda->is_type) {
		has_body = false;
		if (parser->token->kind == '{' || parser->token->kind == '=>') {
			parser->reporter->error(lambda->location, "Body of a lambda can not be specified after a #type directive.");
			return 0;
		}
	} else {
		if (parser->token->kind == '{') {
		} else if (parser->token->kind == '=>') {
			is_short = true;
		} else if (parser->token->kind == ';' || parser->token->kind == '\n' || parser->token->kind == 'eof') {
			has_body = false;
		} else {
			parser->reporter->error(parser->token->string, "Expected {{ or => or ; or newline or return type instead of '{}'.", parser->token->string);
			return 0;
		}
	}

	if (has_body) {
		auto opening_token = parser->token;


#if 0
		if (is_short) {
			parser->next_solid();

			auto expression = parse_expression(parser);
			if (!expression)
				return 0;

			auto ret = AstReturn::create();
			ret->location = expression->location;
			ret->expression = expression;
			ret->lambda = lambda;

			lambda->body_scope->add(ret);
		} else {
			if (!parse_scope(parser, lambda->body_scope, {.allow_no_braces = false}))
				return 0;
		}
#else
		if (is_short) {
			parser->next_solid();

			auto expression = parse_expression(parser);
			if (!expression)
				return 0;

			lambda->body = expression;
		} else {
			lambda->body = parse_block(parser, {.allow_no_braces = false});
			if (!lambda->body)
				return 0;
		}
#endif
	} else {
		if (!lambda->is_type && !lambda->is_intrinsic) {
			// Extern functions

			auto print_example = [&]{
				parser->reporter->info("For example, if you want to link with C library you can do this:\n#extern_library \"library.lib\"\n<Library's functions>\n");
			};
			/*
			if (parser->extern_language.count == 0) {
				parser->reporter->error(lambda->location, "Lambda has no body, but extern language was not provided");
				print_example();
				return 0;
			}
			*/
			if (parser->extern_library.count == 0) {
				parser->reporter->error(lambda->location, "Lambda has no body, but extern library was not provided,");
				print_example();
				return 0;
			}

			/*
			lambda->extern_language = extern_language_from_string(parser->extern_language);
			if (lambda->extern_language == ExternLanguage::none) {
				parser->reporter->error(lambda->location, "Unsupported language: {}.", parser->extern_language);
				print_example();
				return 0;
			}
			*/

			lambda->extern_library = parser->extern_library;

		}
	}

	if (lambda->is_type) {
		return create_lambda_type(lambda);
	}

	if (lambda->is_macro) {
		if (lambda->return_parameter) {
			parser->reporter->error(lambda->location, "TODO: Macros can't return values right now.");
			return 0;
		}

		lambda->return_parameter = make_retparam(compiler->builtin_void.ident, lambda);
	}

	if (lambda->return_parameter)
		assert(lambda->return_parameter->parent_scope, lambda->return_parameter->location, "INTERNAL ERROR: definition's scope is null");

	// NOTE: return parameter may be null. It will be inferred from the body after typechecking.
	return lambda;
}
// token
// v
// (a: T)
// (a, b: T)
// (using a: T)
// ()

bool should_parse_lambda(Parser *parser) {
	assert(parser->token->kind == '(');
	auto start_token = parser->token;
	defer { parser->token = start_token; };

	if (!parser->next_solid())
		return false;

	if (parser->token->kind == ')' || parser->token->kind == Token_using)
		return true;

	if (!parser->next_solid())
		return false;

	if (parser->token->kind == ':' || parser->token->kind == ',')
		return true;

	return false;
}

//
// Make sure all expression statements except last are valid statements. The last one is allowed to be an expression.
//
bool verify_block(Reporter *reporter, Scope *scope) {
	for (auto &statement : scope->statement_list) {
		if (&statement == &scope->statement_list.back())
			continue;

		if (auto est = as<AstExpressionStatement>(statement)) {
			if (!is_statement(est->expression)) {
				reporter->error(est->expression->location, "This expression can not appear in the middle of a block.");
				return false;
			}
		}
	}
	return true;
}

void parse_expression_0(Parser *parser, AstExpression *&);

AstExpression *parse_expression_0(Parser *parser) {
	AstExpression *result = 0;
	parse_expression_0(parser, result);
	return result;
}
void parse_expression_0(Parser *parser, AstExpression *&result) {
	result = 0;
	defer {
		if (result) {


			if (result->type) {
				if (types_match(result, compiler->builtin_ast))
					return;
			}

			assert(!result->type, "INTERNAL ERROR: Types should not be set when parsing, only when typechecking.");
		}
	};

	switch (parser->token->kind) {
		case Token_simd: {
			parser->next();

			auto expr = parse_expression(parser);
			if (!expr)
				return;

			if (expr->kind != Ast_Subscript) {
				parser->reporter->error(expr->location, "Expected an array type after simd keyword, but got {}.", expr->kind);
				return;
			}

			auto array = (AstSubscript *)expr;

			// array->is_simd = true;

			result = array;
			return;
		}
		case Token_string_literal: {


			auto str = parser->token->string;
			assert(str.front() == '"');
			assert(str.back() == '"');
			str.data += 1;
			str.count -= 2;

			auto buffer = erase_all<HeapString::Allocator>(str, u8'\r');
			defer { free(buffer); };

			auto string = as_span(buffer);

			if (starts_with(string, "\n"s)) {
				string.data += 1;
				string.count -= 1;
			}

			if (ends_with(string, "\n"s)) {
				string.count -= 1;
			}

			if (auto unescaped = unescape_string(string)) {
				defer { parser->next(); free(unescaped.value_unchecked()); };
				result = make_string(unescaped.value_unchecked(), parser->token->string);
				return;
			}
			parser->reporter->error(parser->token->string, "Bad escape sequence in string literal. FIXME: show it exactly.");
			return;
		}
		case Token_character_literal: {
			auto character = AstLiteral::create();
			character->literal_kind = LiteralKind::character;
			character->location = parser->token->string;
			auto character_string = unescape_string(parser->token->string);
			if (!character_string) {
				parser->reporter->error(parser->token->string, "Bad escape sequence in string literal. FIXME: show it exactly.");
				return;
			}
			if (character_string.value_unchecked().count != 1) {
				parser->reporter->error(parser->token->string, "Character literal can not contain more than one character.");
				return;
			}
			character->character = character_string.value_unchecked().data[0];
			parser->next();
			result = character;
			return;
		}
		case Token_null: {
			auto null = AstLiteral::create();
			null->literal_kind = LiteralKind::null;
			null->location = parser->token->string;
			parser->next();
			result = null;
			return;
		}
		case Token_float_literal: {
			auto Float = AstLiteral::create();
			Float->literal_kind = LiteralKind::Float;
			if (std::from_chars((char *)parser->token->string.begin(), (char *)parser->token->string.end(), Float->Float).ec == std::errc::invalid_argument) {
				parser->reporter->error(parser->token->string, "Failed to parse floating point number.");
				return;
			}
			Float->location = parser->token->string;
			parser->next();
			result = Float;
			return;
		}
		case Token_integer_literal: {
			BigInteger value;
			if (parser->token->string.count >= 2 && parser->token->string.data[1] == 'x') {
				auto get_quart = [&] (auto i) {
					switch (parser->token->string.data[i]) {
						case '0': return 0;
						case '1': return 1;
						case '2': return 2;
						case '3': return 3;
						case '4': return 4;
						case '5': return 5;
						case '6': return 6;
						case '7': return 7;
						case '8': return 8;
						case '9': return 9;
						case 'a': case 'A': return 10;
						case 'b': case 'B': return 11;
						case 'c': case 'C': return 12;
						case 'd': case 'D': return 13;
						case 'e': case 'E': return 14;
						case 'f': case 'F': return 15;
					}
					invalid_code_path();
				};

				u32 i = 2;
				u64 value64 = 0;
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					if (value64 & 0xF000'0000'0000'0000) {
						break;
					}
					value64 <<= 4;
					value64 |= get_quart(i);
				}
				value = make_big_int<BigInteger>(value64);
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					value <<= 4;
					value |= get_quart(i);
				}
			} else if (parser->token->string.count >= 2 && parser->token->string.data[1] == 'b') {
				auto get_bit = [&] (auto i) {
					assert((u32)(parser->token->string.data[i] - '0') < 2, "invalid binary digit");
					return parser->token->string.data[i] - '0';
				};

				u32 i = 2;
				u64 value64 = 0;
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					if (value64 & 0x8000'0000'0000'0000) {
						break;
					}
					value64 <<= 1;
					value64 |= get_bit(i);
				}
				value = make_big_int<BigInteger>(value64);
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					value <<= 1;
					value |= get_bit(i);
				}
			} else {
				auto get_digit = [&] (auto i) {
					assert((u32)(parser->token->string.data[i] - '0') < 10, "invalid decimal digit");
					return parser->token->string.data[i] - '0';
				};

				u32 i = 0;
				u64 value64 = 0;
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					if (value64 > max_value<u64> / 10) {
						break;
					}
					value64 *= 10;
					value64 += get_digit(i);
				}
				value = make_big_int<BigInteger>(value64);
				for (; i != parser->token->string.count; ++i) {
					if (parser->token->string.data[i] == '_')
						continue;

					value *= 10;
					value += (u64)get_digit(i);
				}
			}
			auto location = parser->token->string;
			parser->next();
			result = make_integer(value, location);
			return;
		}
		case Token_true:
		case Token_false: {
			auto boolean = AstLiteral::create();
			boolean->literal_kind = LiteralKind::boolean;
			boolean->Bool = parser->token->kind == Token_true;
			boolean->location = parser->token->string;
			parser->next();
			result = boolean;
			return;
		}
		case Token_split_identifier:
		case Token_identifier: {
			auto identifier = AstIdentifier::create();
			identifier->location = parser->token->string;
			identifier->name = parse_identifier(parser);
			assert(identifier->name.data);
			parser->next();
			result = identifier;
			return;
		}
		case Token_struct: {
			auto token = parser->token->string;
			parser->next();
			result = parse_struct(parser, token);
			return;
		}
		case Token_union: {
			auto token = parser->token->string;
			parser->next();
			auto Struct = parse_struct(parser, token);
			if (!Struct)
				return;

			Struct->is_union = true;
			result = Struct;
			return;
		}
		case Token_if: {
			auto token = parser->token->string;
			parser->next();

			result = parse_if_expression_starting_with_condition(parser, token);
			return;
		}
		case Token_enum: {
			auto token = parser->token->string;
			parser->next_solid();

			auto Enum = AstEnum::create();
			Enum->location = token;

			if (!parser->expect('{'))
				return;

			parser->next_solid();

			push_scope(Enum->scope);

			while (parser->token->kind != '}') {
				auto name_location = parser->token->string;
				auto name = parse_identifier(parser);
				if (!name.data)
					return;

				auto definition = AstDefinition::create();
				definition->name = name;
				definition->location = name_location;
				definition->is_constant = true;

				parser->next();

				if (parser->token->kind == '\n') {
					parser->next_solid();

				} else if (parser->token->kind == ':') {
					if (!parser->next_expect(':'))
						return;

					parser->next();

					auto expr = parse_expression(parser);
					if (!expr)
						return;

					definition->expression = expr;

					skip_newlines(parser);

				} else {
					parser->reporter->error(parser->token->string, "Unexpected token: {}", parser->token->string);
					return;
				}

				Enum->scope->add(raw(definition));
			}

			parser->next();
			result = Enum;
			return;
		}
		case Token_match: {
			auto Match = AstMatch::create();
			Match->location = parser->token->string;
			parser->next_solid();
			Match->expression = parse_expression(parser);
			if (!Match->expression) {
				return;
			}

			skip_newlines(parser);

			if (!parser->expect('{'))
				return;
			parser->next_solid();

			while (parser->token->kind != '}') {
				auto &Case = Match->cases.add();

				if (parser->token->kind == 'else') {
					if (Match->default_case) {
						parser->reporter->error(parser->token->string, "Redefinition of else block");
						parser->reporter->info(Match->default_case_location, "Previously defined here");
						return;
					}
					Match->default_case_location = parser->token->string;
					Match->default_case = &Case;

					parser->next_solid();
				} else {
					Case.expression = parse_expression(parser);
					if (!Case.expression)
						return;
				}

				if (parser->token->kind == '=>')
					parser->next_solid();

				Case.block = parse_block(parser, {.allow_no_braces = true});
				if (!Case.block)
					return;

				Case.block->scope->node = Match;
				skip_newlines(parser);
			}
			parser->next();

			result = Match;
			return;
		}
					 /*
		case Token_import: {
			auto import = AstImport::create();
			import->location = parser->token->string;
			parser->next();

			import->path = unescape_string(parser->token->string);
			parser->next();

			for (auto import_path : import_paths) {
				auto child = parse_file((String)concatenate(import_path, '\\', import->path));
				if (child->result == ParseResult::ok) {
					import->scope = &child->scope;
					break;
				}
				if (child->result == ParseResult::read_error) {
					continue;
				}
				assert(child->result == ParseResult::syntax_error);
				parser->reporter->reports.add(child->reporter.reports);
				return;
			}

			return import;
		}
					 */
		case '(': {
			if (should_parse_lambda(parser)) {
				auto lambda = parse_lambda(parser);
				if (!lambda)
					return;

				result = lambda;
				return;
			}

			auto start_token = parser->token->string;
			if (!parser->next_solid()) {
				parser->reporter->error(parser->token->string, "Unexpected end of file. Unclosed ')'.");
				return;
			}


			auto expression = parse_expression(parser, true);
			if (!expression) {
				return;
			}

			skip_newlines(parser);

			if (parser->token->kind == ')') {
				auto end_token = parser->token->string;
				expression->location = {start_token.begin(), end_token.end()};
				parser->next();
				result = expression;
				return;
			//} else if (parser->token->kind == ',') {
			//	auto tuple = AstTuple::create();
			//	tuple->expressions.add(expression);
			//
			//	parser->next();
			//
			//	while (1) {
			//		auto expression = parse_expression(parser);
			//		if (!expression) {
			//			return;
			//		}
			//		tuple->expressions.add(expression);
			//
			//		if (parser->token->kind == ')') {
			//			break;
			//		} else if (parser->token->kind == ',') {
			//			parser->next();
			//		} else {
			//			parser->reporter->error(parser->token->string, "Expected ')' or ','");
			//			return;
			//		}
			//	}
			//	parser->next();
			//
			//	return tuple;
			//} else {
			//	parser->reporter->error(parser->token->string, "Expected ')' or ','");
			//	return;
			} else {
				parser->reporter->error(parser->token->string, "Expected ')'");
				return;
			}
		}
		// case '?': {
		// 	auto noinit = AstLiteral::create();
		// 	noinit->location = parser->token->string;
		// 	noinit->literal_kind = LiteralKind::noinit;
		// 	parser->next();
		// 	return noinit;
		// }
		case '[': {
			auto location = parser->token->string;

			parser->next();

			AstExpression *index_expression = 0;
			if (parser->token->kind != ']') {
				index_expression = parse_expression(parser, true);
				if (!index_expression)
					return;

				if (!parser->expect(']'))
					return;
			}

			parser->next();

			auto expression = parse_expression_1(parser);
			if (!expression)
				return;

			location = {location.begin(), parser->token[-1].string.end()};

			if (index_expression) {
				auto array = AstArray::create();
				array->location = location;
				array->element_type = expression;
				array->count_expression = index_expression;
				result = array;
				return;
			} else {
				auto span = AstSpan::create();
				span->location = location;
				span->expression = expression;
				result = span;
				return;
			}
		}
		case '{': {
			result = parse_block(parser, {.allow_no_braces = false});
			return;
		}
		case '.': {
			auto backup = parser->token;
			parser->next_solid();
			if (parser->token->kind == '[') {
				auto array = AstArrayInitializer::create();

				parser->next_solid();
				while (1) {
					auto e = parse_expression(parser);
					if (!e)
						return;
					array->elements.add(e);

					skip_newlines(parser);
					if (parser->token->kind == ',') {
						parser->next_solid();
					}
					if (parser->token->kind == ']') {
						parser->next();
						break;
					}
				}

				array->location = {backup->string.begin(), parser->token[-1].string.end()};

				result = array;
				return;
			} else {
				parser->token = backup;
				goto parse_default;
			}
			break;
		}
		default: {
		parse_default:
			auto operation = as_unary_operation(*parser->token);
			if (operation) {
				auto unop = AstUnaryOperator::create();
				unop->location = parser->token->string;
				unop->operation = operation.value_unchecked();
				parser->next();

				unop->expression = parse_expression_1(parser);
				if (!unop->expression)
					return;

				unop->location = {unop->location.begin(), unop->expression->location.end()};

				switch (unop->operation) {
					using enum UnaryOperation;
					case poly: {
						if (auto ident = as<AstIdentifier>(unop->expression)) {
							if (!parser->current_lambda_for_parameters) {
								parser->reporter->error(unop->location, "{} is used to make a polymorphic type, and it can only appear in lambda's parameter list.", unop->operation);
								return;
							}

							assert(parser->current_parameter);
							parser->current_parameter->is_poly = true;

							auto lambda = parser->current_lambda_for_parameters;
							lambda->is_poly = true;

							if (auto found = lambda->constant_scope->definition_map.find(ident->name)) {
								parser->reporter->error(unop->location, "Template type {} was already defined.", ident->name);
								parser->reporter->info(found->value[0]->location, "Here:");
								return;
							}
							auto definition = AstDefinition::create();
							definition->container_node = lambda;
							definition->is_constant = true;
							definition->location = ident->location;
							definition->name = ident->name;
							definition->is_poly = true;
							lambda->constant_scope->add(definition);

							result = ident;
							return;
						} else {
							parser->reporter->error(unop->location, "{} is used to make a polymorphic type, and you can only put an identifier after it.", unop->operation);
							return;
						}
						break;
					}
				}

				result = unop;
				return;
			} else if (parser->token->kind == Token_directive) {
				if (parser->token->string == "#if"str) {
					auto token = parser->token->string;
					parser->next();

					auto If = parse_if_expression_starting_with_condition(parser, token);
					if (!If)
						return;

					If->is_constant = true;
					result = If;
					return;
				} else if (parser->token->string == "#file"str) {
					auto literal = make_string(parser->lexer->source_info->path, parser->token->string);
					literal->location = parser->token->string;
					parser->next();
					result = literal;
					return;
				} else if (parser->token->string == "#line"str) {
					auto literal = make_integer((u64)get_line_number(parser->token->string.data), parser->token->string);
					parser->next();
					result = literal;
					return;
				} else if (parser->token->string == "#location"str) {
					auto literal = make_string(where(parser->token->string.data), parser->token->string);
					literal->location = parser->token->string;
					parser->next();
					result = literal;
					return;
				} else if (parser->token->string == "#function"str) {
					auto literal = AstLiteral::create();
					literal->location = parser->token->string;
					literal->literal_kind = LiteralKind::lambda_name;
					parser->next();
					result = literal;
					return;
				} else if (parser->token->string == "#container"str) {
					auto literal = AstLiteral::create();
					literal->location = parser->token->string;
					literal->literal_kind = LiteralKind::container_string;
					parser->next();
					result = literal;
					return;
				} else if (parser->token->string == "#compiles"str) {
					auto test = AstTest::create();
					test->location = parser->token->string;

					parser->next();

					if (!parse_scope(parser, test->scope, {.allow_no_braces = true})) {
						return;
					}

					result = test;
					return;
				} else if (parser->token->string == "#") {
					parser->next();

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;

					if (lambda->kind != Ast_Lambda) {
						parser->reporter->error(lambda->location, "Expected a lambda.");
						return;
					}

					((AstLambda *)lambda)->is_evaluated_at_compile_time = true;

					result = lambda;
					return;
				} else if (parser->token->string == "#debug_overload") {
					parser->next_solid();
					result = parse_expression_1(parser);
					if (result) {
						if (auto call = as<AstCall>(result)) {
							call->debug_overload = true;
						}
					}
					return;
				} else if (parser->token->string == "#ast") {
					auto ident = make_identifier(compiler->builtin_ast.definition, parser->token->string);
					parser->next();
					result = ident;
					return;
				} else if (parser->token->string == "#distinct") {
					auto distinct_token = parser->token->string;
					parser->next_solid();
					auto expression = parse_expression_1(parser);
					if (!expression)
						return;

					auto Distinct = AstDistinct::create();
					Distinct->location = {distinct_token.begin(), expression->location.end()};
					Distinct->expression = expression;
					result = Distinct;
					return;
				} else {
					parser->reporter->error(parser->token->string, "Unexpected directive (expression).");
					return;
				}
			} else {
				parser->reporter->error(parser->token->string, "Unexpected token: {}", token_kind_to_string(parser->token->kind));
				return;
			}
		}
	}


	invalid_code_path();
}

AstTuple *make_tuple(List<AstExpression *> expressions) {
	auto tuple = AstTuple::create();
	for (auto e : expressions)
		tuple->expressions.add(e);
	return tuple;
}

void update_location(AstBinaryOperator *binop) {
	binop->location = {binop->left->location.begin(), binop->right->location.end()};
}
void update_location(AstCall *call) {
	call->location = {call->callable->location.begin(), call->location.end()};
}
void update_location(AstSubscript *subscript) {
	subscript->location = {subscript->expression->location.begin(), subscript->location.end()};
}

// parses these:
// expr1.expr2
// expr1(expr2[, ...])
// expr1[expr2]
AstExpression *parse_expression_1(Parser *parser) {
	auto expression = parse_expression_0(parser);
	if (!expression)
		return 0;

	while (parser->token->kind == '(' || parser->token->kind == '[' || parser->token->kind == '.') {
		while (parser->token->kind == '.') {
			auto binop = AstBinaryOperator::create();
			binop->operation = as_binary_operation(parser->token->kind).value();
			binop->left = expression;

			parser->next();

			binop->right = parse_expression_0(parser);
			if (!binop->right)
				return 0;

			update_location(binop);
			expression = binop;
		}
		while (parser->token->kind == '(') {
			auto open_paren = parser->token->string;
			parser->next_solid();

			SmallList<NamedArgument> arguments;
			if (parser->token->kind != ')') {
				for (;;) {
					NamedArgument argument = {};

					argument.expression = parse_expression(parser);
					if (!argument.expression)
						return 0;

					if (argument.expression->kind == Ast_BinaryOperator) {
						auto bin = (AstBinaryOperator *)argument.expression;
						if (bin->operation == BinaryOperation::ass) {

							auto left = bin->left;
							if (left->kind != Ast_Identifier) {
								parser->reporter->error(left->location, "Invalid use of named argument: expected an identifier for left operand.");
								return 0;
							}
							auto ident = (AstIdentifier *)left;

							argument.name = ident->name;
							argument.expression = bin->right;
						}
					}
					if (!argument.name.is_empty()) {
						for (auto &other_argument : arguments) {
							if (argument.name == other_argument.name) {
								parser->reporter->error(argument.expression->location, "Can't use the same named parameter more than once.");
								return 0;
							}
						}
					}

					arguments.add(argument);

					if (!skip_newlines(parser)) {
						parser->reporter->error(parser->token->string, "Unexpected end of file in call");
						return 0;
					}

					if (parser->token->kind == ')') {
						break;
					}

					if (!parser->expect(','))
						return 0;

					parser->next_solid();

					if (parser->token->kind == ')') {
						break;
					}
				}
			}

			u32 named_argument_count = 0;
			for (auto argument : arguments) {
				named_argument_count += argument.name.data != 0;
			}

			auto close_paren = parser->token->string;

			auto call = AstCall::create();
			call->callable = expression;
			call->unsorted_arguments = arguments;

			call->location = parser->token->string;
			update_location(call);

			parser->next();
			expression = call;
		}

		while (parser->token->kind == '[') {
			auto subscript = AstSubscript::create();
			subscript->expression = expression;

			parser->next();

			subscript->index_expression = parse_expression(parser);
			if (!subscript->index_expression)
				return 0;

			if (!parser->expect(']'))
				return 0;

			subscript->location = parser->token->string;
			parser->next();

			update_location(subscript);

			expression = subscript;
		}
	}

	return expression;
}

template <class T>
concept CExpression = is_same<AstExpression *, T> || is_same<Expression<AstExpression>, T>;

// For types:
// Replaces identifiers with structs
// Example: *void parses into:
//   AstUnary(*) -> AstIdentifier(void)
// This will be replaced with:
//   AstUnary(*) -> AstStruct(void)
//
// Reason for this is getting rid of extra pointer dereferences in later use
//
//
// For expressions:
// Replaces operations on constants and constant identifiers with literals
//
// NOTE:
// Does not simplify child expressions as they are already simplified after parsing or typechecking
[[nodiscard]]
bool simplify(Reporter *reporter, CExpression auto *_expression) {
	auto expression = *_expression;
	defer {
		if (expression) {
			expression->location = (*_expression)->location;

			if (expression->type) {
				assert(types_match((*_expression)->type, expression->type));
			} else {
				expression->type = (*_expression)->type;
			}
		}
		*_expression = expression;
	};

	if (!expression->type) // do nothing in parse time if type is unknown yet
		return true;

	bool is_type = ::is_type(expression);// || (expression->type && expression->type->kind == Ast_Lambda);

	if (is_type) {
		switch (expression->kind) {
			case Ast_UnaryOperator: {
				using enum UnaryOperation;
				auto unop = (AstUnaryOperator *)expression;
				if (unop->operation == typeof) {
					expression = unop->expression->type;
					return true;
				}
				break;
			}
			case Ast_BinaryOperator: {
				using enum BinaryOperation;
				auto bin = (AstBinaryOperator *)expression;
				if (bin->operation == dot) {
					expression = bin->right;
					return true;
				}
				break;
			}
		}
	} else {
		switch (expression->kind) {
			case Ast_BinaryOperator: {
				auto binop = (AstBinaryOperator *)expression;

				using enum BinaryOperation;

				switch (binop->operation) {
					case dot: {
						// HACK HACK HACK TODO TODO TODO FIXME FIXME FIXME
						if (binop->left->kind == Ast_Literal) {
							auto left = (AstLiteral *)binop->left;
							if (left->literal_kind == LiteralKind::string) {
								if (binop->right->kind == Ast_Identifier) {
									auto right = (AstIdentifier *)binop->right;
									if (right->name == "count"str) {
										expression = make_integer((u64)left->string.count);
										expression->type = right->type;
									}
								}
							}
						}
						break;
					}
					case add:
					case sub:
					case mul:
					case div:
					case mod:
					case bxor:
					case band:
					case bor:
					case eq:
					case ne:
					case ge:
					case le:
					case bsl:
					case bsr:
					case gt:
					case lt: {
						auto left_literal  = get_literal(binop->left);
						auto right_literal = get_literal(binop->right);
						if (!left_literal || !right_literal)
							return true;
						if (left_literal->literal_kind == LiteralKind::integer && right_literal->literal_kind == LiteralKind::integer) {
							auto left  = left_literal->integer;
							auto right = right_literal->integer;

							BigInteger value;

							switch (binop->operation) {
								case add: expression = make_integer(left + right); return true;
								case sub: expression = make_integer(left - right); return true;
								case mul: expression = make_integer(left * right); return true;
								case div:
								case mod:
									if (right == 0) {
										reporter->error(expression->location, "Integer division by zero.");
										expression = 0;
										return false;
									}
									switch (binop->operation) {
										case div: expression = make_integer(left / right); break;
										case mod: expression = make_integer(left % right); break;
									}
									return true;
								case band: expression = make_integer(left & right); return true;
								case bor:  expression = make_integer(left | right); return true;
								case bxor: expression = make_integer(left ^ right); return true;
								case bsl: {
									constexpr u64 bytes_threshold = 1*MiB;
									auto result = copy(left);
									if (!result.shift_left(right, bytes_threshold,
										[&] { reporter->error(expression->location, "Can't shift left by negative amount."); },
										[&] (BigInteger required_bytes) { reporter->error(expression->location, "Resulting number of this operation will take up more than {}, which is more than allowed amount of {}.", format_bytes(required_bytes), format_bytes(bytes_threshold)); }
									)) {
										expression = 0;
										return false;
									}
									expression = make_integer(result);
									return true;
								}
								case bsr: expression = make_integer(left >> right); return true;
								case lt:  expression = make_bool(left < right); return true;
								case gt:  expression = make_bool(left > right); return true;
								case le: expression = make_bool(left <= right); return true;
								case ge: expression = make_bool(left >= right); return true;
								case ne: expression = make_bool(left != right); return true;
								case eq: expression = make_bool(left == right); return true;
								default: invalid_code_path(); break;
							}
						}

						// Compiler is not smart enough to see that if do_float is true then l and r are always initialized...
						bool do_float = false;
						f64 l = 0, r = 0;
						if (left_literal->literal_kind == LiteralKind::Float) {
							l = left_literal->Float;
							if (right_literal->literal_kind == LiteralKind::Float) {
								r = right_literal->Float;
								do_float = true;
							} else if (right_literal->literal_kind == LiteralKind::integer) {
								r = (f64)right_literal->integer;
								do_float = true;
							}
						} else if (right_literal->literal_kind == LiteralKind::integer) {
							r = (f64)right_literal->integer;
							if (left_literal->literal_kind == LiteralKind::Float) {
								l = left_literal->Float;
								do_float = true;
							} else if (left_literal->literal_kind == LiteralKind::integer) {
								invalid_code_path();
							}
						}

						if (do_float) {
							switch (binop->operation) {
								case add: expression = make_float(l + r); return true;
								case sub: expression = make_float(l - r); return true;
								case mul: expression = make_float(l * r); return true;
								case div: expression = make_float(l / r); return true;
								// case mod: expression = make_float(l % r, {.type = binop->type}); return true;
								// case band: expression = make_float(l & r, {.type = binop->type}); return true;
								// case bor: expression = make_float(l | r, {.type = binop->type}); return true;
								// case bxor: expression = make_float(l ^ r, {.type = binop->type}); return true;
								case lt: expression = make_bool(l < r); return true;
								case gt: expression = make_bool(l > r); return true;
								case le: expression = make_bool(l <= r); return true;
								case ge: expression = make_bool(l >= r); return true;
								case ne: expression = make_bool(l != r); return true;
								case eq: expression = make_bool(l == r); return true;
								default: invalid_code_path(); break;
							}
						}

						break;
					}
					case as: {
						if (!binop->type)
							return true; // do nothing in parsing phase

						auto literal = get_literal(binop->left);
						if (!literal) {
							return true;
						}

						if (literal->literal_kind == LiteralKind::integer) {
							auto result = copy(literal->integer);
							assert(BigInteger::bits_in_part == 64, "this algorithm relies on part being 64 bit wide");
							if (types_match(binop->right, compiler->builtin_u8)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
							} else if (types_match(binop->right, compiler->builtin_u16)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
							} else if (types_match(binop->right, compiler->builtin_u32)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
							} else if (types_match(binop->right, compiler->builtin_u64) || ::is_pointer(binop->right)) {
								result.msb = 0;
								result.parts.resize(1);
							} else if (types_match(binop->right, compiler->builtin_s8)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
								result.msb = result.parts.data[0] & 0x80;
								result.parts.data[0] |= result.msb ? ~0xFF : 0;
							} else if (types_match(binop->right, compiler->builtin_s16)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
								result.msb = result.parts.data[0] & 0x8000;
								result.parts.data[0] |= result.msb ? ~0xFFFF : 0;
							} else if (types_match(binop->right, compiler->builtin_s32)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
								result.msb = result.parts.data[0] & 0x80000000;
								result.parts.data[0] |= result.msb ? ~0xFFFFFFFF : 0;
							} else if (types_match(binop->right, compiler->builtin_s64)) {
								result.parts.resize(1);
								result.msb = result.parts.data[0] & 0x8000000000000000;
							} else {
								immediate_warning(binop->location, "simplify for this case is not implemented");
							}
							expression = make_integer(result);
						} else if (literal->literal_kind == LiteralKind::Float) {
							// FIXME: this relies on c++ conversion rules.
							if (types_match(binop->right, compiler->builtin_s8)) {
								expression = make_integer((s64)(s8)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_s16)) {
								expression = make_integer((s64)(s16)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_s32)) {
								expression = make_integer((s64)(s32)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_s64)) {
								expression = make_integer((s64)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_u8)) {
								expression = make_integer((u64)(u8)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_u16)) {
								expression = make_integer((u64)(u16)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_u32)) {
								expression = make_integer((u64)(u32)literal->Float);
							} else if (types_match(binop->right, compiler->builtin_u64)) {
								expression = make_integer((u64)literal->Float);
							} else {
								immediate_warning(binop->location, "simplify for this case is not implemented");
							}
						}
						expression->type = binop->right;
						break;
					}
					case ass:
					case addass:
					case subass:
					case mulass:
					case divass:
					case modass:
					case borass:
					case bandass:
					case bxorass:
					case bslass:
					case bsrass:
						break;
					 case lor: {
						auto left_literal  = get_literal(binop->left);
						if (left_literal && left_literal->literal_kind == LiteralKind::boolean) {
							if (left_literal->Bool) {
								expression = make_bool(true);
							} else {
								auto right_literal = get_literal(binop->right);
								if (right_literal && right_literal->literal_kind == LiteralKind::boolean) {
									expression = make_bool(right_literal->Bool);
								}
							}
						}
						break;
					 }
					 case land: {
						auto left_literal  = get_literal(binop->left);
						if (left_literal && left_literal->literal_kind == LiteralKind::boolean) {
							if (!left_literal->Bool) {
								expression = make_bool(false);
							} else {
								auto right_literal = get_literal(binop->right);
								if (right_literal && right_literal->literal_kind == LiteralKind::boolean) {
									expression = make_bool(right_literal->Bool);
								}
							}
						}
						break;
					 }
					default:
						with(ConsoleColor::yellow, print("unhandled binary operation in simplify\n"));
						break;
				}
				break;
			}
			case Ast_UnaryOperator: {
				using enum UnaryOperation;
				auto unop = (AstUnaryOperator *)expression;

				switch (unop->operation) {
					case UnaryOperation::move_to_temporary: return true;
				}

				if (unop->expression->kind == Ast_Literal) {
					auto literal = (AstLiteral *)unop->expression;
					switch (literal->literal_kind) {
						case LiteralKind::integer: {
							auto integer = literal->integer;
							switch (unop->operation) {
								case plus: return true;
								case minus: expression = make_integer(-integer); return true;
								case bnot:  expression = make_integer(~integer); return true;
								case autocast: return true;

								default:
									with(ConsoleColor::yellow, print("unhandled unary operation in simplify(integer)\n"));
									break;
							}
							break;
						}
						case LiteralKind::Float: {
							auto Float = literal->Float;
							switch (unop->operation) {
								case plus: return true;
								case minus: expression = make_float(-Float); return true;
								default:
									with(ConsoleColor::yellow, print("unhandled unary operation in simplify(float)\n"));
									break;
							}
						}
					}
				}

				break;
			}
			case Ast_Identifier: {
				auto identifier = (AstIdentifier *)expression;
				auto definition = identifier->definition();
				if (definition) {
					if (definition->is_constant) {
						if (definition->expression) {
							if (definition->expression->kind == Ast_Literal) {
								expression = deep_copy(definition->expression);
							}
						}
					}
				}
				break;
			}
			case Ast_Block: {
				auto Block = (AstBlock *)expression;
				if (Block->scope->statement_list.count == 1) {
					if (auto est = as<AstExpressionStatement>(Block->scope->statement_list[0])) {
						expression = est->expression;
					}
				}
				break;
			}
			case Ast_If:
			case Ast_Match:
			case Ast_Call:
			case Ast_Literal:
			case Ast_Lambda:
			case Ast_LambdaType:
			case Ast_Struct:
			case Ast_Subscript:
			case Ast_Array:
			case Ast_Span:
			case Ast_Test:
			case Ast_Enum:
			case Ast_ArrayInitializer:
				break;
			default:
				with(ConsoleColor::yellow, print("unhandled case in simplify: {}\n", expression->kind));
				break;
		}
	}
	return true;
}

// a.b[c] = ((a).(b))[c]
// a+b[c] = (a)+((b)[c])
int parse_paren_level = 0;
void print_parsed_expression(AstExpression *expression) {
	using enum ConsoleColor;
	static constexpr ConsoleColor colors[] {
		red,
		green,
		blue,
		yellow,
		magenta,
		cyan,
	};
	auto open = [](char c = '(') { with(colors[parse_paren_level++ % count_of(colors)], print(c)); };
	auto close = [](char c = ')') { with(colors[--parse_paren_level % count_of(colors)], print(c)); };
	switch (expression->kind) {
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)expression;
			open();
			print(ident->name);
			close();
			break;
		}
		case Ast_BinaryOperator: {
			auto bin = (AstBinaryOperator *)expression;
			open();
			print_parsed_expression(bin->left);
			print(as_string(bin->operation));
			print_parsed_expression(bin->right);
			close();
			break;
		}
		case Ast_UnaryOperator: {
			auto un = (AstUnaryOperator *)expression;
			open();
			print(as_string(un->operation));
			print_parsed_expression(un->expression);
			close();
			break;
		}
		case Ast_Subscript: {
			auto s = (AstSubscript *)expression;
			open();
			print_parsed_expression(s->expression);
			open('[');
			print_parsed_expression(s->index_expression);
			close(']');
			close();
			break;
		}
		case Ast_Call: {
			auto s = (AstCall *)expression;
			open();
			print_parsed_expression(s->callable);
			open();
			for (int i = 0; i < s->unsorted_arguments.count; ++i) {
				if (i)
					print(", ");
				print_parsed_expression(s->unsorted_arguments[i].expression);
			}
			close();
			close();
			break;
		}
		default:
			open();
			print("unknown");
			close();
			break;
	}
}

bool is_right_associative(BinaryOperation operation) {
	return false;
}

// Implementation of Pratt Precedence
// Thanks to this: https://github.com/richardjennings/prattparser
AstExpression *parse_expression(Parser *parser, bool whitespace_is_skippable_before_binary_operator, int right_precedence) {
	//null denotation
	AstExpression *left = parse_expression_1(parser);
	if (!left)
		return 0;

	// left binding power
	Optional<BinaryOperation> operation;
	while (1) {

		if (whitespace_is_skippable_before_binary_operator) {
			skip_newlines(parser);
		}

		if (parser->token->kind == Token_identifier) {

			// Custom binary operator

			if (right_precedence < custom_precedence) {
				auto name = parser->token;
				parser->next_solid();
				auto right = parse_expression(parser, whitespace_is_skippable_before_binary_operator, custom_precedence);
				if (!right)
					return 0;

				auto ident = AstIdentifier::create();
				ident->name = name->string;
				ident->location = name->string;

				auto call = AstCall::create();
				call->location = {left->location.begin(), right->location.end()};
				call->callable = ident;
				call->unsorted_arguments.add({
					{.expression = left},
					{.expression = right},
				});
				left = call;
				continue;
			}
		} else {
			operation = as_binary_operation(parser->token->kind);
			if (operation && right_precedence < get_precedence(operation.value())) {
				parser->next_solid();

				auto binop = AstBinaryOperator::create();
				binop->left = left;
				binop->operation = operation.value();
				binop->right = parse_expression(parser, whitespace_is_skippable_before_binary_operator, get_precedence(binop->operation) - is_right_associative(operation.value()));

				if (!binop->right)
					return 0;

				update_location(binop);

				left = binop;
				continue;
			}
		}
		break;
	}
	return left;
}

// returns true for functions (overloading), otherwise false
bool is_redefinable(AstDefinition *definition) {
	return is_lambda(definition->expression);
}

//
// Use this if name token is already taken from parser.
//
AstDefinition *parse_definition(String name, String location, Parser *parser, bool make_current_parameter) {
	assert(parser->token->kind == ':');

	parser->next_solid();

	AstExpression *type = 0;
	if (parser->token->kind != ':' && parser->token->kind != '=' ) {
		// not using parse_expression because '=' will be included in the type expression.
		type = parse_expression_1(parser);
		if (!type)  return 0;
	}

	bool is_constant = false;
	bool has_expression = false;
	switch (parser->token->kind) {
		case ':': {
			has_expression = true;
			is_constant = true;
			break;
		}
		case '=': {
			has_expression = true;
			break;
		}
	}

	auto definition = create_definition_in_current_scope(parser, name, type);

	scoped_replace_if(parser->current_parameter, definition, make_current_parameter);


	definition->is_constant = is_constant;
	// definition->add_to_scope(parser->current_scope);

	if (has_expression) {
		parser->next_solid();

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		definition->expression = expression;

		definition->location = {location.begin(), expression->location.end()};

		switch (expression->kind) {
			case Ast_Lambda: {
				auto lambda = (AstLambda *)expression;
				lambda->definition = definition;
				lambda->location = {location.begin(), lambda->location.end()};
				break;
			}
			case Ast_Struct: {
				auto Struct = (AstStruct *)expression;
				Struct->definition = definition;
				Struct->location = {location.begin(), Struct->location.end()};
				break;
			}
			case Ast_Enum: {
				auto Enum = (AstEnum *)expression;
				Enum->definition = definition;
				Enum->location = {location.begin(), Enum->location.end()};
				break;
			}
			case Ast_Distinct: {
				auto Distinct = (AstDistinct *)expression;
				auto ident = AstIdentifier::create();
				ident->directed = Distinct;
				ident->location = definition->location;
				ident->name = definition->name;
				ident->possible_definitions.set(definition);
				ident->type = compiler->builtin_type.ident;
				Distinct->identifier = ident;
				break;
			}
		}
	} else {
		definition->location = {location.begin(), definition->parsed_type->location.end()};
	}

	// FIXME: this code is similar to one in parse_one_or_more_definitions
	if (parser->token->kind == Token_directive) {
		if (parser->token->string == "#at") {
			parser->next_solid();

			definition->placed_at = parse_identifier(parser);
			if (definition->placed_at.is_empty()) {
				parser->reporter->error(parser->token->string, "Expected a member name after #at");
				return 0;
			}

			parser->next();
		}
	}

	return definition;
}
AstDefinition *parse_definition(Parser *parser, bool make_current_parameter) {
	if (!skip_newlines(parser)) {
		parser->reporter->error(parser->token->string, "Expected a definition, but got end of file.");
		return 0;
	}
	auto location = parser->token->string;
	auto name = parse_identifier(parser);
	if (!name.data)
		return 0;

	if (!parser->next_expect(':'))
		return 0;

	return parse_definition(name, location, parser, make_current_parameter);
}

bool is_statement(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Call:
		//case Ast_Import:
			return true;
		case Ast_BinaryOperator: {
			auto bin = (AstBinaryOperator *)expression;
			switch (bin->operation) {
				using enum BinaryOperation;
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
					return true;
			}
			break;
		}
		case Ast_UnaryOperator: {
			auto UnaryOperator = (AstUnaryOperator *)expression;
			if (UnaryOperator->operation == UnaryOperation::insert)
				return true;
			break;
		}
		case Ast_Lambda: {
			auto lambda = (AstLambda *)expression;
			if (lambda->is_evaluated_at_compile_time)
				return true;
			break;
		}
		case Ast_If:
		case Ast_Block:
		case Ast_Match:
			return true;
	}
	return false;
}

AstExpressionStatement *make_statement(AstExpression *expression) {
	if (!expression)
		return 0;

	auto statement = AstExpressionStatement::create();
	statement->expression = expression;
	statement->location = expression->location;
	return statement;
}

bool parse_scope(Parser *parser, Scope *scope, ParseBlockParams params) {
	push_scope(scope);

	bool has_braces = parser->token->kind == '{';

	if (has_braces) {
		parser->next_solid();
		while (1) {

			auto token_right_after_statement = parser->token;
			skip_newlines(parser);
			if (parser->token->kind == '}')
				break;
			parser->token = token_right_after_statement;


			if (parser->token->kind == Token_identifier && parser->token[1].kind == ',') {
				auto definitions = parse_one_or_more_definitions(parser);
				if (!definitions.count)
					return false;
			} else {
				auto statement = parse_statement(parser);
				if (!statement)
					return false;
			}
		}
		parser->next();
	} else {
		if (params.allow_no_braces) {
			auto statement = parse_statement(parser);
			if (!statement)
				return false;
		} else {
			parser->reporter->error(parser->token->string, "Expected a block with braces here.");
			return false;
		}
	}

	if (!verify_block(parser->reporter, scope))
		return false;

	return true;
}
AstBlock *parse_block(Parser *parser, ParseBlockParams params) {
	auto block = AstBlock::create();

	auto start_location = parser->token->string;

	if (!parse_scope(parser, block->scope, params))
		return 0;

	auto end_location = parser->token[-1].string;
	block->location = {start_location.begin(), end_location.end()};

	return block;
}

AstIf *parse_if_expression_starting_with_condition(Parser *parser, String if_token) {
	auto If = AstIf::create();
	If->location = if_token;
	If->condition = parse_expression(parser);
	if (!If->condition)
		return 0;

	skip_newlines(parser);
	if (parser->token->kind == Token_then)
		parser->next_solid();

	if (parser->token->kind == Token_else) {
		parser->reporter->error(parser->token->string, "Unexpected token: 'else'. Did you forget to use 'then' keyword?");
		parser->reporter->info(If->location, "Here is the if statement:");
		return 0;
	}

	If->true_block = parse_block(parser, {.allow_no_braces = true});
	if (!If->true_block)
		return 0;

	auto token_right_after_true_block = parser->token;
	skip_newlines(parser);

	if (parser->token->kind == Token_else) {
		parser->next();

		If->false_block = parse_block(parser, {.allow_no_braces = true});
		if (!If->false_block)
			return 0;
	} else {
		// Cleanup: false_block is empty, maybe don't allocate it? I'm doing this now just to not worry about it being null.
		If->false_block = AstBlock::create();
		If->false_block->scope->parent = parser->current_scope;

		// NOTE: Here we might have skipped a line break, that may break binary operator parsing.
		parser->token = token_right_after_true_block;
	}

	If->location = {If->location.begin(), parser->token[-1].string.end()};

	auto link_scope = [&] (AstBlock *block) {
		block->scope->node = If;
	};

	link_scope(If->true_block);
	link_scope(If->false_block);
	return If;
}

AstEmptyStatement *empty_statement;

bool may_be_terminated_by_semicolon(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_While:
		case Ast_For:
		case Ast_If:
		case Ast_Defer:
		case Ast_Block:
		case Ast_EmptyStatement:
		case Ast_Match:
		case Ast_Test:
			return false;
		default:
			return true;
	}
}

void parse_statement(Parser *parser, AstStatement *&result) {
	timed_function(compiler->profiler);

	result = 0;
	defer {
		if (result && result->kind != Ast_EmptyStatement) {
			scoped_lock(parser->current_scope);
			if (!result->parent_scope)
				parser->current_scope->add(result);
		}
	};

	if (!skip_newlines(parser)) {
		result = empty_statement;
		return;
	}
	defer {
		// NOTE: semicolon right after statement is considered part of that statement, e.g. `x = 12;`
		// A semicolon is considered an empty statement if no other statement immediately precedes it, e.g. `while true;` or it is put on a single line, e.g. `\n;\n`
		if (result) {
			if (may_be_terminated_by_semicolon(result)) {
				if (parser->token->kind == ';')
					parser->next();
			}
			// skip_newlines(parser);
		}
	};

	switch (parser->token->kind) {
		case Token_split_identifier:
		case Token_identifier: {
			auto location = parser->token->string;
			auto name = parse_identifier(parser);
			assert(!name.is_empty());

			parser->next();

			if (parser->token->kind == ':') {
				// Definition

				auto definition = parse_definition(name, location, parser);

				if (!definition) {
					return;
				}

				result = definition;
				return;
			} else {
				// Assignment or expression or something else
				--parser->token;
			}
			break;
		}
		case Token_return: {
			auto return_token = parser->token;

			if (!ensure_not_in_defer(parser, return_token->string))
				return;

			parser->next();

			auto ret = AstReturn::create();

			if (parser->token->kind != '\n' && parser->token->kind != ';') {
				auto expression = parse_expression(parser);
				if (!expression)
					return;
				ret->expression = expression;
				ret->location = {return_token->string.begin(), expression->location.end()};
			} else {
				ret->location = return_token->string;
			}

			if (parser->container_node->kind != Ast_Lambda) {
				parser->reporter->error(ret->location, "Return statement can only be used in lambda body.");
				return;
			}

			ret->lambda = (AstLambda *)parser->container_node;
			result = ret;
			return;
		}
		case Token_yield: {
			parser->reporter->error(parser->token->string, "Yielding is not implemented yet.");
			return;

			auto yield_token = parser->token;

			if (!ensure_not_in_defer(parser, yield_token->string))
				return;

			parser->next();

			auto yld = AstYield::create();

			if (parser->token->kind != '\n' && parser->token->kind != ';') {
				auto expression = parse_expression(parser);
				if (!expression)
					return;
				yld->expression = expression;
				yld->location = {yield_token->string.begin(), expression->location.end()};
			} else {
				yld->location = yield_token->string;
			}

			if (parser->container_node->kind != Ast_Lambda) {
				parser->reporter->error(yld->location, "Yield statement can only be used in lambda body.");
				return;
			}

			result = yld;
			return;
		}
		case Token_while: {
			auto While = AstWhile::create();
			While->location = parser->token->string;
			parser->next();
			auto condition = parse_expression(parser);
			if (!condition) {
				return;
			}
			While->condition = condition;

			if (parser->token->kind == Token_do)
				parser->next_solid();

			if (!parse_scope(parser, While->scope, {.allow_no_braces = true})) {
				return;
			}
			result = While;
			return;
		}
		case 'for': {
			auto For = AstFor::create();
			For->location = parser->token->string;
			parser->next_solid();

			if (parser->token->kind == '*') {
				For->by_pointer = true;
				parser->next_solid();
			}

			For->iterator_name = parse_identifier(parser);
			if (For->iterator_name.is_empty())
				return;

			parser->next_solid();

			if (!parser->expect('in'))
				return;

			parser->next_solid();

			For->range = parse_expression(parser);

			if (parser->token->kind == Token_do)
				parser->next_solid();

			if (!parse_scope(parser, For->scope, {.allow_no_braces = true})) {
				return;
			}

			result = For;
			return;
		}
		case Token_directive: {
			if (parser->token->string == "#assert"str) {
				auto assert = AstAssert::create();
				assert->location = parser->token->string;
				assert->is_constant = true;

				parser->next();

				assert->condition = parse_expression(parser);
				if (!assert->condition) {
					return;
				}

				if (parser->token->kind == ',') {
					parser->next_solid();
					if (!parser->expect(Token_string_literal)) {
						return;
					}

					auto unescaped = unescape_string(parser->token->string);
					if (!unescaped) {
						parser->reporter->error(parser->token->string, "Bad escape sequence. FIXME: show it exactly.");
						return;
					}

					assert->message = unescaped.value();

					parser->next();
				}

				assert->location = {assert->location.begin(), assert->condition->location.end()};

				result = assert;
				return;
			} else if (parser->token->string == "#print"str) {
				auto print = AstPrint::create();
				print->location = parser->token->string;
				parser->next();
				print->expression = parse_expression(parser);
				if (!print->expression)
					return;

				result = print;
				return;
			} else if (parser->token->string == "#parse"str) {
				auto parse = AstParse::create();
				parse->location = parser->token->string;
				parser->next();
				parse->expression = parse_expression(parser);
				if (!parse->expression)
					return;

				print_parsed_expression(parse->expression);
				print('\n');

				result = parse;
				return;
			//} else if (parser->token->string == "#builtin"str) {
			//	parser->next();
			//	auto statement = parse_statement(parser);
			//	if (!statement)
			//		return;

			//	if (statement->kind != Ast_Definition) {
			//		parser->reporter->error(statement->location, "Builtin must be a definition.");
			//		return;
			//	}
			//	auto definition = (AstDefinition *)statement;

			//	// definition->built_in = true;

			//	auto init_builtin = [&](BuiltinStruct &type, AstDefinition *definition) {
			//		assert(!type.Struct);
			//		assert(!type.ident);

			//		type.Struct = (AstStruct *)definition->expression;
			//		type.Struct->type = compiler->builtin_type.ident;
			//		type.Struct->definition = definition;

			//		type.ident = AstIdentifier::create();
			//		type.ident->location = definition->location;
			//		type.ident->name = definition->name;
			//		type.ident->possible_definitions.set(definition);
			//		type.ident->type = type.Struct->type;

			//		init_pointer(type);
			//	};

			//	if (definition->expression->kind != Ast_Struct) {
			//		parser->reporter->error(definition->location, "Invalid builtin.");
			//		return;
			//	}

			//	if (definition->name == "string") {
			//		init_builtin(compiler->builtin_string, definition);
			//	} else if (definition->name == "struct_member") {
			//		init_builtin(compiler->builtin_struct_member, definition);
			//	} else if (definition->name == "typeinfo") {
			//		init_builtin(compiler->builtin_typeinfo, definition);
			//	} else if (definition->name == "any") {
			//		init_builtin(compiler->builtin_any, definition);
			//	} else {
			//		parser->reporter->error(definition->location, "Unknown builtin.");
			//		return;
			//	}

			//	result = definition;
			//	return;
			} else if (parser->token->string == "#") {
				break;
			} else {
				break;
				//parser->reporter->error(parser->token->string, "Unknown statement level directive.");
				//return;
			}
			break;
		}
		case Token_defer: {
			auto Defer = AstDefer::create();
			Defer->location = parser->token->string;
			parser->next();
			if (!parse_scope(parser, Defer->scope, {.allow_no_braces = true})) {
				return;
			}

			result = Defer;
			return;
		}
		case Token_operator: {
			parser->next();

			switch (parser->token->kind) {
				case '+':
				case '-':
				case '*':
				case '/':
				case '%':
				case '^':
				case '&':
				case '|':
				case '&&':
				case '||':
				case '<<':
				case '>>':
				case '==':
				case '!=':
				case '>':
				case '<':
				case '>=':
				case '<=': {
					auto Operator = AstOperatorDefinition::create();
					Operator->location = parser->token->string;
					Operator->operation = parser->token->kind;

					if (!parser->next_expect(':'))
						return;
					if (!parser->next_expect(':'))
						return;
					parser->next();

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;
					if (lambda->kind != Ast_Lambda) {
						parser->reporter->error(lambda->location, "Expected a lambda expression.");
						return;
					}

					Operator->lambda = (AstLambda *)lambda;

					auto unop = as_unary_operation(Operator->operation);
					auto binop = as_binary_operation(Operator->operation);

					auto definition = AstDefinition::create();
					definition->expression = lambda;
					definition->is_constant = true;
					definition->location = Operator->location;

					if (Operator->lambda->parameters.count == 1) {
						if (!unop) {
							parser->reporter->error(Operator->lambda->location, "'{}' is not an unary operator.", Operator->location);
							return;
						}
						definition->name = format("operator{}"str, as_string(unop.value()));
						not_typechecked_unary_operators_count += 1;
					} else if (Operator->lambda->parameters.count == 2) {
						if (!binop) {
							parser->reporter->error(Operator->lambda->location, "'{}' is not a binary operator.", Operator->location);
							return;
						}
						definition->name = format("operator{}"str, as_string(binop.value()));
						not_typechecked_binary_operators_count += 1;
					} else {
						if (unop && binop)
							parser->reporter->error(Operator->lambda->location, "'{}' operator must have either one or two parameters.", Operator->location);
						else if (binop)
							parser->reporter->error(Operator->lambda->location, "'{}' operator must have exactly two parameters.", Operator->location);
						else if (unop)
							parser->reporter->error(Operator->lambda->location, "'{}' operator must have exactly one parameter.", Operator->location);
						return;
					}

					Operator->definition = definition;

					result = Operator;
					return;
				}
				case 'as': {
					parser->next();

					auto Operator = AstOperatorDefinition::create();
					Operator->location = parser->token->string;
					Operator->operation = 'as';
					switch (parser->token->kind) {
						case Token_implicit:
							not_typechecked_implicit_casts_count += 1;
							Operator->is_implicit = true;
							if (!parser->next_expect(':'))
								return;
							break;
						case Token_explicit:
							Operator->is_implicit = false;
							if (!parser->next_expect(':'))
								return;
							break;
						case ':':
							break;
						default:
							parser->reporter->error(parser->token->string, "Expected 'implicit', 'explicit' or ':'");
							return;
					}

					if (!parser->next_expect(':'))
						return;
					parser->next();

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;
					if (lambda->kind != Ast_Lambda) {
						parser->reporter->error(lambda->location, "Expected a lambda expression.");
						return;
					}

					Operator->lambda = (AstLambda *)lambda;

					if (Operator->lambda->parameters.count != 1) {
						parser->reporter->error(lambda->location, "'as' operator must have exactly one parameter.");
						return;
					}

					auto definition = AstDefinition::create();
					definition->expression = lambda;
					definition->is_constant = true;
					definition->location = Operator->location;
					definition->name = format("operator{}"str, as_string(as_binary_operation(Operator->operation).value()));

					Operator->definition = definition;

					result = Operator;
					return;
				}
				case '?': {
					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = '?';

					if (!parser->next_expect(':'))
						return;
					if (!parser->next_expect(':'))
						return;
					parser->next();

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;
					if (lambda->kind != Ast_Lambda) {
						parser->reporter->error(lambda->location, "Expected a lambda expression.");
						return;
					}

					definition->lambda = (AstLambda *)lambda;

					if (definition->lambda->parameters.count != 1) {
						parser->reporter->error(lambda->location, "'?' operator must have exactly one parameter.");
						return;
					}

					not_typechecked_has_value_overloads_count += 1;

					result = definition;
					return;
				}
				case 'for': {
					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = '?';

					if (!parser->next_expect(':'))
						return;
					if (!parser->next_expect(':'))
						return;
					parser->next();

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;

					parser->reporter->error(parser->token->string, "Overloading `for` operator is not implemented yet.");
					return;
				}
				default: {
					parser->reporter->error(parser->token->string, "Expected an overloadable operator.");
					return;
				}
			}

			break;
		}
		case Token_assert: {
			auto assert = AstAssert::create();
			assert->location = parser->token->string;
			assert->is_constant = false;
			parser->next();

			assert->condition = parse_expression(parser);
			if (!assert->condition)
				return;

			if (parser->token->kind == ',') {
				parser->next_solid();
				if (!parser->expect(Token_string_literal)) {
					return;
				}

				auto unescaped = unescape_string(parser->token->string);
				if (!unescaped) {
					parser->reporter->error(parser->token->string, "Bad escape sequence. FIXME: show it exactly.");
					return;
				}

				assert->message = unescaped.value();

				parser->next();
			}

			assert->location = {assert->location.begin(), assert->condition->location.end()};

			result = assert;
			return;
		}
		case Token_break:
		case Token_continue: {
			auto i = AstLoopControl::create();
			i->control = parser->token->kind == Token_break ? LoopControl::Break : LoopControl::Continue;
			i->location = parser->token->string;

			parser->next();

			if (parser->token->kind == Token_identifier) {
				not_implemented("labeled loop control statements are not implemented yet");
				auto ident = parse_identifier(parser);
				if (!ident.data)
					return;
				if (!parser->next_expect(';'))
					return;
				// i->label = ident;
			}

			result = i;
			return;
		}
		case Token_using: {
			auto Using = AstUsing::create();
			Using->location = parser->token->string;
			parser->next_solid();

			auto ident_token = parser->token;

			auto name = parse_identifier(parser);
			if (name.is_empty())
				return;

			parser->next();

			if (parser->token->kind == ':') {
				auto definition = parse_definition(name, ident_token->string, parser);
				if (!definition)
					return;

				definition->has_using = true;

				result = definition;
				return;
			} else {
				Using->expression = make_identifier(name);
				if (!Using->expression) {
					return;
				}

				Using->expression->location = ident_token->string;
			}


			result = Using;
			return;
		}
		// no need to put this in the current scope
		case ';': {
			result = empty_statement;
			parser->next();
			return;
		}
	}

	auto expression = parse_expression(parser);
	if (expression) {
		result = make_statement(expression);
		return;
	}

	parser->reporter->error(parser->token->string, "Failed to parse statement or expression.");
}
AstStatement *parse_statement(Parser *parser) {
	AstStatement *result;
	parse_statement(parser, result);
	return result;
}

AstStatement *parse_global_statement(Parser *parser) {
	timed_function(compiler->profiler);
	auto statement = parse_statement(parser);
	if (!statement) {
		return 0;
	}

	if (!can_be_global(statement)) {
		parser->reporter->error(statement->location, "This statement can not be global.");
		return 0;
	}

	return statement;
}

ParseResult parser_function(Parser *parser);


Lexer *failed_lexer;
Parser *failed_parser;

Map<String, SourceFileContext *> parsed_files;

u64 total_tokens_parsed;

String normalize_path(String path) {
	auto prev_allocator = current_allocator;

	List<List<utf8>> directories;
	scoped_allocator(temporary_allocator);

	auto s = path.begin();
	if (path.count >= 2 && path[1] == ':') {
		s += 3;
	}

	for (auto p = s; p < path.end(); ++p) {
		if (*p == '\\' || *p == '/') {
			auto dir = Span<utf8>{s, p};
			s = p + 1;

			if (dir.is_empty()) {
				continue;
			}

			if (dir == "..") {
				directories.pop();
				continue;
			}

			if (dir == ".") {
				continue;
			}

			directories.add(to_list(dir));
		}
	}

	StringBuilder builder;
	if (path.count >= 2 && path[1] == ':') {
		append(builder, path.subspan(0, 2));
		append(builder, '\\');
	}
	for (auto dir : directories) {
		append(builder, dir);
		append(builder, '\\');
	}
	append(builder, Span<utf8>{s, path.end()});
	return (String)to_string(builder, prev_allocator);
}

SourceFileContext *parse_file(String current_directory, String import_path, String import_location) {
	timed_function(compiler->profiler);

	String full_path = import_path;

	if (!is_absolute_path(full_path))
		full_path = concatenate(current_directory, "\\"str, import_path);

	if (!file_exists(full_path)) {
		full_path = concatenate(compiler->compiler_directory, "\\libs\\"str, import_path);
	}

	full_path = normalize_path(full_path);

	if (auto found = parsed_files.find(full_path)) {
		return found->value;
	}

	// print("Parsing {}\n", full_path);

	auto context =
	parsed_files.get_or_insert(full_path) =
	default_allocator.allocate<SourceFileContext>();

	context->lexer.source_buffer = read_entire_file(full_path, {.extra_space_before=1, .extra_space_after=1});
	if (!context->lexer.source_buffer.data) {
		immediate_error(import_location, "File '{}' does not exist or is not readable.", import_path);
		context->result = ParseResult::read_error;
		return context;
	}

	String source = String(context->lexer.source_buffer);
	source.data += 1;
	source.count -= 2;

	auto bom = Span(context->lexer.source_buffer.data + 1, (umm)3);
	if (bom.end() <= context->lexer.source_buffer.end() && bom == "\xef\xbb\xbf"b) {
		bom.back() = '\0';
		source.data += 3;
		source.count -= 3;
	}


	{
		auto s = source.begin();
		auto d = s;
		while (s < source.end()) {
			if (Span(s, 2) == "\r\n"str) {
				*d++ = '\n';
				s += 2;
			} else if (*s == '\r') {
				*d++ = '\n';
				s++;
			} else {
				*d++ = *s++;
			}
		}
		source.set_end(d);
		source.data[-1] = 0;
		*source.end() = 0;
	}

	context->lexer.source = source;

	// context->scope->parent = &compiler->global_scope;

	context->parser.lexer = &context->lexer;
	context->parser.reporter = context->lexer.reporter = &context->reporter;
	context->parser.current_directory = parse_path(full_path).directory;
	// context->parser.current_scope = &context->scope;

	auto source_info = &::compiler->sources.add({full_path, source});

	{
		utf8 *line_start = source.data;
		List<String> lines;
		lines.reserve(source.count / 16); // Guess 16 bytes per line on average

		for (auto c = source.begin(); c < source.end(); ++c) {
			if (*c == '\n') {
				lines.add({line_start, c});
				line_start = c + 1;
			}
		}
		lines.add({line_start, source.end()});


		source_info->lines = lines;
	}

	context->lexer.source_info = source_info;

	umm max_token_count = source.count;

	context->lexer.tokens_start = (Token *)VirtualAlloc(0, max_token_count * sizeof(Token), MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
	if (!context->lexer.tokens_start) {
		context->reporter.error("Not enough memory.");
		context->result = ParseResult::alloc_error;
		return context;
	}
	context->lexer.tokens_end = context->lexer.tokens_start + max_token_count;
	context->lexer.token_cursor = context->lexer.tokens_start;

	defer {
		context->reporter.print_all();
	};

	if (!lexer_function(&context->lexer)) {
		atomic_set_if_equals(failed_lexer, &context->lexer, (Lexer *)0);
		context->result = ParseResult::syntax_error;
		return context;
	}
	auto parse_result = parser_function(&context->parser);
	if (parse_result != ParseResult::ok) {
		atomic_set_if_equals(failed_parser, &context->parser, (Parser *)0);
		context->result = parse_result;
		return context;
	}

	atomic_add(&total_tokens_parsed, context->lexer.token_cursor - context->lexer.tokens_start);

	return context;
}

ParseResult parser_function(Parser *parser) {
	timed_function(compiler->profiler);

	auto lexer = parser->lexer;

	while (lexer->tokens_lexed() == 0 && !lexer->finished) {} // Wait for tokens

	if (lexer->tokens_lexed() == 0) {
		return ParseResult::ok;
	}

	parser->token = lexer->begin();
	while (parser->token->kind != 'eof') {
		if (!skip_newlines(parser)) {
			break;
		}

		if (parser->token->kind == Token_directive) {
			if (parser->token->string == "#extern_language"str) {
				if (!parser->next_expect(Token_string_literal)) {
					parser->reporter->error("Expected language name. Currently only \"C\" is available.");
					return ParseResult::syntax_error;
				}
				auto unescaped = unescape_string(parser->token->string);
				if (!unescaped) {
					parser->reporter->error(parser->token->string, "Bad escape sequence. FIXME: show it exactly.");
					return ParseResult::syntax_error;
				}
				parser->extern_language = unescaped.value_unchecked();
				if (parser->extern_language != "C"str) {
					parser->reporter->error(parser->token->string, "Only \"C\" is supported.");
					return ParseResult::syntax_error;
				}
				parser->next();
			} else if (parser->token->string == "#extern_library"str) {
				if (!parser->next_expect(Token_string_literal)) {
					parser->reporter->error("Expected library name.");
					return ParseResult::syntax_error;
				}
				auto unescaped = unescape_string(parser->token->string);
				if (!unescaped) {
					parser->reporter->error(parser->token->string, "Bad escape sequence. FIXME: show it exactly.");
					return ParseResult::syntax_error;
				}
				parser->extern_library = unescaped.value_unchecked();
				parser->next();
			} else if (parser->token->string == "#stdcall"str) {
				parser->current_convention = CallingConvention::stdcall;
				parser->next();
			} else if (parser->token->string == "#tlangcall"str) {
				parser->current_convention = CallingConvention::tlang;
				parser->next();
			} else if (parser->token->string == "#layout_c"str) {
				parser->current_struct_layout = StructLayout::c;
				parser->next();
			} else {
				goto _parse_global_statement;
			}
		} else if (parser->token->kind == Token_import) {
			if (!parser->next_expect(Token_string_literal)) {
				parser->reporter->error("Expected library path.");
				return ParseResult::syntax_error;
			}
			auto unescaped = unescape_string(parser->token->string);
			if (!unescaped) {
				parser->reporter->error(parser->token->string, "Bad escape sequence. FIXME: show it exactly.");
				return ParseResult::syntax_error;
			}
			auto libname = unescaped.value_unchecked();
			auto child = parse_file(parser->current_directory, libname, parser->token->string);
			// compiler->global_scope.append(child->scope);
			parser->next();
		} else {
		_parse_global_statement:
			auto statement = parse_global_statement(parser);
			if (!statement) {
				return ParseResult::syntax_error;
			}
		}
	}
	return ParseResult::ok;
}

umm append(StringBuilder &b, AstDefinition *definition) {
	if (definition == nullptr)  return append(b, "(null)");
	return append(b, definition->name);
}

u32 shared_progress = 0;

struct TypecheckState {
	TypecheckState() = default;
	TypecheckState(TypecheckState const &) = delete;

	// FIXME: These may be different nodes after typechecking.
	//        Currently changes are not propagated to the caller.
	AstStatement *root_statement = 0;
	AstExpression *root_expression = 0;

	Scope *root_scope = 0;

	AstNode *current_node = 0;
	AstLambda *lambda = 0;
	AstDefinition *definition = 0;
	AstExpression *waiting_for = 0;
	String waiting_for_name;
	AstExpression *container_node = 0;
	AstLambda *lambda_currently_accepting_poly_types = 0;

	AstDefinition *currently_typechecking_definition = 0;

	// AstLambda *current_lambda = 0;

	Scope *current_scope = 0;

	Reporter reporter;

	void *fiber = 0;
	void *parent_fiber = 0;

	AstWhile *current_loop = 0;

	struct TemplateCall {
		AstCall *call;
		AstLambda *template_lambda;
		AstLambda *hardened_lambda;
	};

	List<TemplateCall> template_call_stack;

	struct EvaluationParameter {
		AstDefinition *definition;
		AstLiteral *value;
	};

	List<EvaluationParameter> evaluation_parameters;

	bool replace_properties_with_getter_call = true;

	TypecheckResult result;

	// msvc crashed when i used `auto value`
	template <class T>
	AstLiteral *make_integer(T value, AstExpression *type, String location = {}) {
		auto x = ::make_integer(value, location);
		x->type = type;
		return x;
	}
	template <class T>
	AstLiteral *make_integer(T value, String location = {}) {
		return make_integer(value, compiler->builtin_unsized_integer.ident, location);
	}
	AstLiteral *make_null(AstExpression *type, String location = {}) {
		auto x = ::make_null(location);
		x->type = type;
		return x;
	}
	AstDefinition *make_retparam(AstExpression *type, AstLambda *parent) {
		auto retparam = ::make_retparam(type, parent);
		retparam->type = type;
		return retparam;
	}
	AstLiteral *make_bool(bool value, String location = {}) {
		auto x = ::make_bool(value, location);
		x->type = compiler->builtin_bool.ident;
		return x;
	}

};



thread_local TypecheckState *current_typecheck_state;

#undef push_scope
#define push_scope(scope) \
	auto CONCAT(old_scope, __LINE__) = state->current_scope; \
	state->current_scope = scope; \
	defer { state->current_scope = CONCAT(old_scope, __LINE__); };



List<TypecheckState *> typecheck_states_pool;

void WINAPI typecheck_pooled(void *_state);

TypecheckState *get_pooled_typecheck_state(AstExpression *container_node, Scope *current_scope, void *parent_fiber) {
	TypecheckState *state = 0;
	if (typecheck_states_pool.count) {
		state = typecheck_states_pool.pop().value();
		state->currently_typechecking_definition = 0;
		state->current_loop = 0;
		state->current_node = 0;
		state->definition = 0;
		state->evaluation_parameters.clear();
		state->lambda = 0;
		state->lambda_currently_accepting_poly_types = 0;
		state->replace_properties_with_getter_call = true;
		state->reporter.reports.clear();
		state->root_expression = 0;
		state->root_scope = 0;
		state->root_statement = 0;
		state->template_call_stack.clear();
		state->waiting_for = 0;
		state->waiting_for_name = {};
	} else {
		state = default_allocator.allocate<TypecheckState>();
		state->fiber = CreateFiber(4096, typecheck_pooled, state);
		if (!state->fiber) {
			invalid_code_path(state->root_statement ? state->root_statement->location : state->root_expression ? state->root_expression->location : state->root_scope->node->location, "INTERNAL COMPILER ERROR: Failed to create fiber");
		}
	}
	state->parent_fiber = parent_fiber;
	state->current_scope = current_scope;
	state->container_node = container_node;
	return state;
}
TypecheckState *get_pooled_typecheck_state(AstStatement *root_statement, AstExpression *container_node, Scope *current_scope, void *parent_fiber) {
	auto state = get_pooled_typecheck_state(container_node, current_scope, parent_fiber);
	state->root_statement = root_statement;
	return state;
}
TypecheckState *get_pooled_typecheck_state(AstExpression *root_expression, AstExpression *container_node, Scope *current_scope, void *parent_fiber) {
	auto state = get_pooled_typecheck_state(container_node, current_scope, parent_fiber);
	state->root_expression = root_expression;
	return state;
}
TypecheckState *get_pooled_typecheck_state(Scope *root_scope, AstExpression *container_node, Scope *current_scope, void *parent_fiber) {
	auto state = get_pooled_typecheck_state(container_node, current_scope, parent_fiber);
	state->root_scope = root_scope;
	return state;
}
TypecheckState *get_pooled_typecheck_state(AstStatement *root_statement, TypecheckState *parent) {
	return get_pooled_typecheck_state(root_statement, parent->container_node, parent->current_scope, parent->fiber);
}
TypecheckState *get_pooled_typecheck_state(AstExpression *root_expression, TypecheckState *parent) {
	return get_pooled_typecheck_state(root_expression, parent->container_node, parent->current_scope, parent->fiber);
}
TypecheckState *get_pooled_typecheck_state(Scope *root_scope, TypecheckState *parent) {
	return get_pooled_typecheck_state(root_scope, parent->container_node, parent->current_scope, parent->fiber);
}

void pre_yield(TypecheckState *state, TypecheckResult result) {
	fiber_result = result;

	if (result != TypecheckResult::wait) {
		typecheck_states_pool.add(state);
	}

	if (result == TypecheckResult::fail) {
		int x = 5;
	}
	if (result == TypecheckResult::wait) {
		++yield_wait_count;
	}
}

void post_yield(TypecheckState *state, TypecheckResult result) {
	current_typecheck_state = state;
	if (result == TypecheckResult::fail) {
		throwing = true;
		throw 0;
	}
}

struct IntegerInfo {
	AstStruct *type;
	s64 bits;
};

IntegerInfo integer_infos[8];

bool ensure_fits(Reporter *reporter, AstExpression *expression, BigInteger integer, IntegerInfo info) {
	auto top_bits = integer >> info.bits;
	defer { free(top_bits); };
	if (top_bits == 0 || top_bits == -1)
		return true;
	if (reporter) {
		auto name = type_name(info.type);
		reporter->error(expression->location, "{} is not implicitly convertible to {} because some bits would be lost. You can explicitly write `({}) as {}` or `@({})` to perform lossy conversion.", integer, name, expression->location, name, expression->location);
	}
	return false;
}

void for_each_top_scope(AstExpression *expression, auto &&fn) {
	switch (expression->kind) {
		case Ast_Block: {
			auto Block = (AstBlock *)expression;
			fn(Block->scope);
			break;
		}
		case Ast_If: {
			auto If = (AstIf *)expression;
			fn(If->true_block->scope);
			fn(If->false_block->scope);
			break;
		}
		case Ast_Lambda: {
			auto Lambda = (AstLambda *)expression;
			fn(Lambda->constant_scope);
			break;
		}
		case Ast_Struct: {
			auto Struct = (AstStruct *)expression;
			fn(Struct->parameter_scope);
			break;
		}
		case Ast_Test: {
			auto Test = (AstTest *)expression;
			fn(Test->scope);
			break;
		}
		case Ast_Enum: {
			auto Enum = (AstEnum *)expression;
			fn(Enum->scope);
			break;
		}
		case Ast_Match: {
			auto Match = (AstMatch *)expression;
			for (auto &Case : Match->cases)
				fn(Case.block->scope);
			break;
		}
	}
}
void for_each_top_scope(AstStatement *statement, auto &&fn) {
	switch (statement->kind) {
		case Ast_ExpressionStatement: {
			auto ExpressionStatement = (AstExpressionStatement *)statement;
			for_each_top_scope(ExpressionStatement->expression, fn);
			break;
		}
		case Ast_Definition: {
			auto Definition = (AstDefinition *)statement;
			if (Definition->expression)
				for_each_top_scope(Definition->expression, fn);
			break;
		}
		case Ast_While: {
			auto While = (AstWhile *)statement;
			fn(While->scope);
			break;
		}
		case Ast_For: {
			auto For = (AstFor *)statement;
			fn(For->scope);
			break;
		}
		case Ast_Defer: {
			auto Defer = (AstDefer *)statement;
			fn(Defer->scope);
			break;
		}
	}
}

AstLiteral *make_type_literal(AstExpression *type) {
	timed_function(compiler->profiler);
	auto result = AstLiteral::create();
	result->literal_kind = LiteralKind::type;
	result->type_value = type;
	result->type = compiler->builtin_type.ident;
	return result;
}

void ensure_definition_is_resolved(TypecheckState *state, AstIdentifier *identifier) {
	if (!identifier->definition()) {
		if (identifier->possible_definitions.count) {
			state->reporter.error(identifier->location, "Multiple definitions with this name");
			for (auto definition : identifier->possible_definitions) {
				state->reporter.info(definition->location, "Here:");
			}
			yield(TypecheckResult::fail);
		} else {
			state->reporter.error(identifier->location, "Undeclared identifier");
			// TODO: is this even reachable??
			invalid_code_path();
			yield(TypecheckResult::fail);
		}
	}
}

s64 get_size(TypecheckState *state, AstExpression *type);
s64 get_align(TypecheckState *state, AstExpression *type);

u32 put_in_section(TypecheckState *state, AstLiteral *, Section &, AstExpression * = 0);

void put_arrays_in_section(TypecheckState *state, AstLiteral *literal, Section &section) {
	switch (literal->literal_kind) {
		using enum LiteralKind;
		case null:
		case boolean:
		case character:
		case integer:
		case Float:
		case pointer:
		case string: // strings are put in here at creation time
			break;
		case array: {
			for (auto val : literal->array_elements)
				put_arrays_in_section(state, val, section);

			literal->array_offset = section.buffer.count;
			for (auto val : literal->array_elements)
				put_in_section(state, val, section);

			break;
		}
		case Struct: {
			for (auto val : literal->struct_values)
				put_arrays_in_section(state, val, section);

			for (auto val : literal->struct_values)
				put_arrays_in_section(state, val, section);
			break;
		}
		default:
			invalid_code_path();
	}
}
u32 put_in_section(TypecheckState *state, AstLiteral *literal, Section &section, AstExpression *target_type) {
	if (!target_type)
		target_type = literal->type;

	switch (literal->literal_kind) {
		using enum LiteralKind;
		case null: {
			auto align = get_align(literal->type);
			auto size  = get_size(literal->type);

			assert(align);
			assert(size);

			section.align(align);
			auto result = section.buffer.count;
			for (umm i=0;i<size;++i) {
				section.w1(0);
			}
			return result;
		}
		case integer: {
			auto size = get_size(literal->type);
			assert(size > 0);
			section.align(size);

			if (::is_integer_internally(literal->type)) {
				switch (size) {
					case 1: return section.w1((u64)literal->integer);
					case 2: return section.w2((u64)literal->integer);
					case 4: return section.w4((u64)literal->integer);
					case 8: return section.w8((u64)literal->integer);
					default:invalid_code_path();
				}
			} else if (::is_float(literal->type)) {
				switch (size) {
					case 4: return section.w4(std::bit_cast<u32>((f32)literal->integer));
					case 8: return section.w8(std::bit_cast<u64>((f64)literal->integer));
					default:invalid_code_path();
				}
			} else {
				invalid_code_path();
			}
			break;
		}
		case Float: {
			auto size = get_size(literal->type);
			assert(size > 0);
			section.align(size);

			if (::is_integer_internally(literal->type)) {
				invalid_code_path();
			} else if (::is_float(literal->type)) {
				switch (size) {
					case 4: return section.w4(std::bit_cast<u32>((f32)literal->Float));
					case 8: return section.w8(std::bit_cast<u64>((f64)literal->Float));
					default:invalid_code_path();
				}
			} else {
				invalid_code_path();
			}
			break;
		}
		case boolean:
			return section.w1(literal->Bool);
		case string: {
			section.align(8);
			auto result =
			section.w8((u64)literal->string.offset);
			section.w8((u64)literal->string.count);
			section.relocations.add(Relocation{
				.section = compiler->kind_of(section),
				.offset = result,
			});
			return result;
		}
		case character:
			return section.w1((u64)literal->character);
		case noinit:
			break;
		case pointer: {
			section.align(8);
			auto result =
			section.w8((u64)literal->pointer.offset);
			section.relocations.add(Relocation{
				.section = literal->pointer.section,
				.offset = result,
			});
			return result;
		}
		case Struct: {
			auto alignment = get_align(state, literal->type);
			assert(alignment);
			section.align(alignment);

			// assert(literal->struct_offset == 0);
			literal->struct_offset = section.buffer.count;
			auto result = section.buffer.count;

			for (umm i = 0; i < literal->struct_values.count; ++i) {
				auto member = literal->struct_values[i];
				auto target_member_type = direct_as<AstStruct>(literal->type)->data_members[i]->type;
				put_in_section(state, member, section, target_member_type);
			}

			return result;
		}
		case array: {
			if (as<AstArray>(target_type)) {
				// Array elements are already written, just return their offset.
				return literal->array_offset;
			} else {
				// Make a span for written array.
				assert(literal->array_offset != -1);
				section.align(8);
				auto result =
				section.w8((u64)literal->array_offset);
				section.w8((u64)literal->array_elements.count);
				section.relocations.add(Relocation{
					.section = compiler->kind_of(section),
					.offset = result,
				});
				return result;
			}
		}
		case type:
			break;
		case lambda_name:
			break;
		default:
			break;
	}
	invalid_code_path();
}

bool are_equal(AstLiteral *l, AstLiteral *r) {
	assert(l->literal_kind == r->literal_kind);
	switch (l->literal_kind) {
		case LiteralKind::integer:   return l->integer == r->integer;
		case LiteralKind::Float:     return l->Float == r->Float;
		case LiteralKind::boolean:   return l->Bool == r->Bool;
		case LiteralKind::string:    return l->string.get() == r->string.get();
		case LiteralKind::character: return l->character == r->character;
		case LiteralKind::type:      return types_match(l->type_value, r->type_value);
	}
	invalid_code_path();
}

AstLiteral *evaluate(TypecheckState *state, AstExpression *expression);
AstLiteral *evaluate(TypecheckState *state, AstDefinition *definition) {
	if (auto param = find_if(state->evaluation_parameters, [&] (auto param) { return param.definition == definition; })) {
		return param->value;
	} else {
		if (!definition->is_constant) {
			state->reporter.error(definition->location, "Can't evaluate expression at compile time: definition is not constant");
			return 0;
		}

		if (definition->evaluated) {
			return definition->evaluated;
		}

		return definition->evaluated = evaluate(state, definition->expression);
	}
}

void evaluate(TypecheckState *state, AstExpression *expression, AstLiteral *&result);
AstLiteral *evaluate(TypecheckState *state, AstExpression *expression) {
	AstLiteral *result = 0;
	evaluate(state, expression, result);
	return result;
}
void evaluate(TypecheckState *state, AstExpression *expression, AstLiteral *&result) {
	timed_function(compiler->profiler);

	result = 0;
	defer {
		if (result) {
			result->type = expression->type;
		}
	};

	//MakeLiteralParams literal_params = {.location = expression->location, .type = expression->type};

	if (expression->type && is_type(expression)) {
		// FIXME: this is supposed to happen with actual types, not with things like `Type()`.
		result = make_type_literal(expression);
		return;
	}

	switch (expression->kind) {
		case Ast_Literal: {
			result = (AstLiteral *)expression;
			return;
		}
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)expression;

			ensure_definition_is_resolved(state, ident);

			result = evaluate(state, ident->definition());
			return;
		}
		case Ast_BinaryOperator: {
			using enum BinaryOperation;
			auto bin = (AstBinaryOperator *)expression;

			switch (bin->operation) {
				case dot: {
					result = evaluate(state, bin->right);
					return;
				}
			}

			auto l = evaluate(state, bin->left);
			if (!l) return;
			auto r = evaluate(state, bin->right);
			if (!r) return;
			switch (bin->operation) {
				case add: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: result = make_integer(l->integer + r->integer); return;
						case Float:   result = make_float  (l->Float   + r->Float  ); return;
						default: invalid_code_path();
					}
				}
				case sub: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: result = make_integer(l->integer - r->integer); return;
						case Float:   result = make_float  (l->Float   - r->Float  ); return;
						default: invalid_code_path();
					}
				}
				case mul: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: result = make_integer(l->integer * r->integer); return;
						case Float:   result = make_float  (l->Float   * r->Float  ); return;
						default: invalid_code_path();
					}
				}
				case div: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: result = make_integer(l->integer / r->integer); return;
						case Float:   result = make_float  (l->Float   / r->Float  ); return;
						default: invalid_code_path();
					}
				}
				case eq: {
					result = make_bool(are_equal(l, r));
					return;
				}
				case ne: {
					result = make_bool(!are_equal(l, r));
					return;
				}
				// TODO: FIXME: should evaluation of right expression be done after we check left value?
				case lor: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case boolean: result = make_bool(l->Bool || r->Bool); return;
						default: invalid_code_path();
					}
				}
				// TODO: FIXME: should evaluation of right expression be done after we check left value?
				case land: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case boolean: result = make_bool(l->Bool && r->Bool); return;
						default: invalid_code_path();
					}
				}
				default:
					invalid_code_path();
					return;
			}
		}
		case Ast_Struct: {
			result = make_type_literal(expression);
			return;
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;

			switch (unop->operation) {
				using enum UnaryOperation;
				case lnot: {
					auto child = evaluate(state, unop->expression);
					if (!child)
						return;
					assert(child->literal_kind == LiteralKind::boolean);
					result = make_bool(!child->Bool);
					return;
				}
				case bnot: {
					auto child = evaluate(state, unop->expression);
					if (!child)
						return;
					switch (child->literal_kind) {
						case LiteralKind::boolean: result = make_bool(!child->Bool); return;
						case LiteralKind::integer: result = make_integer(~child->integer); return;
						default: invalid_code_path();
					}
				}
				case typeof:
					result = make_type_literal(unop->expression->type);
					return;
				case address_of: {
					auto l = AstLiteral::create();
					l->literal_kind = LiteralKind::pointer;

					auto identifier = as<AstIdentifier>(unop->expression);
					assert(identifier);

					auto definition = identifier->definition();
					assert(definition);

					if (definition->is_constant) {
						l->pointer.section = SectionKind::data_readonly;
					} else {
						if (definition->expression) {
							l->pointer.section = SectionKind::data_readwrite;
						} else if (auto Struct = direct_as<AstStruct>(definition->type); Struct && Struct->default_value_offset != -1) {
							l->pointer.section = SectionKind::data_readwrite;
						} else {
							l->pointer.section = SectionKind::data_zero;
						}
					}

					l->pointer.offset = definition->offset;
					result = l;
					return;
				}
				case move_to_temporary: {
					state->reporter.error(expression->location, "FIXME: This expression can't be evaluated at compile time.");
					return;
				}
				default: {
					/*
					// TODO: FIXME: wtf is this?
					if (types_match(expression->type, compiler->builtin_type)) {
						auto child = evaluate(state, unop->expression);
						if (!child)
							return;

						if (child->literal_kind == LiteralKind::type) {
							result = make_type_literal(make_pointer_type(child->type_value));
							return;
						}
					}
					*/
					invalid_code_path();
				}
			}

			break;
		}
		case Ast_Call: {
			// TODO: this is very limited right now
			auto call = (AstCall *)expression;
			if (auto Struct = direct_as<AstStruct>(call->callable)) {
				result = AstLiteral::create();
				result->literal_kind = LiteralKind::Struct;
				result->struct_values = {};
				result->struct_values.resize(Struct->data_members.count);
				for (umm i = 0; i < Struct->data_members.count; ++i) {
					result->struct_values[i] = evaluate(state, call->sorted_arguments[i]);
					if (!result->struct_values[i])
						return;
				}
				result->type = Struct;
				return;
			} else {
				not_implemented();
				/*
				auto lambda = get_lambda(call->callable);

				assert(call->sorted_arguments.count == lambda->parameters.count);

				List<TypecheckState::EvaluationParameter> evaluation_parameters;
				defer { free(evaluation_parameters); };

				for (umm i = 0; i < call->sorted_arguments.count; ++i) {
					auto value = evaluate(state, call->sorted_arguments[i]);
					if (!value)
						return;
					evaluation_parameters.add({.definition = lambda->parameters[i], .value = value});
				}

				scoped_replace(state->evaluation_parameters, evaluation_parameters);

				result = evaluate(state, lambda->body);
				return;
				*/
			}
		}
		case Ast_Lambda: {
			auto lambda = (AstLambda *)expression;
			assert(!lambda->body);
			assert(lambda->is_type);
			result = make_type_literal(lambda);
			return;
		}
		case Ast_Array: {
			auto array = (AstArray *)expression;
			result = make_type_literal(array);
			return;
		}
		case Ast_ArrayInitializer: {
			auto array = (AstArrayInitializer *)expression;
			auto literal = AstLiteral::create();
			literal->literal_kind = LiteralKind::array;
			literal->array_elements = {};
			literal->array_elements.resize(array->elements.count);
			for (umm i = 0; i < array->elements.count; ++i) {
				literal->array_elements[i] = evaluate(state, array->elements[i]);
			}
			literal->type = array->type;
			result = literal;
			return;
		}
		default:
			invalid_code_path();
	}
	return;
}

bool do_all_paths_explicitly_return(AstLambda *lambda, SmallList<AstStatement *> statements) {
	if (statements.count == 0)
		return false;

	if (auto est = as<AstExpressionStatement>(statements.back()))
		if (types_match(lambda->return_parameter->type, est->expression->type))
			return true;

	for (auto statement : statements) {
		switch (statement->kind) {
			case Ast_Return:
				return true;
			case Ast_ExpressionStatement: {
				auto expression = ((AstExpressionStatement *)statement)->expression;
				switch (expression->kind) {
					case Ast_BinaryOperator: {
						auto bin = (AstBinaryOperator *)expression;
						if (bin->operation == BinaryOperation::ass) {
							if (bin->left->kind == Ast_Identifier) {
								auto ident = (AstIdentifier *)bin->left;
								if (ident->definition() == lambda->return_parameter) {
									return true;
								}
							}
						}
						break;
					}
					case Ast_If: {
						auto If = (AstIf *)expression;

						if (do_all_paths_explicitly_return(lambda, If->true_block->scope->statement_list) &&
							do_all_paths_explicitly_return(lambda, If->false_block->scope->statement_list)
						)
							return true;
						break;
					}
					case Ast_Block: {
						auto Block = (AstBlock *)expression;
						if (do_all_paths_explicitly_return(lambda, Block->scope->statement_list))
							return true;
						break;
					}
				}
				break;
			}
		}
	}
	return false;
}
bool do_all_paths_explicitly_return(AstLambda *lambda) {
	if (types_match(lambda->return_parameter->type, compiler->builtin_void) ||
		types_match(lambda->return_parameter->type, compiler->builtin_unreachable)
	)
		return true;

	//return do_all_paths_explicitly_return(lambda, lambda->body_scope->statement_list);

	if (auto block = as<AstBlock>(lambda->body)) {
		return do_all_paths_explicitly_return(lambda, block->scope->statement_list);
	}
	return true;
}

void typecheck(TypecheckState *state, CExpression auto &expression);

AstStatement           *deep_copy(AstStatement           *);
AstDefinition          *deep_copy(AstDefinition          *);
AstDefer               *deep_copy(AstDefer               *);
AstAssert              *deep_copy(AstAssert              *);
AstBlock               *deep_copy(AstBlock               *);
AstExpressionStatement *deep_copy(AstExpressionStatement *);
AstIf                  *deep_copy(AstIf                  *);
AstOperatorDefinition  *deep_copy(AstOperatorDefinition  *);
AstParse               *deep_copy(AstParse               *);
AstPrint               *deep_copy(AstPrint               *);
AstReturn              *deep_copy(AstReturn              *);
AstWhile               *deep_copy(AstWhile               *);
AstFor                 *deep_copy(AstFor                 *);
AstUsing               *deep_copy(AstUsing               *);
AstExpression     *deep_copy(AstExpression     *);
AstIdentifier     *deep_copy(AstIdentifier     *);
AstCall           *deep_copy(AstCall           *);
AstLiteral        *deep_copy(AstLiteral        *);
AstLambda         *deep_copy(AstLambda         *);
AstBinaryOperator *deep_copy(AstBinaryOperator *);
AstUnaryOperator  *deep_copy(AstUnaryOperator  *);
AstStruct         *deep_copy(AstStruct         *);
AstSubscript      *deep_copy(AstSubscript      *);
AstArray          *deep_copy(AstArray          *);
AstTest           *deep_copy(AstTest           *);
AstSpan           *deep_copy(AstSpan           *);

struct CastType {
	AstStruct *from;
	AstStruct *to;
	//CastKind kind;
	bool implicit;
};


bool operator==(CastType a, CastType b) {
	return a.from == b.from && a.to == b.to;
}

LinearSet<CastType> built_in_casts;

AstBinaryOperator *make_cast(AstExpression *expression, AstExpression *type) {
	auto result = AstBinaryOperator::create();
	result->operation = BinaryOperation::as;
	result->left = expression;
	result->right = result->type = type;
	result->location = expression->location;
	return result;
}

AstExpression *make_cast_if_types_dont_match(AstExpression *expression, AstExpression *type) {
	if (types_match(expression->type, type))
		return expression;

	return make_cast(expression, type);
}

bool ensure_addressable(Reporter *reporter, AstExpression *expression) {
	timed_function(compiler->profiler);
	switch (expression->kind) {
		case Ast_Identifier: {
			return true;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != BinaryOperation::dot)
				break;

			return ensure_addressable(reporter, binop->right);
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;

			return ensure_addressable(reporter, subscript->expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			if (unop->operation == UnaryOperation::dereference)
				return true;
			break;
		}
	}

	reporter->error(expression->location, "Expression is not addressable.");
	return false;
}

bool is_poly(AstExpression *type) {
	return types_match(type, compiler->builtin_poly) || (type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)type)->operation == UnaryOperation::poly);
}

// NOTE: returns a copy of the value if it was found.
AstLiteral *find_enum_value(TypecheckState *state, AstEnum *Enum, KeyString name, String location) {
	auto found_member = Enum->scope->definition_map.find(name);
	if (!found_member) {
		state->reporter.error(location, "Enum '{}' does not contain constant '{}'", Enum->definition->name, name);
		yield(TypecheckResult::fail);
	}

	assert(found_member->value.count);

	if (found_member->value.count > 1) {
		state->reporter.error(location, "Ambiguous name: {}", found_member->value[0]->name);
		yield(TypecheckResult::fail);
	}

	auto member = found_member->value.data[0];

	auto ident = AstIdentifier::create();
	ident->location = Enum->location;
	ident->type = Enum->definition->type;
	ident->name = Enum->definition->name;
	ident->possible_definitions.set(Enum->definition);

	auto result = deep_copy(evaluate(state, member->expression));
	result->type = ident;
	result->location = location;
	return result;
	//return state->make_integer(copy(get_constant_integer(member->expression).value()), ident, location);
}

bool wait_for(TypecheckState *state, String location, auto message, auto &&predicate, auto &&error_callback, bool yield_failure = true) {
	while (1) {
		if (predicate()) {
			atomic_add(&shared_progress, 1);
			return true;
		}

		auto prev_shared_progress = shared_progress;

		if (compiler->print_yields) {
			immediate_info(location, "Waiting for {}", message);
		}

		yield(TypecheckResult::wait);

		if (prev_shared_progress == shared_progress) {
			error_callback();
			if (yield_failure)
				yield(TypecheckResult::fail);
			return false;
		}
	}
}

s64 get_size(TypecheckState *state, AstExpression *type) {
	s64 result;
	wait_for(state, type->location, "size"str,
		[&] {
			result = get_size(type, false);
			return result != -1;
		}, [&] {
			state->reporter.error(type->location, "{} does not have known size yet and can't be used here.", type_to_string(type));
		}
	);
	assert(result > 0);
	return result;
}
s64 get_align(TypecheckState *state, AstExpression *type) {
	s64 result;
	wait_for(state, type->location, "align"str,
		[&] {
			result = get_align(type, false);
			return result != -1;
		}, [&] {
			state->reporter.error(type->location, "Failed to compute alignment.");
		}
	);
	assert(result > 0);
	return result;
}


auto evaluate_and_put_in_section(TypecheckState *state, AstExpression *expression, AstExpression *target_type, Section &section) {
	struct {
		AstLiteral *evaluated;
		u32 offset;
	} result;

	result.evaluated = evaluate(state, expression);
	put_arrays_in_section(state, result.evaluated, section);
	result.offset = put_in_section(state, result.evaluated, section, target_type);
	return result;
}

void evaluate_and_put_definition_in_section(TypecheckState *state, AstDefinition *definition, Section &section) {
	auto x = evaluate_and_put_in_section(state, definition->expression, definition->type, section);
	definition->evaluated = x.evaluated;
	definition->offset = x.offset;
}

enum class TypeKind {
	Void,
	Bool,
	U8,
	U16,
	U32,
	U64,
	S8,
	S16,
	S32,
	S64,
	F32,
	F64,
	Struct,
	Enum,
	Pointer,
	Span,
	Array,
	Option,
};

TypeKind get_type_kind(AstExpression *type) {
	using enum TypeKind;
	if (types_match(type, compiler->builtin_void)) return Void;
	if (types_match(type, compiler->builtin_bool)) return Bool;
	if (types_match(type, compiler->builtin_u8))   return U8;
	if (types_match(type, compiler->builtin_u16))  return U16;
	if (types_match(type, compiler->builtin_u32))  return U32;
	if (types_match(type, compiler->builtin_u64))  return U64;
	if (types_match(type, compiler->builtin_s8))   return S8;
	if (types_match(type, compiler->builtin_s16))  return S16;
	if (types_match(type, compiler->builtin_s32))  return S32;
	if (types_match(type, compiler->builtin_s64))  return S64;
	if (types_match(type, compiler->builtin_f32))  return F32;
	if (types_match(type, compiler->builtin_f64))  return F64;
	if (auto Struct = direct_as<AstStruct>(type)) {
		if (Struct->is_span)
			return Span;
		return TypeKind::Struct;
	}
	if (direct_as<AstEnum>(type))        return Enum;
	if (auto unop = direct_as<AstUnaryOperator>(type)) {
		switch (unop->operation) {
			using enum UnaryOperation;
			case pointer: return Pointer;
			case pack: return Span;
			case option: return Option;
		}
	}
	if (direct_as<AstSpan>(type)) return Span;
	if (direct_as<AstArray>(type)) return Array;
	if (direct_as<AstLambdaType>(type)) return Pointer;

	invalid_code_path();
}

AstCall *make_struct_initializer(AstExpression *type, Span<AstExpression *> arguments, String location) {
	auto Struct = direct_as<AstStruct>(type);
	assert(Struct);

	auto initializer = AstCall::create();
	initializer->callable = type;
	initializer->sorted_arguments.resize(Struct->data_members.count);
	initializer->type = type;
	initializer->location = location;

	assert(arguments.count == Struct->data_members.count);

	for (umm i = 0; i < arguments.count; ++i) {
		initializer->sorted_arguments[i] = arguments.data[i];
		assert(types_match(arguments.data[i]->type, Struct->data_members[i]->type));
	}

	return initializer;
}
AstCall *make_struct_initializer(AstExpression *type, std::initializer_list<AstExpression *> arguments, String location) {
	return make_struct_initializer(type, {arguments.begin(), arguments.end()}, location);
}

struct TypeinfoDefinition {
	AstDefinition *definition;
	AstExpression *type;
};

List<TypeinfoDefinition> typeinfo_definitinos;
AstUnaryOperator *get_typeinfo(TypecheckState *state, AstExpression *type, String location) {
	wait_for(state, location, "typeinfo", [&] { return type->type; }, [&]{});

	auto directed = direct(type);

	auto found = find_if(typeinfo_definitinos, [&](auto definition) { return types_match(directed, definition.type); });
	AstDefinition *definition = 0;
	if (found) {
		definition = found->definition;
	} else {
		auto name = type_name(directed);

		definition = AstDefinition::create();
		definition->is_constant = true;
		definition->location = location;
		definition->name = format(u8"\\typeinfo_{}", name);
		definition->type = compiler->builtin_typeinfo.ident;

		typeinfo_definitinos.add({definition, directed});

		compiler->global_scope.add(definition);


		auto create_initializer = [&](BuiltinStruct &type) {
			auto initializer = AstCall::create();
			initializer->callable = type.ident;
			initializer->sorted_arguments.resize(type.Struct->data_members.count);
			initializer->type = type.ident;
			initializer->location = location;
			return initializer;
		};
		auto set_member = [&] (auto call, auto name, AstExpression *expression) {
			auto &members = direct_as<AstStruct>(call->type)->data_members;
			auto member = find_if(members, [&](auto member){ return member->name == name; });
			assert(member);

			if (auto literal = as<AstLiteral>(expression))
				literal->type = (*member)->type;

			auto member_index = index_of(members, member);
			if (expression == 0)
				expression = state->make_null((*member)->type, call->location);
			call->sorted_arguments[member_index] = expression;

			//if (auto ArrayInitializer = as<AstArrayInitializer>(expression)) {
			//	ArrayInitializer->type = make_subscript(get_span_subtype((*member)->type), make_integer((u64)ArrayInitializer->elements.count));
			//}
		};

		auto typeinfo_initializer = create_initializer(compiler->builtin_typeinfo);
		set_member(typeinfo_initializer, "kind", make_integer((s64)get_type_kind(directed), location));
		set_member(typeinfo_initializer, "name", make_string(name, location));

		if (directed->kind == Ast_Struct) {
			auto Struct = (AstStruct *)directed;

			if (!types_match(Struct, compiler->builtin_void) &&
				!types_match(Struct, compiler->builtin_bool) &&
				!types_match(Struct, compiler->builtin_u8) &&
				!types_match(Struct, compiler->builtin_u16) &&
				!types_match(Struct, compiler->builtin_u32) &&
				!types_match(Struct, compiler->builtin_u64) &&
				!types_match(Struct, compiler->builtin_s8) &&
				!types_match(Struct, compiler->builtin_s16) &&
				!types_match(Struct, compiler->builtin_s32) &&
				!types_match(Struct, compiler->builtin_s64) &&
				!types_match(Struct, compiler->builtin_f32) &&
				!types_match(Struct, compiler->builtin_f64))
			{
				auto member_list = AstArrayInitializer::create();
				member_list->elements.resize(Struct->data_members.count);
				for (umm i = 0; i < Struct->data_members.count; ++i) {
					auto &d = member_list->elements[i];
					auto &s = Struct->data_members[i];

					auto member_initializer = create_initializer(compiler->builtin_struct_member);
					set_member(member_initializer, "name", make_string(s->name, location));
					set_member(member_initializer, "type", get_typeinfo(state, s->type, location));
					set_member(member_initializer, "offset", make_integer((u64)s->offset, location));

					d = member_initializer;
				}
				set_member(typeinfo_initializer, "members", member_list);

				auto param_list = AstArrayInitializer::create();
				param_list->elements.resize(Struct->parameter_scope->definition_list.count);
				for (umm i = 0; i < Struct->parameter_scope->definition_list.count; ++i) {
					auto &d = param_list->elements[i];
					auto &s = Struct->parameter_scope->definition_list[i];
					d = get_typeinfo(state, s->expression, location);
				}
				set_member(typeinfo_initializer, "parameters", param_list);
			}
		} else if (directed->kind == Ast_Enum) {
			auto Enum = (AstEnum *)directed;

			auto member_list = AstArrayInitializer::create();
			member_list->elements.resize(Enum->scope->statement_list.count);
			for (umm i = 0; i < Enum->scope->statement_list.count; ++i) {
				auto &d = member_list->elements[i];
				auto &s = (AstDefinition *&)Enum->scope->statement_list[i];
				assert(s->kind == Ast_Definition);

				auto member_initializer = create_initializer(compiler->builtin_enum_member);
				set_member(member_initializer, "name", make_string(s->name, location));
				set_member(member_initializer, "value", evaluate(state, s->expression));

				d = member_initializer;
			}
			set_member(typeinfo_initializer, "enum_members", member_list);
		}
		set_member(typeinfo_initializer, "size", make_integer(get_size(type), location));
		set_member(typeinfo_initializer, "align", make_integer(get_align(type), location));

		if (auto array = as<AstArray>(directed)) {
			set_member(typeinfo_initializer, "array_count", make_integer(array->count, location));
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, array->element_type, location));
		} else if (auto span = (AstSpan *)directed; directed->kind == Ast_Span) {
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, span->expression, location));
		} else if (auto elem_type = get_span_subtype(directed)) {
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, elem_type, location));
		} else if (auto pointer = as_pointer(directed)) {
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, pointer->expression, location));
		} else if (auto option = as_option(directed)) {
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, option->expression, location));
		}

		for (umm i = 0; i < typeinfo_initializer->sorted_arguments.count; ++i) {
			auto &arg = typeinfo_initializer->sorted_arguments[i];
			if (!arg) {
				// FIXME: get rid of this make_null shit
				arg = state->make_null(compiler->builtin_typeinfo.Struct->data_members[i]->type, typeinfo_initializer->location);
			}
		}

		definition->expression = typeinfo_initializer;

		evaluate_and_put_definition_in_section(state, definition, compiler->constant_section);
	}

	auto identifier = AstIdentifier::create();
	identifier->location = location;
	identifier->name = definition->name;
	identifier->possible_definitions.set(definition);
	identifier->type = compiler->builtin_typeinfo.ident;

	auto address = AstUnaryOperator::create();
	address->expression = identifier;
	address->location = location;
	address->operation = UnaryOperation::address_of;
	address->type = compiler->builtin_typeinfo.pointer;

	return address;
}

enum class TypeDistance {
	exact,
	Template,
	basic,
	any_or_custom,
};

struct ArgumentDistance {
	TypeDistance type_distance;
	u8 const_distance; // 1 if passing const value to non-const parameter, otherwise 0
};

int argument_distance(ArgumentDistance distance) {
	return (int)distance.type_distance * 2 + distance.const_distance;
}

// TODO: FIXME: this is a mess. need to clean up.
bool implicitly_cast(TypecheckState *state, Reporter *reporter, CExpression auto *_expression, AstExpression *dst_type, TypeDistance *conversion_distance = 0, bool apply = true) {
	auto expression = *_expression;
	defer { *_expression = expression; };

	auto src_type = expression->type;

	if (conversion_distance)
		*conversion_distance = TypeDistance::exact;

	if (src_type->kind == Ast_LambdaType) {
		auto lambda_type = (AstLambdaType *)src_type;
		wait_for(
			state, lambda_type->lambda->location, "return type deduction",
			[&] { return lambda_type->lambda->return_parameter; },
			[&] {
				state->reporter.error(lambda_type->lambda->location, "Could not deduce return type.");
				state->reporter.info(expression->location, "While trying to implicitly cast this expression:");
				yield(TypecheckResult::fail);
			}
		);
	}

	auto src_distinct = direct_as<AstDistinct>(src_type);
	auto dst_distinct = direct_as<AstDistinct>(dst_type);
	if (!src_distinct) {
		if (dst_distinct) {
			dst_type = dst_distinct->expression;
		}
	} else {
		if (!dst_distinct) {
			src_type = src_distinct->expression;
		}
	}
	defer {
		if (apply) {
			if (dst_distinct && !src_distinct)
				expression->type = dst_distinct->identifier;
			else if (!dst_distinct && src_distinct)
				expression = make_cast(expression, dst_type);
		}
	};

	if (types_match(src_type, dst_type)) {
		return true;
	}

	if (conversion_distance)
		*conversion_distance = TypeDistance::basic;

	auto earray = as<AstArray>(src_type);

	if (earray) {
		if (auto subtype = get_span_subtype(dst_type)) {
			if (types_match(earray->element_type, subtype)) {
				if (apply)
					expression = make_cast(expression, dst_type);
				return true;
			}
		}
	}

	if (types_match(src_type, compiler->builtin_unknown_enum)) {
		if (expression->kind == Ast_UnaryOperator) {
			auto unop = (AstUnaryOperator *)expression;
			if (auto dst_enum = direct_as<AstEnum>(dst_type)) {
				assert(unop->expression->kind == Ast_Identifier);
				auto ident = (AstIdentifier *)unop->expression;
				auto found_value = find_enum_value(state, dst_enum, ident->name, ident->location);
				if (apply)
					expression = found_value;
				return true;
			}
		}
		if (reporter) {
			reporter->error(expression->location, "Can't deduce the enum");
		}
		return false;
	}

	if (expression->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)expression;
		if (unop->operation == UnaryOperation::pack && types_match(unop->expression->type, dst_type)) {
			return true;
		}
	}

	if (types_match(dst_type, compiler->builtin_any)) {
		if (conversion_distance)
			*conversion_distance = TypeDistance::any_or_custom;

		if (apply) {
			make_concrete(state, &expression);

			auto initializer = AstCall::create();
			initializer->callable = compiler->builtin_any.ident;
			initializer->location = expression->location;
			initializer->type = compiler->builtin_any.ident;

			initializer->sorted_arguments.resize(2);

			if (is_addressable(expression)) {
				initializer->sorted_arguments[0] = make_cast_if_types_dont_match(make_address_of(reporter, expression), compiler->builtin_u8.pointer);
				if (!initializer->sorted_arguments[0])
					yield(TypecheckResult::fail);
			} else {
				auto temporary = AstUnaryOperator::create();

				temporary->expression = expression;
				temporary->location = expression->location;
				temporary->operation = UnaryOperation::move_to_temporary;
				temporary->type = make_pointer_type(src_type);

				initializer->sorted_arguments[0] = make_cast_if_types_dont_match(temporary, compiler->builtin_u8.pointer);
			}

			initializer->sorted_arguments[1] = get_typeinfo(state, src_type, expression->location);

			expression = initializer;
		}
		return true;
	}

	if (expression->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)expression;
		if (unop->operation == UnaryOperation::autocast) {
			auto autocast = unop;
			CastType request = {direct_as<AstStruct>(src_type), direct_as<AstStruct>(dst_type)};
			auto found_built_in = find(built_in_casts, request);

			auto allow_autocast = [&] {
				if (apply) {
					if (autocast->expression->kind == Ast_Literal) {
						auto literal = (AstLiteral *)autocast->expression;
						if (literal->literal_kind == LiteralKind::integer) {
							literal->type = dst_type;
							expression = literal;
							return true;
						}
					}
					expression = make_cast(autocast->expression, dst_type);
				}
				return true;
			};

			if (found_built_in) {
				return allow_autocast();
			}

			if (::is_pointer_internally(src_type) && is_lambda(dst_type)) {
				// Pointer can be converted to a lambda.
				return allow_autocast();
			}

			if (::is_integer(src_type)) {
				if (types_match(src_type, compiler->builtin_unsized_integer)) {
					if (::is_pointer_internally(dst_type) || ::is_integer(dst_type)) {
						return allow_autocast();
					}
				} else {
					if (::is_pointer_internally(dst_type)) {
						auto built_in_cast = find_if(built_in_casts, [&](auto c) { return types_match(c.from, src_type); });
						assert(built_in_cast);
						return allow_autocast();
					}
				}
			} else if (::is_pointer_internally(src_type)) {
				if (::is_pointer_internally(dst_type)) {
					return allow_autocast();
				} else if (::is_integer(dst_type)) {
					if (get_size(dst_type) == 8) {
						//cast->cast_kind = CastKind::no_op;
					}

					return allow_autocast();
				}
			} else if (auto src_enum = direct_as<AstEnum>(src_type)) {
				if (::is_integer(dst_type)) {
					return allow_autocast();
				}
			}

			bool was_found = true;
			was_found &= wait_for(state, expression->location, "explicit casts", [&] {
				for (auto lambda : typechecked_explicit_casts) {
					if (lambda->is_poly) {
						if (match_templates(state, 0, 0, 0, lambda->return_parameter->type, dst_type, 0) &&
							match_templates(state, 0, 0, 0, lambda->parameters[0]->type, src_type, 0)
						) {
							if (apply) {
								auto call = AstCall::create();
								call->location = expression->location;
								call->callable = lambda;
								call->unsorted_arguments.set({{}, expression});
								call->sorted_arguments.set(expression);
								call->type = dst_type;
								assert(lambda->type->kind == Ast_LambdaType);
								call->lambda_type = (AstLambdaType *)lambda->type;
								expression = call;
							}
							return true;
						}
					} else {
						if (types_match(lambda->return_parameter->type, dst_type) && types_match(lambda->parameters[0]->type, src_type)) {
							if (apply) {
								auto call = AstCall::create();
								call->location = expression->location;
								call->callable = lambda;
								call->unsorted_arguments.set({{}, expression});
								call->sorted_arguments.set(expression);
								call->type = dst_type;
								assert(lambda->type->kind == Ast_LambdaType);
								call->lambda_type = (AstLambdaType *)lambda->type;
								expression = call;
							}
							return true;
						}
					}
				}
				if (not_typechecked_implicit_casts_count == 0) {
					was_found = false;
					return true;
				}
				return false;
			}, []{}, false);

			if (was_found)
				return true;

			if (reporter) {
				reporter->error(expression->location, "Expression of type {} is not convertible to {}.", type_to_string(src_type), type_to_string(dst_type));
			}
			return false;
		}
	}
	if (src_type->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)src_type;
		if (unop->operation == UnaryOperation::option) {
			if (types_match(dst_type, compiler->builtin_bool)) {
				if (apply)
					expression = make_cast(expression, compiler->builtin_bool.ident);
				return true;
			}
		}
	}
	if (auto dst_option = as_option(dst_type)) {
		if (types_match(src_type, dst_option->expression)) {
			if (apply)
				expression = make_cast(expression, dst_option);
			return true;
		}
		if (expression->kind == Ast_Literal) {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::null) {
				if (apply)
					src_type = dst_type;
				return true;
			}
		}

		if (auto src_option = as_option(src_type)) {

			// FIXME: does not work with user types
			//
			// x: ?S32 => ?F32
			// { x := expr; if x then (*x as F32) as ?F32 else null }

			CastType request = {direct_as<AstStruct>(src_option->expression), direct_as<AstStruct>(dst_option->expression)};
			auto found_built_in = find(built_in_casts, request);

			if (found_built_in && found_built_in->implicit) {
				if (apply) {
					not_implemented();
				}
				return true;
			}
		}
	}
	if (expression->kind == Ast_Literal) {
		auto literal = (AstLiteral *)expression;
		if (literal->literal_kind == LiteralKind::integer) {
			if (auto Enum = direct_as<AstEnum>(literal->type)) {
			} else {
				auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, dst_type); });
				if (found_info) {

					auto &info = *found_info;
					if (!ensure_fits(reporter, expression, literal->integer, info)) {
						return false;
					}

					if (apply) {
						if (types_match(dst_type, compiler->builtin_u8)) {
							literal->integer.msb = false;
							literal->integer.parts.resize(1);
							literal->integer.parts[0] &= 0xff;
						} else if (types_match(dst_type, compiler->builtin_u16)) {
							literal->integer.msb = false;
							literal->integer.parts.resize(1);
							literal->integer.parts[0] &= 0xffff;
						} else if (types_match(dst_type, compiler->builtin_u32)) {
							literal->integer.msb = false;
							literal->integer.parts.resize(1);
							literal->integer.parts[0] &= 0xffffffff;
						} else if (types_match(dst_type, compiler->builtin_u64)) {
							literal->integer.msb = false;
							literal->integer.parts.resize(1);
						}
						expression->type = dst_type;
					}
					return true;
				}

				if (types_match(dst_type, compiler->builtin_f32) || types_match(dst_type, compiler->builtin_f64)) {
					if (apply)
						expression->type = dst_type;
					return true;
				}
			}
		} else if (literal->literal_kind == LiteralKind::Float) {
			if (types_match(dst_type, compiler->builtin_f32) || types_match(dst_type, compiler->builtin_f64)) {
				if (apply)
					expression->type = dst_type;
				return true;
			}
		} else if (literal->literal_kind == LiteralKind::noinit) {
			if (apply)
				expression->type = dst_type;
			return true;
		} else if (literal->literal_kind == LiteralKind::null) {
			if (is_pointer(dst_type)) {
				if (apply)
					expression->type = dst_type;
				return true;
			}
		}
	} else if (expression->kind == Ast_If) {
		auto If = (AstIf *)expression;

		AstExpression *true_block = If->true_block;
		AstExpression *false_block = If->false_block;
		defer {
			assert(true_block == If->true_block && false_block == If->false_block, "implicitly_cast messed up block(s) in if expression (implicitly_cast)");
		};

		return
			(types_match(true_block ->type, compiler->builtin_unreachable) || implicitly_cast(state, reporter, &true_block,  dst_type, 0, apply)) &&
			(types_match(false_block->type, compiler->builtin_unreachable) || implicitly_cast(state, reporter, &false_block, dst_type, 0, apply));

	} else if (expression->kind == Ast_Block) {
		auto Block = (AstBlock *)expression;

		if (Block->scope->statement_list.count == 0) {
			if (reporter)
				reporter->error(Block->location, "Block does not contain any expression.");
			return false;
		}

		auto stmt = Block->scope->statement_list.back();

		if (auto est = as<AstExpressionStatement>(stmt)) {
			return implicitly_cast(state, reporter, &est->expression, dst_type, 0, apply);
		} else {
			if (reporter)
				reporter->error(Block->location, "Block's last statement is not an expression.");
			return false;
		}
	} else if (types_match(dst_type, compiler->builtin_void.pointer)) {
		if (is_pointer(src_type)) {
			return true;
		}
	} else if (types_match(src_type, compiler->builtin_unsized_integer)) {

		auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, dst_type); });
		if (found_info) {
			auto &info = *found_info;
			auto integer = evaluate(state, expression);
			assert(integer);
			assert(integer->literal_kind == LiteralKind::integer);

			if (integer) {
				if (!ensure_fits(reporter, expression, integer->integer, info)) {
					return false;
				}
			} else {
				if (reporter)
					reporter->warning(expression->location, "Could not optimize constants; will do at runtime.");
				return false;
			}
			if (apply)
				expression->type = dst_type;
			return true;
		}

		if (types_match(dst_type, compiler->builtin_f32) || types_match(dst_type, compiler->builtin_f64)) {
			if (apply) {
				expression->type = dst_type;
				if (auto literal = as<AstLiteral>(expression)) {
					literal->literal_kind = LiteralKind::Float;
					literal->Float = (s64)literal->integer;
				}
			}
			return true;
		}
	} else if (types_match(src_type, compiler->builtin_unsized_float)) {
		if (types_match(dst_type, compiler->builtin_f32) || types_match(dst_type, compiler->builtin_f64)) {
			if (apply)
				expression->type = dst_type;
			return true;
		}
	} else if (expression->kind == Ast_Lambda) {
		// Lambda literals can be implicitly casted when calling convention does not match
		// That just means that calling convention can be deduced when assigning (or smth else) a lambda literal.
		// This will not work for named lambdas.

		auto src_lambda = (AstLambda *)expression;
		auto dst_lambda = get_lambda(dst_type);
		if (dst_lambda) {
			if (same_argument_and_return_types(src_lambda, dst_lambda)) {
				src_lambda->convention = dst_lambda->convention;
				return true;
			}
		}
	} else {
		CastType request = {direct_as<AstStruct>(src_type), direct_as<AstStruct>(dst_type)};
		auto found_built_in = find(built_in_casts, request);

		if (found_built_in && found_built_in->implicit) {
			if (apply)
				expression = make_cast(expression, dst_type);
			return true;
		}
	}

	// Implicit address of
	// x => &x
	if (auto dst_ptr = as_pointer(dst_type)) {
		if (types_match(dst_ptr->expression, src_type)) {
			if (apply) {
				if (is_addressable(expression)) {
					expression = make_address_of(reporter, expression);
				} else {
					expression = make_unary(UnaryOperation::move_to_temporary, expression, dst_type);
				}
			}
			return true;
		}
	}
	// Implicit dereference
	if (auto src_ptr = as_pointer(src_type)) {
		if (types_match(src_ptr->expression, dst_type)) {
			if (apply) {
				expression = make_unary(UnaryOperation::dereference, expression, dst_type);
			}
			return true;
		}
	}

	if (conversion_distance)
		*conversion_distance = TypeDistance::any_or_custom;

	bool was_found = true;
	was_found &= wait_for(state, expression->location, "implicit casts", [&] {
		for (auto lambda : implicit_casts) {
			if (types_match(lambda->return_parameter->type, dst_type) && types_match(lambda->parameters[0]->type, src_type)) {
				if (apply) {
					auto call = AstCall::create();
					call->location = expression->location;
					assert(lambda->definition);
					call->callable = make_identifier(lambda->definition, expression->location);
					call->unsorted_arguments.set({{}, expression});
					call->sorted_arguments.set(expression);
					call->type = dst_type;
					assert(lambda->type->kind == Ast_LambdaType);
					call->lambda_type = (AstLambdaType *)lambda->type;
					expression = call;
				}
				return true;
			}
		}
		if (not_typechecked_implicit_casts_count == 0) {
			was_found = false;
			return true;
		}
		return false;
	}, []{}, false);

	if (was_found)
		return true;

	if (reporter) {
		reporter->error(expression->location, "Expression of type {} is not implicitly convertible to {}.", type_to_string(expression->type), type_to_string(dst_type));
	}
	return false;
}

bool is_concrete_type(AstExpression *type) {
	assert(is_type(type), "is_concrete_type accepts a type, but got an expression");
	if (types_match(type, compiler->builtin_unsized_integer) ||
		types_match(type, compiler->builtin_unsized_float) ||
		types_match(type, compiler->builtin_overload_set) ||
		types_match(type, compiler->builtin_unknown_enum) ||
		types_match(type, compiler->builtin_unreachable) ||
		types_match(type, compiler->builtin_deferred_if))
	{
		return false;
	}
	return true;
}

AstExpression *get_concrete_type(AstExpression *type) {
	if (types_match(type, compiler->builtin_unsized_integer))
		return compiler->type_int;

	if (types_match(type, compiler->builtin_unsized_float))
		return compiler->type_float;

	return type;
}

void make_concrete(TypecheckState *state, AstExpression **_expression, AstExpression *target_type = 0);

void make_concrete(TypecheckState *state, AstBlock *block, AstExpression *target_type = 0) {
	if (block->scope->statement_list.count == 0)
		return;

	auto es = as<AstExpressionStatement>(block->scope->statement_list.back());
	if (!es)
		return;

	make_concrete(state, &es->expression, target_type);
	block->type = es->expression->type;
}
void make_concrete(TypecheckState *state, AstExpression **_expression, AstExpression *target_type) {
	auto &expression = *_expression;

	if (is_concrete_type(expression->type)) {
		return;
	}

	switch (expression->kind) {
		case Ast_Literal: {
			auto Literal = (AstLiteral *)expression;

			if (target_type) {

				auto root_type = direct_through_distinct(target_type);

				if (::is_integer(root_type)) {
					assert(Literal->literal_kind == LiteralKind::integer);
				} else if (::is_float(root_type)) {
					if (Literal->literal_kind != LiteralKind::Float) {
						switch (Literal->literal_kind) {
							case LiteralKind::integer: Literal->Float = (s64)Literal->integer; break;
							default: invalid_code_path();
						}
						Literal->literal_kind = LiteralKind::Float;
					}
				} else {
					invalid_code_path();
				}

				Literal->type = target_type;
			} else {
				switch (Literal->literal_kind) {
					case LiteralKind::integer: {
						Literal->type = compiler->type_int;
						break;
					}
					case LiteralKind::Float: {
						Literal->type = compiler->type_float;
						break;
					}
					default:
						break;
				}
			}
			break;
		}
		case Ast_If: {
			auto If = (AstIf *)expression;

			auto tc = is_concrete_type(If->true_block->type);
			auto fc = is_concrete_type(If->false_block->type);

			if (!tc && !fc) {
				make_concrete(state, If->true_block, target_type);
				make_concrete(state, If->false_block, target_type);
			}

			if (types_match(If->true_block->type, If->false_block->type)) {
				If->type = If->true_block->type;
				return;
			}

			if (types_match(If->true_block->type, compiler->builtin_unreachable)) {
				If->type = If->false_block->type;
				return;
			}

			if (types_match(If->false_block->type, compiler->builtin_unreachable)) {
				If->type = If->true_block->type;
				return;
			}

			AstExpression* true_block = If->true_block;
			AstExpression* false_block = If->false_block;
			defer {
				assert(true_block == If->true_block && false_block == If->false_block, "implicitly_cast messed up block(s) in if expression (typecheck)");
			};

			bool true_is_castable_to_false = implicitly_cast(state, 0, &true_block, If->false_block->type, 0, false);
			bool false_is_castable_to_true = implicitly_cast(state, 0, &false_block, If->true_block->type, 0, false);

			if (true_is_castable_to_false && false_is_castable_to_true) {
				state->reporter.error(If->location, "Branches of this if expression have different types, and both are implicitly castable to each other, which makes it ambiguous. The types are {} and {}", type_to_string(If->true_block->type), type_to_string(If->false_block->type));
				yield(TypecheckResult::fail);
			}

			if (!true_is_castable_to_false && !false_is_castable_to_true) {
				state->reporter.error(If->location, "Branches of this if expression have different types, and there is no conversion from one to another. The types are {} and {}", type_to_string(If->true_block->type), type_to_string(If->false_block->type));
				yield(TypecheckResult::fail);
			}

			if (true_is_castable_to_false) {
				if (!implicitly_cast(state, 0, &true_block, If->false_block->type)) {
					state->reporter.error(If->location, "INTERNAL ERROR: implicit cast failed after success.");
					yield(TypecheckResult::fail);
				}
			}
			if (false_is_castable_to_true) {
				if (!implicitly_cast(state, 0, &false_block, If->true_block->type)) {
					state->reporter.error(If->location, "INTERNAL ERROR: implicit cast failed after success.");
					yield(TypecheckResult::fail);
				}
			}

			If->type = If->true_block->type;
			break;
		}
		case Ast_UnaryOperator: {
			auto UnaryOperator = (AstUnaryOperator *)expression;
			switch (UnaryOperator->operation) {
				case UnaryOperation::plus:
				case UnaryOperation::minus: {
					make_concrete(state, &UnaryOperator->expression, target_type);
					UnaryOperator->type = UnaryOperator->expression->type;
					break;
				}
			}
			break;
		}
		case Ast_Block: {
			auto Block = (AstBlock *)expression;
			make_concrete(state, Block, target_type);
			break;
		}
		default:
			break;
	}
}


bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
	timed_function(compiler->profiler);
	switch (expression->kind) {
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition();
			if (definition->is_constant) {
				reporter->error(identifier->location, "Can't assign to '{}' because it is constant.", identifier->location);
				return false;
			}
			if (get_definition_origin(definition) == DefinitionOrigin::parameter) {
				reporter->error(identifier->location, "Right now assigning to function parameters is not allowed.");
				return false;
			}

			return true;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)expression;
			switch (binop->operation) {
				case BinaryOperation::dot: {
					if (is_pointer(binop->left->type))
						return true;
					return ensure_assignable(reporter, binop->left) && ensure_assignable(reporter, binop->right);
				}
			}
			break;
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;
			if (is_pointer(subscript->expression->type))
				return true;
			if (get_span_subtype(subscript->expression->type))
				return true;
			if (types_match(subscript->expression->type, compiler->builtin_string))
				return true;
			return ensure_assignable(reporter, subscript->expression);
		}
		case Ast_UnaryOperator: {
			using enum UnaryOperation;
			auto unop = (AstUnaryOperator *)expression;
			if (unop->operation == dereference)
				return true;

			break;
		}
	}

	reporter->error(expression->location, "Expression is not assignable.");
	return false;
}

void ensure_return_types_match(TypecheckState *state, AstLambda *lambda) {
	assert(lambda->return_parameter);
	assert(lambda->return_parameter->type);
	for (auto ret : lambda->return_statements) {
		if (ret->expression) {
			if (!implicitly_cast(state, &state->reporter, &ret->expression, lambda->return_parameter->type)) {
				if (lambda->return_statement_type_deduced_from) {
					state->reporter.info(lambda->return_statement_type_deduced_from->location, "Return type deduced from this statement:");
				}
				yield(TypecheckResult::fail);
			}
		}
	}
}

void calculate_parameters_size(AstLambda *lambda) {
	if (lambda->parameters_size != -1) {
		immediate_info(lambda->location, "FIXME: Redundant parameter size calculation");
	}
	s64 parameter_size_accumulator = 0;

	for (auto parameter : lambda->parameters) {
		if (parameter->is_constant)
			continue;

		parameter->offset = parameter_size_accumulator;
		parameter_size_accumulator += compiler->stack_word_size;
	}
	lambda->parameters_size = parameter_size_accumulator;
}

// FIXME: ensure 'directed' member on expressions is set properly after copying.

template <class T>
T *copy_node(T *node) {
	auto result = T::create();
	result->location = node->location;
	return result;
}

template <class T>
T *copy_expression(T *expression) {
	auto result = copy_node(expression);
	result->type = expression->type;
	return result;
}

template <class T>
T *copy_expression_keep_type(T *expression) {
	auto result = copy_node(expression);
	result->type = expression->type;
	return result;
}

template <class T>
T *copy_statement(T *statement) {
	auto result = copy_node(statement);
	return result;
}

void add_child(Scope *parent, Scope *child) {
	assert(!child->parent);
	child->parent = parent;
	parent->children.add(child);
}

void deep_copy(Scope *d, Scope *s) {
	assert(d->statement_list.count == 0);
	assert(d->definition_list.count == 0);
	assert(count_of(d->definition_map) == 0);
	assert(d->usings.count == 0);
	for (auto statement : s->statement_list) {
		auto copied = deep_copy(statement);
		d->add(copied);
		switch (copied->kind) {
			case Ast_While: {
				auto While = (AstWhile *)copied;
				add_child(d, While->scope);
				break;
			}
			case Ast_For: {
				auto For = (AstFor *)copied;
				add_child(d, For->scope);
				break;
			}
			case Ast_Defer: {
				auto Defer = (AstDefer *)copied;
				add_child(d, Defer->scope);
				break;
			}
			case Ast_Definition: {
				auto Definition = (AstDefinition *)copied;
				if (Definition->expression) {
					switch (Definition->expression->kind) {
						case Ast_Struct: {
							auto Struct = (AstStruct *)Definition->expression;
							add_child(d, Struct->parameter_scope);
							break;
						}
					}
				}
				break;
			}
			case Ast_ExpressionStatement: {
				auto ExpressionStatement = (AstExpressionStatement *)copied;
				switch (ExpressionStatement->expression->kind) {
					case Ast_If: {
						auto If = (AstIf *)ExpressionStatement->expression;
						add_child(d, If->true_block->scope);
						add_child(d, If->false_block->scope);
						break;
					}
					case Ast_Block: {
						auto Block = (AstBlock *)ExpressionStatement->expression;
						add_child(d, Block->scope);
						break;
					}
				}
				break;
			}
			case Ast_Print:
			case Ast_Return:
			case Ast_Assert:
			case Ast_Using:
				break;
			default:
				invalid_code_path(statement->location, "INTERNAL ERROR: unhandled case in deep_copy(Scope *, Scope *): {}", copied->kind);
		}
	}
}

AstExpression *deep_copy(AstExpression *expression) {
	if (!expression)
		return 0;
	switch (expression->kind) {
		case Ast_Identifier    : return deep_copy((AstIdentifier     *)expression);
		case Ast_Call          : return deep_copy((AstCall           *)expression);
		case Ast_Literal       : return deep_copy((AstLiteral        *)expression);
		case Ast_Lambda        : return deep_copy((AstLambda         *)expression);
		case Ast_BinaryOperator: return deep_copy((AstBinaryOperator *)expression);
		case Ast_UnaryOperator : return deep_copy((AstUnaryOperator  *)expression);
		case Ast_Struct        : return deep_copy((AstStruct         *)expression);
		case Ast_Subscript     : return deep_copy((AstSubscript      *)expression);
		case Ast_Array         : return deep_copy((AstArray          *)expression);
		case Ast_If            : return deep_copy((AstIf             *)expression);
		case Ast_Test          : return deep_copy((AstTest           *)expression);
		case Ast_Span          : return deep_copy((AstSpan           *)expression);
		case Ast_Block         : return deep_copy((AstBlock          *)expression);
		default: invalid_code_path();
	}
}
AstStatement *deep_copy(AstStatement *statement) {
	assert(statement);
	switch (statement->kind) {
		case Ast_Definition:          return deep_copy((AstDefinition          *)statement);
		case Ast_Defer:               return deep_copy((AstDefer               *)statement);
		case Ast_Assert:              return deep_copy((AstAssert              *)statement);
		case Ast_ExpressionStatement: return deep_copy((AstExpressionStatement *)statement);
		case Ast_OperatorDefinition:  return deep_copy((AstOperatorDefinition  *)statement);
		case Ast_Parse:               return deep_copy((AstParse               *)statement);
		case Ast_Print:               return deep_copy((AstPrint               *)statement);
		case Ast_Return:              return deep_copy((AstReturn              *)statement);
		case Ast_While:               return deep_copy((AstWhile               *)statement);
		case Ast_For:                 return deep_copy((AstFor                 *)statement);
		case Ast_Using:               return deep_copy((AstUsing               *)statement);
		default: invalid_code_path();
	}
}

template <class T>
T *shallow_copy(T *node) {
	auto _new = T::create();
	auto uid = _new->uid;
	*_new = *node;
	_new->uid = uid;

	if constexpr (std::is_base_of_v<AstExpression, T>) {
		_new->directed = _new;
	}

	return _new;
}

AstDefinition *deep_copy(AstDefinition *s) {
	auto d = copy_statement(s);
	d->is_poly = s->is_poly;
	d->is_constant = s->is_constant;
	d->is_pack = s->is_pack;
	d->name = s->name;
	d->placed_at = s->placed_at;
	d->expression = deep_copy(s->expression);
	d->evaluated = deep_copy(raw(s->evaluated));
	d->offset = s->offset;
	d->has_using = s->has_using;

	d->parsed_type = deep_copy(s->parsed_type);

	if (d->expression) {
		switch (d->expression->kind) {
			case Ast_Lambda: ((AstLambda *)d->expression)->definition = d; break;
			case Ast_Struct: ((AstStruct *)d->expression)->definition = d; break;
			case Ast_Enum:   ((AstEnum   *)d->expression)->definition = d; break;
		}
	}

	return d;
}
AstDefer *deep_copy(AstDefer *s) {
	auto d = copy_statement(s);
	deep_copy(d->scope, s->scope);
	return d;
}
AstAssert *deep_copy(AstAssert *s) {
	auto d = shallow_copy(s);
	d->condition = deep_copy(s->condition);
	return d;
}
AstBlock *deep_copy(AstBlock *s) {
	auto d = copy_statement(s);
	deep_copy(d->scope, s->scope);
	return d;
}
AstExpressionStatement *deep_copy(AstExpressionStatement *s) {
	auto d = copy_statement(s);
	d->expression = deep_copy(s->expression);
	return d;
}
AstIf *deep_copy(AstIf *s) {
	auto d = copy_statement(s);
	d->is_constant = s->is_constant;
	d->condition = deep_copy(s->condition);
	d->true_block = deep_copy(raw(s->true_block));
	d->false_block = deep_copy(raw(s->false_block));
	return d;
}
AstOperatorDefinition *deep_copy(AstOperatorDefinition *s) {
	auto d = copy_statement(s);
	d->is_implicit = s->is_implicit;
	d->operation = s->operation;
	d->lambda = deep_copy(raw(s->lambda));
	return d;
}
AstParse *deep_copy(AstParse *s) {
	auto d = copy_statement(s);
	d->expression = deep_copy(s->expression);
	return d;
}
AstPrint *deep_copy(AstPrint *s) {
	auto d = copy_statement(s);
	d->expression = deep_copy(s->expression);
	return d;
}
AstReturn *deep_copy(AstReturn *s) {
	auto d = copy_statement(s);
	d->expression = deep_copy(s->expression);
	return d;
}
AstWhile *deep_copy(AstWhile *s) {
	auto d = copy_statement(s);
	d->condition = deep_copy(s->condition);
	deep_copy(d->scope, s->scope);
	return d;
}
AstFor *deep_copy(AstFor *s) {
	auto d = copy_statement(s);
	d->range = deep_copy(s->range);
	d->iterator_name = s->iterator_name;
	deep_copy(d->scope, s->scope);
	return d;
}
AstUsing *deep_copy(AstUsing *s) {
	auto d = copy_statement(s);
	d->expression = deep_copy(s->expression);
	return d;
}

AstIdentifier *deep_copy(AstIdentifier *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->name = s->name;

	// NOTE: First argument of a template lambda can be typechecked before deep copying
	//       In that case we should keep it's definition
	if (s->type) {
		if (s->definition()) {
			d->possible_definitions.set(s->definition());
			d->type = s->definition()->type;
		}
	}

	return d;
}
AstCall *deep_copy(AstCall *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->callable = deep_copy(s->callable);
	for (auto arg : s->unsorted_arguments) {
		d->unsorted_arguments.add({arg.name, deep_copy(arg.expression)});
	}
	return d;
}
AstLiteral *deep_copy(AstLiteral *s) {
	if (!s) return 0;

	auto d = copy_expression_keep_type(s);
	d->literal_kind = s->literal_kind;
	switch (s->literal_kind) {
		using enum LiteralKind;
		case boolean:   d->Bool = s->Bool; break;
		case Float:     d->Float = s->Float; break;
		case character: d->character = s->character; break;
		case integer:   d->integer = copy(s->integer); break;
		case type:      d->type_value = deep_copy(s->type_value); break;
		case string:	d->string.set(s->string.get()); break;
		case Struct:
			d->struct_values = map(s->struct_values, [&] (auto m) { return deep_copy(m); });

			// FIXME: i don't like this
			if (current_typecheck_state)
				put_in_section(current_typecheck_state, d, compiler->constant_section);
			break;
		case null: break;
		case lambda_name: break;
		case pointer: d->pointer = s->pointer; break;
		default:invalid_code_path();
	}
	return d;
}
AstLambda *deep_copy(AstLambda *s) { not_implemented(); }
AstBinaryOperator *deep_copy(AstBinaryOperator *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->left  = deep_copy(s->left );
	d->right = deep_copy(s->right);
	d->operation = s->operation;
	return d;
}
AstUnaryOperator *deep_copy(AstUnaryOperator *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->expression = deep_copy(s->expression);
	d->operation = s->operation;
	return d;
}
AstStruct *deep_copy(AstStruct *s) {
	auto d = copy_expression(s);

	deep_copy(d->parameter_scope, s->parameter_scope);
	deep_copy(d->member_scope, s->member_scope);

	for (auto &m : d->parameter_scope->definition_list) {
		m->container_node = s;
	}

	for (auto &m : d->member_scope->definition_list) {
		if (!m->is_constant) {
			d->data_members.add(m);
		}
		m->container_node = s;
	}
	d->default_value = deep_copy(raw(s->default_value));

	d->alignment = s->alignment;
	d->size = s->size;
	d->default_value_offset = s->default_value_offset;
	d->is_span = s->is_span;
	d->is_template = s->is_template;
	d->is_union = s->is_union;
	d->layout = s->layout;
	return d;
}
AstSubscript *deep_copy(AstSubscript *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->expression       = deep_copy(s->expression      );
	d->index_expression = deep_copy(s->index_expression);
	return d;
}
AstArray *deep_copy(AstArray *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->element_type     = deep_copy(s->element_type    );
	d->count_expression = deep_copy(s->count_expression);
	d->count = s->count;
	return d;
}
AstTest *deep_copy(AstTest *s) {
	auto d = copy_expression(s);
	deep_copy(d->scope, s->scope);
	return d;
}
AstSpan *deep_copy(AstSpan *s) {
	auto d = copy_expression(s);
	d->expression = deep_copy(s->expression);
	return d;
}

void set_local_offset(TypecheckState *state, AstDefinition *definition, AstLambda *lambda) {
	assert((lambda->locals_size & 0xf) == 0);
	definition->offset = lambda->locals_size;
	auto size = ceil(get_size(state, definition->type), 16ll);
	lambda->locals_size += size;
}

[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstDefinition          *Definition);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstReturn              *Return);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstYield               *Yield);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstWhile               *While);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstFor                 *For);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstExpressionStatement *ExpressionStatement);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstAssert              *Assert);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstDefer               *Defer);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstPrint               *Print);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstOperatorDefinition  *OperatorDefinition);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstLoopControl         *LoopControl);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstParse               *Parse);
[[nodiscard]] AstStatement *typecheck(TypecheckState *state, AstUsing               *Using);
void typecheck(TypecheckState *state, AstStatement *&Statement);

[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstIdentifier     *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstCall           *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstLiteral        *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstLambda         *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstLambdaType     *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstBinaryOperator *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstUnaryOperator  *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstStruct         *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstSubscript      *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstArray          *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstSpan           *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstIf             *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstEnum           *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstBlock          *);
[[nodiscard]] AstExpression *typecheck(TypecheckState *, AstMatch          *);
void typecheck(TypecheckState *, CExpression auto &);

void typecheck(TypecheckState *state, Scope *scope) {
	push_scope(scope);
	for (auto &statement : scope->statement_list) {
		typecheck(state, statement);
	}
}

struct TypeWithArticle {
	AstExpression *type;
};

inline umm append(StringBuilder &builder, TypeWithArticle type) {
	auto t = type.type->directed ? raw(type.type->directed) : direct(type.type);
	if (t == compiler->builtin_overload_set.Struct) {
		return append(builder, "an overload set");
	}
	return append(builder, "something else. FIXME");
}

String get_binop_ident_name(BinaryOperation op) {
	switch (op) {
		using enum BinaryOperation;
#define e(name, token) case name: return #token ## str;
		ENUMERATE_BINARY_OPERATIONS
#undef e
	}
	invalid_code_path();
	return ""str;
}

String get_unop_ident_name(UnaryOperation op) {
	switch (op) {
		using enum UnaryOperation;
#define e(name, token) case name: return #token ## str;
		ENUMERATE_UNARY_OPERATIONS
#undef e
	}
	invalid_code_path();
	return ""str;
}

void append_literal(StringBuilder &builder, AstLiteral *value) {
	switch (value->literal_kind) {
		case LiteralKind::type: {
			append_type(builder, value->type_value->directed, true);
			break;
		}
		case LiteralKind::string: {
			append(builder, value->string.get());
			break;
		}
		case LiteralKind::integer: {
			append(builder, value->integer);
			break;
		}
		case LiteralKind::Float: {
			append(builder, value->Float);
			break;
		}
		case LiteralKind::character: {
			append(builder, value->character);
			break;
		}
		case LiteralKind::Struct: {
			append_type(builder, value->type, true);
			append(builder, "(");
			if (value->struct_values.count)
				append_literal(builder, value->struct_values[0]);
			for (auto &value : value->struct_values.skip(1)) {
				append(builder, ", ");
				append_literal(builder, value->struct_values[0]);
			}
			append(builder, ")");
			break;
		}
		default:
			not_implemented();
	}
}

AstExpression *get_not_instantiated_template(AstExpression *type) {
	AstExpression *result = 0;
	visit(type, Combine{
		[&](AstNode *) {},
		[&](AstIdentifier *Identifier) {
			if (result)
				return;

			if (auto Struct = direct_as<AstStruct>(Identifier)) {
				if (Struct->is_template)
					result = Identifier;
			}
		},
		[&](AstStruct *Struct) {
			if (result)
				return;

			if (Struct->is_template)
				result = Struct;
		},
		[&](AstUnaryOperator *UnaryOperator) {
			if (result)
				return;

			if (UnaryOperator->operation == UnaryOperation::poly)
				result = UnaryOperator;
		},
	});

	return result;
}

void ensure_template_is_instantiated(TypecheckState *state, AstExpression *type) {
	if (auto t = get_not_instantiated_template(type)) {
		state->reporter.error(t->location, "{} is a template struct, parameters must be provided.", type_to_string(t));
		state->reporter.info(direct_as<AstStruct>(t)->location, "Here is the definition:");
		yield(TypecheckResult::fail);
	}
}

bool has_template(AstExpression *type) {
	bool result = false;

	visit(type, Combine{
		[&](AstNode *) {},
		[&](AstIdentifier *Identifier) {
			if (auto Struct = direct_as<AstStruct>(Identifier)) {
				result |= Struct->is_template;
			}
		},
		[&](AstStruct *Struct) {
			result |= Struct->is_template;
		},
		[&](AstUnaryOperator *UnaryOperator) {
			result |= UnaryOperator->operation == UnaryOperation::poly;
		},
	});

	return result;
}

struct TypecheckDefinitionParams {
	bool allow_template = false;
};

void typecheck_definition(TypecheckState *state, AstDefinition *definition, TypecheckDefinitionParams params = {}) {
	if (definition->type) {
		return;
	}
	defer { if (!throwing) {
		assert(definition->type);
	}};

	scoped_replace(state->currently_typechecking_definition, definition);

	if (!definition->container_node) {
		definition->container_node = state->container_node;
	}

	auto definition_origin = get_definition_origin(definition);

	bool is_parameter = definition_origin == DefinitionOrigin::parameter;
	enum {
		Global,
		Lambda,
		Struct,
		Enum,
	} location;

	if (state->container_node) {
		switch (state->container_node->kind) {
			case Ast_Struct: location = Struct; break;
			case Ast_Lambda: location = Lambda; break;
			case Ast_Enum:   location = Enum; break;
			default: invalid_code_path();
		}
	} else {
		location = Global;
	}

	if (location == Global) {
		state->definition = definition;
	}

	if (definition->expression) {
		if (location == Global && definition->expression->kind == Ast_Lambda) {
			state->lambda = (AstLambda *)definition->expression;
		}

		typecheck(state, definition->expression);
	}

	if (definition->parsed_type) {

		// Lambda and struct set definition's type earlier to allow self-referencing, so don't typecheck it
		if (!(definition->expression && definition->expression->kind == Ast_Lambda)) {

			typecheck(state, definition->parsed_type);

			if (!definition->parsed_type->type) {
				yield(TypecheckResult::fail);
			}

			definition->type = definition->parsed_type;

			if (definition->expression) {
				if (!implicitly_cast(state, &state->reporter, &definition->expression, definition->type)) {
					yield(TypecheckResult::fail);
				}
			}
		}
	} else {
		if (definition->container_node && definition->container_node->kind == Ast_Enum && !definition->expression) {
			// enums can have definition without a type and without expression. This will be handled in enum typechecking, so nothing to do here.
		} else {
			if (definition->expression->kind == Ast_UnaryOperator && ((AstUnaryOperator *)definition->expression)->operation == UnaryOperation::autocast) {
				state->reporter.error(definition->location, "Can't deduce a type from autocast. You need to explicitly specify the resulting type");
				yield(TypecheckResult::fail);
			}

			if (!definition->is_constant) {
				make_concrete(state, &definition->expression);
			}
			definition->type = definition->expression->type;
		}
	}


	if (!definition->type) {
		state->reporter.error(definition->location, "Could not determine the type of this definition.");
		yield(TypecheckResult::fail);
	}


	if (definition->is_constant) {
		if (definition->expression) {
			if (!is_constant(definition->expression)) {
				state->reporter.error(definition->location, "Definition marked as constant, but assigned expression is not constant");
				yield(TypecheckResult::fail);
			}
		}
	}

	if (params.allow_template) {
		if (has_template(definition->type)) {
			// NOTE: following checks are not supposed for templates
			return;
		}
	} else {
		ensure_template_is_instantiated(state, definition->type);

		//visit(definition->expression, Combine{
		//	[&](AstNode *) {},
		//	[&](AstIdentifier *Identifier) {
		//		if (result)
		//			return;

		//		if (auto Struct = direct_as<AstStruct>(Identifier)) {
		//			if (Struct->is_template)
		//				result = Identifier;
		//		}
		//	},
		//	[&](AstStruct *Struct) {
		//		if (result)
		//			return;

		//		if (Struct->is_template)
		//			result = Struct;
		//	},
		//	[&](AstUnaryOperator *UnaryOperator) {
		//		if (result)
		//			return;

		//		if (UnaryOperator->operation == UnaryOperation::poly)
		//			result = UnaryOperator;
		//	},
		//});
	}

	if (true/*definition->type->kind != Ast_Import*/) {
		if (!definition->type->type) {
			invalid_code_path();
			state->reporter.error(definition->type->location, "This is not a type");
			yield(TypecheckResult::fail);
		}
		if (!is_type(definition->type)) {
			state->reporter.error(definition->type->location, "This must be a type, not {}", TypeWithArticle{definition->type->type});
			if (types_match(definition->type->type, compiler->builtin_overload_set)) {
				assert(definition->type->kind == Ast_Identifier);
				auto ident = (AstIdentifier *)definition->type;
				for (auto def : ident->possible_definitions) {
					state->reporter.info(def->location, "");
				}
			}
			// definition->type = type_unknown;
			yield(TypecheckResult::fail);
		}
		//if (definition_origin != DefinitionOrigin::return_parameter && (!definition->expression || !is_type(definition->expression)) && get_size(definition->type) == 0) {
		//	if (!types_match(definition->type, compiler->builtin_unsized_integer) && !types_match(definition->type, compiler->builtin_unsized_float)) {
		//		state->reporter.error(definition->location.data ? definition->location : definition->type->location, "Defining a variable with 0 size is not allowed. The type is {}.", type_to_string(definition->type));
		//		yield(TypecheckResult::fail);
		//	}
		//}
	}

	if (definition->is_constant) {
		if (state->container_node == 0 || state->container_node->kind != Ast_Enum) {
			if (definition->expression) {
				if (!is_type(definition->expression) && definition->expression->kind != Ast_Lambda && is_concrete_type(definition->type)) {
					evaluate_and_put_definition_in_section(state, definition, compiler->constant_section);
				}
			}
		}
	} else {
		switch (location) {
			case Global: {
				if (definition->expression) {
					if (auto lambda = as<AstLambda>(definition->expression)) {
						auto offset = compiler->data_section.w8(0);
						definition->offset = offset;
						compiler->data_section.relocations.add(Relocation{
							.section = SectionKind::code,
							.offset = offset,
							.lambda = lambda,
						});
						break;
					}
				}

				auto Struct = direct_as<AstStruct>(definition->type);
				if (definition->expression && !is_type(definition->expression) && definition->expression->kind != Ast_Lambda) {
					evaluate_and_put_definition_in_section(state, definition, compiler->data_section);
				} else if (Struct && Struct->default_value) {
					definition->expression = deep_copy(raw(Struct->default_value));
					evaluate_and_put_definition_in_section(state, definition, compiler->data_section);
				} else {
					// NOTE: get_align and get_size may yield, so they should happen before modifying compiler->zero_section_size
					auto align = get_align(state, definition->type);
					auto size = get_size(state, definition->type);

					definition->offset = ceil(compiler->zero_section_size, align);
					compiler->zero_section_size = definition->offset + size;
				}
				break;
			}
			case Lambda: {
				if (definition_origin == DefinitionOrigin::local) {
					assert(state->container_node->kind == Ast_Lambda);
					set_local_offset(state, definition, (AstLambda *)state->container_node);
				}
				break;
			}
		}
	}

	if (definition->has_using)
		state->current_scope->usings.add({0, definition});
}

AstStatement *typecheck(TypecheckState *state, AstDefinition *definition) {
	typecheck_definition(state, definition);
	return definition;
}
AstStatement *typecheck(TypecheckState *state, AstReturn *Return) {
	if (!Return->lambda) {
		assert(state->container_node->kind == Ast_Lambda);
		Return->lambda = (AstLambda *)state->container_node;
		assert(Return->lambda->original_poly);
	}
	auto lambda = Return->lambda;

	auto &expression = Return->expression;
	if (expression) {
		typecheck(state, expression);

		if (lambda->return_parameter) {
			if (!implicitly_cast(state, &state->reporter, &expression, lambda->return_parameter->type))
				yield(TypecheckResult::fail);
		}
	} else {
		if (lambda->return_parameter) {
			if (!types_match(lambda->return_parameter->type, compiler->builtin_void) && lambda->return_parameter->name == "") {
				state->reporter.warning(Return->location,
					"No return expression provided for an unnamed return parameter. "
					"In this case the default value will be returned. "
					"To remove this warning you can either provide a return expression or give the return parameter a name.");
			}
		}
	}
	lambda->return_statements.add(Return);
	return Return;
}
AstStatement *typecheck(TypecheckState *state, AstYield *Yield) { not_implemented(); }
AstStatement *typecheck(TypecheckState *state, AstWhile *While) {
	typecheck(state, While->condition);
	if (!implicitly_cast(state, &state->reporter, &While->condition, compiler->builtin_bool.ident)) {
		yield(TypecheckResult::fail);
	}

	scoped_replace(state->current_loop, While);
	typecheck(state, While->scope);
	return While;
}
AstStatement *typecheck(TypecheckState *state, AstFor *For) {
	// FIXME: Copypasted 6 times? Not bad.

	typecheck(state, For->range);

	Reporter reporter;

	assert(state->container_node->kind == Ast_Lambda);
	auto lambda = (AstLambda *)state->container_node;

	if (For->by_pointer) {
		if (implicitly_cast(state, &reporter, &For->range, compiler->builtin_range.ident)) {
			not_implemented();
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_range"str;
			range_definition->container_node = state->container_node;
			range_definition->expression = For->range;
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("min"str));
			replacement_block->scope->add(raw(it1));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("max"str)));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_identifier(it1->name);
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		} else if (auto subtype = get_span_subtype(For->range->type); subtype || types_match(For->range->type, compiler->builtin_string)) {
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_span"str;
			range_definition->container_node = state->container_node;
			range_definition->expression = For->range;
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("data"str));
			replacement_block->scope->add(raw(it1));

			auto end = AstDefinition::create();
			end->name = "_end"str;
			end->container_node = state->container_node;
			end->expression = make_binop(BinaryOperation::add,
				make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("data"str)),
				make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("count"str))
			);
			replacement_block->scope->add(raw(end));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_identifier(end->name));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_identifier(it1->name);
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		} else if (auto arr = direct_as<AstArray>(For->range->type)) {
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_arr"str;
			range_definition->container_node = state->container_node;
			if (is_addressable(For->range)) {
				range_definition->expression = make_address_of(&reporter, For->range);
			} else {
				range_definition->expression = For->range;
			}
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_unary(UnaryOperation::address_of, make_subscript(make_unary(UnaryOperation::star, make_identifier(range_definition->name)), make_integer(0ll)));
			replacement_block->scope->add(raw(it1));

			auto end = AstDefinition::create();
			end->name = "_end"str;
			end->container_node = state->container_node;

			// FIXME: we already know array count, no need for this
			end->expression = make_binop(BinaryOperation::add,
				make_identifier(it1->name),
				make_binop(BinaryOperation::dot, make_unary(UnaryOperation::star, make_identifier(range_definition->name)), make_identifier("count"str))
			);
			replacement_block->scope->add(raw(end));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_identifier(end->name));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_identifier(it1->name);
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		}
	} else {
		if (implicitly_cast(state, &reporter, &For->range, compiler->builtin_range.ident)) {
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_range"str;
			range_definition->container_node = state->container_node;
			range_definition->expression = For->range;
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("min"str));
			replacement_block->scope->add(raw(it1));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("max"str)));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_identifier(it1->name);
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		} else if (auto subtype = get_span_subtype(For->range->type); subtype || types_match(For->range->type, compiler->builtin_string)) {
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_span"str;
			range_definition->container_node = state->container_node;
			range_definition->expression = For->range;
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("data"str));
			replacement_block->scope->add(raw(it1));

			auto end = AstDefinition::create();
			end->name = "_end"str;
			end->container_node = state->container_node;
			end->expression = make_binop(BinaryOperation::add,
				make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("data"str)),
				make_binop(BinaryOperation::dot, make_identifier(range_definition->name), make_identifier("count"str))
			);
			replacement_block->scope->add(raw(end));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_identifier(end->name));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_unary(UnaryOperation::star, make_identifier(it1->name));
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		} else if (auto arr = direct_as<AstArray>(For->range->type)) {
			auto replacement_block = AstBlock::create();
			replacement_block->scope->parent = state->current_scope;

			auto range_definition = AstDefinition::create();
			range_definition->name = "_arr"str;
			range_definition->container_node = state->container_node;
			if (is_addressable(For->range)) {
				range_definition->expression = make_address_of(&reporter, For->range);
			} else {
				range_definition->expression = For->range;
			}
			range_definition->type = range_definition->expression->type;
			set_local_offset(state, range_definition, lambda);
			replacement_block->scope->add(raw(range_definition));

			auto it1 = AstDefinition::create();
			it1->name = "_it"str;
			it1->container_node = state->container_node;
			it1->expression = make_unary(UnaryOperation::address_of, make_subscript(make_unary(UnaryOperation::star, make_identifier(range_definition->name)), make_integer(0ll)));
			replacement_block->scope->add(raw(it1));

			auto end = AstDefinition::create();
			end->name = "_end"str;
			end->container_node = state->container_node;
			// FIXME: we already know array count, no need for this
			end->expression = make_binop(BinaryOperation::add,
				make_identifier(it1->name),
				make_binop(BinaryOperation::dot, make_unary(UnaryOperation::star, make_identifier(range_definition->name)), make_identifier("count"str))
			);
			replacement_block->scope->add(raw(end));

			auto While = AstWhile::create();
			While->condition = make_binop(BinaryOperation::lt, make_identifier(it1->name), make_identifier(end->name));
			While->scope->parent = replacement_block->scope;
			replacement_block->scope->add(While);

			auto it2 = AstDefinition::create();
			it2->name = For->iterator_name;
			it2->container_node = state->container_node;
			it2->expression = make_unary(UnaryOperation::star, make_identifier(it1->name));
			While->scope->add(raw(it2));

			auto inner = AstBlock::create();
			inner->scope = For->scope;
			inner->scope->parent = While->scope;
			While->scope->add(make_statement(inner));

			auto inc = make_binop(BinaryOperation::addass, make_identifier(it1->name), make_integer(1ll));
			While->scope->add(make_statement(inc));

			typecheck(state, replacement_block);
			return make_statement(replacement_block);
		}
	}
	state->reporter.error(For->range->location, "Expressions of type {} are not iterable. Right now you can iterate over ranges (e.g. 0..5), spans, strings and arrays", type_to_string(For->range->type));
	yield(TypecheckResult::fail);
	return 0;
}
AstStatement *typecheck(TypecheckState *state, AstExpressionStatement *ExpressionStatement) {
	typecheck(state, ExpressionStatement->expression);
	return ExpressionStatement;
}
AstStatement *typecheck(TypecheckState *state, AstAssert *Assert) {

	auto test = as<AstTest>(Assert->condition);

	if (!test) {
		if (auto unop = as<AstUnaryOperator>(Assert->condition))
			test = as<AstTest>(unop->expression);
	}

	typecheck(state, Assert->condition);

	if (!implicitly_cast(state, &state->reporter, &Assert->condition, compiler->builtin_bool.Struct))
		yield(TypecheckResult::fail);

	if (Assert->is_constant) {
		auto result = evaluate(state, Assert->condition);
		if (!result) {
			yield(TypecheckResult::fail);
		}

		if (result->literal_kind != LiteralKind::boolean) {
			state->reporter.error(Assert->condition->location, "Expression must have bool type");
			yield(TypecheckResult::fail);
		}

		if (result->Bool == false) {
			if (test) {
				state->reporter.error(Assert->location, "Assertion failed: {}. #compiles reports:", Assert->message);
				state->reporter.reports.add(test->reports);
			} else {
				state->reporter.error(Assert->location, "Assertion failed: {}", Assert->message);
			}
			yield(TypecheckResult::fail);
		}
	}
	return Assert;
}
AstStatement *typecheck(TypecheckState *state, AstDefer *Defer) {
	typecheck(state, Defer->scope);
	return Defer;
}
AstStatement *typecheck(TypecheckState *state, AstPrint *Print) {
	typecheck(state, Print->expression);

	auto result = evaluate(state, Print->expression);
	if (!result) {
		yield(TypecheckResult::fail);
	}

	StringBuilder builder;
	append_literal(builder, result);

	state->reporter.info(Print->expression->location, "{}", builder);

	return Print;
}
AstStatement *typecheck(TypecheckState *state, AstOperatorDefinition *OperatorDefinition) {
	switch (OperatorDefinition->operation) {
		using enum BinaryOperation;
		case 'as': {
			auto old_lambda = OperatorDefinition->lambda;
			typecheck(state, (Expression<> &)OperatorDefinition->lambda);
			assert(OperatorDefinition->lambda == old_lambda);

			if (OperatorDefinition->is_implicit) {
				implicit_casts.add(OperatorDefinition->lambda);
				not_typechecked_implicit_casts_count -= 1;
			} else {
				typechecked_explicit_casts.add(OperatorDefinition->lambda);
			}
			break;
		}
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '^':
		case '&':
		case '|':
		case '<<':
		case '>>':
		case '==':
		case '!=':
		case '<':
		case '>':
		case '<=':
		case '>=': {
			typecheck(state, (Expression<> &)OperatorDefinition->lambda);

			if (OperatorDefinition->lambda->parameters.count == 1) {
				unary_operators.get_or_insert(as_unary_operation(OperatorDefinition->operation).value()).add({OperatorDefinition->lambda, OperatorDefinition});
				not_typechecked_unary_operators_count -= 1;
			} else {
				binary_operators.get_or_insert(as_binary_operation(OperatorDefinition->operation).value()).add({OperatorDefinition->lambda, OperatorDefinition});
				not_typechecked_binary_operators_count -= 1;
			}
			break;
		}
		case '?': {
			typecheck(state, (Expression<> &)OperatorDefinition->lambda);
			has_value_overloads.add(OperatorDefinition->lambda);
			not_typechecked_has_value_overloads_count -= 1;
			break;
		}
		default: invalid_code_path();
	}

	if (auto binop = as_binary_operation(OperatorDefinition->operation)) {

		auto name = get_binop_ident_name(binop.value());

		auto definition = AstDefinition::create();
		definition->name = name;
		definition->location = OperatorDefinition->location;
		definition->expression = OperatorDefinition->lambda;
		definition->is_constant = true;
		definition->type = OperatorDefinition->lambda->type;
		OperatorDefinition->lambda->definition = definition;

		state->current_scope->add(raw(definition));
	}

	return OperatorDefinition;
}
AstStatement *typecheck(TypecheckState *state, AstLoopControl *LoopControl) {
	if (!state->current_loop) {
		state->reporter.error(LoopControl->location, "Loop control statement may appear only inside loop body.");
		yield(TypecheckResult::fail);
	}
	LoopControl->loop = state->current_loop;
	return LoopControl;
}
AstStatement *typecheck(TypecheckState *state, AstParse *Parse) {
	return Parse;
}
AstStatement *typecheck(TypecheckState *state, AstUsing *Using) {
	typecheck(state, Using->expression);

	if (Using->expression->kind != Ast_Identifier) {
		state->reporter.error(Using->location, "Only identifiers can be used in `using` statments.");
		yield(TypecheckResult::fail);
	}
	auto Ident = (AstIdentifier *)Using->expression;

	auto Struct = direct_as<AstStruct>(Ident->type);
	if (!Struct) {
		state->reporter.error(Using->location, "Using can be applied to structs.");
		yield(TypecheckResult::fail);
	}

	state->current_scope->usings.add({Using, Ident->definition()});
	return Using;
}
void typecheck(TypecheckState *state, AstStatement *&statement) {
	defer { atomic_add(&shared_progress, 1); };
	scoped_replace(state->current_node, statement);
	switch (statement->kind) {
		case Ast_Return:              statement = typecheck(state, (AstReturn              *)statement); break;
		case Ast_Yield:               statement = typecheck(state, (AstYield               *)statement); break;
		case Ast_Definition:          statement = typecheck(state, (AstDefinition          *)statement); break;
		case Ast_While:               statement = typecheck(state, (AstWhile               *)statement); break;
		case Ast_For:                 statement = typecheck(state, (AstFor                 *)statement); break;
		case Ast_ExpressionStatement: statement = typecheck(state, (AstExpressionStatement *)statement); break;
		case Ast_Assert:              statement = typecheck(state, (AstAssert              *)statement); break;
		case Ast_Defer:               statement = typecheck(state, (AstDefer               *)statement); break;
		case Ast_Print:               statement = typecheck(state, (AstPrint               *)statement); break;
		case Ast_OperatorDefinition:  statement = typecheck(state, (AstOperatorDefinition  *)statement); break;
		case Ast_LoopControl:         statement = typecheck(state, (AstLoopControl         *)statement); break;
		case Ast_Parse:               statement = typecheck(state, (AstParse               *)statement); break;
		case Ast_Using:               statement = typecheck(state, (AstUsing               *)statement); break;
		default: {
			state->reporter.error(statement->location, "INTERNAL ERROR: Invalid statement kind: {}", statement->kind);
			yield(TypecheckResult::fail);
		}
	}
}

bool is_hardenable(AstExpression *type) {
	if (types_match(type, compiler->builtin_unsized_integer) ||
		types_match(type, compiler->builtin_unsized_float) ||
		types_match(type, compiler->builtin_unknown_enum) ||
		types_match(type, compiler->builtin_deferred_if))
	{
		return true;
	}
	return false;
}

AstExpression *set_common_return_type(TypecheckState *state, Span<AstExpression **> expressions) {
	if (expressions.count == 0)
		return compiler->builtin_void.ident;

	auto void_if_unreachable = [&](AstExpression *type) -> AstExpression * {
		if (types_match(type, compiler->builtin_unreachable))
			return compiler->builtin_void.ident;
		return type;
	};

	if (expressions.count == 1) {
		make_concrete(state, expressions.data[0]);
		return void_if_unreachable((*expressions.data[0])->type);
	}

	struct Concrete {
		AstExpression **expression = 0;
		int count_of_expressions_castable_to_this = 0;
	};

	List<Concrete> concretes;
	concretes.allocator = temporary_allocator;

	List<AstExpression **> inconcretes;
	inconcretes.allocator = temporary_allocator;

	for (auto _expression : expressions) {
		auto &expression = *_expression;
		if (is_concrete_type(expression->type)) {
			concretes.add({_expression});
		} else {
			inconcretes.add(_expression);
		}
	}

	if (concretes.count == 0) {
		for (umm i = 0; i < inconcretes.count; ++i) {
			if (is_hardenable((*inconcretes[i])->type)) {
				auto &expression = *inconcretes[i];

				make_concrete(state, &expression);
				//if (!is_concrete_type(expression->type)) {
				//	state->reporter.error(expression->location, "Could not make concrete type for this expression.");
				//	yield(TypecheckResult::fail);
				//}

				inconcretes.erase_at(i);
				concretes.add({&expression});
				break;
			}
		}
	}

	assert(concretes.count);

	for (auto &target : concretes) {
		for (auto &other : concretes) {
			if (&target == &other)
				continue;

			if (implicitly_cast(state, 0, other.expression, (*target.expression)->type, 0, false)) {
				target.count_of_expressions_castable_to_this += 1;
			}
		}
	}

	List<AstExpression **> matches;
	matches.allocator = temporary_allocator;

	for (auto &concrete : concretes) {
		if (concrete.count_of_expressions_castable_to_this == concretes.count - 1) {
			matches.add(concrete.expression);
		}
	}

	if (matches.count == 0) {

		// FIXME: location isn't great
		state->reporter.error((*expressions[0])->location, "Could not deduce common return type.");

		yield(TypecheckResult::fail);
	}

	auto match = matches[0];
	for (auto other : matches.skip(1)) {
		if (!types_match((*match)->type, (*other)->type)) {
			state->reporter.error((*other)->location, "Ambiguous common return type. {} deduced here:", type_to_string((*other)->type));
			state->reporter.info((*match)->location, "And {} deduced here:", type_to_string((*match)->type));
			yield(TypecheckResult::fail);
		}
	}

	for (auto &other : concretes) {
		if (other.expression == match)
			continue;

		if (!implicitly_cast(state, &state->reporter, other.expression, (*match)->type)) {
			state->reporter.error((*other.expression)->location, "INTERNAL ERROR: implicit cast of this expression to {} should have succeeded", type_to_string((*match)->type));
			yield(TypecheckResult::fail);
		}
	}

	for (auto &_inconcrete : inconcretes) {
		auto &inconcrete = *_inconcrete;

		if (types_match(inconcrete->type, compiler->builtin_unreachable))
			continue;

		if (!implicitly_cast(state, &state->reporter, &inconcrete, (*match)->type)) {
			yield(TypecheckResult::fail);
		}
	}

	return (*match)->type;
}

void typecheck_body(TypecheckState *state, AstLambda *lambda) {
	assert(!lambda->is_poly);

	bool deferred_function_name = true;

	if (lambda->body) {
		if (lambda->return_parameter) {
			deferred_function_name = false;
			lambda->type_name = type_to_string(lambda->type);
		} else {
			lambda->type_name = "undefined"str;
		}
		typecheck(state, lambda->body);
		make_concrete(state, &lambda->body);
	}

	if (lambda->return_parameter) {
		for (auto ret : lambda->return_statements) {
			if (ret->expression) {
				if (!implicitly_cast(state, &state->reporter, &ret->expression, lambda->return_parameter->type)) {
					yield(TypecheckResult::fail);
				}
			}
		}

		if (lambda->body) {
			if (auto block = as<AstBlock>(lambda->body)) {
				if (block->scope->statement_list.count) {
					auto last = block->scope->statement_list.back();
					if (auto est = as<AstExpressionStatement>(last)) {
						if (!types_match(est->expression->type, compiler->builtin_void) && !types_match(lambda->return_parameter->type, compiler->builtin_void)) {
							if (!implicitly_cast(state, &state->reporter, &est->expression, lambda->return_parameter->type)) {
								yield(TypecheckResult::fail);
							}
							block->type = est->expression->type;
						}
					}
				}
			} else {
				if (!implicitly_cast(state, &state->reporter, &lambda->body, lambda->return_parameter->type)) {
					yield(TypecheckResult::fail);
				}
			}
		}
	} else {
		List<AstExpression **> return_expressions;
		return_expressions.allocator = temporary_allocator;

		return_expressions.reserve(lambda->return_statements.count + 1);

		if (lambda->body)
			return_expressions.add(&lambda->body);

		for (auto ret : lambda->return_statements) {
			if (ret->expression) {
				return_expressions.add(&ret->expression);
			}
		}

		auto return_type = set_common_return_type(state, return_expressions);
		lambda->return_parameter = state->make_retparam(return_type, lambda);
	}

	if (lambda->body) {
		compiler->lambdas_with_body.add(lambda);
	} else {
		compiler->lambdas_without_body.add(lambda);
	}

	// NOTE: if this string is used in compile time expressions before lambda's type is determined,
	// it will evaluate to a default string, which may be unexpected.
	if (deferred_function_name) {
		auto type_name = type_to_string(lambda->type);
		for (auto fd : lambda->function_directives) {
			fd->string.set(type_name);
		}
	}
}

struct MatchTemplatesResult {
	bool implicit_address_of = false;
	bool implicit_dereference = false;
	AstExpression *mismatch_poly = 0;
	AstExpression *mismatch_matching = 0;
};

bool match_templates(TypecheckState *state, Reporter *reporter, AstLambda *hardened_lambda, AstDefinition *matching_parameter, AstExpression *template_expression, AstExpression *matching_expression, MatchTemplatesResult *result) {
	switch (template_expression->kind) {
		case Ast_Identifier: {
			auto p = (AstIdentifier *)template_expression;

			KeyValue<KeyString, DefinitionList> *found = 0;
			if (hardened_lambda)
				found = hardened_lambda->constant_scope->definition_map.find(p->name);
			if (found) {
				auto definiton = found->value[0];
				assert(definiton->container_node);
				assert(definiton->container_node == hardened_lambda);
				assert(definiton->parent_scope == hardened_lambda->constant_scope);

				p->possible_definitions.set(definiton);

				if (definiton->expression) {
					if (types_match(definiton->expression, matching_expression)) {
						return true;
					} else {
						reporter->error(matching_expression->location, "Type {} does not match {}.", type_to_string(matching_expression), type_to_string(definiton->expression));
						break;
					}
				} else {
					definiton->expression = get_concrete_type(matching_expression);
					definiton->type = definiton->expression->type;
					return true;
				}
			} else {
				typecheck(state, template_expression);
				if (auto template_struct = direct_as<AstStruct>(template_expression)) {
					if (template_struct->is_template) {
						if (auto instantiated_struct = direct_as<AstStruct>(matching_expression)) {
							if (instantiated_struct->instantiated_from == template_struct) {
								if (hardened_lambda) {
									for (auto parameter : instantiated_struct->parameter_scope->definition_list) {
										auto definition = AstDefinition::create();
										definition->name = format("\\{}.{}"str, matching_parameter->name, parameter->name);
										definition->is_constant = true;
										definition->expression = parameter->expression;
										definition->type = definition->expression->type;
										definition->location = template_expression->location;
										definition->container_node = hardened_lambda;
										hardened_lambda->constant_scope->add(definition);
									}
								}
								return true;
							}
						}
					}
				}

				if (types_match(template_expression, matching_expression)) {
					return true;
				}
			}
			break;
		}
		case Ast_UnaryOperator: {
			auto p = (AstUnaryOperator *)template_expression;
			if (p->operation == UnaryOperation::star)
				p->operation = UnaryOperation::pointer;

			if (auto e = as<AstUnaryOperator>(matching_expression)) {
				if (p->operation == e->operation) {
					if (match_templates(state, reporter, hardened_lambda, matching_parameter, p->expression, e->expression, result))
						return true;
				}
			}

			break;
		}
		case Ast_Call: {
			auto call = (AstCall *)template_expression;
			if (auto matching_struct = direct_as<AstStruct>(matching_expression)) {
				typecheck(state, call->callable);
				if (auto poly_struct = direct_as<AstStruct>(call->callable)) {
					if (matching_struct->instantiated_from == poly_struct) {

						if (call->unsorted_arguments.count == poly_struct->parameter_scope->definition_list.count) {
							for (umm i = 0; i < call->unsorted_arguments.count; ++i) {
								if (!match_templates(state, reporter, hardened_lambda, matching_parameter, call->unsorted_arguments[i].expression, matching_struct->parameter_scope->definition_list[i]->expression, result)) {
									return false;
								}
							}
							return true;
						} else {
							reporter->error(call->location, "Not enough template arguments.");
						}
					}
				}
			}
			break;
		}
		case Ast_Literal: {
			auto p = (AstLiteral *)template_expression;
			if (auto m = as<AstLiteral>(matching_expression)) {
				if (p->literal_kind == m->literal_kind) {
					switch (p->literal_kind) {
						case LiteralKind::integer: if (p->integer == m->integer) return true; break;
						case LiteralKind::Float: if (p->Float == m->Float) return true; break;
						case LiteralKind::boolean: if (p->Bool == m->Bool) return true; break;
						case LiteralKind::character: if (p->character == m->character) return true; break;
						case LiteralKind::string: if (p->string.get() == m->string.get()) return true; break;
					}
				}
			}
			break;
		}
	}

	// Allow implicit dereferencing
	if (auto e = as<AstUnaryOperator>(matching_expression); e && e->operation == UnaryOperation::pointer) {
		if (match_templates(state, reporter, hardened_lambda, matching_parameter, template_expression, e->expression, result)) {
			if (result) {
				result->implicit_dereference = true;
			}
			return true;
		}
	}

	// Allow implicit address of
	if (auto e = as<AstUnaryOperator>(template_expression); e && e->operation == UnaryOperation::pointer) {
		if (match_templates(state, reporter, hardened_lambda, matching_parameter, e->expression, matching_expression, result)) {
			if (result) {
				result->implicit_address_of = true;
			}
			return true;
		}
	}

	if (result) {
		result->mismatch_poly = template_expression;
		result->mismatch_matching = matching_expression;
	}
	return false;
}

struct StringizePolyTypes {
	AstLambda *lambda;
};

umm append(StringBuilder &builder, StringizePolyTypes s) {
	// NOTE: statements are ordered, but definitions are not.
	umm result = 0;
	for (auto statement : s.lambda->constant_scope->statement_list) {
		assert(statement->kind == Ast_Definition);
		auto definition = (AstDefinition *)statement;
		result += append_format(builder, "\n                                {} = {}", definition->name, direct(definition->expression));
	}
	return result;
}

struct Overload {
	AstExpression *type = {};
	AstDefinition *definition = {};
	List<NamedArgument> unsorted_arguments;

	Reporter reporter;
	bool success = false;
	AstLambda *lambda = 0;
	AstLambda *instantiated_lambda = 0;
	int distance = 0;

	// Outer list corresponds to lambda parameters.
	// Inner list corresponds to a parameter pack.
	// For example:
	//     foo :: (a: Int, b: ..String, c: Float)
	//     foo(1, "hello", "world", 2)
	//     sorted_arguments = [1, ["hello", "world"], 2]
	List<List<AstExpression *>> sorted_arguments_lambda;

	// Structs can't have packs, so theres no need in inner list.
	List<AstExpression *> sorted_arguments_struct;

	AstStruct *Struct = 0;
	AstExpression *callable = {};

	AstDistinct *Distinct = 0;
};

// Right now these copy everything.
// Maybe copy only nodes that depend on a poly parameter?
AstLambda *instantiate_head(TypecheckState *state, Reporter *reporter, Overload &overload, Span<NamedArgument> unsorted_arguments, AstLambda *lambda, String call_location, bool *success) {

	*success = false;

	// NOTE: We have to create a context for parameter typechecking, no way to get rid of that.
	auto hardened_lambda = AstLambda::create();
	auto hardened_lambda_definition = AstDefinition::create();

	assert(lambda->definition); // TODO: deal with unnamed poly lambdas
	hardened_lambda_definition->expression = hardened_lambda;
	hardened_lambda_definition->location = hardened_lambda->location;
	hardened_lambda_definition->name = lambda->definition->name;
	hardened_lambda_definition->is_constant = true;
	hardened_lambda_definition->container_node = lambda->definition->container_node;
	hardened_lambda_definition->parent_scope = lambda->definition->parent_scope;
	hardened_lambda->definition = hardened_lambda_definition;

	hardened_lambda->is_poly = false;
	hardened_lambda->convention = lambda->convention;
	hardened_lambda->extern_language = lambda->extern_language;
	hardened_lambda->extern_library = lambda->extern_library;
	hardened_lambda->finished_typechecking_head = false;
	hardened_lambda->is_intrinsic = lambda->is_intrinsic;
	hardened_lambda->is_type = lambda->is_type;
	hardened_lambda->location = lambda->location;
	hardened_lambda->extern_library = lambda->extern_library;
	hardened_lambda->print_bytecode = lambda->print_bytecode;
	hardened_lambda->type = create_lambda_type(hardened_lambda);
	hardened_lambda->type->type = compiler->builtin_type.ident;
	hardened_lambda->outer_scope()->parent = lambda->outer_scope()->parent;
	hardened_lambda->original_poly = lambda;

	for (auto &parameter : lambda->parameters) {
		auto new_parameter = deep_copy(parameter);
		new_parameter->container_node = hardened_lambda;
		hardened_lambda->parameters.add(new_parameter);
		hardened_lambda->parameter_scope->add(new_parameter);
	}


	for (auto definition : lambda->constant_scope->definition_list) {
		auto copied = deep_copy(definition);
		copied->container_node = hardened_lambda;
		hardened_lambda->constant_scope->add(copied);
	}

	auto &sorted_arguments = overload.sorted_arguments_lambda;

	auto match_poly_param = [&](AstDefinition *parameter, AstExpression *argument) {
		MatchTemplatesResult result;
		if (!match_templates(state, reporter, hardened_lambda, parameter, parameter->parsed_type, argument->type, &result)) {
			reporter->error(argument->location, "Could not match the type {} to {}", type_to_string(argument->type), type_to_string(parameter->parsed_type));
			reporter->info(parameter->location, "Because {} doesn't match {}", result.mismatch_matching, result.mismatch_poly);
			return false;
		}

		parameter->type = get_concrete_type(argument->type);

		if (result.implicit_address_of)
			parameter->type = make_pointer_type(parameter->type);
		else if (result.implicit_dereference)
			parameter->type = as_pointer(parameter->type)->expression;

		parameter->is_poly = false;

		return true;
	};

	// Assign named arguments to corresponding parameters

	List<AstExpression *> remaining_arguments;


	for (auto &argument : unsorted_arguments) {
		if (argument.name.is_empty()) {
			remaining_arguments.add(argument.expression);
			continue;
		}

		auto found_parameter = find_if(lambda->parameters, [&](AstDefinition *parameter) { return parameter->name == argument.name; });
		if (!found_parameter) {
			reporter->error(call_location, "Lambda does not take argument named '{}'.", argument.name);
			*success = false;
			return 0;
		}

		auto parameter_index = index_of(lambda->parameters, found_parameter);
		auto parameter = hardened_lambda->parameters[parameter_index];

		if (parameter->is_constant) invalid_code_path("Not implemented");

		if (sorted_arguments[parameter_index].count) {
			reporter->error(argument.expression->location, "Argument '{}' was already assigned.", argument.name);
			*success = false;
			return 0;
		}

		// FIXME: MAKE SURE TYPES MATCH!!!

		sorted_arguments[parameter_index].set(argument.expression);

		if (parameter->is_poly) {
			if (!match_poly_param(parameter, argument.expression))
				return 0;
		}

		if (parameter->is_constant) {
			parameter->expression = argument.expression;
		}
	}


	// Now that named arguments are assigned, assign unnamed arguments
	{
		umm parameter_index = 0;

		while (1) {
			if (parameter_index >= sorted_arguments.count)
				break;

			if (sorted_arguments[parameter_index].count == 0)
				break;

			++parameter_index;
		}

		// Returns
		//     true if there is an unassigned parameter
		//     false if there is no parameters remaining
		auto next_unassigned_parameter = [&] {
			while (1) {
				++parameter_index;
				if (parameter_index >= sorted_arguments.count)
					return false;

				if (sorted_arguments[parameter_index].count == 0)
					return true;
			}
		};

		for (auto &argument : remaining_arguments) {
		retry_argument:
			if (parameter_index >= sorted_arguments.count) {
				reporter->error(call_location, "Too many arguments.");
				*success = false;
				return 0;
			}

			auto parameter = hardened_lambda->parameters[parameter_index];

			if (parameter->is_constant) {
				if (!is_constant(argument)) {
					reporter->error(argument->location, "Argument is not constant");
					return 0;
				}
			}

			if (!parameter->is_poly && !parameter->type) {
				scoped_replace(state->container_node, hardened_lambda);
				scoped_replace(state->current_scope, hardened_lambda->parameter_scope);
				typecheck(state, parameter->parsed_type);
				if (!parameter->parsed_type->type)
					yield(TypecheckResult::fail);

				parameter->type = parameter->parsed_type;

				if (parameter->expression) {
					typecheck(state, parameter->expression);
					if (!parameter->expression->type)
						yield(TypecheckResult::fail);
				}
			}

			if (parameter->is_poly) {
				if (parameter->is_pack) invalid_code_path("Not implemented");

				if (!match_poly_param(parameter, argument))
					return 0;

				assert(sorted_arguments[parameter_index].count == 0);
				sorted_arguments[parameter_index].set(argument);

				if (parameter->has_using) {
					hardened_lambda->parameter_scope->usings.add(UsingAndDefinition{.definition = parameter});
				}

				overload.distance += argument_distance({.type_distance = TypeDistance::Template, .const_distance = is_constant(argument) && !parameter->is_constant});
			} else {
				if (parameter->is_pack) {
					assert(!parameter->is_constant, "Not implemented");

					auto subtype = get_span_subtype(parameter->type);
					assert(subtype);

					if (implicitly_cast(state, reporter, &argument, subtype, 0, false)) {
						sorted_arguments[parameter_index].add(argument);
						continue;
					} else {
						next_unassigned_parameter();
						goto retry_argument;
					}
				} else {
					auto distance = TypeDistance::exact;
					if (!implicitly_cast(state, reporter, &argument, parameter->type, &distance, false)) {
						*success = false;
						return 0;
					}

					assert(sorted_arguments[parameter_index].count == 0);
					sorted_arguments[parameter_index].set(argument);
					overload.distance += argument_distance({.type_distance = distance, .const_distance = is_constant(argument) && !parameter->is_constant});
				}
			}

			if (parameter->is_constant) {
				parameter->expression = argument;
			}

			next_unassigned_parameter();
		}

		// Assign default values to everything that is left over.
		for (umm i = 0; i != sorted_arguments.count; ++i) {
			auto &args = sorted_arguments[i];
			auto parameter = hardened_lambda->parameters[i];

			if (parameter->expression && args.count == 0) {
				if (parameter->is_constant) {
					typecheck(state, parameter->parsed_type);
					if (!parameter->parsed_type)
						yield(TypecheckResult::fail);

					parameter->type = parameter->parsed_type;

					typecheck(state, parameter->expression);
					if (!implicitly_cast(state, reporter, &parameter->expression, parameter->type))
						return 0;

					if (!is_constant(parameter->expression)) {
						reporter->error(parameter->expression->location, "Argument is not constant");
						return 0;
					}
				}
				auto copied = deep_copy(parameter->expression);
				args.add(copied);
			}
		}


		// Make sure argument count matches parameter count
		for (umm i = 0; i != sorted_arguments.count; ++i) {
			auto args = sorted_arguments[i];
			auto param = hardened_lambda->parameters[i];

			if (args.count == 0 && !param->is_pack) {
				reporter->error(call_location, "Not enough arguments. Value for {} was not provided", param->name);
				*success = false;
				return 0;
			}
		}
	}


	for (auto &parameter : hardened_lambda->parameters) {
		assert(parameter->parent_scope);
	}

	scoped_replace(state->container_node, hardened_lambda);
	scoped_replace(state->current_scope, hardened_lambda->parameter_scope);

	if (compiler->debug_template) {
		immediate_info(call_location, "Instantiated poly head: {} with {}", type_to_string(lambda->type), StringizePolyTypes{hardened_lambda});
	}

	if (lambda->return_parameter) {
		hardened_lambda->return_parameter = deep_copy(raw(lambda->return_parameter));
		hardened_lambda->parameter_scope->add(raw(hardened_lambda->return_parameter));
		typecheck(state, hardened_lambda->return_parameter);
	}

	hardened_lambda->finished_typechecking_head = true;

	*success = true;

	for (auto &cached : lambda->cached_instantiations) {
		for (umm i = 0; i < hardened_lambda->constant_scope->definition_list.count; ++i) {
			if (!are_equal(evaluate(state, hardened_lambda->constant_scope->definition_list[i]), evaluate(state, cached.lambda->constant_scope->definition_list[i]))) {
				goto next_cached;
			}
		}
		for (umm i = 0; i < hardened_lambda->parameters.count; ++i) {
			if (hardened_lambda->parameters[i]->is_constant) {
				if (!are_equal(evaluate(state, hardened_lambda->parameters[i]), evaluate(state, cached.lambda->parameters[i]))) {
					goto next_cached;
				}
			}
		}
		// FIXME: hardened_lambda leaked
		return cached.lambda;
	next_cached:;
	}

	lambda->cached_instantiations.add({.lambda=hardened_lambda, .definition=hardened_lambda_definition});
	return hardened_lambda;
}
void instantiate_body(TypecheckState *state, AstLambda *hardened_lambda) {
	auto original_lambda = hardened_lambda->original_poly;

	if (!original_lambda) {
		immediate_error(hardened_lambda->location, "INTERNAL ERROR: Attempt to instantiate non-template lambda's body.");
		yield(TypecheckResult::fail);
	}


	if (hardened_lambda->was_instantiated)
		return;

	hardened_lambda->was_instantiated = true;

	if (compiler->debug_template) {
		state->reporter.info(hardened_lambda->location, "Instantiating poly body: {} with {}", type_to_string(original_lambda->type), StringizePolyTypes{hardened_lambda});
	}

	{
		scoped_replace(state->container_node, hardened_lambda);
		scoped_replace(state->current_scope, hardened_lambda->parameter_scope);

		for (auto &definition : hardened_lambda->constant_scope->definition_list) {
			assert(definition->is_constant);
			if (!is_type(definition->expression)) {
				evaluate_and_put_definition_in_section(state, definition, compiler->constant_section);
			}
			int stupid_debugger = 1;
		}

		//deep_copy(hardened_lambda->body_scope, original_lambda->body_scope);
		hardened_lambda->body = deep_copy(original_lambda->body);
		for_each_top_scope(hardened_lambda->body, [&] (Scope *scope) {
			scope->parent = hardened_lambda->parameter_scope;
		});

		visit(hardened_lambda->body, Combine {
			[&](AstNode *){},
			[&](AstDefinition *definition){
				if (!definition->container_node)
					definition->container_node = get_container_node(definition);
			},
		});

		typecheck_body(state, hardened_lambda);

		calculate_parameters_size(hardened_lambda);
	}

	hardened_lambda->return_parameter->container_node = hardened_lambda;

	visit(hardened_lambda, Combine{
		[&]	(AstNode *) {},
		[&]	(AstDefinition *Definition) {
			if (Definition->parent_scope->node && Definition->parent_scope->node->kind == Ast_Struct) {
				assert(Definition->container_node);
				assert(Definition->container_node->kind == Ast_Struct);
			}
		}
	});
}

Optional<SmallList<AstExpression *>> sort_arguments(Reporter *reporter, AstCall *call, DefinitionList parameters) {
	auto unsorted_arguments = call->unsorted_arguments;
	SmallList<AstExpression *> sorted_arguments;
	sorted_arguments.resize(unsorted_arguments.count);
	for (int argument_index = 0; argument_index < unsorted_arguments.count; ++argument_index) {
		auto &argument = unsorted_arguments[argument_index];
		if (!argument.name.is_empty()) {
			bool found = false;
			for (int parameter_index = 0; parameter_index < parameters.count; ++parameter_index) {
				auto &parameter = parameters[parameter_index];
				if (argument.name == parameter->name) {
					sorted_arguments[parameter_index] = argument.expression;
					found = true;
					break;
				}
			}
			if (!found) {
				reporter->error(argument.expression->location, "'{}' does not have a parameter named '{}'.", call->callable->location, argument.name);
				return {};
			}
		}
	}
	for (auto &arg : unsorted_arguments) {
		if (arg.name.is_empty()) {
			for (auto &sorted_arg : sorted_arguments) {
				if (!sorted_arg) {
					sorted_arg = arg.expression;
					break;
				}
			}
		}
	}
	return sorted_arguments;
}

struct BinaryTypecheckerKey {
	BinaryOperation operation;
	AstExpression *left_type;
	AstExpression *right_type;

	inline constexpr auto operator<=>(BinaryTypecheckerKey const &) const = default;
};

template <>
umm get_hash(BinaryTypecheckerKey const &key) {
	return get_hash(key.operation) ^ get_hash(key.left_type) ^ get_hash(key.right_type);
}

inline umm append(StringBuilder &builder, BinaryTypecheckerKey k) {
	return append_format(builder, "{} {} {}", type_to_string(k.left_type), k.operation, type_to_string(k.right_type));
}

Map<BinaryTypecheckerKey, AstExpression *(*)(TypecheckState *, AstBinaryOperator *)> binary_typecheckers;

// :span hack:
// FIXME: this is just hardcoded template.
AstIdentifier *instantiate_span(AstExpression *subtype, String location) {
	auto &instantiation = compiler->span_instantiations.get_or_insert(subtype);
	if (!instantiation) {
		auto instantiation_definition = AstDefinition::create();
		instantiation = AstStruct::create();

		instantiation_definition->expression = instantiation;
		instantiation_definition->name = format("Span({})"str, type_to_string(subtype));
		instantiation_definition->parent_scope = &compiler->global_scope;
		instantiation_definition->is_constant = true;
		instantiation_definition->type = compiler->builtin_type.ident;

		instantiation->layout = StructLayout::tlang;
		instantiation->definition = instantiation_definition;
		instantiation->is_span = true;
		instantiation->alignment = compiler->stack_word_size;
		instantiation->size = compiler->stack_word_size * 2;
		instantiation->type = compiler->builtin_type.ident;

		auto data = AstDefinition::create();
		data->name = "data"str;
		data->type = make_pointer_type(subtype);
		data->offset = 0;
		data->container_node = instantiation;
		instantiation->member_scope->add(raw(data));
		instantiation->data_members.add(data);

		auto count = AstDefinition::create();
		count->name = "count"str;
		count->type = compiler->type_int;
		count->offset = compiler->stack_word_size;
		count->container_node = instantiation;
		instantiation->member_scope->add(raw(count));
		instantiation->data_members.add(count);
	}

	auto ident = AstIdentifier::create();
	ident->possible_definitions.set(instantiation->definition);
	ident->location = location;
	ident->type = instantiation->definition->type;
	ident->name = instantiation->definition->name;
	ident->directed = instantiation;
	return ident;
}

void get_overloads(TypecheckState *state, AstCall *call, List<Overload> &overloads) {
	switch (call->callable->kind) {
		case Ast_Identifier: {
			typecheck(state, call->callable);

		identifier_case:

			assert(call->callable->kind = Ast_Identifier);
			auto ident = (AstIdentifier *)call->callable;

			for (auto definition : ident->possible_definitions) {
				auto type = definition->type;
				if (definition->expression && (direct_as<AstStruct>(definition->expression) || direct_as<AstDistinct>(definition->expression)))
					type = definition->expression;

				overloads.add({
					.type = type,
					.definition = definition,
					.unsorted_arguments = copy(call->unsorted_arguments),
					.callable = call->callable,
				});
			}
			break;
		}
		case Ast_BinaryOperator: {
			auto binop = (AstBinaryOperator *)call->callable;

			// `foo` from `foo.bar()`
			AstExpression *probably_this = 0;

			// `bar` from `foo.bar()`
			AstExpression *member_call = 0;

			bool should_typecheck_binop = true;
			AstIdentifier *ident = 0;

			if (binop->operation == BinaryOperation::dot) {
				if (ident = as<AstIdentifier>(binop->right)) {

					typecheck(state, binop->left);

					if (auto Struct = direct_as<AstStruct>(binop->left->type); Struct && Struct->member_scope->definition_map.find(ident->name)) {
						should_typecheck_binop = true;
					} else {
						should_typecheck_binop = false;
					}
				}
			}

			if (should_typecheck_binop) {
				typecheck(state, call->callable);

				assert(call->callable->kind == Ast_BinaryOperator);
				binop = (AstBinaryOperator *)call->callable;

				if (binop->operation == BinaryOperation::dot) {
					if (auto ident = as<AstIdentifier>(binop->right)) {
						for (auto definition : ident->possible_definitions) {
							overloads.add({
								.type = definition->type,
								.definition = definition,
								.unsorted_arguments = copy(call->unsorted_arguments),
								.callable = call->callable,
							});
						}
						probably_this = binop->left;
						member_call = binop->right;
					}
				}
			} else {
				assert(ident);

				DefinitionList all_methods;
				for (auto scope = state->current_scope; scope; scope = scope->parent) {
					if (auto definitions = scope->definition_map.find(ident->name)) {
						for (auto &method : definitions->value) {
							if (method->is_constant && method->expression && method->expression->kind == Ast_Lambda) {
								auto lambda = (AstLambda *)method->expression;
								if (lambda->parameters.count >= 1) {
									all_methods.add(method);
								}
							}
						}
					}
				}

				DefinitionList ready_methods;

				wait_for(state, call->callable->location, "methods",
					[&] {
						//if (ident->name == "is_nan")
						//	debug_break();

						ready_methods.clear();

						for (auto &method : all_methods) {
							auto lambda = (AstLambda *)method->expression;
							if (!method->type) {
								return false;
							}

							if (lambda->is_poly) {
								if (lambda->parameters[0]->is_poly) {
									ready_methods.add(method);
									continue;
								} else {

									// NOTE: Template lambdas are never typechecked, so are their arguments.
									//       But we need to know the type of the first argument, so we have to do this ourselves.

									if (!lambda->parameters[0]->type) {
										scoped_replace(state->current_scope, lambda->parameter_scope);
										scoped_replace(state->container_node, lambda);
										typecheck(state, (AstStatement *&)lambda->parameters[0]);
									}
								}
							}

							auto this_type = direct(lambda->parameters[0]->type);

							if (!this_type)
								return false;

							if (!this_type->type)
								return false;

							if (implicitly_cast(state, 0, &binop->left, this_type, 0, false) ||
								is_pointer_to(this_type, binop->left->type)
							) {
								ready_methods.add(method);
							}
						}

						return true;
					},
					[&] {
						state->reporter.error(call->callable->location, "Member function {} was not found or failed typechecking", ident->location);
					}
				);

				probably_this = binop->left;
				member_call = binop->right;

				for (auto definition : ready_methods) {
					AstExpression *This = 0;

					auto unsorted_arguments = copy(call->unsorted_arguments);

					unsorted_arguments.insert_at({.expression = probably_this}, 0);

					overloads.add({
						.type = definition->type,
						.definition = definition,
						.unsorted_arguments = unsorted_arguments,
						.callable = member_call,
					});
				}


				if (ready_methods.count > 1) {
					ident->possible_definitions.set(ready_methods);
					ident->type = binop->type = compiler->builtin_overload_set.ident;

					break;
				}

				if (ready_methods.count == 1) {

					auto definition = ready_methods[0];

					wait_for(state, definition->location, "definition type",
						[&] { return definition->type; },
						[&] {
							state->reporter.error(definition->location, "Failed to resolve definition's type");
							state->reporter.error(call->callable->location, "While typechecking this expression:");
						}
					);


					ident->possible_definitions.set(ready_methods[0]);
					ident->type = binop->type = ready_methods[0]->type;
					break;
				}
			}

			break;
		}
		case Ast_Struct: {
			typecheck(state, call->callable);

			// NOTE: anonymous structs are replaced with identifiers to anonymous definitions.
			if (call->callable->kind == Ast_Identifier)
				goto identifier_case;

			assert(call->callable->kind = Ast_Struct);
			auto Struct = (AstStruct *)call->callable;

			overloads.add({
				.type = Struct,
				.definition = Struct->definition,
				.unsorted_arguments = copy(call->unsorted_arguments),
				.callable = call->callable,
			});
			break;
		}
		default: {
			typecheck(state, call->callable);

			// Expressions like []Int are replaced with identifiers to span instantiations, e.g. Span(Int).
			switch (call->callable->kind) {
				case Ast_Identifier: {
					goto identifier_case;
				}
			}

			auto d = direct(call->callable->type);
			switch (d->kind) {
				case Ast_LambdaType: {
					overloads.add({
						.type = d,
						.unsorted_arguments = copy(call->unsorted_arguments),
						.callable = call->callable,
					});
					break;
				}
				default: {
					state->reporter.error(call->callable->location, "This expression is not callable");
					yield(TypecheckResult::fail);
					break;
				}
			}
			break;
		}
	}
}

bool try_match_overload_lambda(TypecheckState *state, Reporter &reporter, AstCall *call, AstLambdaType *lambda_type, Overload &overload) {
	auto lambda = lambda_type->lambda;


	// Lambda                          | Call                 | Sorted argument list
	// --------------------------------+----------------------+---------------------
	// foo :: (a: Int, b: Int, c: int) | foo(b=1, a=2, 3)     | foo(2, 1, 3)
	//                                 | 1.foo(c=2, 3)        | foo(1, 3, 2)

	auto &unsorted_arguments = overload.unsorted_arguments;

	auto &sorted_arguments = overload.sorted_arguments_lambda;
	sorted_arguments.resize(lambda->parameters.count);

	overload.distance += lambda->has_pack; // If we have 2 matches, one with a pack and other without, select the one without.

	if (lambda->is_poly) {
		bool success;
		{
			state->template_call_stack.add({.call = call, .template_lambda = lambda});
			defer { state->template_call_stack.pop(); };
			overload.instantiated_lambda = instantiate_head(state, &reporter, overload, unsorted_arguments, lambda, call->location, &success);
		}
		if (!success)
			return false;

		assert(!as<AstLambdaType>(overload.instantiated_lambda->type)->lambda->is_poly);

		overload.lambda = lambda;
	} else {
		wait_for(
			state, lambda->location, "typechecked head",
			[&] { return lambda->finished_typechecking_head; },
			[&] {
				state->reporter.error(lambda->location, "Lambda's head couldn't finish typechecking");
				state->reporter.info(call->location, "Waited here:");
			}
		);

		// Assign named arguments to corresponding parameters

		List<AstExpression *> remaining_arguments;

		for (auto &argument : unsorted_arguments) {
			if (argument.name.is_empty()) {
				remaining_arguments.add(argument.expression);
				continue;
			}

			auto found_parameter = find_if(lambda->parameters, [&](AstDefinition *parameter) { return parameter->name == argument.name; });
			if (!found_parameter) {
				reporter.error(call->location, "Lambda does not take argument named '{}'.", argument.name);
				return false;
			}

			auto parameter_index = index_of(lambda->parameters, found_parameter);

			if (sorted_arguments[parameter_index].count) {
				reporter.error(argument.expression->location, "Argument '{}' was already assigned.", argument.name);
				return false;
			}


			if (!implicitly_cast(state, &reporter, &argument.expression, lambda->parameters[parameter_index]->type, 0, false)) {
				return false;
			}

			sorted_arguments[parameter_index].set(argument.expression);
		}


		// Now that named arguments are assigned, assign unnamed arguments
		{
			umm parameter_index = 0;

			while (parameter_index < sorted_arguments.count && sorted_arguments[parameter_index].count)
				++parameter_index;

			// Returns
			//     true if there is an unassigned parameter
			//     false if there is no parameters remaining
			auto next_unassigned_parameter = [&] {
				while (1) {
					++parameter_index;
					if (parameter_index >= sorted_arguments.count)
						return false;

					if (sorted_arguments[parameter_index].count == 0)
						return true;
				}
			};

			for (auto &argument : remaining_arguments) {
				auto distance = TypeDistance::exact;
				defer {
					overload.distance += argument_distance({.type_distance = distance, .const_distance = true});
				};

			retry_argument:
				if (parameter_index >= sorted_arguments.count) {
					reporter.error(call->location, "Too many arguments.");
					return false;
				}

				auto parameter = lambda->parameters[parameter_index];

				if (parameter->is_pack) {
					auto param_subtype = get_span_subtype(parameter->type);
					assert(param_subtype);

					if (auto arg_subtype = get_span_subtype(argument->type)) {
						if (types_match(arg_subtype, param_subtype)) {
							// Pass the span to the pack
							sorted_arguments[parameter_index].add(argument);
							next_unassigned_parameter();
							continue;
						}
					} else {
						if (implicitly_cast(state, &reporter, &argument, param_subtype, &distance, false)) {
							sorted_arguments[parameter_index].add(argument);
							continue;
						} else {
							next_unassigned_parameter();
							goto retry_argument;
						}
					}
				} else {
					if (!implicitly_cast(state, &reporter, &argument, parameter->type, &distance, false)) {
						return false;
					}

					assert(sorted_arguments[parameter_index].count == 0);
					sorted_arguments[parameter_index].set(argument);

				}

				next_unassigned_parameter();
			}
		}


		// Assign default values to everything that is left over.
		for (umm i = 0; i != sorted_arguments.count; ++i) {
			auto &args = sorted_arguments[i];
			auto param = lambda->parameters[i];

			if (param->expression && args.count == 0) {

				// Put the value for default argument in constant section once, then reference it.

				AstIdentifier *default_value_ident = 0;

				if (auto found = lambda->parameter_to_default_value.find(param)) {
					default_value_ident = found->value;
				} else {
					auto default_value_def = AstDefinition::create();
					default_value_def->is_constant = true;
					default_value_def->location = param->location;
					default_value_def->expression = param->expression;
					default_value_def->parsed_type = param->type;
					compiler->global_scope.add(default_value_def);

					{
						scoped_replace(state->current_scope, &compiler->global_scope);
						scoped_replace(state->container_node, 0);
						typecheck(state, default_value_def);
					}

					default_value_ident = AstIdentifier::create();
					default_value_ident->possible_definitions.set(default_value_def);
					default_value_ident->location = param->location;
					default_value_ident->type = default_value_def->type;

					lambda->parameter_to_default_value.get_or_insert(param) = default_value_ident;
				}

				args.add(deep_copy(default_value_ident));
			}
		}

		// Ensure every parameter has a matching argument
		for (umm i = 0; i != sorted_arguments.count; ++i) {
			auto args = sorted_arguments[i];
			auto param = lambda->parameters[i];

			if (args.count == 0 && !param->is_pack && !param->expression) {
				reporter.error(call->location, "Not enough arguments. Value for {} was not provided", param->name);
				return false;
			}
		}

		overload.lambda = lambda;
	}
	return true;
}
bool try_match_overload_struct(TypecheckState *state, Reporter &reporter, AstCall *call, AstStruct *Struct, Overload &overload) {
	wait_for(
		state, Struct->location, "typechecked struct",
		[&] {
			if (!Struct->type)
				return false;
			for (auto &member : Struct->data_members) {
				if (!member->type)
					return false;
			}
			return true;
		},
		[&] {
			state->reporter.error(Struct->location, "Struct couldn't finish typechecking");
			state->reporter.info(call->location, "Waited here:");
		}
	);

	if (Struct->is_template) {

		if (call->unsorted_arguments.count > Struct->parameter_scope->statement_list.count) {
			reporter.error(call->location, "Too many parameters. Expected {}, got {}", Struct->parameter_scope->statement_list.count, call->unsorted_arguments.count);
			return false;
		}

		List<NamedArgument> named_arguments;
		List<AstExpression *> unnamed_arguments;
		for (auto arg : call->unsorted_arguments) {
			if (arg.name.is_empty()) {
				unnamed_arguments.add(arg.expression);
			} else {
				for (auto other : named_arguments) {
					if (other.name == arg.name) {
						state->reporter.error(arg.expression->location, "Using the same parameter twice is not allowed.");
						state->reporter.info(other.expression->location, "Here is the first one.");
						yield(TypecheckResult::fail);
					}
				}
				named_arguments.add(arg);
			}
		}

		List<AstExpression *> arguments;
		arguments.resize(Struct->parameter_scope->statement_list.count);

		for (auto arg : named_arguments) {
			auto parameter = find_if(Struct->parameter_scope->statement_list, [&](auto parameter){ return arg.name == ((AstDefinition *)parameter)->name;});
			assert(parameter);
			arguments[index_of(Struct->parameter_scope->statement_list, parameter)] = arg.expression;
		}

		for (auto arg : unnamed_arguments) {
			*find(arguments, (AstExpression *)nullptr) = arg;
		}

		overload.sorted_arguments_struct = arguments;

		if (arguments.count != Struct->parameter_scope->statement_list.count) {
			reporter.error(call->location, "Argument count does not match. Expected {}, got {}", Struct->parameter_scope->statement_list.count, arguments.count);
			return false;
		}

		for (umm i = 0; i < arguments.count; ++i) {
			auto &argument = arguments[i];
			if (argument) {
				auto &parameter = (AstDefinition *&)Struct->parameter_scope->statement_list[i];

				if (!implicitly_cast(state, &reporter, &argument, parameter->type, 0, false)) {
					return false;
				}
			}
		}

		overload.Struct = Struct;
	} else {
		if (call->unsorted_arguments.count > Struct->data_members.count) {
			reporter.error(call->location, "Too many arguments. Expected {}, got {}", Struct->data_members.count, call->unsorted_arguments.count);
			return false;
		}

		List<NamedArgument> named_arguments;
		List<AstExpression *> unnamed_arguments;
		for (auto arg : call->unsorted_arguments) {
			if (arg.name.is_empty()) {
				unnamed_arguments.add(arg.expression);
			} else {
				for (auto other : named_arguments) {
					if (other.name == arg.name) {
						state->reporter.error(arg.expression->location, "Using the same parameter twice is not allowed.");
						state->reporter.info(other.expression->location, "Here is the first one.");
						yield(TypecheckResult::fail);
					}
				}
				named_arguments.add(arg);
			}
		}

		List<AstExpression *> arguments;
		arguments.resize(Struct->data_members.count);

		for (auto arg : named_arguments) {
			auto member = find_if(Struct->data_members, [&](auto member){return arg.name == ((AstDefinition *)member)->name;});
			if (!member) {
				return false;
			}
			arguments[index_of(Struct->data_members, member)] = arg.expression;
		}

		for (auto arg : unnamed_arguments) {
			*find(arguments, (AstExpression *)nullptr) = arg;
		}

		overload.sorted_arguments_struct = arguments;

		if (arguments.count != Struct->data_members.count) {
			reporter.error(call->location, "Argument count does not match. Expected {}, got {}", Struct->data_members.count, arguments.count);
			return false;
		}

		for (umm i = 0; i < arguments.count; ++i) {
			auto &argument = arguments[i];
			if (argument) {
				auto &member = (AstDefinition *&)Struct->data_members[i];

				if (!implicitly_cast(state, &reporter, &argument, member->type, 0, false)) {
					return false;
				}
			}
		}

		overload.Struct = Struct;
	}
	return true;
}
bool try_match_overload_distinct(TypecheckState *state, Reporter &reporter, AstCall *call, AstDistinct *Distinct, Overload &overload) {
	if (call->unsorted_arguments.count == 0) {
		not_implemented("Default initialized for distinct type");
	}
	if (call->unsorted_arguments.count != 1) {
		reporter.error(call->location, "Distinct type initializer can not have more than one argument.");
		return false;
	}

	if (!implicitly_cast(state, &reporter, &call->unsorted_arguments[0].expression, Distinct->expression, 0, false)) {
		return false;
	}

	overload.Distinct = Distinct;

	call->sorted_arguments.set(call->unsorted_arguments[0].expression);

	return true;
}

bool try_match_overload(TypecheckState *state, Reporter &reporter, AstCall *call, AstExpression *overload_type, Overload &overload) {
	switch (overload_type->kind)  {
		case Ast_LambdaType: return try_match_overload_lambda  (state, reporter, call, (AstLambdaType *)overload_type, overload);
		case Ast_Struct:     return try_match_overload_struct  (state, reporter, call, (AstStruct     *)overload_type, overload);
		case Ast_Distinct:   return try_match_overload_distinct(state, reporter, call, (AstDistinct   *)overload_type, overload);
		default: invalid_code_path("unable to match overload: unknown kind");
	}
}

AstExpression *apply_overload_lambda(TypecheckState *state, AstCall *call, Overload &match)  {
	if (match.lambda->is_poly) {
		match.lambda = match.instantiated_lambda;
		auto lambda = match.lambda;
		{
			assert(match.instantiated_lambda);
			state->template_call_stack.add({.call = call, .template_lambda = match.lambda, .hardened_lambda = match.instantiated_lambda});
			defer { state->template_call_stack.pop(); };

			for (u32 i = 0; i < match.sorted_arguments_lambda.count; ++i) {
				auto &argument = match.sorted_arguments_lambda[i];
				auto &parameter = lambda->parameters[i];

				if (parameter->is_constant) {
					if (!implicitly_cast(state, &state->reporter, &parameter->expression, parameter->type)) {
						state->reporter.error(parameter->location, "INTERNAL ERROR: implicit cast did not succeed the second time for constant parameter.");
						state->reporter.info(argument[0]->location, "Argument is here:");
						yield(TypecheckResult::fail);
					}
				}
			}

			instantiate_body(state, match.instantiated_lambda);
		}
		assert(match.instantiated_lambda->definition); // TODO: deal with unnamed poly lambdas

		AstIdentifier *ident = 0;

		if (call->callable->kind == Ast_Identifier) {
			ident = (AstIdentifier *)call->callable;
		} else if (auto binop = as<AstBinaryOperator>(call->callable)) {
			assert(binop->operation == BinaryOperation::dot);
			assert(binop->right->kind == Ast_Identifier);
			call->callable = ident = (AstIdentifier *)binop->right;
		} else {
			// NOTE: if callable is not an identifier, it can only be immediately invoked lambda, right?
			assert(call->callable->kind == Ast_Lambda);

			ident = AstIdentifier::create();
			ident->name = match.instantiated_lambda->definition->name;
			ident->location = call->callable->location;
			call->callable = ident;
		}

		ident->possible_definitions.set(match.instantiated_lambda->definition);
		ident->type = match.instantiated_lambda->type;
	}

	auto lambda = match.lambda;


	// Set call->sorted_arguments
	if (lambda->has_pack) {
		call->sorted_arguments.resize(match.sorted_arguments_lambda.count);
		for (umm i = 0; i < match.sorted_arguments_lambda.count; ++i) {
			auto &pack = match.sorted_arguments_lambda[i];
			auto &parameter = match.lambda->parameters[i];

			if (parameter->is_pack) {
				auto elem_type = get_span_subtype(parameter->type);
				assert(elem_type);

				// NOTE: Allow passing spans to packs as-is.
				if (pack.count == 1) {
					auto expr = pack[0];

					if (get_span_subtype(expr->type)) {
						call->sorted_arguments[i] = pack[0];
						continue;
					}
				}

				for (auto &elem : pack) {
					make_concrete(state, &elem);
					if (!implicitly_cast(state, &state->reporter, &elem, elem_type, 0)) {
						state->reporter.error(elem->location, "INTERNAL ERROR: implicit cast did not succeed the second time for pack.");
						yield(TypecheckResult::fail);
					}
				}

				auto array = AstArrayInitializer::create();
				array->elements = (decltype(array->elements))pack;
				if (array->elements.count)
					array->location = {array->elements.front()->location.begin(), array->elements.back()->location.end()};

				auto type = AstArray::create();
				type->element_type = elem_type;
				type->count = (u64)array->elements.count;
				// Cleanup: Is this needed?
				type->count_expression = make_integer(type->count);
				type->location = array->location;
				type->type = compiler->builtin_type.ident;

				array->type = type;
				call->sorted_arguments[i] = array;
			} else {
				assert(pack.count == 1);
				call->sorted_arguments[i] = pack[0];
			}
		}
	} else {
		call->sorted_arguments.reserve(match.sorted_arguments_lambda.count);
		for (umm i = 0; i < match.sorted_arguments_lambda.count; ++i) {
			auto param = match.lambda->parameters[i];
			auto &args = match.sorted_arguments_lambda[i];

			assert(args.count == 1);
			if (param->is_constant) {
				call->sorted_arguments.add(0);
			} else {
				make_concrete(state, &args.data[0]);
				call->sorted_arguments.add(args.data[0]);
			}
		}
	}


	assert(call->sorted_arguments.count == lambda->parameters.count);

	for (u32 i = 0; i < call->sorted_arguments.count; ++i) {
		auto &argument = call->sorted_arguments[i];
		auto &parameter = lambda->parameters[i];

		assert(!parameter->is_poly);
		if (argument) {
			if (!implicitly_cast(state, &state->reporter, &argument, parameter->type, 0)) {
				state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
				yield(TypecheckResult::fail);
			}
		}
	}

	//if (lambda->is_member) {
	//	assert(probably_this);
	//	if (!is_type(probably_this)) {
	//		auto pointer = make_address_of(&state->reporter, probably_this);
	//		if (!pointer)
	//			yield(TypecheckResult::fail);
	//		call->sorted_arguments.insert_at(pointer, 0);
	//	}
	//}

	// NOTE: not sure if this wait is necessary
	wait_for(
		state, lambda->location, "deduced return type",
		[&] { return lambda->return_parameter; },
		[&] {
			// This message feels like noise
			// state->reporter.error(lambda->location, "Could not deduce the return type.");
			// state->reporter.info(call->location, "Called here:");
		}
	);

	assert(lambda->return_parameter->container_node == lambda);
	if (!lambda->return_parameter) {
		state->reporter.error(call->location, "Lambda's return parameter is undefined. This is probably due to errors while hardening a poly.");
		yield(TypecheckResult::fail);
	}
	call->type = lambda->return_parameter->type;

	assert(lambda->type->kind == Ast_LambdaType);
	call->lambda_type = (AstLambdaType *)lambda->type;
	assert(call->lambda_type);


	if (lambda->is_macro) {
		auto inserted_block = AstBlock::create();
		inserted_block->location = call->location;
		inserted_block->scope->parent = state->current_scope;

		for (umm i = 0; i < call->sorted_arguments.count; ++i) {
			auto &arg = call->sorted_arguments[i];
			auto &param = lambda->parameters[i];

			auto def = AstDefinition::create();
			def->container_node = state->container_node;
			def->name = param->name;
			def->expression = arg;
			def->location = arg->location;
			def->parent_scope = inserted_block->scope;

			inserted_block->scope->add(raw(def));
		}

#if 0

		for (auto &statement : lambda->body_scope->statement_list) {
			auto new_statement = deep_copy(statement);

			inserted_block->scope->add(new_statement);
			for_each_top_scope(new_statement, [&](Scope *scope) {
				scope->parent = inserted_block->scope;
			});

			visit(new_statement, Combine{
				[&](AstNode *){},
				[&](AstReturn *Return) {
					if (auto Lambda = as<AstLambda>(state->container_node)) {
						Return->lambda = Lambda;
					} else {
						state->reporter.error(new_statement->location, "There's return statement in macro expansion, but we are currently not in a lambda.");
						yield(TypecheckResult::fail);
					}
				},
			});
		}
#else
		auto copied_block = deep_copy(lambda->body);
		copied_block->location = call->location;

		inserted_block->scope->add(make_statement(copied_block));
		for_each_top_scope(copied_block, [&](Scope *scope) {
			scope->parent = inserted_block->scope;
		});

		visit(copied_block, Combine{
			[&](AstNode *){},
			[&](AstReturn *Return) {
				if (auto Lambda = as<AstLambda>(state->container_node)) {
					Return->lambda = Lambda;
				} else {
					state->reporter.error(copied_block->location, "There's return statement in macro expansion, but we are currently not in a lambda.");
					yield(TypecheckResult::fail);
				}
			},
		});
#endif

		typecheck(state, inserted_block);

		return inserted_block;
	}

	if (auto lambda_type = direct_as<AstLambdaType>(call->callable->type)) {
		assert(!lambda_type->lambda->is_poly);
	}

	return call;
}
AstExpression *apply_overload_struct(TypecheckState *state, AstCall *call, Overload &match) {
	call->sorted_arguments = (decltype(call->sorted_arguments))match.sorted_arguments_struct;

	if (match.Struct->is_template) {
		// Make sure arguments are constant
		for (auto &argument : call->sorted_arguments) {
			if (argument) {
				if (!is_constant(argument)) {
					state->reporter.error(argument->location, "Only constants can be here.");
					yield(TypecheckResult::fail);
				}
			} else {
				state->reporter.error(call->location, "Not enough template arguments.");
				yield(TypecheckResult::fail);
			}
		}

		// Search cached instantiations
		for (auto instantiated : match.Struct->instantiations) {
			assert(instantiated.arguments.count == call->sorted_arguments.count);
			bool all_args_match = true;
			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				if (!types_match(instantiated.arguments[i], call->sorted_arguments[i])) {
					all_args_match = false;
					break;
				}
			}
			if (all_args_match) {
				return instantiated.ident;
			}
		}
		// NOTE: We need to add this before typechecking the instantiated struct, so it can reference itself.
		auto &instantiation = match.Struct->instantiations.add();
		instantiation.arguments = call->sorted_arguments;


		// Instantiate a struct with specified arguments

		auto instantiated_struct = AstStruct::create();

		instantiation.Struct = instantiated_struct;

		instantiated_struct->instantiated_from = match.Struct;
		instantiated_struct->is_template = false;
		instantiated_struct->is_union = match.Struct->is_union;
		instantiated_struct->layout = match.Struct->layout;
		instantiated_struct->location = call->location;
		instantiated_struct->parameter_scope->parent = match.Struct->parameter_scope->parent;
		for (umm i = 0; i < call->sorted_arguments.count; ++i) {
			auto &argument = call->sorted_arguments[i];
			auto &template_parameter = match.Struct->parameter_scope->definition_list[i];
			auto parameter = AstDefinition::create();
			parameter->name = template_parameter->name;
			parameter->location = template_parameter->location;
			parameter->container_node = instantiated_struct;
			parameter->expression = argument;
			parameter->is_constant = true;
			parameter->type = argument->type;
			instantiated_struct->parameter_scope->add(raw(parameter));
		}

		for (umm i = 0; i < call->sorted_arguments.count; ++i) {
			auto &argument = call->sorted_arguments[i];
			auto &member = instantiated_struct->parameter_scope->definition_list[i];
			if (argument) {
				if (!implicitly_cast(state, &state->reporter, &argument, member->type, 0)) {
					state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
					yield(TypecheckResult::fail);
				}
			} else {
				argument = state->make_null(member->type, call->location);
			}
		}

		auto instantiated_struct_definition = AstDefinition::create();
		instantiated_struct_definition->name = type_to_string(call);

		assert(match.Struct->definition);
		instantiated_struct_definition->container_node = match.Struct->definition->container_node;
		instantiated_struct_definition->expression = instantiated_struct;
		instantiated_struct_definition->is_constant = true;
		instantiated_struct_definition->parent_scope = match.Struct->definition->parent_scope;
		instantiated_struct_definition->type = compiler->builtin_type.ident;
		instantiated_struct->definition = instantiated_struct_definition;

		// Typecheck instantiated body.

		deep_copy(instantiated_struct->member_scope, match.Struct->member_scope);

		// NOTE: instantiation.ident must be set before calling typecheck on instantiated_struct
		auto instantiated_struct_ident = AstIdentifier::create();

		instantiation.ident = instantiated_struct_ident;

		instantiated_struct_ident->location = instantiated_struct->location;
		instantiated_struct_ident->possible_definitions.set(instantiated_struct_definition);
		instantiated_struct_ident->type = instantiated_struct_definition->type;
		instantiated_struct_ident->directed = instantiated_struct;
		instantiated_struct_ident->name = call->location;

		typecheck(state, instantiated_struct);

		return instantiated_struct_ident;
	} else {
		for (umm i = 0; i < call->sorted_arguments.count; ++i) {
			auto &argument = call->sorted_arguments[i];
			auto &member = match.Struct->data_members[i];
			if (argument) {
				if (!implicitly_cast(state, &state->reporter, &argument, member->type, 0)) {
					state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
					yield(TypecheckResult::fail);
				}
			} else {
				if (member->expression) {
					argument = evaluate(state, member->expression);
				} else {
					if (auto member_struct = direct_as<AstStruct>(member->type); member_struct && member_struct->default_value) {
						argument = deep_copy(raw(member_struct->default_value));
					} else {
						argument = state->make_null(member->type, call->location);
					}
				}
			}
		}
		call->type = call->callable;
		if (call->callable->kind == Ast_Identifier) {
			auto ident = (AstIdentifier *)call->callable;
			ident->possible_definitions.set(match.definition);
		}

		return call;
	}
}
AstExpression *apply_overload_distinct(TypecheckState *state, AstCall *call, Overload &match) {
	auto Distinct = match.Distinct;
	if (!implicitly_cast(state, &state->reporter, &call->sorted_arguments[0], Distinct->expression)) {
		state->reporter.error(call->sorted_arguments[0]->location, "INTERNAL ERROR: implicit cast did not succeed the second time (distinct).");
		yield(TypecheckResult::fail);
	}

	auto result = call->sorted_arguments[0];
	result->type = Distinct->identifier;
	return result;
}

AstExpression *apply_overload(TypecheckState *state, AstCall *call, Overload &match) {

	if (call->debug_overload) {
		immediate_info(match.definition->location, "DEBUG: Selected");
	}

	call->callable = match.callable;

	// NOTE: overloading is possible only with definitions, so match.definition must be set.
	if (types_match(call->callable->type, compiler->builtin_overload_set)) {
		call->callable->type = match.definition->type;

		// Right now i don't see what other nodes could be here
		assert(call->callable->kind == Ast_Identifier);
		auto identifier = (AstIdentifier *)call->callable;
		identifier->possible_definitions.set(match.definition);
	}

	if (match.lambda) return apply_overload_lambda(state, call, match);
	if (match.Struct) return apply_overload_struct(state, call, match);
	if (match.Distinct) return apply_overload_distinct(state, call, match);

	invalid_code_path();
}

HashMap<AstStruct *, AstIdentifier *> generated_comparers;

AstExpression *typecheck(TypecheckState *state, AstIdentifier *identifier) {
	if (identifier->possible_definitions.count) {
		// immediate_info(identifier->location, "Redundant typecheck.");
		return identifier;
	}

	DefinitionList definitions, inaccessible_definitions;

	while (1) {
		definitions.clear();
		inaccessible_definitions.clear();

		auto is_lambda_scope = [&] (Scope *scope) {
			return scope->node && scope->node->kind == Ast_Lambda && scope == ((AstLambda *)scope->node)->outer_scope();
		};

		auto scope = state->current_scope;
		while (1) {
			if (auto found_local = scope->definition_map.find(identifier->name)) {
				for (auto definition : found_local->value) {
					if (definition->is_hidden)
						continue;

					if (definition->container_node == 0 || definition->is_constant || state->container_node == definition->container_node) {
						definitions.add(definition);
					} else {
						inaccessible_definitions.add(definition);
					}
				}
			}

			struct SuitableMember {
				AstUsing *Using;
				AstDefinition *definition;
				AstDefinition *member;
			};

			List<SuitableMember> suitable_members;
			for (auto Using : scope->usings) {
				AstStruct *Struct = direct_as<AstStruct>(Using.definition->type);
				if (!Struct) {
					if (auto pointer = as_pointer(Using.definition->type)) {
						Struct = direct_as<AstStruct>(pointer->expression);
					}
				}
				if (!Struct) {
					state->reporter.error(Using.Using ? Using.Using->location : Using.definition->location, "`using` expects a struct or pointer to struct");
					yield(TypecheckResult::fail);
				}

				if (auto found = Struct->member_scope->definition_map.find(identifier->name)) {
					for (auto member : found->value) {
						suitable_members.add({
							.Using = Using.Using,
							.definition = Using.definition,
							.member = member,
						});
					}
				}
			}

			if (suitable_members.count) {
				if (suitable_members.count == 1) {
					auto Using = suitable_members[0].Using;
					auto definition = suitable_members[0].definition;
					auto member = suitable_members[0].member;

					wait_for(state, identifier->location, "using member type", [&] {
						return member->type;
					}, [&] {
					});

					identifier->possible_definitions.set(member);
					identifier->type = member->type;

					auto base = AstIdentifier::create();
					base->name = definition->name;
					base->location = identifier->location;
					base->possible_definitions.set(definition);
					base->type = definition->type;

					auto bin = AstBinaryOperator::create();
					bin->operation = BinaryOperation::dot;
					bin->location = identifier->location;
					bin->left  = base;
					bin->right = identifier;
					bin->type = member->type;
					return bin;
				}

				state->reporter.error(identifier->location, "Ambiguous identifier");
				for (auto member : suitable_members) {
					state->reporter.info(member.Using ? member.Using->location : member.definition->location, "Imported from here:");
					state->reporter.info(member.member->location, "Declared here:");
				}
				yield(TypecheckResult::fail);
			}

			if (definitions.count) {
				break;
			}

			if (!scope->parent) {
				assert(scope == &compiler->global_scope, "incomplete relationships between scopes. probably you forgot to set scope's parent.");
				break;
			}

			assert(scope != scope->parent, "INTERNAL ERROR: scope is a parent of itself");
			scope = scope->parent;
		}
		for (auto definition : definitions)
			assert(identifier->name == definition->name);

		auto wait_iteration = [&] {
			if (compiler->print_yields) {
				immediate_info(identifier->location, "Waiting for identifier");
			}

			auto old_progress = shared_progress;
			yield(TypecheckResult::wait);

			if (old_progress == shared_progress) {
				if (state->currently_typechecking_definition && state->currently_typechecking_definition->name == identifier->name) {
					state->reporter.error(identifier->location, "Can't use object while defining it.");
					state->reporter.info(state->currently_typechecking_definition->location, "Declared here:");
				} else {
					if (inaccessible_definitions.count) {
						state->reporter.error(identifier->location, "Can't access {} from here.", identifier->name);
						for (auto definition : inaccessible_definitions) {
							state->reporter.info(definition->location, "Definition:");
						}
					} else {
						List<AstDefinition *> failed_definitions;
						for (auto definition : definitions) {
							if (!definition->type) {
								failed_definitions.add(definition);
							}
						}
						if (failed_definitions.count) {
							state->reporter.error(identifier->location, "Unable to resolve identifier {}.", identifier->name);


							if (failed_definitions.count == 1) {
								if (auto lambda = as<AstLambda>(state->container_node); lambda && failed_definitions[0]->parent_scope == lambda->constant_scope) {
									state->reporter.info(failed_definitions[0]->location, "You are trying to reference a template parameter that is defined later and can't be resolved.");
								} else {
									state->reporter.info(failed_definitions[0]->location, "Here is the definition that failed typechecking:");
								}
							} else {
								state->reporter.info("Here is the list of definitions that failed typechecking:");
								for (auto definition : failed_definitions) {
									state->reporter.info(definition->location, "Here:");
								}
							}
						} else {
							state->reporter.error(identifier->location, "{} was not declared.", identifier->name);
						}
					}
					// state->reporter.error(location, "was not declared or compiler failed to infer it's definition's type.");
				}
				yield(TypecheckResult::fail);
			}
		};

		if (definitions.count) {
			for (auto definition : definitions) {
				while (!definition->type) {
					wait_iteration();
				}
			}

			for (auto definition : definitions)
				assert(identifier->name == definition->name);

			atomic_add(&shared_progress, 1);
			break;
		}

		wait_iteration();
	}

	for (auto definition : definitions)
		assert(identifier->name == definition->name);

	if (definitions.count == 1) {
		auto definition = definitions[0];
		assert(definition->type);
		wait_for(state, identifier->location, "definition type type", [&]{ return definition->type->type; }, [&]{
			state->reporter.error(definition->type->location, "Failed to resolve this type.");
			state->reporter.info(identifier->location, "While typechecking this identifier.");
		});
		identifier->possible_definitions.set(definition);
		identifier->type = definition->type;
		identifier->directed = definition->expression ? definition->expression->directed : nullptr;

		assert(identifier->type);

		// if (definition->evaluated) {
		// 	return definition->evaluated;
		// }
	} else {
		identifier->possible_definitions = definitions;
		identifier->type = compiler->builtin_overload_set.ident;
		identifier->directed = 0;
	}

	return identifier;
}
AstExpression *typecheck(TypecheckState *state, AstCall *call) {
	if (call->debug_overload) {
		immediate_info(call->location, "DEBUG: Overload resolution");
	}

	for (auto &argument : call->unsorted_arguments) {
		typecheck(state, argument.expression);
	}

	List<Overload> overloads;

	get_overloads(state, call, overloads);

	int min_distance = max_value<int>;

	List<Overload *> matches;
	matches.allocator = temporary_allocator;

	for (umm overload_index = 0; overload_index != overloads.count; ++overload_index) {
		auto &overload = overloads[overload_index];
		auto &reporter = overload.reporter;

		auto overload_type = direct(overload.type);

		assert(overload.callable);

		if (try_match_overload(state, reporter, call, overload_type, overload)) {
			overload.success = true;
			matches.add(&overload);
			min_distance = min(min_distance, overload.distance);

			if (call->debug_overload) {
				immediate_info(overload.definition->location, "DEBUG: Distance: {}", overload.distance);
			}
		}
	}
	Overload *match = 0;
	if (matches.count == 0) {
		//state->reporter.error(
		//	call->location,
		//	"No match was found for {}{}",
		//	member_call ? member_call->location : call->callable->kind == Ast_Identifier ? ((AstIdentifier *)call->callable)->name : call->callable->location,
		//	FormatSpan {
		//		.value = map(call->unsorted_arguments, [](NamedArgument arg) -> String {
		//			if (arg.name.is_empty()) return type_to_string(arg.expression->type);
		//			else                     return format("{}={}"str, arg.name, type_to_string(arg.expression->type));
		//		}),
		//		.before = "("b,
		//		.after = ")"b
		//	}
		//);
		state->reporter.error(call->location, "No match was found");
		if (overloads.count) {
			if (overloads.count > 1) {
				//state->reporter.info("Here is the list of possible overloads:");
				//for (auto definition : overloads) {
				//	defer { ++overload_index; };
				//	state->reporter.info(definition->location, "Overload #{}:", overload_index);
				//}
				//state->reporter.info("Here are the reasons of failed overload overloads:");
				//overload_index = 0;
				for (umm overload_index = 0; overload_index != overloads.count; ++overload_index) {
					auto &overload = overloads[overload_index];
					auto &definition = overload.definition;
					auto &reporter = overload.reporter;
					if (overload.instantiated_lambda && overload.instantiated_lambda->constant_scope->statement_list.count) {
						state->reporter.info(definition ? definition->location : call->callable->location, "Overload #{} with {}", overload_index + 1, StringizePolyTypes{overload.instantiated_lambda});
					} else {
						state->reporter.info(definition ? definition->location : call->callable->location, "Overload #{}:", overload_index + 1);
					}
					state->reporter.reports.add(reporter.reports);
				}
			} else {
				auto &overload = overloads[0];
				auto &definition = overload.definition;
				auto &reporter = overload.reporter;
				state->reporter.reports.add(reporter.reports);
				if (overload.instantiated_lambda) {
					state->reporter.info(definition ? definition->location : call->callable->location, "Couldn't match this with{}", StringizePolyTypes{overload.instantiated_lambda});
				} else {
					state->reporter.info(definition ? definition->location : call->callable->location, "Couldn't match this:");
				}
			}
		}
		yield(TypecheckResult::fail);
	} else if (matches.count == 1) {
		match = matches[0];
	} else {
		List<Overload *> best_matches;
		for (auto overload : matches) {
			if (overload->distance == min_distance)
				best_matches.add(overload);
		}

		if (best_matches.count == 1) {
			match = best_matches[0];
		} else {
			state->reporter.error(call->location, "Ambiguous matches were found");
			for (auto overload : best_matches) {
				state->reporter.info(overload->definition->location, "Here");
			}
			yield(TypecheckResult::fail);
		}
	}

	return apply_overload(state, call, *match);
}
AstExpression *typecheck(TypecheckState *state, AstLiteral *literal) {
	switch (literal->literal_kind) {
		using enum LiteralKind;
		case integer:
			literal->type = compiler->builtin_unsized_integer.ident;
			break;
		case Float:
			literal->type = compiler->builtin_unsized_float.ident;
			break;
		case boolean:
			literal->type = compiler->builtin_bool.ident;
			break;
		case string:
			literal->type = compiler->builtin_string.ident;
			break;
		case character:
			literal->type = compiler->builtin_u8.ident;
			break;
		case noinit:
			literal->type = compiler->builtin_noinit.ident;
			break;
		case null:
			literal->type = compiler->builtin_void.pointer;
			break;
		case lambda_name: {
			if (state->container_node->kind == Ast_Lambda) {
				auto lambda = (AstLambda *)state->container_node;

				literal->type = compiler->builtin_string.ident;
				literal->literal_kind = LiteralKind::string;
				literal->string.set(lambda->type_name);
				lambda->function_directives.add(literal);
			}
			break;
		}
		case container_string: {
			literal->type = compiler->builtin_string.ident;
			literal->literal_kind = LiteralKind::string;
			literal->string.set([&]() -> String {
				if (!state->container_node)
					return ""str;

				switch (state->container_node->kind) {
					case Ast_Lambda: return ((AstLambda *)state->container_node)->definition->name;
					case Ast_Struct: return ((AstStruct *)state->container_node)->definition->name;
					case Ast_Enum:   return ((AstEnum *)state->container_node)->definition->name;
				}
				invalid_code_path();
			}());
			break;
		}
		default:
			not_implemented();
	}
	return literal;
}
AstExpression *typecheck(TypecheckState *state, AstLambda *lambda) {
	scoped_replace(state->container_node, lambda);
	scoped_replace(state->current_loop, 0);
	push_scope(lambda->parameter_scope);

	lambda->type = create_lambda_type(lambda);
	lambda->type->type = compiler->builtin_type.ident;

	if (lambda->is_poly) {
		return lambda;
	}

	{
		scoped_replace(state->lambda_currently_accepting_poly_types, lambda);
		for (auto parameter : lambda->parameters) {
			typecheck_definition(state, parameter, {.allow_template = true});
			if (has_template(parameter->type)) {
				parameter->is_poly = true;
				lambda->is_poly = true;
			}
		}
	}

	if (lambda->is_poly) {
		return lambda;
	}

	if (lambda->return_parameter) {
		typecheck(state, lambda->return_parameter);
	}

	calculate_parameters_size(lambda);

	// lambda->definition can be null in case the lambda is a type pointer to lambda
	if (lambda->definition) {
		lambda->definition->type = lambda->type;
	}

	lambda->finished_typechecking_head = true;

	if (lambda->is_macro) {
		return lambda;
	}

	typecheck_body(state, lambda);

	return lambda;
}
AstExpression *typecheck(TypecheckState *state, AstLambdaType *lambda_type) {
	typecheck(state, lambda_type->lambda);
	lambda_type->type = compiler->builtin_type.ident;
	return lambda_type;
}
AstExpression *typecheck(TypecheckState *state, AstBinaryOperator *bin) {
	using enum BinaryOperation;

	{
		scoped_replace(state->replace_properties_with_getter_call, [&] {
			switch (bin->operation) {
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
					return false;
			}
			return true;
		}());

		typecheck(state, bin->left);
	}

	auto report_type_mismatch = [&] {
		state->reporter.error(bin->location, "Can't use binary {} on types {} and {}", as_string(bin->operation), type_to_string(bin->left->type), type_to_string(bin->right->type));
		yield(TypecheckResult::fail);
	};

	switch (bin->operation) {
		case dot: {
			if (false/*bin->left->type->kind == Ast_Import*/) {
				// not_implemented();
				// auto import = (AstImport *)get_definition_expression(bin->left);
				// assert(import->kind == Ast_Import);
				// import->scope;
			} else {
				if (bin->right->kind == Ast_Identifier) {
					auto member_identifier = (AstIdentifier *)bin->right;
					auto name = member_identifier->name;

					//if (bin->location == "List.Node")
					//	debug_break();

					bool left_is_type = is_type(bin->left);
					AstStruct *Struct = 0;

					if (auto pointer = as_pointer(bin->left->type))
						Struct = direct_as<AstStruct>(pointer->expression);
					else
						Struct = direct_as<AstStruct>(left_is_type ? bin->left : bin->left->type);

					if (Struct) {
						// struct_instance.member
						// struct_instance.method()
						// StructType.member
						// StructType.method()

						DefinitionList definitions;

						if (auto member = Struct->member_scope->definition_map.find(name)) {
							definitions.add(member->value);
						}
						if (auto parameter = Struct->parameter_scope->definition_map.find(name)) {
							definitions.add(parameter->value);
						}

						if (definitions.count == 0) {
							bool has_getter = false;
							bool has_setter = false;

							auto getter_name = format("get_{}"str, name);
							auto setter_name = format("set_{}"str, name);

							for (auto scope = state->current_scope; scope; scope = scope->parent) {
								auto get_property_lambda_definition = [&] (DefinitionList list, bool is_setter) {
									AstLambda *result = 0;
									for (auto definition : list) {
										if (definition->is_constant && definition->expression) {
											if (auto Lambda = ::as<AstLambda>(definition->expression)) {
												if (Lambda->parameters.count == (is_setter ? 2 : 1))
													return true;
											}
										}
									}
									return false;
								};

								if (!has_getter) {
									if (auto found = scope->definition_map.find(getter_name)) {
										has_getter = get_property_lambda_definition(found->value, false);
									}
								}
								if (!has_setter) {
									if (auto found = scope->definition_map.find(setter_name)) {
										has_setter = get_property_lambda_definition(found->value, true);
									}
								}

							}

							if (has_getter || has_setter) {
								if (state->replace_properties_with_getter_call) {
									if (!has_getter) {
										state->reporter.error(bin->location, "There is no such get_{} lambda that accepts {}.", bin->right->location, type_to_string(bin->left->type));
										yield(TypecheckResult::fail);
									}

									auto ident = AstIdentifier::create();
									ident->name = getter_name;
									ident->location = bin->right->location;

									auto call = AstCall::create();
									call->callable = ident;
									call->unsorted_arguments.set({.expression = bin->left});
									call->location = bin->location;

									typecheck(state, call);

									return call;
								} else {
									if (!has_setter) {
										state->reporter.error(bin->location, "There is no such set_{} lambda that accepts {}.", bin->right->location, type_to_string(bin->left->type));
										yield(TypecheckResult::fail);
									}
									bin->is_property = true;
									bin->type = compiler->builtin_void.ident;
									return bin;
								}
							} else {
								if (left_is_type) {
									state->reporter.error(bin->right->location, "Type '{}' does not contain constant '{}'", Struct->definition->name, bin->right->location);
								} else {
									state->reporter.error(bin->right->location, "'{}' is not a member of '{}'", bin->right->location, Struct->definition->name);

									if (Struct->member_scope->definition_list.count) {
										StringBuilder available_members;
										available_members.allocator = temporary_allocator;

										append(available_members, "Available members:\n");

										for (auto &definition : Struct->member_scope->definition_list) {
											append_format(available_members, "    {}: {}\n", definition->name, type_name(definition->type));
										}

										state->reporter.info("{}", available_members);
									}
								}
								yield(TypecheckResult::fail);
							}
						}

						if (definitions.count > 1) {
							member_identifier->possible_definitions.set(definitions);
							member_identifier->type = bin->type = compiler->builtin_overload_set.ident;
							return bin;
						}

						auto definition = definitions[0];

						if (left_is_type && !definition->is_constant) {
							state->reporter.error(bin->location, "To access {} you need an instance of type {}, not the type itself.", definition->name, type_to_string(Struct));
							yield(TypecheckResult::fail);
						}

						wait_for(state, definition->location, "definition type",
							[&] { return definition->type; },
							[&] {
								state->reporter.error(definition->location, "Failed to resolve definition's type");
								state->reporter.error(bin->location, "While typechecking this expression:");
							}
						);

						member_identifier->possible_definitions.set(definition);
						member_identifier->type = definition->type;
						bin->type = bin->right->type;

						if (is_type(bin->right)) {
							assert(definition->expression);
							bin->directed = bin->right->directed = definition->expression->directed;
						}

						if (definition->is_constant) {
							if (!is_type(definition->expression)) {
								auto result = deep_copy(evaluate(state, definition->expression));
								result->location = bin->location;
								return result;
							}
						}
					} else if (auto array_type = ::as<AstArray>(bin->left->type)) {
						if (bin->right->kind != Ast_Identifier) {
							state->reporter.error(bin->left->location, "The only members of any array type are 'data' and 'count'");
							yield(TypecheckResult::fail);
						}

						auto identifier = (AstIdentifier *)bin->right;

						if (identifier->name == "data"str) {
							bin->type = make_pointer_type(array_type->element_type);

							auto array_address = is_addressable(bin->left) ?
								make_address_of(&state->reporter, bin->left) :
								make_unary(UnaryOperation::move_to_temporary, bin->left, make_pointer_type(bin->left->type));

							if (!array_address)
								yield(TypecheckResult::fail);

							array_address->location = bin->location;

							return make_cast(array_address, bin->type);
						} else if (identifier->name == "count"str) {
							return state->make_integer(array_type->count, array_type->count_expression->location); // unsized
						} else {
							state->reporter.error(bin->left->location, "The only members of any array type are identifiers 'data' and 'count'. You asked for '{}', which does not exist", identifier->name);
							yield(TypecheckResult::fail);
						}
					} else if (auto Enum = direct_as<AstEnum>(bin->left)) {
						// EnumType.member
						auto result = find_enum_value(state, Enum, name, bin->right->location);
						result->location = bin->location;
						return result;
					} else {
						state->reporter.error(bin->left->location, "Dot operator can not be applied to an expression of type {}", type_to_string(bin->left->type));
						yield(TypecheckResult::fail);
					}
				} else {
					state->reporter.error(bin->right->location, "This expression can not appear to the right of the dot.");
					yield(TypecheckResult::fail);
				}
			}
			break;
		}
		case as: {
			auto cast = bin;
			typecheck(state, cast->right);
			cast->type = cast->right;

			auto &src_type = cast->left->type;
			auto &dst_type = cast->type;

			// NOTE: break out if cast is allowed
			breakable_scope {
				if (types_match(src_type, dst_type)) {
					// Types are exactly the same, no need in cast.
					return cast->left;
				}

				if (implicitly_cast(state, 0, &cast->left, cast->right)) {
					// implicitly_cast replaced cast->left with 'as binop' or with call.

					cast->left->location = cast->location;
					return cast->left;
				}

				auto found_built_in = find(built_in_casts, {direct_as<AstStruct>(src_type), direct_as<AstStruct>(dst_type)});
				if (found_built_in) {
					break;
				}

				if (::is_integer(src_type)) {
					if (types_match(src_type, compiler->builtin_unsized_integer)) {
						if (::is_pointer(dst_type) || ::is_integer(dst_type)) {
							break;
						}
					} else {
						if (::is_pointer(dst_type)) {
							auto built_in_cast = find_if(built_in_casts, [&](auto c) { return types_match(c.from, src_type); });
							assert(built_in_cast);
							break;
						}
					}

					if (auto Enum = direct_as<AstEnum>(dst_type)) {
						break;
					}
				} else if (::is_float(src_type)) {
					if (types_match(src_type, compiler->builtin_unsized_float)) {
						if (::is_integer(dst_type)) {
							break;
						}
					}
				} else if (::is_pointer(src_type)) {
					if (::is_pointer(dst_type)) {
						break;
					} else if (::is_integer(dst_type)) {
						if (get_size(dst_type) == 8) {
							break;
						}

						auto built_in_cast = find_if(built_in_casts, [&](auto c) {
							return types_match(c.from, compiler->builtin_u64) && types_match(c.to, dst_type);
						});
						assert(built_in_cast);
						break;
					}
				} else if (auto src_array = ::as<AstArray>(src_type)) {
					if (auto dst_array = ::as<AstArray>(dst_type)) {
						if (src_array->count != dst_array->count) {
							state->reporter.error(cast->location, "Can not convert from {} to {}. Count does not match.", type_to_string(src_type), type_to_string(dst_type));
							yield(TypecheckResult::fail);
						}

						// TODO: FIXME: HACK:
						// This is a horrible hack. This should be done without creating new ast nodes.
						//
						// check if src[0] casts to dst[0]

						auto hack_src = AstSubscript::create();
						hack_src->expression = cast->left;
						hack_src->index_expression = make_integer(0ull, cast->location);
						hack_src->location = cast->location;

						auto hack_dst = dst_array->element_type;

						auto hack_cast = AstBinaryOperator::create();
						hack_cast->operation = BinaryOperation::as;
						hack_cast->left = hack_src;
						hack_cast->right = hack_dst;
						hack_cast->location = cast->location;

						typecheck(state, hack_cast);

						break;
					}
				} else if (auto src_enum = direct_as<AstEnum>(src_type)) {
					if (auto dst_enum = direct_as<AstEnum>(dst_type); dst_enum || ::is_integer(dst_type)) {
						break;
					}
				}

				AstExpression *result = 0;
				wait_for(state, bin->location, "explicit casts", [&] {
					for (auto lambda : typechecked_explicit_casts) {
						if (types_match(lambda->return_parameter->type, dst_type) && types_match(lambda->parameters[0]->type, src_type)) {
							auto call = AstCall::create();
							call->location = cast->location;
							call->callable = lambda;
							call->unsorted_arguments.set({{}, cast->left});
							call->sorted_arguments.set(cast->left);
							call->type = dst_type;
							assert(lambda->type->kind == Ast_LambdaType);
							call->lambda_type = (AstLambdaType *)lambda->type;
							result = call;
							return true;
						}
					}
					if (not_typechecked_implicit_casts_count == 0) {
						return true;
					}
					return false;
				}, []{}, false);

				if (result)
					return result;

				state->reporter.error(
					cast->location,
					"Conversion from {} to {} does not exist",
					type_to_string(src_type),
					type_to_string(dst_type)
				);
				yield(TypecheckResult::fail);
			}
			break;
		}
		case ass: {
			if (auto property = ::as<AstBinaryOperator>(bin->left); property && property->is_property) {
				auto ident = AstIdentifier::create();
				ident->name = format("set_{}"str, ::as<AstIdentifier>(property->right)->name);
				ident->location = bin->right->location;

				auto call = AstCall::create();
				call->callable = ident;
				call->unsorted_arguments.set({{ .expression = make_address_of(0, property->left)}, {.expression = bin->right }});
				call->location = bin->location;

				typecheck(state, call);

				return call;

			} else {

				if (!ensure_assignable(&state->reporter, bin->left)) {
					yield(TypecheckResult::fail);
				}

				typecheck(state, bin->right);

				if (!implicitly_cast(state, &state->reporter, &bin->right, bin->left->type)) {
					yield(TypecheckResult::fail);
				}

				bin->type = compiler->builtin_void.ident;
			}
			return bin;
		}
		case lor:
		case land: {
			if (!types_match(bin->left->type, compiler->builtin_bool)) {
				state->reporter.error(bin->left->location, "This must be a boolean");
				yield(TypecheckResult::fail);
			}

			typecheck(state, bin->right);
			if (!types_match(bin->right->type, compiler->builtin_bool)) {
				state->reporter.error(bin->right->location, "This must be a boolean");
				yield(TypecheckResult::fail);
			}
			bin->type = compiler->builtin_bool.ident;
			break;
		}
		case range: {
			typecheck(state, bin->left);
			if (!implicitly_cast(state, &state->reporter, &bin->left, compiler->type_int)) {
				yield(TypecheckResult::fail);
			}

			typecheck(state, bin->right);
			if (!implicitly_cast(state, &state->reporter, &bin->right, compiler->type_int)) {
				yield(TypecheckResult::fail);
			}

			return make_struct_initializer(compiler->builtin_range.ident, {bin->left, bin->right}, bin->location);
		}
		default: {

			auto remove_assignment = [](BinaryOperation op) -> Optional<BinaryOperation> {
				switch (op) {
					case addass: return add;
					case subass: return sub;
					case mulass: return mul;
					case divass: return div;
					case modass: return mod;
					case bxorass: return bxor;
					case bandass: return band;
					case borass: return bor;
					case bslass: return bsl;
					case bsrass: return bsr;
				}
				return {};
			};

			if (auto unass = remove_assignment(bin->operation)) {
				// v.length += 10
				//      vvv
				// set_length(&v, get_length(v) + 10)
				if (auto property = ::as<AstBinaryOperator>(bin->left); property && property->is_property) {
					auto property_name = ::as<AstIdentifier>(property->right)->name;

					auto get_ident = AstIdentifier::create();
					get_ident->name = format("get_{}"str, property_name);
					get_ident->location = bin->right->location;

					auto get_call = AstCall::create();
					get_call->callable = get_ident;
					get_call->unsorted_arguments.set({{ .expression = property->left }});
					get_call->location = bin->location;

					bin->left = get_call;
					bin->operation = unass.value();

					auto set_ident = AstIdentifier::create();
					set_ident->name = format("set_{}"str, property_name);
					set_ident->location = bin->right->location;

					auto set_call = AstCall::create();
					set_call->callable = set_ident;
					set_call->unsorted_arguments.set({{ .expression = make_address_of(0, property->left)}, {.expression = bin }});
					set_call->location = bin->location;

					typecheck(state, set_call);

					return set_call;
				}
			}

			typecheck(state, bin->right);

#define DEDUCE_ENUM(left, right) \
			if (auto Enum = direct_as<AstEnum>(left->type)) { \
				if (types_match(right->type, compiler->builtin_unknown_enum)) { \
					assert(right->kind == Ast_UnaryOperator); \
					auto ident = (AstIdentifier *)((AstUnaryOperator *)right)->expression; \
					assert(ident->kind == Ast_Identifier); \
					right = find_enum_value(state, Enum, ident->name, ident->location); \
				} \
			}

			DEDUCE_ENUM(bin->left, bin->right)
			DEDUCE_ENUM(bin->right, bin->left)

			auto l = direct(bin->left->type);
			auto r = direct(bin->right->type);

			auto ld = ::as<AstDistinct>(l);
			auto rd = ::as<AstDistinct>(r);

			// There's no builtin operators on different distinct types.
			if (!ld || !rd || ld == rd) {
				if (ld) l = direct(ld->expression);
				if (rd) r = direct(rd->expression);

				if (auto found = binary_typecheckers.find({bin->operation, l, r})) {
					auto result = found->value(state, bin);
					if (result)
						return result;

					report_type_mismatch();
					return 0;
				}

				bool li = ::is_integer(l);
				bool ri = ::is_integer(r);

				bool lf = ::is_float(l);
				bool rf = ::is_float(r);

				bool lp = ::is_pointer(l);
				bool rp = ::is_pointer(r);

				auto is_null_literal = [](AstExpression *expression) {
					if (expression->kind != Ast_Literal)
						return false;

					auto literal = (AstLiteral *)expression;
					return literal->literal_kind == LiteralKind::null;
				};

				switch (bin->operation) {
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
					case bsrass: {
						if (!ensure_assignable(&state->reporter, bin->left)) {
							yield(TypecheckResult::fail);
						}
						break;
					}
					case ne:
					case eq: {
						if ((direct_as<AstEnum>(l) && direct_as<AstEnum>(r)) ||
							(direct_as<AstEnum>(l) && ::is_integer(r)) ||
							(::is_integer(l)       && direct_as<AstEnum>(r)))
						{
							bin->type = compiler->builtin_bool.ident;
							return bin;
						}
						break;
					}
				}

				if (l->kind == Ast_Struct && r->kind == Ast_Struct) {
					REDECLARE_VAL(l, (AstStruct *)l);
					REDECLARE_VAL(r, (AstStruct *)r);

					if (struct_is_built_in(l) && struct_is_built_in(r)) {
						// BEGIN UNSIZED INTEGER DEAL
						switch (bin->operation) {
							case add:
							case sub:
							case mul:
							case div:
							case mod:
							case bor:
							case band:
							case bxor:
							case bsl:
							case bsr:
							case addass:
							case subass:
							case mulass:
							case divass:
							case modass:
							case borass:
							case bandass:
							case bxorass:
							case bslass:
							case bsrass: {
								auto lui = types_match(l, compiler->builtin_unsized_integer);
								auto rui = types_match(r, compiler->builtin_unsized_integer);
								auto luf = types_match(l, compiler->builtin_unsized_float);
								auto ruf = types_match(r, compiler->builtin_unsized_float);

								if (lui && rui) {
									bin->type = compiler->builtin_unsized_integer.ident;
									return bin;
								}
								if (luf && ruf) {
									bin->type = compiler->builtin_unsized_float.ident;
									return bin;
								}
								if ((lui || luf) && (ri || rf)) {
									make_concrete(state, &bin->left, bin->right->type);
									bin->type = bin->right->type;
									return bin;
								}
								if ((li || lf) && (rui || ruf)) {
									make_concrete(state, &bin->right, bin->left->type);
									bin->type = bin->left->type;
									return bin;
								}
								break;
							}
							case ne:
							case eq:
							case lt:
							case gt:
							case le:
							case ge: {
								bin->type = compiler->builtin_bool.ident;
								if (types_match(l, compiler->builtin_unsized_integer) && types_match(r, compiler->builtin_unsized_integer) ) {
									return bin;
								} else if (types_match(l, compiler->builtin_unsized_integer) && ri) {
									make_concrete(state, &bin->left, bin->right->type);
									return bin;
								} else if (types_match(r, compiler->builtin_unsized_integer) && li) {
									make_concrete(state, &bin->right, bin->left->type);
									return bin;
								}

								break;
							}
						}
						// END UNSIZED INTEGER DEAL


						switch (bin->operation) {
							case add:
							case sub:
							case mul:
							case div:
							case mod:
							case addass:
							case subass:
							case mulass:
							case divass:
							case modass: {
								if (li && ri) {
									if (l->size == r->size) {
										if (signedness_matches(l, r)) {
											assert(l == r);
											bin->type = bin->left->type;
											return bin;
										}
										break;
									} else {
										if (l->size > r->size) {
											bin->right = make_cast(bin->right, bin->left->type);
											bin->type = bin->left->type;
										} else {
											bin->left = make_cast(bin->left, bin->right->type);
											bin->type = bin->right->type;
										}
										return bin;
									}
									break;
								}

								if (lf && rf) {
									bin->type = l->size > r->size ? l : r;
									return bin;
								}
								break;
							}

							case ne:
							case eq: {
								bin->type = compiler->builtin_bool.ident;

								if (types_match(l, r)) {
									return bin;
								}

								if ((li && types_match(r, compiler->builtin_unsized_integer) ) || (ri && types_match(l, compiler->builtin_unsized_integer) )) {
									return bin;
								}

								break;
							}

							case lt:
							case gt:
							case le:
							case ge: {
								bin->type = compiler->builtin_bool.ident;

								if (li && ri) {
									if (signedness_matches(l, r)) {
										return bin;
									}
									break;
								}

								if ((li && types_match(r, compiler->builtin_unsized_integer) ) || (ri && types_match(l, compiler->builtin_unsized_integer) )) {
									return bin;
								}

								if (lf && rf && types_match(l, r)) {
									return bin;
								}

								break;
							}

							case bor:
							case band:
							case bxor:
							case borass:
							case bandass:
							case bxorass: {
								if (li && ri) {
									if (signedness_matches(l, r)) {
										bin->type = l->size > r->size ? l : r;
										return bin;
									}
									break;
								}
								break;
							}

							case bsl:
							case bsr:
							case bslass:
							case bsrass: {
								if ((li && types_match(r, compiler->builtin_unsized_integer) ) || (ri && types_match(l, compiler->builtin_unsized_integer) )) {
									bin->type = types_match(r, compiler->builtin_unsized_integer) ? l : r;
									return bin;
								}

								break;
							}
						}
					}
				} else if (lp && rp) {
					// Both are pointers
					REDECLARE_VAL(l, (AstUnaryOperator *)l);
					REDECLARE_VAL(r, (AstUnaryOperator *)r);
					switch (bin->operation) {
						case eq:
						case ne:
						case lt:
						case gt:
						case le:
						case ge: {
							bin->type = compiler->builtin_bool.ident;
							if (types_match(l->expression, r->expression))
								return bin;
							if ((r == compiler->builtin_void.pointer) ||
								(l == compiler->builtin_void.pointer))
							{
								return bin;
							}
							break;
						}
						case sub: {
							if (!types_match(l->expression, r->expression)) {
								state->reporter.error(bin->location, "Can't subtract pointers to different types.");
								yield(TypecheckResult::fail);
							}
							if (types_match(l->expression, compiler->builtin_void)) {
								state->reporter.error(bin->location, "Can't subtract pointers to void.");
								yield(TypecheckResult::fail);
							}
							bin->type = compiler->type_int;
							return bin;
						}
					}
				} else if ((lp && ri) || (li && rp)) {
					// One is pointer and other is integer
					if (lp) {
						bin->type = bin->left->type;
						make_concrete(state, &bin->right);
					} else {
						bin->type = bin->right->type;
						make_concrete(state, &bin->left);
					}
					return bin;
				} else if ((is_lambda_type(l) && is_null_literal(bin->right)) || (is_lambda_type(r) && is_null_literal(bin->left))) {
					bin->type = compiler->builtin_bool.ident;
					return bin;
				}
			}

			// Resort to finding an overloaded operator.
			{
				switch (bin->operation) {
					case add:
					case sub:
					case mul:
					case div:
					case mod:
					case bxor:
					case band:
					case bor:
					case bsl:
					case bsr:
					case eq:
					case ne:
					case gt:
					case lt:
					case ge:
					case le:
					case land:
					case lor: {
						auto op_ident = AstIdentifier::create();
						op_ident->name = get_binop_ident_name(bin->operation);
						op_ident->location = bin->location;

						auto call = AstCall::create();
						call->location = bin->location;
						call->callable = op_ident;
						call->unsorted_arguments.add({{}, bin->left});
						call->unsorted_arguments.add({{}, bin->right});

						auto new_state = get_pooled_typecheck_state(call, state);

						auto get_comparer_ident = [&](AstStruct *comparee) {

							if (auto found = generated_comparers.find(comparee)) {
								return found->value;
							}

							auto arg0ident = [&] { auto ident = AstIdentifier::create(); ident->name = "a"str; return ident; };
							auto arg1ident = [&] { auto ident = AstIdentifier::create(); ident->name = "b"str; return ident; };

							auto lambda = AstLambda::create();
							lambda->convention = CallingConvention::tlang;
							lambda->constant_scope->parent = &compiler->global_scope;

							auto add_param = [&] (KeyString name) {
								auto param = AstDefinition::create();
								param->name = name;
								param->container_node = lambda;

								// TODO: all structs, named and unnamed should have a definition.
								assert(comparee->definition, comparee->location, "Struct has no definition and is used in ==. This case is not implemented yet.");

								param->type = make_identifier(comparee->definition);
								lambda->parameters.add(param);
								lambda->parameter_scope->add(param);
							};

							add_param("a"str);
							add_param("b"str);

							// a.m0 == b.m0 && a.m1 == b.m1 &&  ...
							for (auto member : comparee->data_members) {
								auto member_ident = [&]() {
									auto ident = AstIdentifier::create();
									ident->name = member->name;
									return ident;
								};

								auto comparison = make_binop(eq, make_binop(dot, arg0ident(), member_ident()), make_binop(dot, arg1ident(), member_ident()));
								if (lambda->body) {
									lambda->body = make_binop(land, lambda->body, comparison);
								} else {
									lambda->body = comparison;
								}
							}

							auto comparer_def = AstDefinition::create();
							comparer_def->parent_scope = &compiler->global_scope;
							comparer_def->expression = lambda;
							comparer_def->is_constant = true;
							comparer_def->type = lambda->type;


							{
								scoped_replace(state->current_scope, &compiler->global_scope);
								scoped_replace(state->container_node, 0);
								typecheck(state, comparer_def);
							}

							auto comparer_ident = AstIdentifier::create();
							comparer_ident->possible_definitions.set(comparer_def);
							comparer_ident->type = comparer_def->type;

							generated_comparers.get_or_insert(comparee) = comparer_ident;

							return comparer_ident;
						};


					retry:
						SWITCH_TO_FIBER(new_state->fiber);
						current_typecheck_state = state;
						switch (fiber_result) {
							case TypecheckResult::wait: {
								yield(TypecheckResult::wait);
								goto retry;
							}
							case TypecheckResult::fail: {
								// HACK: this is cringe
								if (find(new_state->reporter.reports[0].message, "was not declared"str)) {
									if (auto ls = ::as<AstStruct>(l)) {
										// Create default implementation for == or !=
										if (types_match(l, r) && !struct_is_built_in(ls)) {
											switch (bin->operation) {
												case eq: {
													call->callable = get_comparer_ident(ls);
													typecheck(state, call);
													return call;
												}
												case ne: {
													call->callable = get_comparer_ident(ls);
													auto result = make_unary(UnaryOperation::lnot, call);
													typecheck(state, result);
													return result;
												}
											}
											break;
										}
									}

									state->reporter.error(bin->location, "No binary operator {} defined for {} and {}", bin->operation, type_to_string(bin->left->type), type_to_string(bin->right->type));
								} else {
									state->reporter.reports.add(new_state->reporter.reports);
								}
								yield(TypecheckResult::fail);
								break;
							}
						}

						return call;
					}
					case addass:
					case subass:
					case mulass:
					case divass:
					case modass:
					case bxorass:
					case bandass:
					case borass:
					case bslass:
					case bsrass: {
						auto op_ident = AstIdentifier::create();
						op_ident->name = get_binop_ident_name(bin->operation);
						op_ident->location = bin->location;

						auto call = AstCall::create();
						call->location = bin->location;
						call->callable = op_ident;
						call->unsorted_arguments.add({{}, bin->left});
						call->unsorted_arguments.add({{}, bin->right});



						auto new_state = get_pooled_typecheck_state(call, state);


					retry2:
						SWITCH_TO_FIBER(new_state->fiber);
						current_typecheck_state = state;
						switch (fiber_result) {
							case TypecheckResult::wait: {
								yield(TypecheckResult::wait);
								goto retry2;
							}
							case TypecheckResult::fail: {
								String op = get_binop_ident_name(bin->operation);
								op = op.subspan(0, op.count - 1); // strip =

								// FIXME: ugly hack
								if (new_state->reporter.reports.count == 1 && find(new_state->reporter.reports[0].message, tformat("{}= was not declared"str, op))) {
									op_ident->name = op;

									auto ass = make_binop(BinaryOperation::ass, bin->left, call);
									new_state = get_pooled_typecheck_state(ass, state);

								retry3:
									SWITCH_TO_FIBER(new_state->fiber);
									current_typecheck_state = state;
									switch (fiber_result) {
										case TypecheckResult::wait: {
											yield(TypecheckResult::wait);
											goto retry3;
										}
										case TypecheckResult::fail: {
											// HACK: this is cringe
											if (find(new_state->reporter.reports[0].message, "was not declared"str)) {
												state->reporter.error(bin->location, "No operator {} nor {}= defined for {} and {}", op, op, type_to_string(bin->left->type), type_to_string(bin->right->type));
											} else {
												state->reporter.reports.add(new_state->reporter.reports);
											}
											yield(TypecheckResult::fail);
											break;
										}
									}

									return ass;
								} else {
									state->reporter.reports.add(new_state->reporter.reports);
									yield(TypecheckResult::fail);
								}
								break;
							}
						}

						return call;
					}
					default:
						invalid_code_path("Not implemented");
						break;
				}
			}

			struct Candidate {
				OeratorOverload overload;
				bool ass;
			};
			List<Candidate> candidates;

			wait_for(state, bin->location, "binary overload", [&] {
				candidates.clear();

				auto found = binary_operators.find(bin->operation);
				if (found) {
					for (auto overload : found->value) {
						if (
							implicitly_cast(state, 0, &bin->left,  overload.lambda->parameters[0]->type, 0, false) &&
							implicitly_cast(state, 0, &bin->right, overload.lambda->parameters[1]->type, 0, false)
						) {
							candidates.add({overload});
						}
					}

					// nocheckin there was no return before
					return true;
				}
				if (not_typechecked_binary_operators_count == 0)
					return true;

				return false;
			}, []{}, false);

			if (candidates.count == 0) {
				switch (bin->operation) {
					case addass:
					case subass:
					case mulass:
					case divass:
					case modass:
					case bxorass:
					case bandass:
					case borass:
					case bslass:
					case bsrass:  {
						auto operation = [&]{switch (bin->operation) {
							case addass:  return add;
							case subass:  return sub;
							case mulass:  return mul;
							case divass:  return div;
							case modass:  return mod;
							case bxorass: return bxor;
							case bandass: return band;
							case borass:  return bor;
							case bslass:  return bsl;
							case bsrass:  return bsr;
							default: return count;
						}}();

						wait_for(state, bin->location, "binary overload", [&] {
							candidates.clear();
							auto found = binary_operators.find(operation);
							if (found) {
								for (auto overload : found->value) {
									if (
										implicitly_cast(state, 0, &bin->left,  overload.lambda->parameters[0]->type, 0, false) &&
										implicitly_cast(state, 0, &bin->right, overload.lambda->parameters[1]->type, 0, false)
									) {
										candidates.add({overload, true});
									}
								}

								// nocheckin there was no return before
								return true;
							}
							if (not_typechecked_binary_operators_count == 0)
								return true;
							return false;
						}, []{}, false);

						break;
					}
				}
			}

			if (candidates.count == 0) {
				report_type_mismatch();
				return 0;
			}

			if (candidates.count != 1) {
				state->reporter.error(bin->location, "Ambiguous operators:");
				for (auto candidate : candidates) {
					state->reporter.info(candidate.overload.lambda->location, "Here:");
				}
				yield(TypecheckResult::fail);
			}

			auto selected = candidates[0];

			assert(implicitly_cast(state, 0, &bin->left,  selected.overload.lambda->parameters[0]->type));
			assert(implicitly_cast(state, 0, &bin->right, selected.overload.lambda->parameters[1]->type));

			auto call = AstCall::create();
			call->location = bin->location;
			call->unsorted_arguments.add({{}, bin->left});
			call->unsorted_arguments.add({{}, bin->right});
			call->sorted_arguments.add(bin->left);
			call->sorted_arguments.add(bin->right);
			call->callable = make_identifier(selected.overload.definition->definition);
			assert(selected.overload.lambda->type->kind == Ast_LambdaType);
			call->lambda_type = (AstLambdaType *)selected.overload.lambda->type;
			call->type = selected.overload.lambda->return_parameter->type;

			if (selected.ass) {
				auto ass = make_binop(BinaryOperation::ass, bin->left, call);
				ass->location = bin->location;
				ass->type = compiler->builtin_void.ident;

				return ass;
			} else {
				return call;
			}
		}
	}
	return bin;
}
AstExpression *typecheck(TypecheckState *state, AstUnaryOperator *unop) {
	using enum UnaryOperation;

	// NOTE: if following switch does not set the type, overloads will be searched

	switch (unop->operation) {
		case plus: {
			typecheck(state, unop->expression);

			// FIXME: Forbid use on structs, etc.

			return unop->expression;
		}
		case minus: {
			typecheck(state, unop->expression);
			if (::is_integer(unop->expression->type)) {
				unop->type = unop->expression->type;
			} else if (::is_float(unop->expression->type)) {
				unop->type = unop->expression->type;
			}
			break;
		}
		case address_of: {
			typecheck(state, unop->expression);
			if (!is_addressable(unop->expression)) {
				unop->operation = UnaryOperation::move_to_temporary;
			}
			make_concrete(state, &unop->expression);
			unop->type = make_pointer_type(unop->expression->type);
			break;
		}
		case star: {
			typecheck(state, unop->expression);

			if (is_type(unop->expression)) {
				unop->operation = pointer;
				goto _pointer;
			} else if (as_option(unop->expression->type)) {
				unop->operation = unwrap;
				goto _unwrap;
			} else if (is_pointer(unop->expression->type)) {
				unop->operation = dereference;
				goto _dereference;
			}

			break;
		}
		case pointer: {
		_pointer:
			// done in case star
			//typecheck(state, unop->expression);
			assert(is_type(unop->expression));
			unop->type = compiler->builtin_type.ident;
			break;
		}
		case dereference: {
		_dereference:
			// done in case star
			//typecheck(state, unop->expression);
			assert(!is_type(unop->expression));
			if (!is_pointer(unop->expression->type)) {
				state->reporter.error(unop->location, "{} is not a pointer nor an option, can't dereference it", type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
			}
			unop->type = ((AstUnaryOperator *)unop->expression->type)->expression;
			break;
		}
		case unwrap: {
		_unwrap:
			// done in case star
			//typecheck(state, unop->expression);
			assert(!is_type(unop->expression));
			auto option = as_option(unop->expression->type);
			assert(option);
			unop->type = option->expression;
			break;
		}
		case lnot: {
			typecheck(state, unop->expression);

			if (!implicitly_cast(state, &state->reporter, &unop->expression, compiler->builtin_bool.ident)) {
				yield(TypecheckResult::fail);
			}

			unop->type = compiler->builtin_bool.ident;

			break;
		}
		case bnot: {
			typecheck(state, unop->expression);

			if (types_match(unop->expression->type, compiler->builtin_bool)) {
				unop->type = unop->expression->type;
				break;
			}

			if (::is_integer(unop->expression->type)) {
				unop->type = unop->expression->type;
				break;
			}

			break;
		}
		case Sizeof: {
			auto size_of = unop;

			typecheck(state, size_of->expression);

			if (!is_type(size_of->expression)) {
				state->reporter.error(size_of->expression->location, "Expression must be a type");
				yield(TypecheckResult::fail);
			}

			if (!is_constant(size_of->expression)) {
				state->reporter.error(size_of->expression->location, "Expression must be constant");
				yield(TypecheckResult::fail);
			}

			return state->make_integer(get_size(size_of->expression), size_of->location);
		}
		case autocast: {
			auto cast = unop;

			typecheck(state, cast->expression);

			cast->type = cast->expression->type;

			break;
		}
		case typeof: {
			auto typeof = unop;
			typecheck(state, typeof->expression);
			make_concrete(state, &typeof->expression);
			typeof->type = compiler->builtin_type.ident;
			//  print("typeof {} is {}\n", typeof->expression->location, type_to_string(typeof->expression->type));
			break;
		}
		case option: {
			typecheck(state, unop->expression);
			if (is_type(unop->expression)) {
				unop->type = compiler->builtin_type.ident;
			} else {
				state->reporter.error(unop->location, "Can't apply unary {} to an expression of type {}.", as_string(unop->operation), type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
			}
			break;
		}
		case typeinfo: {
			typecheck(state, unop->expression);
			if (!is_type(unop->expression)) {
				state->reporter.error(unop->expression->location, "This is not a type. Maybe you meant #typeinfo #typeof {}?", unop->expression->location);
				yield(TypecheckResult::fail);
			}

			return get_typeinfo(state, unop->expression, unop->location);
		}
		case dot: {
			if (unop->expression->kind != Ast_Identifier) {
				state->reporter.error(unop->expression->location, "Dot unary operation may apply to identifiers only");
				yield(TypecheckResult::fail);
			}
			unop->type =
			unop->expression->type = compiler->builtin_unknown_enum.ident;
			break;
		}
		case pack: {
			typecheck(state, unop->expression);
			if (is_type(unop->expression)) {
				unop->type = compiler->builtin_type.ident;
			} else {
				if (auto Struct = direct_as<AstStruct>(unop->expression->type)) {
					if (Struct->is_span) {
						unop->type = unop->expression->type;
						break;
					}
				}
				state->reporter.error(unop->expression->location, "Type {} is not unpackable. Only span type can be unpacked.", type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
			}
			break;
		}
		case poly: {
			auto ident = as<AstIdentifier>(unop->expression);
			if (!ident) {
				state->reporter.error(unop->location, "There must be an identifier after {}.", unop->operation);
				yield(TypecheckResult::fail);
			}

			if (!state->lambda_currently_accepting_poly_types) {
				state->reporter.error(unop->location, "This can only appear in lambda's parameter list.");
				yield(TypecheckResult::fail);
			}

			auto def = AstDefinition::create();
			def->location = unop->location;
			def->type = compiler->builtin_type.ident;
			def->name = ident->name;
			def->is_constant = true;

			state->lambda_currently_accepting_poly_types->constant_scope->add(def);

			unop->type = compiler->builtin_type.ident;
			break;
		}
		default: {
			state->reporter.error(unop->location, "INTERNAL ERROR: Unknown unary operation.");
			yield(TypecheckResult::fail);
		}
	}

	if (!unop->type) {
		auto op_ident = AstIdentifier::create();
		op_ident->name = get_unop_ident_name(unop->operation);
		op_ident->location = unop->location;

		auto call = AstCall::create();
		call->location = unop->location;
		call->callable = op_ident;
		call->unsorted_arguments.add({{}, unop->expression});

		auto new_state = get_pooled_typecheck_state(call, state);

	retry:
		SWITCH_TO_FIBER(new_state->fiber);
		current_typecheck_state = state;
		switch (fiber_result) {
			case TypecheckResult::wait: {
				yield(TypecheckResult::wait);
				goto retry;
			}
			case TypecheckResult::fail: {
				state->reporter.error(unop->location, "No unary operator {} defined for {}", unop->operation, type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
				break;
			}
		}

		return call;
	}

	return unop;
}
AstExpression *typecheck(TypecheckState *state, AstStruct *Struct) {
	bool anonymous = false;

	if (!Struct->definition) {
		Struct->definition = AstDefinition::create();
		Struct->definition->location = Struct->location;
		Struct->definition->name = format("_{}"str, Struct->uid);
		Struct->definition->type = compiler->builtin_type.ident;
		Struct->definition->expression = Struct;
		Struct->definition->is_constant = true;
		Struct->definition->is_hidden = true;
		state->current_scope->add(Struct->definition);
		anonymous = true;
	}

	Struct->definition->type = Struct->type = compiler->builtin_type.ident;

	scoped_replace(state->container_node, Struct);
	scoped_replace(state->current_loop, 0);
	push_scope(Struct->parameter_scope);

	if (Struct->is_template) {
		for (auto &parameter : Struct->parameter_scope->definition_list) {
			typecheck(state, parameter);
		}
	} else {
		push_scope(Struct->member_scope);

		s64 struct_size = 0;
		s64 struct_alignment = 1;
		s64 member_offset_cursor = 0;


		for (auto &definition : Struct->member_scope->definition_list) {
			if (!definition->is_constant)
				Struct->data_members.add(definition);
		}

		if (Struct->data_members.count == 0) {
			Struct->size = 1;
			Struct->alignment = 1;
		}

		// NOTE: Struct->member_scope may change in following loops, because
		//       unnamed structs inside them may insert their definitions into this scope.

		auto member_statements = copy(Struct->member_scope->statement_list);

		List<TypecheckState *> new_states;
		new_states.resize(Struct->member_scope->statement_list.count);

		for (umm i = 0; i < member_statements.count; ++i) {
			auto &new_state = new_states[i];
			auto &statement = member_statements[i];

			new_state = get_pooled_typecheck_state(statement, state);
		}

		while (1) {
			bool all_finished = true;
			for (umm i = 0; i < member_statements.count; ++i) {
				auto &new_state = new_states[i];
				if (new_state->result != TypecheckResult::wait)
					continue;

				SWITCH_TO_FIBER(new_state->fiber);
				current_typecheck_state = state;
				switch ((TypecheckResult)fiber_result) {
					case TypecheckResult::fail: {
						state->reporter.reports.add(new_state->reporter.reports);
						yield(TypecheckResult::fail);
						break;
					}
					case TypecheckResult::wait: {
						all_finished = false;
						yield(TypecheckResult::wait);
						break;
					}

					case TypecheckResult::success: {
						auto all = [](auto x, auto predicate) {
							for (auto v : x) {
								if (!predicate(v))
									return false;
							}
							return true;
						};

						if (Struct->size == -1 && all(Struct->data_members, [](auto d){ return d->type; })) {

							bool has_default_initialized_members = false;

							for (auto &member : Struct->data_members) {
								if (types_match(member->type, compiler->builtin_void)) {
									state->reporter.error(member->location, "Can't use member of type Void in a struct.");
									yield(TypecheckResult::fail);
								}
							}

							for (auto member : Struct->data_members) {
								if (member->expression) {
									has_default_initialized_members = true;
								}

								if (auto member_struct = direct_as<AstStruct>(member->type)) {
									if (member_struct->default_value) {
										has_default_initialized_members = true;
									}
								}
							}

							if (Struct->is_union) {
								for (auto &member : Struct->data_members) {
									switch (Struct->layout) {
										case StructLayout::tlang:
										case StructLayout::c: {
											auto member_size  = get_size (state, member->type);
											auto member_align = get_align(state, member->type);

											struct_alignment = max(struct_alignment, member_align);

											member->offset = 0;
											struct_size = max(struct_size, member_size);
											break;
										}
										default:
											invalid_code_path();
									}
								}
							} else {
								for (auto &member : Struct->data_members) {
									switch (Struct->layout) {
										case StructLayout::tlang:
										case StructLayout::c: {
											auto member_size  = get_size (state, member->type);
											auto member_align = get_align(state, member->type);

											if (member->placed_at.is_empty()) {
												member_offset_cursor = ceil(member_offset_cursor, member_align);
												member->offset = member_offset_cursor;
											} else {
												auto found_destination = find_if(Struct->data_members, [&](AstDefinition *other) {return other->name == member->placed_at; });
												if (!found_destination) {
													state->reporter.error(member->location, "Can't place member at '{}', it was not defined.", member->placed_at);
													yield(TypecheckResult::fail);
												}

												auto destination = *found_destination;

												if (destination->offset == -1) {
													state->reporter.error(member->location, "Can't place member at '{}', it's offset is not computed yet. You can place this definition after '{}' to solve this issue.", member->placed_at, member->placed_at);
													yield(TypecheckResult::fail);
												}
												member->offset = destination->offset;

												if (member->offset % member_align != 0) {
													state->reporter.warning(member->location, "This is placed at offset {}, which is not divisible by it's alignment {}.", member->offset, member_align);
												}

												member_offset_cursor = destination->offset;
											}

											member_offset_cursor += member_size;

											struct_alignment = max(struct_alignment, member_align);
											struct_size = max(struct_size, member_offset_cursor);

											break;
										}
										default:
											invalid_code_path();
									}
								}
							}
							Struct->alignment = struct_alignment;
							Struct->size = max(1, ceil(struct_size, struct_alignment));

							if (has_default_initialized_members) {
								List<AstExpression *> values;
								for (auto member : Struct->data_members) {
									if (member->expression) {
										values.add(member->expression);
									} else {
										if (auto member_struct = direct_as<AstStruct>(member->type); member_struct && member_struct->default_value)
											values.add(deep_copy(raw(member_struct->default_value)));
										else
											values.add(state->make_null(member->type, {}));
									}
								}

								auto initializer = make_struct_initializer(Struct, values, Struct->location);
								Struct->default_value = evaluate(state, initializer);
								if (!Struct->default_value) {
									yield(TypecheckResult::fail);
								}
								Struct->default_value_offset = put_in_section(state, Struct->default_value, compiler->constant_section);
							}
						}
						break;
					}
				}
			}
			if (all_finished)
				break;
		}
	}

	if (anonymous) {
		auto ident = AstIdentifier::create();
		ident->location = Struct->location;
		ident->type = Struct->definition->type;
		ident->name = Struct->definition->name;
		ident->possible_definitions.set(Struct->definition);
		return ident;
	} else {
		return Struct;
	}
}
AstExpression *typecheck(TypecheckState *state, AstSubscript *subscript) {
	if (subscript->index_expression) {
		typecheck(state, subscript->index_expression);
		make_concrete(state, &subscript->index_expression);

		if (!::is_integer(subscript->index_expression->type)) {
			state->reporter.error(subscript->index_expression->location, "Expression must be of type integer but is {}", type_to_string(subscript->index_expression->type));
			yield(TypecheckResult::fail);
		}
	}

	typecheck(state, subscript->expression);
	auto type = subscript->expression->type;
	if (auto array = as<AstArray>(type)) {
		subscript->type = array->element_type;
	} else if (auto subtype = get_span_subtype(type)) {
		subscript->type = subtype;
	} else if (auto pointer = as_pointer(type)) {
		subscript->type = pointer->expression;
	} else if (types_match(type, compiler->builtin_string)) {
		subscript->type = compiler->builtin_u8.ident;
	} else if (
		types_match(type, compiler->builtin_u8) ||
		types_match(type, compiler->builtin_u16) ||
		types_match(type, compiler->builtin_u32) ||
		types_match(type, compiler->builtin_u64) ||
		types_match(type, compiler->builtin_s8) ||
		types_match(type, compiler->builtin_s16) ||
		types_match(type, compiler->builtin_s32) ||
		types_match(type, compiler->builtin_s64)
	) {
		subscript->type = compiler->builtin_bool.ident;
	} else {
		state->reporter.error(subscript->location, "Expression is not subscriptable");
		yield(TypecheckResult::fail);
	}
	return subscript;
}
AstExpression *typecheck(TypecheckState *state, AstArray *array) {
	if (array->count_expression) {
		typecheck(state, array->count_expression);

		auto evaluated = evaluate(state, array->count_expression);

		if (!evaluated) {
			state->reporter.error(array->count_expression->location, "Could not evaluate expression.");
			yield(TypecheckResult::fail);
		}

		if (evaluated->literal_kind != LiteralKind::integer) {
			state->reporter.error(array->count_expression->location, "Expression must be of type integer but is {}", type_to_string(array->count_expression->type));
			yield(TypecheckResult::fail);
		}

		if (evaluated->integer.msb) {
			state->reporter.error(array->count_expression->location, "Array count must be positive");
			yield(TypecheckResult::fail);
		}

		if (evaluated->integer.parts.count > 1) {
			state->reporter.error(array->count_expression->location, "Array count is too big");
			yield(TypecheckResult::fail);
		}

		array->count = (s64)evaluated->integer;
	}

	typecheck(state, array->element_type);
	array->type = compiler->builtin_type.ident;
	return array;
}
AstExpression *typecheck(TypecheckState *state, AstSpan *span) {
	typecheck(state, span->expression);
	if (!is_type(span->expression)) {
		state->reporter.error(span->expression->location, "This must be a type.");
		yield(TypecheckResult::fail);
	}

	auto subtype = direct(span->expression);

	return instantiate_span(subtype, span->location);
}
AstExpression *typecheck(TypecheckState *state, AstIf *If) {
	typecheck(state, If->condition);
	if (!implicitly_cast(state, &state->reporter, &If->condition, compiler->builtin_bool.ident)) {
		yield(TypecheckResult::fail);
	}

	if (If->is_constant) {
		auto condition = evaluate(state, If->condition);
		if (!condition)
			yield(TypecheckResult::fail);


		if (condition->literal_kind != LiteralKind::boolean) {
			state->reporter.error(If->condition->location, "Expression must have bool type");
			yield(TypecheckResult::fail);
		}

		If->true_branch_was_taken = condition->Bool;
		auto block = condition->Bool ? If->true_block : If->false_block;

		// FIXME: Figure out what to do with defers.
		// FIXME: Put statements directly in parent scope in the right order.
		//
		// Instead of this:
		//
		//     typecheck(state, scope);
		//
		// We do this:
		//
		for (auto statement : block->scope->statement_list) {
			typecheck(state, statement);
		}
		//
		// Because constant if's scopes are not regular.
		// We want all statements to be outside these scopes as if they were directly
		// in the parent scope.
		// `typecheck(Scope)` sets the scope as the parent, and this is unwanted in this case.
		//
		// Note that right now this if statement remains to be in the parent's statement list.
		// I think a better solution to this problem would be to add all selected statements into parent scope and remove the if.
		//

		// TODO:
		// This should put children's statements in the place of children scope.
		//   block->scope->parent->consume(block->scope);

		auto found = find(block->scope->parent->children, block->scope);
		assert(found);
		erase(block->scope->parent->children, found);
		block->scope->parent->append(*block->scope);

		// FIXME: hack
		If->type = compiler->builtin_void.ident;
	} else {
		typecheck(state, If->true_block);
		typecheck(state, If->false_block);

		if (types_match(If->true_block->type, If->false_block->type)) {
			If->type = If->true_block->type;
		} else {
			If->type = compiler->builtin_deferred_if.ident;
		}
	}
	return If;
}
AstExpression *typecheck(TypecheckState *state, AstTest *test) {
	auto new_state = get_pooled_typecheck_state(test->scope, state);

	bool did_compile = false;

	while (1) {
		SWITCH_TO_FIBER(new_state->fiber);
		current_typecheck_state = state;
		switch (fiber_result) {
			case TypecheckResult::success: {
				did_compile = true;
				goto _break;
			}
			case TypecheckResult::fail: {
				did_compile = false;
				goto _break;
			}
			case TypecheckResult::wait: {
				yield(TypecheckResult::wait);
				break;
			}
			default: {
				invalid_code_path("something wrong with fiberutines");
			}
		}
	}
_break:

	auto result = state->make_bool(did_compile);
	result->location = test->location;
	return result;
}
AstExpression *typecheck(TypecheckState *state, AstEnum *Enum) {
	bool anonymous = false;

	if (!Enum->definition) {
		Enum->definition = AstDefinition::create();
		Enum->definition->location = Enum->location;
		Enum->definition->name = format("_{}"str, Enum->uid);
		Enum->definition->type = compiler->builtin_type.ident;
		Enum->definition->expression = Enum;
		Enum->definition->is_constant = true;
		Enum->definition->is_hidden = true;
		state->current_scope->add(Enum->definition);
		anonymous = true;
	}

	Enum->definition->type = Enum->type = compiler->builtin_type.ident;

	BigInteger counter;

	scoped_replace(state->container_node, Enum);
	scoped_replace(state->current_loop, 0);
	for (auto definition : Enum->scope->definition_list) {
		if (definition->expression) {

			typecheck(state, definition->expression);

			if (!is_constant(definition->expression)) {
				state->reporter.error(definition->expression->location, "Expression must be constant");
				yield(TypecheckResult::fail);
			}

			if (!::is_integer(definition->type)) {
				state->reporter.error(definition->location, "This must be an integer");
				yield(TypecheckResult::fail);
			}

			not_implemented();
			//counter = copy(get_constant_integer(definition->expression).value());
		} else {
			definition->expression = state->make_integer(copy(counter), definition->location);
		}

		definition->type = Enum;

		counter += 1ull;
	}

	if (anonymous) {
		auto ident = AstIdentifier::create();
		ident->location = Enum->location;
		ident->type = Enum->definition->type;
		ident->name = Enum->definition->name;
		ident->possible_definitions.set(Enum->definition);
		return ident;
	} else {
		return Enum;
	}
}
AstExpression *typecheck(TypecheckState *state, AstArrayInitializer *ArrayInitializer) {
	if (ArrayInitializer->elements.count == 0) {
		state->reporter.error(ArrayInitializer->location, "Array literal must have at least one element");
		yield(TypecheckResult::fail);
	}

	for (auto &element : ArrayInitializer->elements) {
		typecheck(state, element);
	}

	// TODO: maybe delay hardening until array is used somewhere.
	make_concrete(state, &ArrayInitializer->elements[0]);
	for (auto &element : ArrayInitializer->elements.skip(1)) {
		if (!implicitly_cast(state, &state->reporter, &element, ArrayInitializer->elements[0]->type)) {
			state->reporter.info(ArrayInitializer->elements[0]->location, "Every element of array literal must be implicitly convertible to type of first element");
			yield(TypecheckResult::fail);
		}
	}

	auto type = AstArray::create();
	type->element_type = ArrayInitializer->elements[0]->type;
	type->count = ArrayInitializer->elements.count;
	type->count_expression = make_integer(type->count);
	type->location = ArrayInitializer->location;
	type->type = compiler->builtin_type.ident;

	ArrayInitializer->type = type;

	return ArrayInitializer;
}
AstExpression *typecheck(TypecheckState *state, AstBlock *Block) {
	typecheck(state, Block->scope);

	Block->type = [&] () -> AstExpression * {
		for (auto statement : Block->scope->statement_list) {
			switch (statement->kind) {
				case Ast_Return:
				case Ast_LoopControl:
					return compiler->builtin_unreachable.ident;
			}
		}

		if (Block->scope->statement_list.count) {
			auto last_statement = Block->scope->statement_list.back();
			if (auto est = as<AstExpressionStatement>(last_statement)) {
				return est->expression->type;
			}
		}
		return compiler->builtin_void.ident;
	}();

	return Block;
}
AstExpression *typecheck(TypecheckState *state, AstMatch *Match) {
	typecheck(state, Match->expression);

	bool has_default_case = false;
	u32 n_cases_handled = 0;

	for (auto &Case : Match->cases) {
		if (Case.expression) {
			typecheck(state, Case.expression);
			if (!implicitly_cast(state, &state->reporter, &Case.expression, Match->expression->type)) {
				// state->reporter.error(Case.expression->location, "Expression has type {}, which does not match the matchable type {}.", type_to_string(Case.expression->type), type_to_string(Match->expression->type));
				yield(TypecheckResult::fail);
			}

			auto case_value = evaluate(state, Case.expression);
			if (!case_value) {
				state->reporter.error(Case.expression->location, "Could not evaluate this value.");
				yield(TypecheckResult::fail);
			}

			switch (case_value->literal_kind) {
				case LiteralKind::integer:
					Case.value = (u64)case_value->integer;
					break;
				case LiteralKind::character:
					Case.value = (u64)case_value->character;
					break;
				default:
					state->reporter.error(Case.expression->location, "Currently only integers and characters are supported in match cases.");
					yield(TypecheckResult::fail);
			}

			n_cases_handled += 1;
		} else {
			has_default_case = true;
		}
		typecheck(state, Case.block);
	}

	if (!has_default_case) {
		switch (Match->expression->type->directed->kind) {
			case Ast_Enum: {
				auto Enum = (AstEnum *)Match->expression->type->directed;

				if (n_cases_handled != Enum->scope->statement_list.count) {
					List<bool> handled;
					handled.resize(Enum->scope->statement_list.count);


					for (auto &Case : Match->cases) {
						auto found_enum_value_defn = find_if(Enum->scope->statement_list, [&](AstStatement *s) {
							assert(s->kind == Ast_Definition);

							auto enum_value = evaluate(state, ((AstDefinition *)s)->expression);
							if (enum_value->literal_kind != LiteralKind::integer) {
								state->reporter.error(Case.expression->location, "INTERNAL ERROR: abracadabra");
								yield(TypecheckResult::fail);
							}

							return Case.value == enum_value->integer;
						});

						assert(found_enum_value_defn);

						handled[index_of(Enum->scope->statement_list, found_enum_value_defn)] = true;
					}

					state->reporter.error(Match->location, "Not all cases handled.");
					for (umm i = 0; i < Enum->scope->statement_list.count; ++i) {
						if (!handled[i]) {
							state->reporter.info("{}", Enum->scope->statement_list[i]->location);
						}
					}

					yield(TypecheckResult::fail);
				}
				break;
			}
		}
	}

	if (Match->cases.count) {
		auto result_type = Match->cases[0].block->type;
		for (auto &Case : Match->cases) {
			if (&Case == &Match->cases[0])
				continue;

			if (!types_match(result_type, Case.block->type)) {
				result_type = compiler->builtin_void.ident;
				break;
			}
		}

		Match->type = result_type;
	} else {
		Match->type = compiler->builtin_void.ident;
	}

	return Match;
}
AstExpression *typecheck(TypecheckState *state, AstDistinct *Distinct) {
	typecheck(state, Distinct->expression);

	if (!is_type(Distinct->expression)) {
		state->reporter.error(Distinct->location, "Expression after #distinct must be a type");
		yield(TypecheckResult::fail);
	}

	if (!Distinct->identifier) {
		state->reporter.error(Distinct->location, "#distinct can only be used in constant definition expressions, e.g. `Radians :: #distinct Float`");
		yield(TypecheckResult::fail);
	}

	Distinct->type = Distinct->expression->type;
	return Distinct;
}

void typecheck(TypecheckState *state, CExpression auto &expression) {
	assert(expression);

	if (expression->type) {
		return;
	}

	defer { atomic_add(&shared_progress, 1); };

	defer { if (!throwing) {
		assert(expression->type);
	}};

	scoped_replace(state->current_node, expression);

	auto new_expression = expression;

	switch (expression->kind) {
		case Ast_Identifier:       new_expression = typecheck(state, (AstIdentifier       *)expression); break;
		case Ast_Call:             new_expression = typecheck(state, (AstCall             *)expression); break;
		case Ast_Literal:          new_expression = typecheck(state, (AstLiteral          *)expression); break;
		case Ast_Lambda:           new_expression = typecheck(state, (AstLambda           *)expression); break;
		case Ast_LambdaType:       new_expression = typecheck(state, (AstLambdaType       *)expression); break;
		case Ast_BinaryOperator:   new_expression = typecheck(state, (AstBinaryOperator   *)expression); break;
		case Ast_UnaryOperator:    new_expression = typecheck(state, (AstUnaryOperator    *)expression); break;
		case Ast_Struct:           new_expression = typecheck(state, (AstStruct           *)expression); break;
		case Ast_Subscript:        new_expression = typecheck(state, (AstSubscript        *)expression); break;
		case Ast_Array:            new_expression = typecheck(state, (AstArray            *)expression); break;
		case Ast_Span:             new_expression = typecheck(state, (AstSpan             *)expression); break;
		case Ast_If:               new_expression = typecheck(state, (AstIf               *)expression); break;
		case Ast_Test:             new_expression = typecheck(state, (AstTest             *)expression); break;
		case Ast_Enum:             new_expression = typecheck(state, (AstEnum             *)expression); break;
		case Ast_ArrayInitializer: new_expression = typecheck(state, (AstArrayInitializer *)expression); break;
		case Ast_Block:            new_expression = typecheck(state, (AstBlock            *)expression); break;
		case Ast_Match:            new_expression = typecheck(state, (AstMatch            *)expression); break;
		case Ast_Distinct:         new_expression = typecheck(state, (AstDistinct         *)expression); break;
#if 0
		case Ast_Import: {
			auto import = (AstImport *)expression;
			import->type = import; // MYTYPEISME
			state->current_scope->append(*import->scope);

			// TODO: if import path is a literal, maybe do it at parse time?

			auto path_literal = evaluate(state, import->expression);
			if (!path_literal) {
				yield(TypecheckResult::fail);
			}

			if (path_literal->literal_kind != LiteralKind::string) {
				state->reporter.error(import->expression->location, "Import expression must be a string");
				yield(TypecheckResult::fail);
			}

			for (auto import_path : import_paths) {
				auto child = parse_file((String)concatenate(import_path, '\\', path_literal->string));
				if (child->result == ParseResult::ok) {
					break;
				}
				if (child->result == ParseResult::read_error) {
					continue;
				}
				assert(child->result == ParseResult::syntax_error);
				state->reporter.reports.add(child->reporter.reports);
				yield(TypecheckResult::fail);
			}

			break;
		}
#endif
		default: {
			state->reporter.error(new_expression->location, "Internal error: typecheck(AstExpression *): unhandled case '{}'", new_expression->kind);
			invalid_code_path();
			yield(TypecheckResult::fail);
		}
	}
	if (!throwing) {
		if (new_expression->kind == Ast_Identifier && ((AstIdentifier *)new_expression)->possible_definitions.count > 1) {
		} else {
			assert(new_expression->type);
			if (!simplify(&state->reporter, &new_expression))
				yield(TypecheckResult::fail);
			assert(new_expression->type);


			wait_for(state, new_expression->location, "expression type type", [&]{ return new_expression->type->type; }, [&]{
				state->reporter.error(new_expression->location, "Failed to resolve type of type of this expression.");
			});

			if (!direct(new_expression->type))
				yield(TypecheckResult::fail);

			// assert(new_expression->type->directed == direct(new_expression->type));
		}
	}
	expression = new_expression;
}

void WINAPI typecheck_pooled(void *_state) {
	auto state = (TypecheckState *)_state;
	while (1) {
		throwing = false;
		assert(state->current_scope);

		try {
			if (state->root_statement)
				typecheck(state, state->root_statement);
			else if (state->root_expression)
				typecheck(state, state->root_expression);
			else
				typecheck(state, state->root_scope);
		} catch (int x) {
			continue;
		}
		yield(TypecheckResult::success);
		current_typecheck_state = state;
	}
}

void add_member(AstStruct *destination, AstExpression *type, String name, AstLiteral *value, bool constant, s32 offset) {
	auto d = AstDefinition::create();
	d->location = name;
	d->name = name;
	d->expression = value;
	d->is_constant = constant;
	d->type = type;
	d->offset = offset;
	if (value) {
		value->type = type;
	}

	destination->member_scope->add(raw(d));
	if (!constant)
		destination->data_members.add(d);
	d->container_node = destination;
}

static void write_test_source() {
	StringBuilder test;
	append_format(test, R"FOOBAR(
main :: () {{
	xxx := foofoo(42, 69);
	write_to_console("\n\n\n\n\n\n\n\n\nYOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO\n\n\n\n\n\n\n\n");

	init_allocator();

	println("abc" == "abc");

	get_array :: () {{
		a: [3]Int;
		a[0] = 1;
		a[1] = 2;
		a[2] = 3;
		return a;
	}}

	span_to_expr := get_array() as []Int;
	println(span_to_expr);
	foobar(4, 5, 6);

	#print #typeof span_to_expr.data;

	bar(1, 2, "hello");
	bar2(1, 2, "hello");

	one := 1;
	two := 2;
	hello := "hello";
	array: [3]Any;
	array[0] = 1;
	array[1] = 2;
	array[2] = "hello";
	span := array as []Any;
	bar(..span);

	b := StringBuilder.create();

	dprintln((&b.first.next as *U8) - (&b as *U8));
	dprintln(&b.first as UInt);
	dprintln(b.last as UInt);
	dprintln(b.alloc_last as UInt);
	dprintln(b.first.next as UInt);
	println(b);
	dprintln(&b.first as UInt);
	dprintln(b.last as UInt);
	dprintln(b.alloc_last as UInt);
	dprintln(b.first.next as UInt);

	append_format(&b, "burbek {{}} lol {{}} kek {{}} 123", 12, 34, "cheburek");
	println(to_string(&b));

	// println("burbek {{}} lol {{}} kek {{}} 123", 12, 34, "cheburek");

	test := Test("hello", 42);
	using test;
	println(test);
	str = "world";
	value = 12;
	println(test);

	foo();
}}
)FOOBAR");
	for (u32 i = 0; i < 4096; ++i) {
		append_format(test, R"FOOBAR(
import "string.tl"

_{} :: () {{
	make_string :: (begin: *U8, end: *U8) => String(begin, end as UInt - begin as UInt);

	operator ? :: (str: String) => str.data != null;

	// operator for :: (array: []$T, body: %Code) {{
	// 	i: UInt = 0;
	// 	while i != array.count {{
	// 		#insert body;
	// 		i += 1;
	// 	}}
	// }}

	find :: (where: String, what: String): ?String {{
		if (where.count - what.count + 1) as Int <= 0
			return null;

		i: UInt = 0;
		same := true;
		j: UInt = 0;
		while i < where.count - what.count + 1 {{
			defer i += 1;
			same = true;
			j = 0;
			while j < what.count {{
				defer j += 1;
				if what[j] != where[i + j]
					same = false;
			}}
			if same
				return String(&where[i], what.count);
		}}
		return null;
	}}
	append_format :: (b: *StringBuilder, format: String, args: ..Any): UInt {{
		remaining := format;

		i: UInt = 0;
		while i != args.count {{
			y := find(remaining, "{{}}");
			assert y; // invalid format String
			x := *y;

			append(b, String(remaining.data, @(x.data - remaining.data)));

			append(b, args[i]);

			remaining.count -= (x.data - remaining.data) as UInt + 2;
			remaining.data = x.data + 2;

			i += 1;
		}}

		append(b, remaining);

		return 0;
	}}

	Test :: struct {{
		str: String;
		value: Int;
	}}

	bar :: (values: ..Any) {{
		i: UInt = 0;
		while i < values.count {{
			print(values[i]);
			dprint_char(' ');
			i += 1;
		}}
		dprint_char('\n');
	}}
	bar2 :: (values: ..Any) => bar(..values);

	foobar :: (a: ..Int) {{
		println(a);
		println(..a);
	}}

	foofoo :: (a: Int, b: Int) => a * b;

	main :: () {{
		xxx := foofoo(42, 69);
		write_to_console("\n\n\n\n\n\n\n\n\nYOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO\n\n\n\n\n\n\n\n");

		init_allocator();

		println("abc" == "abc");

		get_array :: () {{
			a: [3]Int;
			a[0] = 1;
			a[1] = 2;
			a[2] = 3;
			return a;
		}}

		span_to_expr := get_array() as []Int;
		println(span_to_expr);
		foobar(4, 5, 6);

		#print #typeof span_to_expr.data;

		bar(1, 2, "hello");
		bar2(1, 2, "hello");

		one := 1;
		two := 2;
		hello := "hello";
		array: [3]Any;
		array[0] = 1;
		array[1] = 2;
		array[2] = "hello";
		span := array as []Any;
		bar(..span);

		b := StringBuilder.create();

		dprintln((&b.first.next as *U8) - (&b as *U8));
		dprintln(&b.first as UInt);
		dprintln(b.last as UInt);
		dprintln(b.alloc_last as UInt);
		dprintln(b.first.next as UInt);
		println(b);
		dprintln(&b.first as UInt);
		dprintln(b.last as UInt);
		dprintln(b.alloc_last as UInt);
		dprintln(b.first.next as UInt);

		append_format(&b, "burbek {{}} lol {{}} kek {{}} 123", 12, 34, "cheburek");
		println(to_string(&b));

		// println("burbek {{}} lol {{}} kek {{}} 123", 12, 34, "cheburek");

		test := Test("hello", 42);
		using test;
		println(test);
		str = "world";
		value = 12;
		println(test);

		foo();
	}}

	foo :: () {{
		a := 1;
		b := 2;
		c := 3;
		d := 4;
		e := 5;
		f := a + b * c + d * e;
		println(f);
		return f;
	}}
}}

)FOOBAR", i);
	}
	write_entire_file("performance_test.tl"s, as_bytes(to_string(test)));
}

#define e(name) void mark_referenced_definitions(Ast##name *node);
ENUMERATE_AST_KIND
#undef e

void mark_referenced_definitions(AstNode *node) {
	switch (node->kind) {
#define e(name) case Ast_##name: return mark_referenced_definitions((Ast##name *)node);
ENUMERATE_AST_KIND
#undef e
	}
	invalid_code_path();
}

void mark_referenced_definitions(Scope *scope) {
	for (auto statement : scope->statement_list) {
		mark_referenced_definitions(statement);
	}
}

void mark_referenced_definitions(AstDefinition *Definition) {
	if (Definition->is_referenced)
		return;
	Definition->is_referenced = true;
	if (Definition->container_node)
		mark_referenced_definitions(Definition->container_node);

	if (Definition->expression)
		mark_referenced_definitions(Definition->expression);
}
void mark_referenced_definitions(AstReturn *Return) {
	if (Return->expression)
		mark_referenced_definitions(Return->expression);
}
void mark_referenced_definitions(AstYield *Yield) {
	if (Yield->expression)
		mark_referenced_definitions(Yield->expression);
}
void mark_referenced_definitions(AstIdentifier *Identifier) {
	assert(Identifier->possible_definitions.count == 1);
	mark_referenced_definitions(Identifier->possible_definitions[0]);
}
void mark_referenced_definitions(AstLambda *Lambda) {
	if (Lambda->definition) {
		Lambda->definition->is_referenced = true;
	}
	//mark_referenced_definitions(Lambda->body_scope);
	mark_referenced_definitions(Lambda->body);
}
void mark_referenced_definitions(AstBinaryOperator *BinaryOperator) {
	mark_referenced_definitions(BinaryOperator->left);
	mark_referenced_definitions(BinaryOperator->right);
}
void mark_referenced_definitions(AstUnaryOperator *UnaryOperator) {
	mark_referenced_definitions(UnaryOperator->expression);
}
void mark_referenced_definitions(AstStruct *Struct) {
	mark_referenced_definitions(Struct->member_scope);
}
void mark_referenced_definitions(AstCall *Call) {
	mark_referenced_definitions(Call->callable);
	for (auto argument : Call->sorted_arguments)
		mark_referenced_definitions(argument);
}
void mark_referenced_definitions(AstIf *If) {
	mark_referenced_definitions(If->condition);
	mark_referenced_definitions(raw(If->true_block));
	mark_referenced_definitions(raw(If->false_block));
}
void mark_referenced_definitions(AstWhile *While) {
	mark_referenced_definitions(While->condition);
	mark_referenced_definitions(While->scope);
}
void mark_referenced_definitions(AstBlock *Block) {
	mark_referenced_definitions(Block->scope);
}
void mark_referenced_definitions(AstTuple *Tuple) {
	for (auto expression : Tuple->expressions)
		mark_referenced_definitions(expression);
}
void mark_referenced_definitions(AstExpressionStatement *ExpressionStatement) {
	mark_referenced_definitions(ExpressionStatement->expression);
}
void mark_referenced_definitions(AstSubscript *Subscript) {
	mark_referenced_definitions(Subscript->expression);
	mark_referenced_definitions(Subscript->index_expression);
}
void mark_referenced_definitions(AstArray *Array) {
	mark_referenced_definitions(Array->element_type);
	mark_referenced_definitions(Array->count_expression);
}
void mark_referenced_definitions(AstAssert *Assert) {
	if (!Assert->is_constant)
		mark_referenced_definitions(Assert->condition);
}
void mark_referenced_definitions(AstDefer *Defer) {
	mark_referenced_definitions(Defer->scope);
}
void mark_referenced_definitions(AstOperatorDefinition *OperatorDefinition) {
	mark_referenced_definitions(raw(OperatorDefinition->definition));
}
void mark_referenced_definitions(AstMatch *Match) {
	mark_referenced_definitions(Match->expression);
	for_each(Match->cases, [&](MatchCase &Case) {
		if (Case.expression)
			mark_referenced_definitions(Case.expression);
		mark_referenced_definitions(raw(Case.block));
	});
}
void mark_referenced_definitions(AstUsing *Using) {
	mark_referenced_definitions(Using->expression);
}
void mark_referenced_definitions(AstArrayInitializer *ArrayInitializer) {
	for (auto &element : ArrayInitializer->elements)
		mark_referenced_definitions(element);
}
void mark_referenced_definitions(AstLambdaType *LambdaType) {}
void mark_referenced_definitions(AstLiteral *Literal) {}
void mark_referenced_definitions(AstTest *Test) {}
void mark_referenced_definitions(AstPrint *Print) {}
void mark_referenced_definitions(AstParse *Parse) {}
void mark_referenced_definitions(AstEnum *Enum) {}
void mark_referenced_definitions(AstLoopControl *LoopControl) {}
void mark_referenced_definitions(AstEmptyStatement *EmptyStatement) {}
void mark_referenced_definitions(AstFor *For) { invalid_code_path("'For' node should be replaced with 'While' after typechecking"); }
void mark_referenced_definitions(AstSpan *Span) { invalid_code_path("'Span' node should be replaced with an instance of 'Span' struct after typechecking"); }
void mark_referenced_definitions(AstDistinct *Distinct) {
	mark_referenced_definitions(Distinct->expression);
}

struct ParsedArguments {
	String target;
	String output;

	List<String> source_files;

	bool no_typecheck = false;
	bool debug_paths = false;
	bool track_allocations = false;
	bool success = false;
};

ParsedArguments parse_arguments(Span<Span<utf8>> arguments) {
	timed_function(compiler->profiler);

	ParsedArguments result = {};

	compiler->compiler_path = (String)get_executable_path();

	auto parsed = parse_path(compiler->compiler_path);
	compiler->compiler_name = parsed.name;
	compiler->compiler_directory = parsed.directory;

	// nasm is debuggable, fasm is fast
	// set default target to fasm because waiting for nasm is not fun
	result.target = "fasm_x86_64_windows"str;

	//print("compiler_path: {}\ncompiler_name: {}\ncompiler_directory: {}\n", compiler_path, compiler_name, compiler_directory);

	if (arguments.count == 1) {
		print_help();
		return result;
	}

	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] == "--print-ast"str) {
			++i;
			if (i >= arguments.count) {
				immediate_error("Expected an argument after --print-ast.\n");
				return result;
			}
			else immediate_error("Unexpected argument after --print-ast.\n");
		} else if (arguments[i] == "--no-type-check"str) {
			result.no_typecheck = true;
		} else if (arguments[i] == "--debug-paths"str) {
			result.debug_paths = true;
		} else if (arguments[i] == "--track"str) {
			result.track_allocations = true;
		} else if (arguments[i] == "--target"str) {
			++i;
			if (i >= arguments.count) {
				immediate_error("Expected an argument after --target.\n");
				return result;
			}
			result.target = arguments[i];
		} else if (arguments[i] == "--output"str) {
			++i;
			if (i >= arguments.count) {
				immediate_error("Expected an argument after --output.\n");
				return result;
			}
			result.output = arguments[i];
		} else if (arguments[i] == "--profile"str) {
			compiler->do_profile = true;
		} else if (arguments[i] == "--keep-temp"str) {
			compiler->keep_temp = true;
		} else if (arguments[i] == "--debug-template"str) {
			compiler->debug_template = true;
		} else if (starts_with(arguments[i], "--lower"str)) {
			compiler->print_lowered = true;
			compiler->print_lowered_filter = arguments[i].subspan("--lower"str.count);
		} else if (arguments[i] == "--optimize"str) {
			compiler->optimize = true;
		} else if (arguments[i] == "--yield"str) {
			compiler->print_yields = true;
		} else if (arguments[i] == "--dce"str) {
			compiler->enable_dce = true;
		} else if (arguments[i] == "--passes"str) {
			++i;
			if (i >= arguments.count) {
				immediate_error("Expected a number of optimization passes after --passes.\n");
				return result;
			}
			compiler->optimization_pass_count = parse_u64(arguments[i]).value_or(4);
		} else if (starts_with(arguments[i], "--"str)) {
			immediate_info("Unknown argument: {}", arguments[i]);
		} else {
			result.source_files.add(arguments[i]);
		}
	}

	result.success = true;
	return result;
}

#include <tl/masked_block_list.h>

#if 0

struct SlabAllocator {
	template <umm size>
	struct Slabs {
		struct alignas(size) Slab {
			u8 data[size];
		};

		inline static constexpr umm slabs_per_block = 1 * MiB / sizeof(Slab);

		StaticMaskedBlockList<Slab, slabs_per_block> list;

		void *allocate() {
			auto result = list.add();
			return result.pointer;
		}
		void free(void *data) {
			list.remove((Slab *)data);
		}
	};


	Slabs<  32> slabs32;
	Slabs<  64> slabs64;
	Slabs< 128> slabs128;
	Slabs< 256> slabs256;
	Slabs< 512> slabs512;
	Slabs<1024> slabs1024;
	Slabs<2048> slabs2048;
	Slabs<4096> slabs4096;

	void *allocate(umm size, umm alignment, std::source_location location) {
		assert(alignment > 0);
		assert(size > 0);
		assert(size >= alignment);
		assert(size % alignment == 0);

		auto slab_size = ceil(size, alignment);
		if (slab_size <= 256) if (slab_size <=   64) if (slab_size <=   32) return slabs32  .allocate();
													 else                   return slabs64  .allocate();
							  else                   if (slab_size <=  128) return slabs128 .allocate();
													 else                   return slabs256 .allocate();
		else                  if (slab_size <= 1024) if (slab_size <=  512) return slabs512 .allocate();
													 else                   return slabs1024.allocate();
							  else                   if (slab_size <= 2048) return slabs2048.allocate();
													 else                   return slabs4096.allocate();
		return MyAllocator{}.allocate_impl(size, alignment, location);
	}
	void *reallocate(void *data, umm old_size, umm new_size, umm alignment, std::source_location location) {
		assert(alignment > 0);
		assert(new_size > 0);
		assert(new_size >= alignment);
		assert(new_size % alignment == 0);
		assert(old_size > 0);
		assert(old_size >= alignment);
		assert(old_size % alignment == 0);

		auto result = allocate(new_size, alignment, location);
		memcpy(result, data, old_size);
		deallocate(data, old_size, alignment, location);
		return result;
	}
	void deallocate(void *data, umm size, umm alignment, std::source_location location) {
		assert(alignment > 0);
		assert(size > 0);
		assert(size >= alignment);
		assert(size % alignment == 0);

		auto slab_size = ceil(size, alignment);

		if (slab_size <= 256) if (slab_size <=   64) if (slab_size <=   32) return slabs32  .free(data);
													 else                   return slabs64  .free(data);
							  else                   if (slab_size <=  128) return slabs128 .free(data);
													 else                   return slabs256 .free(data);
		else                  if (slab_size <= 1024) if (slab_size <=  512) return slabs512 .free(data);
													 else                   return slabs1024.free(data);
							  else                   if (slab_size <= 2048) return slabs2048.free(data);
													 else                   return slabs4096.free(data);
		return MyAllocator{}.deallocate_impl(data, size, alignment, location);
	}
};

SlabAllocator slab_allocator;

auto slab_allocator_func(AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> void * {
	switch (mode) {
		case Allocator_allocate: {
			return slab_allocator.allocate(new_size, align, location);
		}
		case Allocator_reallocate: {
			return slab_allocator.reallocate(data, old_size, new_size, align, location);
		}
		case Allocator_free: {
			slab_allocator.deallocate(data, new_size, align, location);
			break;
		}
	}
	return 0;
}
#endif

#include <tl/tracking_allocator.h>

void init_strings();

// :type_offset:
//
// TODO: explain
void init_compiler_builtin_types() {
	// Builtin type creation is done in 4 steps:
	// 1. Create all necessary nodes for structs, enums and aliases.
	// 2. Init all structs and enums.
	// 3. Init aliases.
	// 4. Add members to structs. They depend on aliases being initialized.


	// ========================
	// Step 1: Create all nodes
	// ========================
	auto create_builtin = [&](auto &builtin) {
		builtin.definition = AstDefinition::create();
		builtin.ident = AstIdentifier::create();
	};
	auto create_struct = [&](BuiltinStruct &type) {
		create_builtin(type);
		type.ident->directed = type.Struct = AstStruct::create();
	};
	auto create_enum = [&](BuiltinEnum &type) {
		create_builtin(type);
		type.ident->directed = type.Enum = AstEnum::create();
	};

	// Type:
	create_struct(compiler->builtin_type);

	// Primitives:
	create_struct(compiler->builtin_void);
	create_struct(compiler->builtin_bool);
	create_struct(compiler->builtin_u8);
	create_struct(compiler->builtin_u16);
	create_struct(compiler->builtin_u32);
	create_struct(compiler->builtin_u64);
	create_struct(compiler->builtin_s8);
	create_struct(compiler->builtin_s16);
	create_struct(compiler->builtin_s32);
	create_struct(compiler->builtin_s64);
	create_struct(compiler->builtin_f32);
	create_struct(compiler->builtin_f64);

	// Structs:  NOTE: members are appended after every builtin type was initialized.
	create_struct(compiler->builtin_string);
	create_struct(compiler->builtin_struct_member);
	create_struct(compiler->builtin_enum_member);
	create_struct(compiler->builtin_typeinfo);
	create_struct(compiler->builtin_any);
	create_struct(compiler->builtin_range);

	// Enums:
	create_enum(compiler->builtin_type_kind);

	// Internal types:
	create_struct(compiler->builtin_unsized_integer);
	create_struct(compiler->builtin_unsized_float);
	create_struct(compiler->builtin_noinit);
	create_struct(compiler->builtin_unknown_enum);
	create_struct(compiler->builtin_poly);
	create_struct(compiler->builtin_overload_set);
	create_struct(compiler->builtin_unreachable);
	create_struct(compiler->builtin_deferred_if);
	create_struct(compiler->builtin_ast);
	create_struct(compiler->builtin_template);

	auto create_global_alias = [&](String name) {
		auto defn = AstDefinition::create();
		defn->location = name;
		defn->name = name;
		defn->is_constant = true;
		defn->offset = 0; // :type_offset:
		// defn->add_to_scope(&compiler->global_scope);
		compiler->global_scope.add(raw(defn));

		auto ident = AstIdentifier::create();
		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(defn);

		return ident;
	};
	compiler->type_sint  = create_global_alias("SInt"str);
	compiler->type_uint  = create_global_alias("UInt"str);
	compiler->type_int   = create_global_alias("Int"str);
	compiler->type_float = create_global_alias("Float"str);
	auto alias_s8  = create_global_alias("SInt8"str);
	auto alias_s16 = create_global_alias("SInt16"str);
	auto alias_s32 = create_global_alias("SInt32"str);
	auto alias_s64 = create_global_alias("SInt64"str);
	auto alias_u8  = create_global_alias("UInt8"str);
	auto alias_u16 = create_global_alias("UInt16"str);
	auto alias_u32 = create_global_alias("UInt32"str);
	auto alias_u64 = create_global_alias("UInt64"str);
	auto alias_f32 = create_global_alias("Float32"str);
	auto alias_f64 = create_global_alias("Float64"str);

	// ======================================
	// Step 2: Init builtin structs and enums
	// ======================================

	auto init_builtin = [&](auto &builtin, auto thing, String name) {
		auto definition = thing->definition = builtin.definition;
		auto ident = builtin.ident;

		thing->type = compiler->builtin_type.ident;

		definition->is_constant = true;
		definition->expression = thing;
		definition->location = name;
		definition->name = name;
		definition->type = thing->type;
		definition->offset = 0; // :type_offset:

		compiler->global_scope.add(raw(definition));
		//typechecked_globals.get_or_insert(name) = definition;

		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(definition);
		ident->type = thing->type;
		ident->directed = thing->directed;

		// NOTE: not every builtin struct requires this, but i'd rather
		// not have to debug for an hour because i forgot to do this.
		builtin.pointer = make_pointer_type(builtin.ident);
		builtin.span = instantiate_span(builtin.ident, {});
	};
	auto init_struct = [&](BuiltinStruct &type, String name, s64 size, s64 align) {
		type.Struct->size = size;
		type.Struct->alignment = align;
		type.Struct->location = name;
		type.Struct->parameter_scope->parent = &compiler->global_scope;
		type.Struct->layout = StructLayout::tlang;
		init_builtin(type, type.Struct, name);

	};
	auto init_enum = [&](BuiltinEnum &type, String name) {
		type.Enum->location = name;
		type.Enum->scope->parent = &compiler->global_scope;
		init_builtin(type, type.Enum, name);
	};

	// Type:
	init_struct(compiler->builtin_type, "Type"str, 8, 8);

	// Primitives:
	init_struct(compiler->builtin_void, "Void"str, 0, 0);
	init_struct(compiler->builtin_bool, "Bool"str, 1, 1);
	init_struct(compiler->builtin_u8,   "U8"str,   1, 1);
	init_struct(compiler->builtin_u16,  "U16"str,  2, 2);
	init_struct(compiler->builtin_u32,  "U32"str,  4, 4);
	init_struct(compiler->builtin_u64,  "U64"str,  8, 8);
	init_struct(compiler->builtin_s8,   "S8"str,   1, 1);
	init_struct(compiler->builtin_s16,  "S16"str,  2, 2);
	init_struct(compiler->builtin_s32,  "S32"str,  4, 4);
	init_struct(compiler->builtin_s64,  "S64"str,  8, 8);
	init_struct(compiler->builtin_f32,  "F32"str,  4, 4);
	init_struct(compiler->builtin_f64,  "F64"str,  8, 8);

	// Structs:  NOTE: members are appended after every builtin type was initialized.
	init_struct(compiler->builtin_string,        "String"str, 0, 0);
	init_struct(compiler->builtin_struct_member, "StructMember"str, 0, 0);
	init_struct(compiler->builtin_enum_member,   "EnumMember"str, 0, 0);
	init_struct(compiler->builtin_typeinfo,      "TypeInfo"str, 0, 0);
	init_struct(compiler->builtin_any,           "Any"str, 0, 0);
	init_struct(compiler->builtin_range,         "Range"str, 0, 0);

	// Enums:
	init_enum(compiler->builtin_type_kind, "TypeKind"str);

	// Internal types:
	init_struct(compiler->builtin_unsized_integer, "<integer>"str, 0, 0);
	init_struct(compiler->builtin_unsized_float,   "<float>"str, 0, 0);
	init_struct(compiler->builtin_noinit,          "<noinit>"str, 0, 0);
	init_struct(compiler->builtin_unknown_enum,    "<unknown enum>"str, 0, 0);
	init_struct(compiler->builtin_poly,            "<poly>"str, 0, 0);
	init_struct(compiler->builtin_overload_set,    "<overload set>"str, 0, 0);
	init_struct(compiler->builtin_unreachable,     "<unreachable>"str, 0, 0);
	init_struct(compiler->builtin_deferred_if,     "<deferred if>"str, 0, 0);
	init_struct(compiler->builtin_ast,             "<ast>"str, 0, 0);
	init_struct(compiler->builtin_template,        "<template>"str, 0, 0);

	// ====================
	// Step 3: Init aliases
	// ====================
	switch (compiler->register_size) {
		case 4:
			compiler->builtin_default_signed_integer = &compiler->builtin_s32;
			compiler->builtin_default_unsigned_integer = &compiler->builtin_u32;
			break;
		case 8:
			compiler->builtin_default_signed_integer = &compiler->builtin_s64;
			compiler->builtin_default_unsigned_integer = &compiler->builtin_u64;
			break;
		default:
			invalid_code_path("Unsupported register size: {}", compiler->register_size);
	}
	compiler->builtin_default_integer = compiler->builtin_default_signed_integer;
	compiler->builtin_default_float = &compiler->builtin_f64;

	auto init_global_alias = [&](AstIdentifier *ident, AstExpression *expression) {
		assert(expression->type);

		auto defn = ident->possible_definitions[0];
		defn->type = expression->type;
		defn->expression = expression;

		ident->type = expression->type;
		ident->directed = expression->directed;

		return ident;
	};
	init_global_alias(compiler->type_sint,  compiler->builtin_default_integer->ident);
	init_global_alias(compiler->type_uint,  compiler->builtin_default_unsigned_integer->ident);
	init_global_alias(compiler->type_int,   compiler->builtin_default_signed_integer->ident);
	init_global_alias(compiler->type_float, compiler->builtin_default_float->ident);
	init_global_alias(alias_s8,  compiler->builtin_s8.ident);
	init_global_alias(alias_s16, compiler->builtin_s16.ident);
	init_global_alias(alias_s32, compiler->builtin_s32.ident);
	init_global_alias(alias_s64, compiler->builtin_s64.ident);
	init_global_alias(alias_u8,  compiler->builtin_u8.ident);
	init_global_alias(alias_u16, compiler->builtin_u16.ident);
	init_global_alias(alias_u32, compiler->builtin_u32.ident);
	init_global_alias(alias_u64, compiler->builtin_u64.ident);
	init_global_alias(alias_f32, compiler->builtin_f32.ident);
	init_global_alias(alias_f64, compiler->builtin_f64.ident);

	// =============================
	// Step 4: Append struct members
	// =============================
	auto append_member = [](BuiltinStruct &builtin, String name, AstExpression *type) {
		auto Struct = builtin.Struct;

		auto d = AstDefinition::create();
		d->location = name;
		d->name = name;
		d->type = type;

		auto member_size = get_size(type);
		auto member_align = get_align(type);

		d->offset = ceil(Struct->size, member_align);

		Struct->alignment = max(Struct->alignment, member_align);
		Struct->size += max(Struct->alignment, member_size);

		Struct->member_scope->add(raw(d));
		Struct->data_members.add(d);
		d->container_node = Struct;

		return d;
	};
	auto append_value = [](BuiltinEnum &builtin, String name, s64 value) {
		auto Enum = builtin.Enum;

		auto d = AstDefinition::create();
		d->location = name;
		d->name = name;
		d->type = compiler->type_int;
		d->expression = make_integer(value, name);
		d->expression->type = compiler->type_int;

		Enum->scope->add(raw(d));
		d->container_node = Enum;

		return d;
	};

	add_member(compiler->builtin_u8 .Struct, compiler->builtin_u8 .ident, "min"str, make_integer((u64)min_value<u8 >), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u16.Struct, compiler->builtin_u16.ident, "min"str, make_integer((u64)min_value<u16>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u32.Struct, compiler->builtin_u32.ident, "min"str, make_integer((u64)min_value<u32>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u64.Struct, compiler->builtin_u64.ident, "min"str, make_integer((u64)min_value<u64>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u8 .Struct, compiler->builtin_u8 .ident, "max"str, make_integer((u64)max_value<u8 >), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u16.Struct, compiler->builtin_u16.ident, "max"str, make_integer((u64)max_value<u16>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u32.Struct, compiler->builtin_u32.ident, "max"str, make_integer((u64)max_value<u32>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_u64.Struct, compiler->builtin_u64.ident, "max"str, make_integer((u64)max_value<u64>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s8 .Struct, compiler->builtin_s8 .ident, "min"str, make_integer((s64)min_value<s8 >), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s16.Struct, compiler->builtin_s16.ident, "min"str, make_integer((s64)min_value<s16>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s32.Struct, compiler->builtin_s32.ident, "min"str, make_integer((s64)min_value<s32>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s64.Struct, compiler->builtin_s64.ident, "min"str, make_integer((s64)min_value<s64>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s8 .Struct, compiler->builtin_s8 .ident, "max"str, make_integer((s64)max_value<s8 >), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s16.Struct, compiler->builtin_s16.ident, "max"str, make_integer((s64)max_value<s16>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s32.Struct, compiler->builtin_s32.ident, "max"str, make_integer((s64)max_value<s32>), true, INVALID_MEMBER_OFFSET);
	add_member(compiler->builtin_s64.Struct, compiler->builtin_s64.ident, "max"str, make_integer((s64)max_value<s64>), true, INVALID_MEMBER_OFFSET);

	// String
	append_member(compiler->builtin_string, "data"str,  compiler->builtin_u8.pointer);
	append_member(compiler->builtin_string, "count"str, compiler->type_int);

	// type_kind
	{
		using enum TypeKind;
		append_value(compiler->builtin_type_kind, "Void"str,    (s64)Void   );
		append_value(compiler->builtin_type_kind, "Bool"str,    (s64)Bool   );
		append_value(compiler->builtin_type_kind, "U8"str,      (s64)U8     );
		append_value(compiler->builtin_type_kind, "U16"str,     (s64)U16    );
		append_value(compiler->builtin_type_kind, "U32"str,     (s64)U32    );
		append_value(compiler->builtin_type_kind, "U64"str,     (s64)U64    );
		append_value(compiler->builtin_type_kind, "S8"str,      (s64)S8     );
		append_value(compiler->builtin_type_kind, "S16"str,     (s64)S16    );
		append_value(compiler->builtin_type_kind, "S32"str,     (s64)S32    );
		append_value(compiler->builtin_type_kind, "S64"str,     (s64)S64    );
		append_value(compiler->builtin_type_kind, "F32"str,     (s64)F32    );
		append_value(compiler->builtin_type_kind, "F64"str,     (s64)F64    );
		append_value(compiler->builtin_type_kind, "struct"str,  (s64)Struct );
		append_value(compiler->builtin_type_kind, "enum"str,    (s64)Enum   );
		append_value(compiler->builtin_type_kind, "pointer"str, (s64)Pointer);
		append_value(compiler->builtin_type_kind, "span"str,    (s64)Span   );
		append_value(compiler->builtin_type_kind, "array"str,   (s64)Array  );
		append_value(compiler->builtin_type_kind, "option"str,  (s64)Option );
	}

	// struct_member
	append_member(compiler->builtin_struct_member, "name"str,   compiler->builtin_string.ident);
	append_member(compiler->builtin_struct_member, "type"str,   compiler->builtin_typeinfo.pointer);
	append_member(compiler->builtin_struct_member, "offset"str, compiler->type_int);

	// enum_member
	append_member(compiler->builtin_enum_member, "value"str, compiler->type_int);
	append_member(compiler->builtin_enum_member, "name"str,  compiler->builtin_string.ident);

	// typeinfo
	append_member(compiler->builtin_typeinfo, "kind"str, compiler->builtin_type_kind.ident);
	append_member(compiler->builtin_typeinfo, "name"str, compiler->builtin_string.ident);
	append_member(compiler->builtin_typeinfo, "size"str, compiler->type_int);
	append_member(compiler->builtin_typeinfo, "align"str, compiler->type_int);
	append_member(compiler->builtin_typeinfo, "members"str, compiler->builtin_struct_member.span);
	append_member(compiler->builtin_typeinfo, "pointee"str, compiler->builtin_typeinfo.pointer);
	append_member(compiler->builtin_typeinfo, "array_count"str, compiler->type_int);
	append_member(compiler->builtin_typeinfo, "enum_members"str, compiler->builtin_enum_member.span);
	append_member(compiler->builtin_typeinfo, "parameters"str, instantiate_span(compiler->builtin_typeinfo.pointer, {}));

	// any
	append_member(compiler->builtin_any, "pointer"str, compiler->builtin_u8      .pointer);
	append_member(compiler->builtin_any, "type"str,    compiler->builtin_typeinfo.pointer);

	// range
	// TODO: make this a template. might be useful for aabb
	append_member(compiler->builtin_range, "min"str, compiler->type_int);
	append_member(compiler->builtin_range, "max"str, compiler->type_int);
}

void init_binary_typecheckers() {

#define BINOP(op, l, r) \
binary_typecheckers.get_or_insert({BinaryOperation::op, compiler->builtin_##l.Struct, compiler->builtin_##r.Struct})

#define DECL(x) auto default_##x = [] (TypecheckState *state, AstBinaryOperator *bin) -> AstExpression * { bin->type = compiler->builtin_##x.ident; return bin; };
	DECL(void);
	DECL(bool);
	DECL(u8);
	DECL(u16);
	DECL(u32);
	DECL(u64);
	DECL(s8);
	DECL(s16);
	DECL(s32);
	DECL(s64);
	DECL(f32);
	DECL(f64);
#undef DECL


#define D(op, l, r, out) BINOP(op, l, r) = default_##out

#define INT(s8) \
	D(add,  s8, s8, s8); \
	D(sub,  s8, s8, s8); \
	D(mul,  s8, s8, s8); \
	D(div,  s8, s8, s8); \
	D(mod,  s8, s8, s8); \
	D(adds, s8, s8, s8); \
	D(subs, s8, s8, s8); \
	D(eq, s8, s8, bool); \
	D(ne, s8, s8, bool); \
	D(lt, s8, s8, bool); \
	D(gt, s8, s8, bool); \
	D(le, s8, s8, bool); \
	D(ge, s8, s8, bool); \

	INT(s8)
	INT(s16)
	INT(s32)
	INT(s64)
	INT(u8)
	INT(u16)
	INT(u32)
	INT(u64)



	static auto float_and_unsized_integer = [](TypecheckState *state, AstBinaryOperator *bin) -> AstExpression * {
		auto left_is_unsized_integer = types_match(bin->left->type, compiler->builtin_unsized_integer);

		auto &sized_float     = left_is_unsized_integer ? bin->right : bin->left;
		auto &unsized_integer = left_is_unsized_integer ? bin->left  : bin->right;

		AstLiteral *literal = as<AstLiteral>(unsized_integer);

		if (literal) {
			assert(literal->literal_kind == LiteralKind::integer);
		} else {
			literal = deep_copy(get_literal(unsized_integer));
		}

		literal->literal_kind = LiteralKind::Float;
		literal->Float = (f64)literal->integer;
		literal->type = sized_float->type;

		switch (bin->operation) {
			using enum BinaryOperation;

			case eq:
			case ne:
			case lt:
			case gt:
			case le:
			case ge:
				bin->type = compiler->builtin_bool.ident;
				break;
			default:
				bin->type = sized_float->type;
				break;
		}
		return bin;
	};

	static auto float_and_unsized_float = [](TypecheckState *state, AstBinaryOperator *bin) -> AstExpression * {
		AstExpression *sized;
		AstExpression *unsized;
		if (types_match(bin->left->type, compiler->builtin_unsized_float)) {
			unsized = bin->left;
			sized   = bin->right;
		} else {
			unsized = bin->right;
			sized   = bin->left;
		}
		unsized->type = sized->type;
		switch (bin->operation) {
			using enum BinaryOperation;

			case eq:
			case ne:
			case lt:
			case gt:
			case le:
			case ge:
				bin->type = compiler->builtin_bool.ident;
				break;
			default:
				bin->type = sized->type;
				break;
		}
		return bin;
	};

#define UNSIZED_FLOAT_BINOP_0(operation, other_type) \
BINOP(operation, other_type, unsized_float) = float_and_unsized_float;  \
BINOP(operation, unsized_float, other_type) = float_and_unsized_float;  \
BINOP(operation, other_type, unsized_integer) = float_and_unsized_integer;  \
BINOP(operation, unsized_integer, other_type) = float_and_unsized_integer;  \

#define UNSIZED_FLOAT_BINOP(other_type) \
UNSIZED_FLOAT_BINOP_0(add, other_type); \
UNSIZED_FLOAT_BINOP_0(sub, other_type); \
UNSIZED_FLOAT_BINOP_0(mul, other_type); \
UNSIZED_FLOAT_BINOP_0(div, other_type); \
UNSIZED_FLOAT_BINOP_0(eq, other_type); \
UNSIZED_FLOAT_BINOP_0(ne, other_type); \
UNSIZED_FLOAT_BINOP_0(lt, other_type); \
UNSIZED_FLOAT_BINOP_0(gt, other_type); \
UNSIZED_FLOAT_BINOP_0(le, other_type); \
UNSIZED_FLOAT_BINOP_0(ge, other_type); \

	UNSIZED_FLOAT_BINOP(f32);
	UNSIZED_FLOAT_BINOP(f64);

#undef UNSIZED_FLOAT_BINOP
#undef UNSIZED_FLOAT_BINOP_0

	static auto unsized_float_and_unsized_integer = [](TypecheckState *state, AstBinaryOperator *bin) -> AstExpression * {
		auto left_is_float = types_match(bin->left->type, compiler->builtin_unsized_float);

		auto left_literal  = get_literal(bin->left);
		auto right_literal = get_literal(bin->right);

		f64 a = left_is_float ? left_literal->Float : (f64)left_literal->integer;
		f64 b = left_is_float ? (f64)right_literal->integer : right_literal->Float;

		auto result = AstLiteral::create();
		switch (bin->operation) {
			using enum BinaryOperation;

			case eq:
			case ne:
			case lt:
			case gt:
			case le:
			case ge:
				result->literal_kind = LiteralKind::boolean;
				result->type = compiler->builtin_bool.ident;
				switch (bin->operation) {
					case eq: result->Bool = a == b; break;
					case ne: result->Bool = a != b; break;
					case lt: result->Bool = a <  b; break;
					case gt: result->Bool = a >  b; break;
					case le: result->Bool = a <= b; break;
					case ge: result->Bool = a >= b; break;
				}
				break;
			default:
				result->literal_kind = LiteralKind::Float;
				result->type = compiler->builtin_unsized_float.ident;
				switch (bin->operation) {
					case add: result->Float = a + b; break;
					case sub: result->Float = a - b; break;
					case mul: result->Float = a * b; break;
					case div: result->Float = a / b; break;
					default: return 0;
				}
				break;
		}
		return result;
	};

#define UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(operation) \
BINOP(operation, unsized_float, unsized_integer) = unsized_float_and_unsized_integer;  \
BINOP(operation, unsized_integer, unsized_float) = unsized_float_and_unsized_integer;  \

	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(add);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(sub);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(mul);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(div);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(eq);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(ne);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(lt);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(gt);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(le);
	UNSIZED_FLOAT_AND_UNSIZED_INT_BINOP(ge);

#undef D
#undef BINOP
}

s32 tl_main(Span<Span<utf8>> arguments) {
	Compiler _compiler = {};
	compiler = &_compiler;

	init_strings();

	auto global_timer = create_precise_timer();

	set_console_encoding(Encoding_utf8);

	defer {
		if (compiler->do_profile) {
			print("Execution finished in {} ms\n", reset(global_timer) * 1000);
			print("Peak memory usage: {}\n", format_bytes(get_memory_info().peak_usage));
		}
	};

	// write_test_source();

	init_my_allocator();
	defer { deinit_my_allocator(); };

	default_allocator = current_allocator = make_allocator_from_static_class<MyAllocator>();

#if 0//TL_ENABLE_PROFILER
	static Allocator real_os_allocator = os_allocator;
	os_allocator = {
		[](AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> void * {
			switch (mode) {
				case Allocator_allocate: {
					Profiler::mark("allocation", 0xff0000);
					break;
				}
				case Allocator_reallocate: {
					Profiler::mark("reallocation", 0x00ff00);
					break;
				}
				case Allocator_free: {
					Profiler::mark("free", 0x0000ff);
					break;
				}
			}
			return real_os_allocator.func(mode, data, old_size, new_size, align, location, 0);
		},
		0
	};

#endif
#if TL_ENABLE_PROFILER
	compiler->profiler.init();
	defer {
		write_entire_file("profile.tmd"s, compiler->profiler.output_for_timed());
		compiler->profiler.deinit();
	};
#endif

#if 0

	static HashMap<CallStack, u64> allocations;

	default_allocator = current_allocator = {
		[](AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *state) -> void * {
			switch (mode) {
				case Allocator_allocate: {
					scoped_allocator(os_allocator);

					auto call_stack = get_call_stack();

					if (auto found = allocations.find(call_stack)) {
						*found += new_size;
						free(call_stack);
					} else {
						allocations.get_or_insert(call_stack) = new_size;
					}

					break;
				}
			}
			return os_allocator.func(mode, data, old_size, new_size, align, location, state);
		},
		0
	};
	defer {
		for_each(tracked_allocations, [](auto x, AllocationInfo info) {
			print("{} bytes from:\n{}\n\n", info.size, info.call_stack);
		});
	};
#endif

	timed_function(compiler->profiler);

restart_main:

	auto timer = compiler->profiler.scoped_timer("setup");

	compiler->current_directory = get_current_directory();

	auto args = parse_arguments(arguments);

	if (args.track_allocations) {
		default_allocator = current_allocator = make_tracking_allocator(current_allocator);
	}
	defer {
		if (args.track_allocations) {
			print("Allocations:\n");

			auto allocations = get_tracked_allocations(current_allocator);

			std::sort(allocations.begin(), allocations.end(), [](AllocationInfo a, AllocationInfo b) {
				return a.total_size > b.total_size;
			});

			for (auto a : allocations) {
				print("{}: {}", a.location, format_bytes(a.total_size));
				if (a.current_size) {
					if (a.current_size == a.total_size) {
						print(", all leaked");
					} else {
						print(". leaked: {}", format_bytes(a.current_size));
					}
				}
				print('\n');
			}
		}
	};

	if (args.debug_paths) print("compiler_directory: {}\n", compiler->compiler_directory);

	if (args.debug_paths) print("current_directory: {}\n", compiler->current_directory);

	if (args.source_files.count == 0) {
		immediate_error(compiler->strings.no_source_path_received);
		return 1;
	}

	compiler->source_path = args.source_files[0];
	if (!is_absolute_path(compiler->source_path)) {
		compiler->source_path = make_absolute_path(compiler->source_path);
	}

	if (args.debug_paths) print("source_path: {}\n", compiler->source_path);

	compiler->source_path_without_extension = parse_path(compiler->source_path).path_without_extension();
	if (args.debug_paths) print("source_path_without_extension: {}\n", compiler->source_path_without_extension);

	if (args.output.count) {
		compiler->output_path = args.output;
		if (!is_absolute_path(compiler->output_path)) {
			compiler->output_path = make_absolute_path(compiler->output_path);
		}
	} else {
		compiler->output_path = (String)format("{}\\{}.exe", get_current_directory(), parse_path(compiler->source_path).name);
	}

	construct(parsed_files);
	construct(built_in_casts);

	construct(double_char_tokens);
	construct(triple_char_tokens);
	construct(import_paths);

	construct(implicit_casts);
	construct(typechecked_explicit_casts);
	construct(binary_operators);
	construct(unary_operators);

	construct(has_value_overloads);

	construct(typeinfo_definitinos);

	construct(generated_comparers);

	construct(typecheck_states_pool);

#if SMALL_AST
	Pool32<AstExpression>::init();
	Pool32<AstStatement>::init();
	defer {
		if (compiler->do_profile) {
			println("Expression pool is {}", format_bytes(Pool32<AstExpression>::size()));
			println("Statement pool is {}", format_bytes(Pool32<AstStatement>::size()));
		}
		Pool32<AstExpression>::clear();
		Pool32<AstStatement>::clear();
	};
#endif

	HMODULE lib = 0;
	if (args.target == "none"str) {
		compiler->register_size = 8;
		compiler->stack_word_size = 8;
		compiler->general_purpose_register_count = 16;
	} else {
		scoped_phase("Collecting target information");

		with(temporary_allocator, lib = LoadLibraryW((wchar *)to_utf16(format("{}\\targets\\{}\\target.dll"str, compiler->compiler_directory, args.target), true).data));

		if (!lib) {
			print("Failed to load target code generator '{}'. Check \"tlang\\bin\\targets\" directory\n", args.target);
			return 1;
		}

		auto get_target_information = (TargetInformationGetter)GetProcAddress(lib, "tlang_get_target_information");
		if (!get_target_information) {
			print("Failed to load target code generator '{}'. There is no `tlang_get_target_information` function\n", args.target);
			return 1;
		}

		get_target_information(compiler);
	}




	import_paths.add(compiler->current_directory);
	import_paths.add(concatenate(compiler->compiler_directory, "\\libs"str));

	double_char_tokens.insert("=="str);
	double_char_tokens.insert("=>"str);
	double_char_tokens.insert("!="str);
	double_char_tokens.insert(">="str);
	double_char_tokens.insert("<="str);
	double_char_tokens.insert("+="str);
	double_char_tokens.insert("-="str);
	double_char_tokens.insert("*="str);
	double_char_tokens.insert("/="str);
	double_char_tokens.insert("%="str);
	double_char_tokens.insert("|="str);
	double_char_tokens.insert("&="str);
	double_char_tokens.insert("^="str);
	double_char_tokens.insert("->"str);
	double_char_tokens.insert(">>"str);
	double_char_tokens.insert("<<"str);
	double_char_tokens.insert(".."str);
	double_char_tokens.insert("||"str);
	double_char_tokens.insert("&&"str);
	double_char_tokens.insert("+|"str);
	double_char_tokens.insert("-|"str);

	triple_char_tokens.insert(">>="str);
	triple_char_tokens.insert("<<="str);

	construct(keywords);
#define E(name, value) keywords.get_or_insert((String)u8#name##s) = value;
	ENUMERATE_KEYWORDS(E);
#undef E

	init_compiler_builtin_types();

#if 1
	integer_infos[0] = {compiler->builtin_u8 .Struct, 8 };
	integer_infos[1] = {compiler->builtin_u16.Struct, 16};
	integer_infos[2] = {compiler->builtin_u32.Struct, 32};
	integer_infos[3] = {compiler->builtin_u64.Struct, 64};
	integer_infos[4] = {compiler->builtin_s8 .Struct, 8 };
	integer_infos[5] = {compiler->builtin_s16.Struct, 16};
	integer_infos[6] = {compiler->builtin_s32.Struct, 32};
	integer_infos[7] = {compiler->builtin_s64.Struct, 64};
#else
	integer_infos[0] = {compiler->builtin_u8 .Struct, -make_big_int<BigInteger>((u64)0xff),               make_big_int<BigInteger>((u64)0xff)              };
	integer_infos[1] = {compiler->builtin_u16.Struct, -make_big_int<BigInteger>((u64)0xffff),             make_big_int<BigInteger>((u64)0xffff)            };
	integer_infos[2] = {compiler->builtin_u32.Struct, -make_big_int<BigInteger>((u64)0xffffffff),         make_big_int<BigInteger>((u64)0xffffffff)        };
	integer_infos[3] = {compiler->builtin_u64.Struct, -make_big_int<BigInteger>((u64)0xffffffffffffffff), make_big_int<BigInteger>((u64)0xffffffffffffffff)};
	integer_infos[4] = {compiler->builtin_s8 .Struct, -make_big_int<BigInteger>((u64)0xff),               make_big_int<BigInteger>((u64)0xff)              };
	integer_infos[5] = {compiler->builtin_s16.Struct, -make_big_int<BigInteger>((u64)0xffff),             make_big_int<BigInteger>((u64)0xffff)            };
	integer_infos[6] = {compiler->builtin_s32.Struct, -make_big_int<BigInteger>((u64)0xffffffff),         make_big_int<BigInteger>((u64)0xffffffff)        };
	integer_infos[7] = {compiler->builtin_s64.Struct, -make_big_int<BigInteger>((u64)0xffffffffffffffff), make_big_int<BigInteger>((u64)0xffffffffffffffff)};
#endif

	// Int to Int
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_s8 .Struct, /*CastKind::u8_s8  , */false});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_s16.Struct, /*CastKind::u8_s16 , */true});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_s32.Struct, /*CastKind::u8_s32 , */true});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_s64.Struct, /*CastKind::u8_s64 , */true});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_u16.Struct, /*CastKind::u8_u16 , */true});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_u32.Struct, /*CastKind::u8_u32 , */true});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_u64.Struct, /*CastKind::u8_u64 , */true});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_s8 .Struct, /*CastKind::u16_s8 , */false});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_s16.Struct, /*CastKind::u16_s16, */false});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_s32.Struct, /*CastKind::u16_s32, */true});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_s64.Struct, /*CastKind::u16_s64, */true});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_u8 .Struct, /*CastKind::u16_u8 , */false});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_u32.Struct, /*CastKind::u16_u32, */true});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_u64.Struct, /*CastKind::u16_u64, */true});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_s8 .Struct, /*CastKind::u32_s8 , */false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_s16.Struct, /*CastKind::u32_s16, */false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_s32.Struct, /*CastKind::u32_s32, */false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_s64.Struct, /*CastKind::u32_s64, */true});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_u8 .Struct, /*CastKind::u32_u8 , */false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_u16.Struct, /*CastKind::u32_u16, */false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_u64.Struct, /*CastKind::u32_u64, */true});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_s8 .Struct, /*CastKind::u64_s8 , */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_s16.Struct, /*CastKind::u64_s16, */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_s32.Struct, /*CastKind::u64_s32, */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_s64.Struct, /*CastKind::u64_s64, */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_u8 .Struct, /*CastKind::u64_u8 , */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_u16.Struct, /*CastKind::u64_u16, */false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_u32.Struct, /*CastKind::u64_u32, */false});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_s16.Struct, /*CastKind::s8_s16 , */true});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_s32.Struct, /*CastKind::s8_s32 , */true});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_s64.Struct, /*CastKind::s8_s64 , */true});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_u8 .Struct, /*CastKind::s8_u8  , */false});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_u16.Struct, /*CastKind::s8_u16 , */false});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_u32.Struct, /*CastKind::s8_u32 , */false});
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_u64.Struct, /*CastKind::s8_u64 , */false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_s8 .Struct, /*CastKind::s16_s8 , */false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_s32.Struct, /*CastKind::s16_s32, */true});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_s64.Struct, /*CastKind::s16_s64, */true});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_u8 .Struct, /*CastKind::s16_u8 , */false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_u16.Struct, /*CastKind::s16_u16, */false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_u32.Struct, /*CastKind::s16_u32, */false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_u64.Struct, /*CastKind::s16_u64, */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_s8 .Struct, /*CastKind::s32_s8 , */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_s16.Struct, /*CastKind::s32_s16, */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_s64.Struct, /*CastKind::s32_s64, */true});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_u8 .Struct, /*CastKind::s32_u8 , */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_u16.Struct, /*CastKind::s32_u16, */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_u32.Struct, /*CastKind::s32_u32, */false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_u64.Struct, /*CastKind::s32_u64, */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_s8 .Struct, /*CastKind::s64_s8 , */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_s16.Struct, /*CastKind::s64_s16, */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_s32.Struct, /*CastKind::s64_s32, */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_u8 .Struct, /*CastKind::s64_u8 , */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_u16.Struct, /*CastKind::s64_u16, */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_u32.Struct, /*CastKind::s64_u32, */false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_u64.Struct, /*CastKind::s64_u64, */false});

	// Bool to Int
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_s8 .Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_s16.Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_s32.Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_s64.Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_u8 .Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_u16.Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_u32.Struct, false});
	built_in_casts.add({compiler->builtin_bool.Struct, compiler->builtin_u64.Struct, false});

	// Int to Bool
	built_in_casts.add({compiler->builtin_s8 .Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_s16.Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_u8 .Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_u16.Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_u32.Struct, compiler->builtin_bool.Struct, false});
	built_in_casts.add({compiler->builtin_u64.Struct, compiler->builtin_bool.Struct, false});

	// Float to Int
	built_in_casts.add({compiler->builtin_f32.Struct, compiler->builtin_s32.Struct, false});
	built_in_casts.add({compiler->builtin_f64.Struct, compiler->builtin_s64.Struct, false});

	// Int to Float
	built_in_casts.add({compiler->builtin_s32.Struct, compiler->builtin_f32.Struct, false});
	built_in_casts.add({compiler->builtin_s64.Struct, compiler->builtin_f64.Struct, false});

	// Float to Float
	built_in_casts.add({compiler->builtin_f32.Struct, compiler->builtin_f64.Struct, true});
	built_in_casts.add({compiler->builtin_f64.Struct, compiler->builtin_f32.Struct, false});

	empty_statement = AstEmptyStatement::create();

	construct(binary_typecheckers);

	init_binary_typecheckers();

	current_printer = {
		[](Span<utf8> span, void *) {
			WriteFile(std_out, span.data, (DWORD)span.count, 0, 0);
			OutputDebugStringA(with(temporary_allocator, (char *)null_terminate(span).data));
		},
		0
	};

	timed_end("setup"str);

	{
		scoped_phase("Parsing");

		auto parsed = parse_file(compiler->current_directory, "tlang/preload.tl"str, {});
		if (parsed->result == ParseResult::read_error) {
			immediate_error("Unable to read preload.tl");
			return 1;
		}
		// compiler->global_scope.append(parsed->scope);


		for (auto path : args.source_files) {
			parsed = parse_file(compiler->current_directory, path, {});
			switch (parsed->result) {
				case ParseResult::read_error:
				case ParseResult::alloc_error:
					return 1;
			}

			// compiler->global_scope.append(parsed->scope);
		}

		if (failed_lexer) {
			if (!printed_reports_count) {
				immediate_error("Lexer failed. Exiting.\n");
			}
			return 1;
		}
		if (failed_parser) {
			if (!printed_reports_count) {
				immediate_error("Parser failed. Exiting.\n");
			}
			return 1;
		}
	}

	if (!args.no_typecheck) {
		defer {
			if (compiler->print_lowered) {
				print_lowered();
			}
		};

		scoped_phase("Typechecking");

		timed_block(compiler->profiler, "typecheck"str);

		auto main_fiber = ConvertThreadToFiber(0);

		struct StatementToTypecheck {
			AstStatement *statement = 0;
			TypecheckState *state = 0;
		};

		List<StatementToTypecheck> current_statements_to_typecheck;
		List<StatementToTypecheck> next_statements_to_typecheck;
		bool fail = false;

		u64 const coro_size = 1*MiB;

		for (auto &statement : compiler->global_scope.statement_list)  {
			if (statement->kind == Ast_Definition && ((AstDefinition *)statement)->type)
				continue;

			auto &state = current_statements_to_typecheck.add();
			state.statement = statement;
		}

		get_current_node = []{ return current_typecheck_state ? current_typecheck_state->current_node : 0; };

		while (1) {
			next_statements_to_typecheck.clear();
			for (auto &statement : current_statements_to_typecheck) {
				if (!statement.state) {
					statement.state = get_pooled_typecheck_state(statement.statement, 0, &compiler->global_scope, main_fiber);
				}

				SWITCH_TO_FIBER(statement.state->fiber);
				current_typecheck_state = 0;
				auto result = fiber_result;

				switch (result) {
					case TypecheckResult::success:
					case TypecheckResult::fail:
						if (result == TypecheckResult::fail) {
							fail = true;
							for (auto entry : reverse_iterate(statement.state->template_call_stack)) {
								if (entry.hardened_lambda) {
									StringBuilder builder;
									builder.allocator = temporary_allocator;
									for (auto definition : entry.hardened_lambda->constant_scope->definition_list) {
										append_format(builder, "{} = ", definition->name);
										append(builder, direct(definition->expression));
										append(builder, ", ");
									}
									statement.state->reporter.info(entry.call->location, "While resolving {}", entry.call->callable->location);
									statement.state->reporter.info(entry.template_lambda->location, "With {}", to_string(builder));
								}
							}
						}
						statement.state->reporter.print_all();
						break;
					case TypecheckResult::wait:
						next_statements_to_typecheck.add(statement);
						break;
				}
			}

			if (next_statements_to_typecheck.count == 0)
				break;

			swap(next_statements_to_typecheck, current_statements_to_typecheck);
		}

		if (fail) {
			with(ConsoleColor::red, print("Typechecking failed.\n"));
			return 1;
		}
	}

	if (args.no_typecheck) {
		return 1;
	}

	auto found_build_definitions = compiler->global_scope.definition_map.find("build"str);
	if (found_build_definitions) {
		auto build_definitions = found_build_definitions->value;
		if (build_definitions.count != 1) {
			immediate_error("Found multiple `build` definitions");
			for (auto definition : build_definitions) {
				immediate_info(definition->location, "Here");
			}
			return 1;
		}
		auto build_definition = build_definitions[0];
		if (is_lambda(build_definition->expression)) {
			compiler->build_lambda = get_lambda(build_definition->expression);
			s64 test = 69;
			// invoke(build_lambda, &test);
			with(ConsoleColor::red, print("test is {}\n", test));
		}
	}

	auto find_lambda_by_name = [&] (String name) -> AstLambda * {
		auto found_definitions = compiler->global_scope.definition_map.find(name);
		if (found_definitions) {
			auto definitions = found_definitions->value;
			if (definitions.count != 1) {
				immediate_error("Found multiple '{}' definitions", name);
				for (auto definition : definitions) {
					immediate_info(definition->location, "Here");
				}
				return 0;
			}
			auto definition = definitions[0];
			if (!is_lambda(definition->expression)) {
				immediate_error(definition->location, "'{}' must be a lambda", name);
				return 0;
			}

			return get_lambda(definition->expression);
		} else {
			immediate_error("'{}' function was not defined.", name);
			return 0;
		}
	};

	compiler->main_lambda = find_lambda_by_name("main"str);
	if (!compiler->main_lambda) {
		return 1;
	}

	if (!::is_integer(compiler->main_lambda->return_parameter->type) && !types_match(compiler->main_lambda->return_parameter->type, compiler->builtin_void)) {
		immediate_error(compiler->main_lambda->location, "Main function can return any type of integer or void, but not {}", type_to_string(compiler->main_lambda->return_parameter->type));
		return 1;
	}

	compiler->init_runtime_lambda = find_lambda_by_name("init_runtime"str);
	if (!compiler->init_runtime_lambda) {
		return 1;
	}

	if (compiler->enable_dce) {
		scoped_phase("Marking referenced definitions");

		compiler->init_runtime_lambda->definition->is_referenced = true;
		mark_referenced_definitions(compiler->init_runtime_lambda);

		compiler->main_lambda->definition->is_referenced = true;
		mark_referenced_definitions(compiler->main_lambda);
	}

	for (auto lambda : compiler->lambdas_without_body) {
		if (lambda->extern_library.data) {
			compiler->extern_libraries.get_or_insert(lambda->extern_library).add(lambda->definition->name);
		}
	}

	switch (compiler->build_from) {
		case BuildFrom::bytecode: {
			Bytecode bytecode;
			{
				scoped_phase("Building bytecode");
				bytecode = build_bytecode();
			}

			if (args.target != "none"str) {
				scoped_phase("Generating executable");

				auto build = (OutputBuilder)GetProcAddress(lib, "tlang_build_output");
				if (!build) {
					immediate_error("Failed to load output generator '{}'. There is no 'tlang_build_output' function\n", args.target);
					return 1;
				}

				if (!build({}, bytecode)) {
					return 1;
				}
			}
			break;
		}
		case BuildFrom::ast: {
			if (args.target != "none"str) {
				scoped_phase("Generating executable");

				auto build = (OutputBuilder)GetProcAddress(lib, "tlang_build_output");
				if (!build) {
					immediate_error("Failed to load output generator '{}'. There is no 'tlang_build_output' function\n", args.target);
					return 1;
				}

				if (!build(compiler->global_scope.statement_list, {})) {
					return 1;
				}
			}
			break;
		}
		default:
			invalid_code_path();
			break;
	}

	if (compiler->do_profile) {
		print("Total token count: {}\nYield count: {}\n", total_tokens_parsed, yield_wait_count);
	}

	return 0;
}
