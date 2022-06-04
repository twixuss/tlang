#define TL_IMPL
#include <common.h>
#include <tl/main.h>
#include <tl/cpu.h>
#include <tl/time.h>
#include <tl/hash_set.h>
#include "ast.h"
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

void typecheck_global(struct CoroState *corostate, struct TypecheckState *state);

void yield_debug_break(TypecheckResult result) {
	if (result == TypecheckResult::fail) {
		int x = 5;
	}
}

#if USE_FIBERS
TypecheckResult fiber_result;
bool throwing = false;
#define PARENT_FIBER state->parent_fiber
// NOTE:
// after yielding TypecheckResult::fail this fiber is pooled, so if it is used again - call typecheck_global for new statement. But we don't return to the root before
// calling typecheck_global again, meaning that failed function's stack is not freed and not reused, just wasted memory. It can be solved by
// using exceptions and stack unwinding, and i think it should be, because the stack is growing FAST. Note that this throw is executed only on fail, so it should not
// slow down the typechecker that much.
#define yield(x) (fiber_result = x, yield_debug_break(x), SWITCH_TO_FIBER(PARENT_FIBER), x == TypecheckResult::fail ? ((throwing = true, throw 0), 0) : 0)
#else
#define YIELD_STATE state->coro
// #define yield(x) (_ReadWriteBarrier(), ::tl::print("remaining stack size: {}\n", (u8 *)YIELD_STATE->sp - (u8 *)YIELD_STATE->buffer_base), ::coro_yield(YIELD_STATE, (size_t)x))
#define yield(x) (_ReadWriteBarrier(), ::coro_yield(YIELD_STATE, (size_t)x))
#endif

#define USE_SLABS 0
#define NO_PROGRESS_THRESHOLD 256

struct Parser;
struct Reporter;
struct SourceFileContext;
struct TypecheckState;

static List<String> import_paths;

AstDefinition *parse_definition(String name, String location, Parser *parser);
AstDefinition *parse_definition(Parser *parser);
bool parse_block_or_single_statement(Parser *parser, Scope *scope);
AstUnaryOperator *make_pointer_type(AstExpression *type);
AstSpan *make_span_type(AstExpression *type);
BinaryOperation binary_operation_from_token(TokenKind kind);
bool evaluate_if_possible(TypecheckState *state, AstDefinition *definition);
void put_strings_in_constant_section(AstLiteral *);

void init_pointer(auto &builtin_type) {
	builtin_type.pointer = make_pointer_type(builtin_type.ident);
}

void init_span(auto &builtin_type) {
	builtin_type.span = make_span_type(builtin_type.ident);
}

void print_help() {
    print(strings.usage, context.compiler_name);
}

u64 printed_reports_count = 0;

struct Reporter {
	List<Report> reports;

	void print_all() {
		printed_reports_count += reports.count;
		for (auto report : reports) {
			print_report(report);
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
	timed_function(context.profiler);

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
		//if (ends_with(get_source_path(token.string.data), u8"std.tl"str))
		//	print("{}\n", token.string);
		//
		//if (token.string == u8"while"str)
		//	debug_break();
	};

	utf8 *line_start = current_p;
	List<String> lines;
	lines.reserve(lexer->source.count / 32); // Guess 32 bytes per line on average

	auto push_line = [&] {
		lines.add({line_start, current_p + 1});
		line_start = current_p + 1;
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
				push_line();
				token.string.data += 1;
				if (!next_char()) {
					goto lexer_success;
				}
				goto nc;

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
			case '^':
			case '.': {
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
						next_char();
						if (prev != '\\') {
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
								} else if (c == '\n') {
									push_line();
								}
							} else if (c == '/') {
								check_closed;
								if (c == '*') {
									deepness += 1;
								} else if (c == '\n') {
									push_line();
								}
							} else if (c == '\n') {
								push_line();
								check_closed;
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
						}
					}
				} else {
					while (is_digit(c) || c == '_') {
						if (!next_char()) {
							break;
						}
					}
				}

				if (c == '.') {
					if (number_kind != decimal) {
						lexer->reporter->error(token.string, "Float literal are supported only in decimal form.");
						return false;
					}
					token.kind = Token_float_literal;
					next_char();

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
				token.string.data = current_p;
				skip_identifier_chars();
				token.string.count = current_p - token.string.data;
				if (token.string.count == 0) {
					lexer->reporter->error(token.string, "Expected an identifier after \\.");
					return false;
				}
				token.kind = Token_identifier;

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

	push_line();
	lexer->source_info->lines = lines;
	lexer->success = true;
	return true;
}

u32 main_return_value = 0;

struct Parser {
	Lexer *lexer = 0;
	Token *token;
	u32 token_index = 0;
	AstLambda *current_lambda = 0;
	Reporter *reporter;
	String extern_language;
	String extern_library;
	Scope *current_scope = &global_scope;
	u32 scope_count = 0;
	CallingConvention current_convention = CallingConvention::tlang;
	StructLayout current_struct_layout = StructLayout::tlang;
	AstIdentifier *poly_identifier = 0;

	bool next() {
		assert(token->kind != 'eof');
		++token;
		if (token->kind == 'eof') {
			return false;
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
};

enum class ParseResult {
	ok,
	read_error,
	syntax_error,
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
u32 untypechecked_implicit_casts_count;

AstLiteral *make_null(AstExpression *type, String location) {
	auto null = AstLiteral::create();
	null->literal_kind = LiteralKind::null;
	null->location = location;
	null->type = type;
	return null;
}

AstLiteral *make_string(String value, String location) {
	auto i = AstLiteral::create();
	i->location = location;
	i->literal_kind = LiteralKind::string;
	i->type = builtin_string.ident;

	i->string.set(value);

	return i;
}
AstLiteral *make_string(String value) {
	return make_string(value, value);
}

AstLiteral *make_integer(String location, BigInteger value, AstExpression *type = builtin_unsized_integer.ident) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::integer;
	i->integer = value;
	i->type = type;
	i->location = location;
	return i;
}

AstLiteral *make_integer(String location, u64 value, AstExpression *type = builtin_unsized_integer.ident) {
	return make_integer(location, make_big_int<BigInteger>(value), type);
}
AstLiteral *make_integer(String location, s64 value, AstExpression *type = builtin_unsized_integer.ident) {
	return make_integer(location, make_big_int<BigInteger>(value), type);
}

AstLiteral *make_boolean(bool value) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::boolean;
	i->Bool = value;
	i->type = builtin_bool.ident;
	return i;
}

AstLiteral *make_float(f64 value, AstExpression *type = builtin_unsized_float.ident) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::Float;
	i->Float = value;
	i->type = type;
	return i;
}

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser);
AstExpression *parse_expression_1(Parser *parser);
AstDefinition *parse_definition(Parser *parser);

void push_scope_check(Scope *scope) {
	assert(scope);
	if (scope->node->kind == Ast_Lambda) {
		auto lambda = (AstLambda *)scope->node;
		if (scope == &lambda->body_scope || scope == &lambda->parameter_scope) {
			// these already have their parents set.
		} else {
			assert(!scope->parent);
		}
	} else {
		assert(!scope->parent);
	}
}

#define push_scope(scope) \
	auto CONCAT(new_scope, __LINE__) = scope; \
	push_scope_check(CONCAT(new_scope, __LINE__)); \
	auto CONCAT(old_scope, __LINE__) = (CONCAT(new_scope, __LINE__)->parent = parser->current_scope); \
	parser->current_scope->children.add(CONCAT(new_scope, __LINE__)); \
	parser->current_scope = CONCAT(new_scope, __LINE__); \
	parser->current_scope->level = CONCAT(old_scope, __LINE__)->level + 1; \
	defer { parser->current_scope = CONCAT(old_scope, __LINE__); }; \
	parser->scope_count += 1;

void add_to_scope(AstStatement *statement, Scope *scope) {
	assert(!statement->parent_scope, "This already has parent");
	statement->parent_scope = scope;
	scope->statements.add(statement);
}
void add_to_scope(AstDefinition *definition, Scope *scope) {
	add_to_scope((AstStatement *)definition, scope);
	scope->definitions.get_or_insert(definition->name).add(definition);
}

ExternLanguage extern_language_from_string(String string) {
	if (string == "C"str) return ExternLanguage::c;

	return ExternLanguage::none;
}

AstDefinition *make_retparam(AstExpression *type, AstLambda *parent) {
	auto retparam = AstDefinition::create();

	retparam->type = type;
	retparam->definition_location = DefinitionLocation::lambda_return_parameter;
	retparam->parent_lambda_or_struct = parent;
	retparam->location = type->location;

	// retparam->add_to_scope(parser->current_scope);

	return retparam;
}

bool ensure_return_is_not_in_defer(Parser *parser, String location) {
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

AstStruct *parse_struct(Parser *parser, String token) {
	auto Struct = AstStruct::create();
	Struct->location = token;

	if (!parser->expect('{'))
		return 0;

	parser->next();

	push_scope(&Struct->scope);

	while (parser->token->kind != '}') {
		auto definition = parse_definition(parser);
		if (!definition)
			return 0;

		if (!definition->expression || needs_semicolon(definition->expression)) {
			if (!parser->expect(';')) {
				return 0;
			}
			parser->next();
		}

		// Struct->members.add(definition);
		// add_to_scope(definition, &Struct->scope);
	}

	Struct->layout = parser->current_struct_layout;

	parser->next();
	return Struct;
}

KeyString parse_identifier(Token *token) {
	if (token->kind == Token_split_identifier) {
		auto remaining = token->string;
		List<utf8> name;
		name.allocator = temporary_allocator;
		while (1) {
			auto slash = find(remaining, u8'\\');
			if (!slash) {
				name.add(remaining);
				break;
			}

			name.add(Span(remaining.begin(), slash));

			auto w = slash + 1;
			while (is_whitespace((ascii)*w)) { // NOTE: if non-ascii whitespace is used, this will not work.
				++w;
			}

			remaining.set_begin(w);
		}
		return name;
	} else if (token->kind == Token_identifier) {
		return token->string;
	} else {
		return {};
	}
}

inline AstUnaryOperator *make_unary(UnaryOperation operation) {
	auto unop = AstUnaryOperator::create();
	unop->operation = operation;
	return unop;
}
inline AstUnaryOperator *make_sizeof() { return make_unary(UnaryOperation::Sizeof); }
inline AstUnaryOperator *make_typeof() { return make_unary(UnaryOperation::typeof); }

// FIXME: this is only for lambdas right now
AstDefinition *create_definition(Parser *parser, String name, AstExpression *type) {

	auto definition = AstDefinition::create();

	definition->name = name;
	definition->type = type;
	definition->parent_lambda_or_struct = parser->current_lambda;

	return definition;
}

// FIXME: this is only for lambdas right now
AstDefinition *create_definition_in_current_scope(Parser *parser, String name, AstExpression *type) {

	auto definition = create_definition(parser, name, type);

	if (name != "_"str) {
		scoped_lock(parser->current_scope);

		add_to_scope(definition, parser->current_scope);
	}

	return definition;
}

AstLambdaType *create_lambda_type(AstLambda *lambda) {
	auto type = AstLambdaType::create();
	type->location = lambda->location;
	type->lambda = lambda;
	return type;
}

AstLambda *parse_lambda(Parser *parser, bool is_parsing_type) {
	auto lambda = AstLambda::create();

	lambda->location = parser->token->string;

	parser->next();

	if (parser->token->kind == Token_directive) {
		if (parser->token->string == "#stdcall"str) {
			lambda->convention = CallingConvention::stdcall;
		} else if (parser->token->string == "#intrinsic"str) {
			lambda->is_intrinsic = true;
		} else {
			parser->reporter->error(parser->token->string, "Unknown directive.");
			return 0;
		}
		parser->next();
	}

	if (lambda->convention == CallingConvention::none) {
		lambda->convention = parser->current_convention;
	}

	if (!parser->expect('('))
		return 0;

	parser->next();

	push_scope(&lambda->type_scope);
	push_scope(&lambda->parameter_scope);

	if (parser->token->kind != ')') {
		for (;;) {
			auto name_location = parser->token->string;
			auto name = parse_identifier(parser->token);

			if (parser->token->kind == 'this') {
				lambda->is_member = true;

				parser->next();

				if (parser->token->kind == ')') {
					break;
				}

				if (parser->token->kind == ',') {
					parser->next();
					continue;
				}

				parser->reporter->error(parser->token->string, "Expected ',' or ')', but got '{}'", token_kind_to_string(parser->token->kind));
				return 0;
			}

			if (!parser->expect(Token_identifier)) {
				return 0;
			}

			parser->next();

			AstDefinition *definition = 0;
			if (parser->token->kind == ':') {
				parser->next();

				definition = create_definition_in_current_scope(parser, name, 0);

				if (parser->token->kind == '..') {
					definition->is_pack = true;
					parser->next();
				}

				if (parser->token->kind == '%') {
					definition->is_constant = true;
					lambda->is_poly = true;
					parser->next();
				}

				parser->poly_identifier = 0;

				definition->type = parse_expression(parser);
				if (!definition->type)
					return 0;

				definition->location = {name_location.begin(), definition->type->location.end()};

				definition->poly_ident = parser->poly_identifier;
				definition->is_poly = definition->poly_ident != 0;
				lambda->is_poly |= definition->is_poly;
				if (definition->is_pack) {
					auto span = AstSpan::create();
					span->location = definition->type->location;
					span->expression = definition->type;
					definition->type = span;
					lambda->has_pack = true;
				}
			} else {
				definition = create_definition_in_current_scope(parser, name, builtin_poly.ident);
				definition->location = name_location;
				lambda->is_poly = true;
				definition->is_poly = true;
			}

			definition->definition_location = DefinitionLocation::lambda_parameter;
			definition->parent_lambda_or_struct = lambda;

			for (auto &other_parameter : lambda->parameters) {
				if (definition->name != "_"str && definition->name == other_parameter->name) {
					parser->reporter->error(definition->location, "Can't use identical names for different parameters.");
					return 0;
				}
			}

			lambda->parameters.add(definition);

			if (parser->token->kind == ')') {
				break;
			}

			if (!parser->expect(','))
				return 0;

			parser->next();
		}
	}

	parser->next();

	if (parser->token->kind == ':') {
		parser->next();

		auto first_retparam_token = parser->token;
		if (parser->token->kind == Token_identifier) {
			auto ident_location = parser->token->string;
			auto ident = parser->token->string;
			parser->next();

			if (parser->token->kind == ':') {
				lambda->return_parameter = parse_definition(ident, ident_location, parser);
				if (!lambda->return_parameter) {
					return 0;
				}
				lambda->return_parameter->definition_location = DefinitionLocation::lambda_return_parameter;
			} else {
				parser->token = first_retparam_token;
				goto parse_retparam_expression;
			}
		} else {
		parse_retparam_expression:
			auto return_type = parse_expression(parser);
			if (!return_type)
				return 0;
			lambda->return_parameter = make_retparam(return_type, lambda);
			lambda->return_parameter->location = return_type->location;
		}
	}

	lambda->location = {lambda->location.begin(), parser->token[-1].string.end()};

	if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';') {
		parser->reporter->error(parser->token->string, "Expected '{{' or '=>' or ';' or ':' instead of '{}'.", parser->token->string);
		return 0;
	}

	bool is_short = false;

	if (is_parsing_type) {
		lambda->has_body = false;
		if (parser->token->kind == '{' || parser->token->kind == '=>') {
			parser->reporter->error(lambda->location, "Body of a lambda can not be specified after a #type directive.");
			return 0;
		} else if (parser->token->kind != ';') {
			parser->reporter->error(parser->token->string, "Expected ';' or return type instead of '{}'.", parser->token->string);
			return 0;
		}
	} else {

		if (parser->token->kind == '{') {
		} else if (parser->token->kind == '=>') {
			is_short = true;
		} else if (parser->token->kind == ';') {
			lambda->has_body = false;
		} else {
			parser->reporter->error(parser->token->string, "Expected '{' or '=>' or ';' or return type instead of '{}'.", parser->token->string);
			return 0;
		}
	}

	scoped_replace(parser->current_lambda, lambda);

	push_scope(&lambda->body_scope);

	if (lambda->has_body) {
		auto opening_token = parser->token;

		parser->next();

		if (is_short) {

			auto expression = parse_expression(parser);
			if (!expression)
				return 0;

			if (!parser->expect(';'))
				return 0;

			auto ret = AstReturn::create();
			ret->expression = expression;
			ret->location = expression->location;
			ret->lambda = lambda;
			add_to_scope(ret, &lambda->body_scope);
		} else {
			while (parser->token->kind != '}') {
				auto statement = parse_statement(parser);
				if (!statement) {
					return 0;
				}
				// This will be performed by parse_statement
				// lambda->body_scope.statements.add(statement);
			}
		}
		parser->next();
	} else {
		if (!is_parsing_type && !lambda->is_intrinsic) {
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

	return lambda;
}

AstExpression *parse_expression_0(Parser *parser) {
	bool is_parsing_type = false;

	switch (parser->token->kind) {
		case Token_simd: {
			parser->next();

			auto expr = parse_expression(parser);
			if (!expr)
				return 0;

			if (expr->kind != Ast_Subscript) {
				parser->reporter->error(expr->location, "Expected an array type after simd keyword, but got {}.", expr->kind);
				return 0;
			}

			auto array = (AstSubscript *)expr;

			// array->is_simd = true;

			return array;
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
				return make_string(unescaped.value_unchecked(), parser->token->string);
			}
			parser->reporter->error(parser->token->string, "Bad escape sequence in string literal.");
			return 0;
		}
		case Token_character_literal: {
			auto character = AstLiteral::create();
			character->literal_kind = LiteralKind::character;
			character->location = parser->token->string;
			auto character_string = unescape_string(parser->token->string);
			if (!character_string) {
				parser->reporter->error(parser->token->string, "Bad escape sequence in string literal.");
				return 0;
			}
			if (character_string.value_unchecked().count != 1) {
				parser->reporter->error(parser->token->string, "Character literal can not contain more than one character.");
				return 0;
			}
			character->character = character_string.value_unchecked().data[0];
			parser->next();
			return character;
		}
		case Token_null: {
			auto result = AstLiteral::create();
			result->literal_kind = LiteralKind::null;
			result->location = parser->token->string;
			parser->next();
			return result;
		}
		case Token_float_literal: {
			auto result = AstLiteral::create();
			result->literal_kind = LiteralKind::Float;
			if (std::from_chars((char *)parser->token->string.begin(), (char *)parser->token->string.end(), result->Float).ec == std::errc::invalid_argument) {
				parser->reporter->error(parser->token->string, "Failed to parse floating point number.");
				return 0;
			}
			result->type = builtin_unsized_float.ident;
			result->location = parser->token->string;
			parser->next();
			return result;
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
			return make_integer(location, value);
		}
		case Token_true:
		case Token_false: {
			auto boolean = AstLiteral::create();
			boolean->literal_kind = LiteralKind::boolean;
			boolean->Bool = parser->token->kind == Token_true;
			boolean->location = parser->token->string;
			parser->next();
			return boolean;
		}
		case Token_split_identifier:
		case Token_identifier: {
			auto identifier = AstIdentifier::create();
			identifier->location = parser->token->string;
			identifier->name = parse_identifier(parser->token);
			assert(!identifier->name.is_empty());
			parser->next();
			return identifier;
		}
		case Token_fn: {

		parse_function:

			auto lambda = parse_lambda(parser, is_parsing_type);
			if (is_parsing_type) {
				lambda->is_type = true;
				return create_lambda_type(lambda);
			}

			return lambda;
		}
		case Token_struct: {
			auto token = parser->token->string;
			parser->next();
			return parse_struct(parser, token);
		}
		case Token_union: {
			auto token = parser->token->string;
			parser->next();
			auto Struct = parse_struct(parser, token);
			if (!Struct)
				return 0;

			Struct->is_union = true;
			return Struct;
		}
		case Token_if: {
			auto If = AstIfx::create();
			If->location = parser->token->string;
			parser->next();

			If->condition = parse_expression(parser);
			if (!If->condition)
				return 0;

			if (parser->token->kind == Token_then)
				parser->next();

			If->true_expression = parse_expression(parser);
			if (!If->true_expression)
				return 0;

			if (!parser->expect(Token_else)) {
				return 0;
			}

			parser->next();

			If->false_expression = parse_expression(parser);
			if (!If->false_expression)
				return 0;

			return If;
		}
		case Token_enum: {
			auto token = parser->token->string;
			parser->next();

			auto Enum = AstEnum::create();
			Enum->location = token;

			if (!parser->expect('{'))
				return 0;

			parser->next();

			push_scope(&Enum->scope);

			auto previous_counter = make_big_int<BigInteger>((s64)-1);

			while (parser->token->kind != '}') {
				auto name_location = parser->token->string;
				auto name = parse_identifier(parser->token);

				if (name.is_empty()) {
					parser->reporter->error(parser->token->string, "Expected a name.");
					return 0;
				}

				parser->next();

				if (parser->token->kind == ';') {

					parser->next();

					auto definition = AstDefinition::create();
					definition->name = name;
					definition->location = name_location;
					definition->is_constant = true;
					definition->type = builtin_unsized_integer.ident;

					previous_counter += (u64)1;

					definition->expression = make_integer(name, copy(previous_counter), Enum);

					add_to_scope(definition, &Enum->scope);
				} else {
					not_implemented();
					auto definition = parse_definition(name, name_location, parser);
					if (!definition)
						return 0;

					if (!definition->expression || needs_semicolon(definition->expression)) {
						if (!parser->expect(';')) {
							return 0;
						}
						parser->next();
					}
				}
			}

			parser->next();
			return Enum;
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
				return 0;
			}

			return import;
		}
					 */
		case '(': {
			auto start_token = parser->token->string;
			if (!parser->next()) {
				parser->reporter->error(parser->token->string, "Unexpected end of file. Unclosed ')'.");
				return 0;
			}

			auto expression = parse_expression(parser);
			if (!expression) {
				return 0;
			}

			if (parser->token->kind == ')') {
				expression->is_parenthesized = true;
				auto end_token = parser->token->string;
				expression->location = {start_token.begin(), end_token.end()};
				parser->next();
				return expression;
			//} else if (parser->token->kind == ',') {
			//	auto tuple = AstTuple::create();
			//	tuple->expressions.add(expression);
			//
			//	parser->next();
			//
			//	while (1) {
			//		auto expression = parse_expression(parser);
			//		if (!expression) {
			//			return 0;
			//		}
			//		tuple->expressions.add(expression);
			//
			//		if (parser->token->kind == ')') {
			//			break;
			//		} else if (parser->token->kind == ',') {
			//			parser->next();
			//		} else {
			//			parser->reporter->error(parser->token->string, "Expected ')' or ','");
			//			return 0;
			//		}
			//	}
			//	parser->next();
			//
			//	return tuple;
			//} else {
			//	parser->reporter->error(parser->token->string, "Expected ')' or ','");
			//	return 0;
			} else {
				parser->reporter->error(parser->token->string, "Expected ')'");
				return 0;
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
				index_expression = parse_expression(parser);
				if (!index_expression)
					return 0;

				if (!parser->expect(']'))
					return 0;
			}

			parser->next();

			auto expression = parse_expression_0(parser);
			if (!expression)
				return 0;

			location = {location.begin(), parser->token[-1].string.end()};

			if (index_expression) {
				auto subscript = AstSubscript::create();
				subscript->location = location;
				subscript->expression = expression;
				subscript->index_expression = index_expression;
				return subscript;
			} else {
				auto span = AstSpan::create();
				span->location = location;
				span->expression = expression;
				return span;
			}
		}
		default: {
			auto operation = as_unary_operation(*parser->token);
			if (operation) {
				auto unop = AstUnaryOperator::create();
				unop->location = parser->token->string;
				unop->operation = operation.value_unchecked();
				parser->next();

				unop->expression = parse_expression_1(parser);
				if (!unop->expression)
					return 0;

				unop->location = {unop->location.begin(), unop->expression->location.end()};

				switch (unop->operation) {
					using enum UnaryOperation;
					case poly: {
						if (unop->expression->kind != Ast_Identifier) {
							parser->reporter->error(unop->location, "To make a polymorphic type you must put an identifier after {}", unop->operation);
							return 0;
						}
						if (parser->poly_identifier) {
							parser->reporter->error(unop->location, "Right now only one polymorphic type per parameter is allowed.");
							parser->reporter->info(parser->poly_identifier->location, "Previous definition here:");
							return 0;
						}
						parser->poly_identifier = (AstIdentifier *)unop->expression;
						break;
					}
				}

				return unop;
			} else if (parser->token->kind == Token_directive) {
				if (parser->token->string == "#type"str) {
					if (!parser->next_expect(Token_fn))
						return 0;

					is_parsing_type = true;
					goto parse_function;
				} else if (parser->token->string == "#file"str) {
					auto result = make_string(parser->lexer->source_info->path);
					result->location = parser->token->string;
					parser->next();
					return result;
				} else if (parser->token->string == "#line"str) {
					auto result = make_integer(parser->token->string, (u64)get_line_number(parser->token->string.data));
					parser->next();
					return result;
				} else if (parser->token->string == "#location"str) {
					auto result = make_string(where(parser->token->string.data));
					result->location = parser->token->string;
					parser->next();
					return result;
				} else if (parser->token->string == "#function"str) {
					auto result = AstLiteral::create();
					result->location = parser->token->string;
					result->literal_kind = LiteralKind::lambda_name;
					parser->next();
					return result;
				} else if (parser->token->string == "#compiles"str) {
					auto test = AstTest::create();
					test->location = parser->token->string;

					parser->next();

					if (!parse_block_or_single_statement(parser, &test->scope)) {
						return 0;
					}

					return test;
				} else {
					parser->reporter->error(parser->token->string, "Unexpected directive (expression).");
					return 0;
				}
			} else {
				parser->reporter->error(parser->token->string, "Unexpected {}: {}.", token_kind_to_string(parser->token->kind), parser->token->string);
				return 0;
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
	if (!binop->is_parenthesized)
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
			binop->operation = binary_operation_from_token(parser->token->kind);
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
			parser->next();

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

					if (parser->token->kind == ')') {
						break;
					}

					if (!parser->expect(','))
						return 0;

					parser->next();
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
			subscript->is_prefix = false;
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
bool simplify(Reporter *reporter, AstExpression **_expression) {

#define SIMPLIFY(reporter, x) ([&] { auto y = raw(x); auto result = simplify(reporter, &y); x = y; return result; }())

	auto expression = *_expression;
	defer {
		if (expression) {
			expression->location = (*_expression)->location;
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
										expression = make_integer(binop->location, (u64)left->string.count, right->type);
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
								case add: expression = make_integer({}, left + right, binop->type); return true;
								case sub: expression = make_integer({}, left - right, binop->type); return true;
								case mul: expression = make_integer({}, left * right, binop->type); return true;
								case div:
									if (right == 0) {
										reporter->error(expression->location, "Integer division by zero.");
										expression = 0;
										return false;
									}
									expression = make_integer({}, left / right, binop->type);
									return true;
								case mod: expression = make_integer({}, left % right, binop->type); return true;
								case band: expression = make_integer({}, left & right, binop->type); return true;
								case bor: expression = make_integer({}, left | right, binop->type); return true;
								case bxor: expression = make_integer({}, left ^ right, binop->type); return true;
								case bsl:
									if (right.msb) {
										reporter->error(expression->location, "Can't shift left by negative amount.");
										expression = 0;
										return false;
									}
									if (right.parts.count > 1) {
										reporter->error(expression->location, "Can't shift left by amount that big. Resulting number will not fit in memory.");
										expression = 0;
										return false;
									}
									expression = make_integer({}, left << right, binop->type);
									return true;
								case bsr: expression = make_integer({}, left >> right, binop->type); return true;
								case lt:  expression = make_boolean(left < right); return true;
								case gt:  expression = make_boolean(left > right); return true;
								case le: expression = make_boolean(left <= right); return true;
								case ge: expression = make_boolean(left >= right); return true;
								case ne: expression = make_boolean(left != right); return true;
								case eq: expression = make_boolean(left == right); return true;
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
							if (types_match(binop->right, builtin_u8)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
							} else if (types_match(binop->right, builtin_u16)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
							} else if (types_match(binop->right, builtin_u32)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
							} else if (types_match(binop->right, builtin_u64) || ::is_pointer(binop->right)) {
								result.msb = 0;
								result.parts.resize(1);
							} else if (types_match(binop->right, builtin_s8)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
								result.msb = result.parts.data[0] & 0x80;
								result.parts.data[0] |= result.msb ? ~0xFF : 0;
							} else if (types_match(binop->right, builtin_s16)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
								result.msb = result.parts.data[0] & 0x8000;
								result.parts.data[0] |= result.msb ? ~0xFFFF : 0;
							} else if (types_match(binop->right, builtin_s32)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
								result.msb = result.parts.data[0] & 0x80000000;
								result.parts.data[0] |= result.msb ? ~0xFFFFFFFF : 0;
							} else if (types_match(binop->right, builtin_s64)) {
								result.parts.resize(1);
								result.msb = result.parts.data[0] & 0x8000000000000000;
							} else {
								invalid_code_path();
							}
							expression = make_integer({}, result, binop->right);
						} else {
							invalid_code_path();
						}
						expression->location = binop->location;
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
					default:
						print(Print_warning, "unhandled binary operation in simplify\n");
						break;
				}
				break;
			}
			case Ast_UnaryOperator: {
				using enum UnaryOperation;
				auto unop = (AstUnaryOperator *)expression;

				if (unop->expression->kind == Ast_Literal) {
					auto literal = (AstLiteral *)unop->expression;
					switch (literal->literal_kind) {
						case LiteralKind::integer: {
							auto integer = literal->integer;
							switch (unop->operation) {
								case plus: return true;
								case minus: expression = make_integer({}, -integer); return true;
								case bnot:  expression = make_integer({}, ~integer); return true;
								case autocast: return true;

								default:
									print(Print_warning, "unhandled unary operation in simplify(integer)\n");
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
									print(Print_warning, "unhandled unary operation in simplify(float)\n");
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
							if (definition->expression->kind != Ast_Lambda) {
								expression = definition->expression;
								// assert(expression->kind == Ast_Literal);
							}
						}
					}
				}
				break;
			}
			case Ast_Call:
			case Ast_Literal:
			case Ast_Lambda:
			case Ast_LambdaType:
			case Ast_Struct:
			case Ast_Subscript:
			case Ast_Span:
			case Ast_Ifx:
			case Ast_Test:
			case Ast_Enum:
				break;
			default:
				print(Print_warning, "unhandled case in simplify: {}\n", expression->kind);
				break;
		}
	}
	return true;
}

BinaryOperation binary_operation_from_token(TokenKind kind) {
	using enum BinaryOperation;
	switch (kind) {
		case '+':  return add;
		case '-':  return sub;
		case '*':  return mul;
		case '/':  return div;
		case '%':  return mod;
		case '^':  return bxor;
		case '&':  return band;
		case '|':  return bor;
		case '<<': return bsl;
		case '>>': return bsr;
		case '&&': return land;
		case '||': return lor;
		case '==': return eq;
		case '!=': return ne;
		case '<':  return lt;
		case '>':  return gt;
		case '<=': return le;
		case '>=': return ge;
		case '.':  return dot;
		case '=':  return ass;

		case '+=':  return addass;
		case '-=':  return subass;
		case '*=':  return mulass;
		case '/=':  return divass;
		case '%=':  return modass;
		case '^=':  return bxorass;
		case '&=':  return bandass;
		case '|=':  return borass;
		case '<<=': return bslass;
		case '>>=': return bsrass;

		case Token_as: return as;

		default:
			invalid_code_path();
			break;
	}
}

// a.b[c] = ((a).(b))[c]
// a+b[c] = (a)+((b)[c])
int parse_paren_level = 0;
void print_parsed_expression(AstExpression *expression) {
	static constexpr PrintKind print_kinds[] {
		Print_info,
		Print_error,
		Print_warning,
	};
	auto open = [](char c = '(') { print(print_kinds[parse_paren_level++ % 3], c); };
	auto close = [](char c = ')') { print(print_kinds[--parse_paren_level % 3], c); };
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

// NOTE: I do not have deep understanding how to properly do operator precedence.
// I just feel like this is not the best way to do it.
void parse_expression(Parser *parser, AstExpression **_expression) {
	timed_function(context.profiler);
	auto &expression = *_expression;
	defer { *_expression = expression; };

	expression = parse_expression_1(parser);
	if (!expression) {
		return;
	}

	if (parser->token->kind == 'eof') {
		return;
	}

	defer {
		if (expression) {
			if (!simplify(parser->reporter, &expression))
				expression = 0;
		}
	};

	AstBinaryOperator *top_binop = 0;
	AstBinaryOperator *previous_binop = 0;
	s32 previous_precedence = 0;

	while (auto maybe_operation = as_binary_operation(parser->token->kind)) {
		auto operation = maybe_operation.value_unchecked();

		auto binop = AstBinaryOperator::create();

		defer { previous_binop = binop; };

		binop->left = expression;

		binop->operation = operation;

		if (!parser->next()) {
			parser->reporter->error("Unexpected end of file after binary operator.");
			expression = 0;
			return;
		}

		binop->right = parse_expression_1(parser);
		if (!binop->right) {
			expression = 0;
			return;
		}
		update_location(binop);

#if 0
		if (binop->operation == BinaryOperation::dot) {
			switch (binop->right->kind) {
				case Ast_Call: {
					auto call = (AstCall *)binop->right;

					if (binop->operation == BinaryOperation::dot) {
						binop->right = call->callable;
						call->callable = binop;

						update_location(binop);
						update_location(call);

						expression = call;
						continue;
					}
					break;
				}
				case Ast_Subscript: {
					auto subscript = (AstSubscript *)binop->right;

					if (binop->operation == BinaryOperation::dot) {
						binop->right = subscript->expression;
						subscript->expression = binop;

						update_location(binop);
						update_location(subscript);

						expression = subscript;
						continue;
					}
					break;
				}
			}
		}
#endif
		if (!binop->left->is_parenthesized) {
			switch (binop->left->kind) {
				case Ast_BinaryOperator: {
					auto child = (AstBinaryOperator *)binop->left;
					if (get_precedence(child->operation) < get_precedence(binop->operation)) {
						binop->left = child->right;
						child->right = binop;

						update_location(child);
						update_location(binop);

						expression = child;
						continue;
					}
					break;
				}
			}
		}
		expression = binop;
	}
}
AstExpression *parse_expression(Parser *parser) {
	AstExpression *expression = 0;
	parse_expression(parser, &expression);
	return expression;
}

// returns true for functions (overloading), otherwise false
bool is_redefinable(AstDefinition *definition) {
	return is_lambda(definition->expression);
}

//
// Use this if name token is already taken from parser.
//
AstDefinition *parse_definition(String name, String location, Parser *parser) {
	assert(parser->token->kind == ':');

	parser->next();

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

	definition->is_constant = is_constant;
	// definition->add_to_scope(parser->current_scope);

	if (has_expression) {
		parser->next();

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		definition->expression = expression;
		switch (expression->kind) {
			case Ast_Lambda: {
				auto lambda = (AstLambda *)expression;
				lambda->definition = definition;
				break;
			}
			case Ast_Struct: {
				auto Struct = (AstStruct *)expression;
				Struct->definition = definition;
				break;
			}
			case Ast_Enum: {
				auto Enum = (AstEnum *)expression;
				Enum->definition = definition;
				break;
			}
		}
		definition->location = {location.begin(), definition->expression->location.end()};
	} else {
		definition->location = {location.begin(), definition->type->location.end()};
	}

	if (definition->is_constant) {
		definition->definition_location = DefinitionLocation::global;
	}

	return definition;
}
AstDefinition *parse_definition(Parser *parser) {
	auto location = parser->token->string;
	auto name = parse_identifier(parser->token);
	if (name.is_empty()) {
		parser->reporter->error(location, "Failed to parse definition.");
		return 0;
	}

	if (!parser->next_expect(':'))
		return 0;

	return parse_definition(name, location, parser);
}

bool is_statement(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Call:
		case Ast_Import:
			return true;
		case Ast_BinaryOperator:
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

Map<BinaryOperation, List<AstLambda *>> binary_operators;

bool parse_block_or_single_statement(Parser *parser, Scope *scope) {
	push_scope(scope);

	bool has_braces = parser->token->kind == '{';

	if (has_braces) {
		parser->next();
		while (parser->token->kind != '}') {
			auto statement = parse_statement(parser);
			if (!statement)
				return false;
		}
		parser->next();
	} else {
		auto statement = parse_statement(parser);
		if (!statement)
			return false;
	}

	return true;
}
AstIf *parse_if_statement(Parser *parser, String token) {
	auto If = AstIf::create();
	If->location = token;
	If->condition = parse_expression(parser);
	if (!If->condition)
		return 0;

	if (parser->token->kind == Token_then)
		parser->next();

	if (!parse_block_or_single_statement(parser, &If->true_scope))
		return 0;

	if (parser->token->kind == Token_else) {
		parser->next();

		if (!parse_block_or_single_statement(parser, &If->false_scope))
			return 0;
	}
	return If;
}

AstEmptyStatement *empty_statement;

void parse_statement(Parser *parser, AstStatement *&result) {
	timed_function(context.profiler);


	result = 0;
	defer {
		if (result && result->kind != Ast_EmptyStatement) {
			scoped_lock(parser->current_scope);
			if (!result->parent_scope)
				add_to_scope(result, parser->current_scope);
		}
	};

	switch (parser->token->kind) {
		case Token_split_identifier:
		case Token_identifier: {
			auto location = parser->token->string;
			auto name = parse_identifier(parser->token);
			assert(!name.is_empty());

			parser->next();

			if (parser->token->kind == ':') {
				// Definition

				auto definition = parse_definition(name, location, parser);

				if (!definition) {
					return;
				}

				if (!definition->expression || needs_semicolon(definition->expression)) {
					if (!parser->expect(';'))
						return;
					parser->next();
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

			if (!ensure_return_is_not_in_defer(parser, return_token->string))
				return;

			parser->next();

			auto ret = AstReturn::create();

			if (parser->token->kind != ';') {
				auto expression = parse_expression(parser);
				if (!expression)
					return;
				if (!parser->expect(';'))
					return;
				ret->expression = expression;
			}

			parser->next();

			ret->lambda = parser->current_lambda;
			ret->location = return_token->string;
			result = ret;
			return;
		}
		case Token_if: {
			auto token = parser->token->string;
			parser->next();

			result = parse_if_statement(parser, token);
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

			if (!parse_block_or_single_statement(parser, &While->scope)) {
				return;
			}
			result = While;
			return;
		}
		case Token_directive: {
			if (parser->token->string == "#if"str) {
				auto token = parser->token->string;
				parser->next();

				auto If = parse_if_statement(parser, token);
				if (!If)
					return;

				If->is_constant = true;
				result = If;
				return;
			} else if (parser->token->string == "#assert"str) {
				auto assert = AstAssert::create();
				assert->location = parser->token->string;
				assert->is_constant = true;

				parser->next();

				assert->condition = parse_expression(parser);
				if (!assert->condition) {
					return;
				}
				if (!parser->expect(';'))
					return;
				parser->next();

				result = assert;
				return;
			} else if (parser->token->string == "#print"str) {
				auto print = AstPrint::create();
				print->location = parser->token->string;
				parser->next();
				print->expression = parse_expression(parser);
				if (!parser->expect(';'))
					return;
				parser->next();

				result = print;
				return;
			} else if (parser->token->string == "#parse"str) {
				auto parse = AstParse::create();
				parse->location = parser->token->string;
				parser->next();
				parse->expression = parse_expression(parser);
				if (!parser->expect(';'))
					return;
				parser->next();

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
			//		type.Struct->type = builtin_type.ident;
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
			//		init_builtin(builtin_string, definition);
			//	} else if (definition->name == "struct_member") {
			//		init_builtin(builtin_struct_member, definition);
			//	} else if (definition->name == "typeinfo") {
			//		init_builtin(builtin_typeinfo, definition);
			//	} else if (definition->name == "any") {
			//		init_builtin(builtin_any, definition);
			//	} else {
			//		parser->reporter->error(definition->location, "Unknown builtin.");
			//		return;
			//	}

			//	result = definition;
			//	return;
			} else {
				parser->reporter->error(parser->token->string, "Unknown statement level directive.");
				return;
			}
			break;
		}
		case Token_defer: {
			auto Defer = AstDefer::create();
			Defer->location = parser->token->string;
			parser->next();
			if (!parse_block_or_single_statement(parser, &Defer->scope)) {
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
				{
					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = as_binary_operation(parser->token->kind).value();

					if (!parser->next_expect(':'))
						return;
					if (!parser->next_expect(':'))
						return;
					parser->next();

					definition->lambda = parse_lambda(parser, false);
					if (!definition->lambda)
						return;

					if (definition->lambda->parameters.count != 2) {
						parser->reporter->error(definition->lambda->location, "'{}' operator must have exactly two parameters.", definition->location);
						return;
					}

					result = definition;
					return;
				}
				case 'as': {
					parser->next();

					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = BinaryOperation::as;
					switch (parser->token->kind) {
						case Token_implicit:
							untypechecked_implicit_casts_count += 1;
							definition->is_implicit = true;
							if (!parser->next_expect(':'))
								return;
							break;
						case Token_explicit:
							definition->is_implicit = false;
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

					auto lambda = parse_lambda(parser, false);
					if (!lambda)
						return;

					if (lambda->parameters.count != 1) {
						parser->reporter->error(lambda->location, "'as' operator must have exactly one parameter.");
						return;
					}

					definition->lambda = lambda;
					result = definition;
					return;
				}
				default: {
					parser->reporter->error(parser->token->string, "Expected an operator.");
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

			if (!parser->expect(';'))
				return;

			parser->next();

			result = assert;
			return;
		}
		case '{': {
			auto block = AstBlock::create();

			push_scope(&block->scope);

			parser->next();
			while (parser->token->kind != '}') {
				auto statement = parse_statement(parser);
				if (!statement)
					return;
			}
			parser->next();

			result = block;
			return;
		}
		// no need to put this in the current scope
		case ';': {
			parser->reporter->warning(parser->token->string, "Redundant semicolon.");
			result = empty_statement;
			parser->next();
			return;
		}
	}

	auto expression = parse_expression(parser);
	if (expression) {
		if (parser->token->kind == ';') {
			if (!is_statement(expression)) {
				parser->reporter->error(expression->location, "This expression is not a statement.");
				return;
			}
			parser->next();
			result = make_statement(expression);
			return;
		}
	}

	parser->reporter->error(parser->token->string, "Failed to parse statement or expression.");
}
AstStatement *parse_statement(Parser *parser) {
	AstStatement *result;
	parse_statement(parser, result);
	return result;
}

AstStatement *parse_global_statement(Parser *parser) {
	timed_function(context.profiler);
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

SourceFileContext *parse_file(String path) {
	timed_function(context.profiler);

	if (auto found = parsed_files.find(path)) {
		return found->value;
	}

	// print("Parsing {}\n", path);

	auto context =
	parsed_files.get_or_insert(path) =
	default_allocator.allocate<SourceFileContext>();

	context->lexer.source_buffer = read_entire_file(to_pathchars(path), {.extra_space_before=1, .extra_space_after=1});
	if (!context->lexer.source_buffer.data) {
		print("Failed to read '{}'. Exiting.\n", path);
		context->result = ParseResult::read_error;
		return context;
	}

	context->lexer.source_buffer.front() = '\0';
	context->lexer.source_buffer.back() = '\0';

	String source = String(context->lexer.source_buffer);
	source.data += 1;
	source.count -= 2;

	auto bom = Span(context->lexer.source_buffer.data + 1, (umm)3);
	if (bom.end() <= context->lexer.source_buffer.end() && bom == "\xef\xbb\xbf"b) {
		bom.back() = '\0';
		source.data += 3;
		source.count -= 3;
	}
	context->lexer.source = source;

	// context->scope.parent = &global_scope;

	context->parser.lexer = &context->lexer;
	context->parser.reporter = context->lexer.reporter = &context->reporter;
	// context->parser.current_scope = &context->scope;

	auto source_info = &::context.sources.add({path, source});

	context->lexer.source_info = source_info;

	constexpr auto max_token_count = (1*GiB)/sizeof(Token);

	context->lexer.tokens_start = (Token *)VirtualAlloc(0, max_token_count*sizeof(Token), MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
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
	timed_function(context.profiler);

	auto lexer = parser->lexer;

	while (lexer->tokens_lexed() == 0 && !lexer->finished) {} // Wait for tokens

	if (lexer->tokens_lexed() == 0) {
		return ParseResult::ok;
	}

	parser->token = lexer->begin();
	while (parser->token->kind != 'eof') {
		if (parser->token->kind == Token_directive) {
			if (parser->token->string == "#extern_language"str) {
				if (!parser->next_expect(Token_string_literal)) {
					parser->reporter->error("Expected language name. Currently only \"C\" is available.");
					return ParseResult::syntax_error;
				}
				auto unescaped = unescape_string(parser->token->string);
				if (!unescaped) {
					parser->reporter->error(parser->token->string, "Bad escape sequence");
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
					parser->reporter->error(parser->token->string, "Bad escape sequence");
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
				parser->reporter->error(parser->token->string, "Bad escape sequence");
				return ParseResult::syntax_error;
			}
			auto libname = unescaped.value_unchecked();
			auto child = parse_file((String)concatenate(context.compiler_directory, "\\libs\\", libname));
			// global_scope.append(child->scope);
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

struct TypecheckState {
	TypecheckState() = default;
	TypecheckState(TypecheckState const &) = delete;

	AstStatement *statement = 0;
	AstLambda *lambda = 0;
	AstDefinition *definition = 0;
	AstExpression *waiting_for = 0;
	String waiting_for_name;
	AstExpression *current_lambda_or_struct_or_enum = 0;

	AstDefinition *currently_typechecking_definition = 0;

	AstLambda *current_lambda = 0;

	Scope *current_scope = 0;

	Reporter reporter;

	bool finished = false;

	CoroState *coro = 0;
	void *fiber = 0;
	void *parent_fiber = 0;

	u32 no_progress_counter = 0;

	TypecheckState *next_state = 0;

	List<AstCall *> poly_call_stack;
};

#undef push_scope
#define push_scope(scope) \
	auto CONCAT(old_scope, __LINE__) = state->current_scope; \
	state->current_scope = scope; \
	defer { state->current_scope = CONCAT(old_scope, __LINE__); };

#define typecheck_scope(scope) \
	{ \
		push_scope(scope); \
		for (auto statement : (scope)->statements) { \
			typecheck(state, statement); \
		} \
	}

DefinitionList get_definitions(TypecheckState *state, KeyString name) {
	timed_function(context.profiler);
	DefinitionList result;
	auto scope = state->current_scope;

	auto is_lambda_scope = [&] (Scope *scope) {
		return scope->node && scope->node->kind == Ast_Lambda && scope == ((AstLambda *)scope->node)->outer_scope();
	};

	Scope *prev_scope = 0;
	while (scope) {
		auto found_local = scope->definitions.find(name);
		if (found_local)
			result.add(found_local->value);
		if (is_lambda_scope(scope)) {
			if (result.count) {
				prev_scope = &global_scope; // FIXME
				break;
			}

			// skip all scopes of parent lambda
			auto top_lambda = scope;

			while (scope) {
				if (is_lambda_scope(scope)) {
					top_lambda = scope;
				}
				scope = scope->parent;
			}

			scope = top_lambda->parent;
			continue;
		}

		prev_scope = scope;
		scope = scope->parent;
	}
	assert(prev_scope == &global_scope, "incomplete relationships between scopes. probably you forgot to set scope's parent.");
	for (auto definition : result)
		assert(name == definition->name);
	return result;
}

struct IntegerInfo {
	AstStruct *type;
	s64 bits;
};

IntegerInfo integer_infos[8];

void harden_type(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_Literal: {
			auto literal = (AstLiteral *)expression;
			 // Literals may already have their type set by, for example, cast expressions
			if (types_match(literal->type, builtin_unsized_integer)) {
				literal->type = type_int;
			} else if (types_match(literal->type, builtin_unsized_float)) {
				literal->type = type_float;
			}
			break;
		}
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;
			 // Literals may already have their type set by, for example, cast expressions
			if (types_match(identifier->type, builtin_unsized_integer)) {
				identifier->type = type_int;
			} else if (types_match(identifier->type, builtin_unsized_float)) {
				identifier->type = type_float;
			}
			break;
		}
		case Ast_BinaryOperator:
		case Ast_Call:
		case Ast_Subscript:
		case Ast_UnaryOperator:
		case Ast_Struct:
		case Ast_Lambda:
		case Ast_Ifx:
			break;
		default:
			invalid_code_path();
	}
}

AstLiteral *make_type_literal(AstExpression *type) {
	timed_function(context.profiler);
	auto result = AstLiteral::create();
	result->literal_kind = LiteralKind::type;
	result->type_value = type;
	result->type = builtin_type.ident;
	return result;
}

AstLiteral *make_bool(bool val) {
	timed_function(context.profiler);
	auto result = AstLiteral::create();
	result->literal_kind = LiteralKind::boolean;
	result->Bool = val;
	result->type = builtin_bool.ident;
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

u32 put_in_section(AstLiteral *, Section &);

void put_arrays_in_section(AstLiteral *literal, Section &section) {
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
		case pack: {
			for (auto val : literal->pack_values)
				put_arrays_in_section(val, section);

			literal->pack_offset = section.buffer.count;
			for (auto val : literal->pack_values)
				put_in_section(val, section);

			break;
		}
		case Struct: {
			for (auto val : literal->struct_values)
				put_arrays_in_section(val, section);

			literal->struct_offset = section.buffer.count;
			for (auto val : literal->struct_values)
				put_arrays_in_section(val, section);
			break;
		}
		default:
			invalid_code_path();
	}
}
u32 put_in_section(AstLiteral *literal, Section &section) {
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
		case integer:
			section.align(8);
			return section.w8((u64)literal->integer);
		case boolean:
			return section.w1(literal->Bool);
		case string: {
			section.align(8);
			auto result =
			section.w8((u64)literal->string.offset);
			section.w8((u64)literal->string.count);
			section.relocations.add(result);
			return result;
		}
		case character:
			return section.w1((u64)literal->character);
		case noinit:
			break;
		case Float:
			if (types_match(literal->type, builtin_f32)) {
				section.align(4);
				return section.w4((u64)std::bit_cast<u32>((f32)literal->Float));
			} else if (types_match(literal->type, builtin_f64)) {
				section.align(8);
				return section.w8(*(u64 *)&literal->Float);
			} else {
				invalid_code_path();
			}
		case pointer: {
			section.align(8);
			auto result =
			section.w8((u64)literal->pointer.offset);
			section.relocations.add(result);
			return result;
		}
		case Struct: {
			auto alignment = get_align(literal->type);
			assert(alignment);
			section.align(alignment);

			auto result = section.buffer.count;

			for (auto member : literal->struct_values) {
				put_in_section(member, section);
			}

			return result;
		}
		case pack: {
			assert(literal->pack_offset != -1);
			section.align(8);
			auto result =
			section.w8((u64)literal->pack_offset);
			section.w8((u64)literal->pack_values.count);
			section.relocations.add(result);
			return result;
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

AstLiteral *evaluate(TypecheckState *state, AstDefinition *definition);
AstLiteral *evaluate(TypecheckState *state, AstExpression *expression);

AstLiteral *evaluate(TypecheckState *state, AstDefinition *definition) {
	if (!definition->is_constant) {
		state->reporter.error(definition->location, "Can't evaluate expression at compile time: definition is not constant");
		return 0;
	}

	if (definition->evaluated)
		return definition->evaluated;

	return definition->evaluated = evaluate(state, definition->expression);
}
// TODO: fix locations for make_integer and others
AstLiteral *evaluate(TypecheckState *state, AstExpression *expression) {
	timed_function(context.profiler);
	switch (expression->kind) {
		case Ast_Literal: return (AstLiteral *)expression;
		case Ast_Identifier: {
			auto ident = (AstIdentifier *)expression;

			ensure_definition_is_resolved(state, ident);

			auto definition = ident->definition();

			return evaluate(state, definition);
		}
		case Ast_BinaryOperator: {
			auto bin = (AstBinaryOperator *)expression;
			auto l = evaluate(state, bin->left);
			if (!l) return nullptr;
			auto r = evaluate(state, bin->right);
			if (!r) return nullptr;
			using enum BinaryOperation;
			switch (bin->operation) {
				case add: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer({}, l->integer + r->integer, bin->type);
						case Float:   return make_float  (l->Float   + r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case sub: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer({}, l->integer - r->integer, bin->type);
						case Float:   return make_float  (l->Float   - r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case mul: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer({}, l->integer * r->integer, bin->type);
						case Float:   return make_float  (l->Float   * r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case div: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer({}, l->integer / r->integer, bin->type);
						case Float:   return make_float  (l->Float   / r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case eq: {
					assert(l->literal_kind == LiteralKind::type);
					assert(r->literal_kind == LiteralKind::type);
					bool val = types_match(l->type_value, r->type_value);
					if (!val) {
						int x = 4;
					}
					return make_bool(val);
				}
				default:
					invalid_code_path();
					return nullptr;
			}
		}
		case Ast_Struct: {
			return make_type_literal(expression);
		}
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;

			switch (unop->operation) {
				using enum UnaryOperation;
				case bnot: {
					auto child = evaluate(state, unop->expression);
					if (!child)
						return nullptr;
					assert(child->literal_kind == LiteralKind::boolean);
					return make_bool(!child->Bool);
				}
				case typeof:
					return make_type_literal(unop->expression->type);
				case address_of: {
					auto l = AstLiteral::create();
					l->literal_kind = LiteralKind::pointer;
					l->pointer.section = SectionKind::constant;

					assert(unop->expression->kind == Ast_Identifier);
					auto identifier = (AstIdentifier *)unop->expression;

					auto definition = identifier->definition();
					assert(definition);

					l->pointer.offset = definition->offset;
					return l;
				}
				default: {
					if (types_match(expression->type, builtin_type)) {
						auto child = evaluate(state, unop->expression);
						if (!child)
							return nullptr;

						if (child->literal_kind == LiteralKind::type) {
							return make_type_literal(make_pointer_type(child->type_value));
						}
					}
					invalid_code_path();
				}
			}

			break;
		}
		case Ast_Call: {
			// TODO: this is very limited right now
			auto call = (AstCall *)expression;
			if (auto Struct = direct_as<AstStruct>(call->callable)) {
				auto result = AstLiteral::create();
				result->literal_kind = LiteralKind::Struct;
				result->struct_values.resize(Struct->data_members.count);
				for (umm i = 0; i < Struct->data_members.count; ++i) {
					result->struct_values[i] = evaluate(state, call->sorted_arguments[i]);
				}
				result->type = Struct;
				return result;
			} else {
				auto lambda = get_lambda(call->callable);
				if (lambda->body_scope.statements.count != 1) {
					state->reporter.error(expression->location, "Can't evaluate expression at compile time: lambda can have only one return statement for now.");
					return nullptr;
				}
				assert(lambda->body_scope.statements[0]->kind == Ast_Return);
				return evaluate(state, ((AstReturn *)lambda->body_scope.statements[0])->expression);
			}
		}
		case Ast_Lambda: {
			auto lambda = (AstLambda *)expression;
			assert(!lambda->has_body);
			assert(lambda->is_type);
			return make_type_literal(lambda);
		}
		case Ast_Subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(is_type(subscript->expression));
			return make_type_literal(subscript);
		}
		case Ast_Pack: {
			auto pack = (AstPack *)expression;
			auto literal = AstLiteral::create();
			literal->literal_kind = LiteralKind::pack;
			literal->pack_values.resize(pack->expressions.count);
			for (umm i = 0; i < pack->expressions.count; ++i) {
				literal->pack_values[i] = evaluate(state, pack->expressions[i]);
			}
			literal->type = pack->type;
			return literal;
		}
		default:
			invalid_code_path();
	}
	return nullptr;
}

bool do_all_paths_explicitly_return(AstLambda *lambda, SmallList<AstStatement *> statements) {
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
				}
				break;
			}
			case Ast_If: {
				auto If = (AstIf *)statement;

				if (do_all_paths_explicitly_return(lambda, If->true_scope.statements) &&
					do_all_paths_explicitly_return(lambda, If->false_scope.statements)
				)
					return true;
				break;
			}
		}
	}
	return false;
}
bool do_all_paths_explicitly_return(AstLambda *lambda) {
	if (types_match(lambda->return_parameter->type, builtin_void))
		return true;

	return do_all_paths_explicitly_return(lambda, lambda->body_scope.statements);
}

AstUnaryOperator *make_pointer_type(AstExpression *type) {
	using enum UnaryOperation;
	auto unop = AstUnaryOperator::create();
	unop->expression = type;
	unop->type = builtin_type.ident;
	unop->operation = pointer;
	return unop;
}
AstSpan *make_span_type(AstExpression *type) {
	auto span = AstSpan::create();
	span->expression = type;
	span->type = builtin_type.ident;
	return span;
}

void typecheck(TypecheckState *state, Expression<> &expression);

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

bool ensure_fits(Reporter *reporter, AstExpression *expression, BigInteger integer, IntegerInfo info) {
	auto top_bits = integer >> info.bits;
	defer { free(top_bits); };
	if (top_bits == 0 || top_bits == -1)
		return true;
	if (reporter) {
		auto name = type_name(info.type);
		reporter->error(expression->location, "{} is not implicitly convertible to {} because some bits would be lost. You can explicitly write `{} as {}` or `@{}` to perform lossy conversion.", integer, name, expression->location, name, expression->location);
	}
	return false;
}

AstBinaryOperator *make_cast(AstExpression *expression, AstExpression *type) {
	auto result = AstBinaryOperator::create();
	result->operation = BinaryOperation::as;
	result->left = expression;
	result->right = result->type = type;
	result->location = expression->location;
	return result;
}

List<AstLambda *> implicit_casts;
List<AstLambda *> explicit_casts;

bool ensure_addressable(Reporter *reporter, AstExpression *expression) {
	timed_function(context.profiler);
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
						  /*
		case Ast_UnaryOperator: {
			auto unop = (AstUnaryOperator *)expression;
			if (unop->operation == '*')
				return true;
			break;
		}
						  */
	}

	reporter->error(expression->location, "Expression is not addressable.");
	return false;
}

bool is_poly(AstExpression *type) {
	return types_match(type, builtin_poly) || (type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)type)->operation == UnaryOperation::poly);
}

AstLiteral *find_enum_value(TypecheckState *state, AstEnum *Enum, KeyString name, String location) {
	auto found_member = Enum->scope.definitions.find(name);
	if (!found_member) {
		state->reporter.error(location, "Enum '{}' does not contain constant '{}'", Enum->definition->name, name);
		yield(TypecheckResult::fail);
	}

	assert(found_member->value.count);

	if (found_member->value.count > 1) {
		state->reporter.error(location, "Ambiguous name: {}", found_member->value[0]->name);
		yield(TypecheckResult::fail);
	}

	auto definition = found_member->value.data[0];
	assert(definition->expression);
	assert(definition->expression->kind == Ast_Literal);
	auto literal = (AstLiteral *)definition->expression;
	assert(literal->literal_kind == LiteralKind::integer);
	return make_integer(literal->location, copy(literal->integer), Enum);
}

void wait_for(TypecheckState *state, auto predicate, auto error_callback) {
	while (1) {
		if (predicate())
			break;

		if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) {
			error_callback();
			yield(TypecheckResult::fail);
		}
		state->no_progress_counter += 1;
		yield(TypecheckResult::wait);
	}
}

void evaluate_and_put_definition_in_section(TypecheckState *state, AstDefinition *definition, Section &section) {
	definition->evaluated = evaluate(state, definition->expression);
	put_arrays_in_section(definition->evaluated, section);
	definition->offset = put_in_section(definition->evaluated, section);
}

u64 get_type_kind(AstExpression *type) {
	if (types_match(type, builtin_void)) return 0;
	if (types_match(type, builtin_bool)) return 1;
	if (types_match(type, builtin_u8))   return 2;
	if (types_match(type, builtin_u16))  return 3;
	if (types_match(type, builtin_u32))  return 4;
	if (types_match(type, builtin_u64))  return 5;
	if (types_match(type, builtin_s8))   return 6;
	if (types_match(type, builtin_s16))  return 7;
	if (types_match(type, builtin_s32))  return 8;
	if (types_match(type, builtin_s64))  return 9;
	if (types_match(type, builtin_f32))  return 10;
	if (types_match(type, builtin_f64))  return 11;
	if (direct_as<AstStruct>(type))      return 12;
	if (direct_as<AstEnum>(type))        return 13;
	if (auto unop = direct_as<AstUnaryOperator>(type)) {
		switch (unop->operation) {
			using enum UnaryOperation;
			case pointer: return 14;
		}
	}
	if (direct_as<AstSpan>(type)) return 15;

	invalid_code_path();
}

struct TypeinfoDefinition {
	AstDefinition *definition;
	AstExpression *type;
};

List<TypeinfoDefinition> typeinfo_definitinos;
AstUnaryOperator *get_typeinfo(TypecheckState *state, AstExpression *type, String location) {
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
		definition->name = format(u8"<typeinfo>{}", name);
		definition->type = builtin_typeinfo.ident;
		definition->definition_location = DefinitionLocation::global;

		typeinfo_definitinos.add({definition, directed});

		add_to_scope(definition, &global_scope);


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

			auto member_index = index_of(members, member);
			if (expression == 0)
				expression = make_null(members[member_index]->type, call->location);
			call->sorted_arguments[member_index] = expression;
		};

		auto typeinfo_initializer = create_initializer(builtin_typeinfo);
		set_member(typeinfo_initializer, "kind", make_integer(location, get_type_kind(directed)));
		set_member(typeinfo_initializer, "name", make_string(name));

		if (directed->kind == Ast_Struct) {
			auto Struct = (AstStruct *)directed;

			if (!types_match(Struct, builtin_void) &&
				!types_match(Struct, builtin_bool) &&
				!types_match(Struct, builtin_u8) &&
				!types_match(Struct, builtin_u16) &&
				!types_match(Struct, builtin_u32) &&
				!types_match(Struct, builtin_u64) &&
				!types_match(Struct, builtin_s8) &&
				!types_match(Struct, builtin_s16) &&
				!types_match(Struct, builtin_s32) &&
				!types_match(Struct, builtin_s64) &&
				!types_match(Struct, builtin_f32) &&
				!types_match(Struct, builtin_f64))
			{
				auto member_list = AstPack::create();
				member_list->expressions.resize(Struct->data_members.count);
				for (umm i = 0; i < Struct->data_members.count; ++i) {
					auto &d = member_list->expressions[i];
					auto &s = Struct->data_members[i];

					auto member_initializer = create_initializer(builtin_struct_member);
					set_member(member_initializer, "name", make_string(s->name));
					set_member(member_initializer, "type", get_typeinfo(state, s->type, location));

					d = member_initializer;
				}

				set_member(typeinfo_initializer, "members", member_list);
			} else {
				set_member(typeinfo_initializer, "members", 0);
			}
		} else {
			set_member(typeinfo_initializer, "members", 0);
		}
		set_member(typeinfo_initializer, "pointee", 0);

		definition->expression = typeinfo_initializer;

		evaluate_and_put_definition_in_section(state, definition, context.constant_section);
	}

	auto identifier = AstIdentifier::create();
	identifier->location = location;
	identifier->name = definition->name;
	identifier->possible_definitions.set(definition);
	identifier->type = builtin_typeinfo.ident;

	auto address = AstUnaryOperator::create();
	address->expression = identifier;
	address->location = location;
	address->operation = UnaryOperation::address_of;
	address->type = builtin_typeinfo.pointer;
	address->is_parenthesized = true;

	return address;
}

// TODO: FIXME: this is a mess. need to clean up.
bool implicitly_cast(TypecheckState *state, Reporter *reporter, Expression<> *_expression, AstExpression *type, int *conversion_distance = 0, bool apply = true) {
	auto expression = *_expression;
	defer { *_expression = expression; };

	if (conversion_distance)
		*conversion_distance = 0;

	if (expression->type->kind == Ast_LambdaType) {
		auto lambda_type = (AstLambdaType *)expression->type;
		wait_for(
			state,
			[&] { return lambda_type->lambda->return_parameter; },
			[&] {
				state->reporter.error(lambda_type->lambda->location, "Could not deduce return type.");
				state->reporter.info(expression->location, "While trying to implicitly cast this expression:");
				yield(TypecheckResult::fail);
			}
		);
	}

	if (types_match(expression->type, type)) {
		return true;
	}

	if (conversion_distance)
		*conversion_distance = 1;

	auto earray = as_array(expression->type);
	auto tspan = as_span(type);

	if (earray && tspan) {
		if (types_match(earray->expression, tspan->expression)) {
			if (apply)
				expression = make_cast(expression, type);
			return true;
		}
	}

	if (types_match(expression->type, builtin_unknown_enum)) {
		if (expression->kind == Ast_UnaryOperator) {
			auto unop = (AstUnaryOperator *)expression;
			if (auto dst_enum = direct_as<AstEnum>(type)) {
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

	if (types_match(type, builtin_any)) {
		if (conversion_distance)
			*conversion_distance = 3;

		if (apply) {
			harden_type(expression);

			auto [temporary, initializer] = create_expressions<AstUnaryOperator, AstCall>();

			temporary->expression = expression;
			temporary->location = expression->location;
			temporary->operation = UnaryOperation::internal_move_to_temporary;
			temporary->type = builtin_void.pointer;

			initializer->callable = builtin_any.ident;
			initializer->location = expression->location;
			initializer->type = builtin_any.ident;
			initializer->sorted_arguments.resize(2);
			initializer->sorted_arguments[0] = temporary;
			initializer->sorted_arguments[1] = get_typeinfo(state, expression->type, expression->location);

			expression = initializer;
		}
		return true;
	}

	if (expression->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)expression;
		if (unop->operation == UnaryOperation::autocast) {
			auto autocast = unop;
			CastType request = {direct_as<AstStruct>(expression->type), direct_as<AstStruct>(type)};
			auto found_built_in = find(built_in_casts, request);

			// if (autocast->location == "@0")
			// 	debug_break();

			auto allow_autocast = [&] {
				if (apply) {
					if (autocast->expression->kind == Ast_Literal) {
						auto literal = (AstLiteral *)autocast->expression;
						if (literal->literal_kind == LiteralKind::integer) {
							literal->type = type;
							expression = literal;
							return true;
						}
					}
					expression = make_cast(autocast->expression, type);
				}
				return true;
			};

			if (found_built_in) {
				return allow_autocast();
			}

			if (::is_pointer_internally(expression->type) && is_lambda(type)) {
				// Pointer can be converted to a lambda.
				return allow_autocast();
			}

			auto &src_type = expression->type;
			auto &dst_type = type;
			if (::is_integer(src_type)) {
				if (types_match(src_type, builtin_unsized_integer)) {
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

			if (reporter) {
				reporter->error(expression->location, "Expression of type {} is not convertible to {}.", type_to_string(expression->type), type_to_string(type));
			}
			return false;
		}
	}
	if (expression->type->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)expression->type;
		if (unop->operation == UnaryOperation::option) {
			if (types_match(type, builtin_bool)) {
				if (apply)
					expression = make_cast(expression, builtin_bool.ident);
				return true;
			}
		}
	}
	if (auto option = as_option(type)) {
		if (types_match(expression->type, option->expression)) {
			if (apply)
				expression = make_cast(expression, option);
			return true;
		}
	}
	if (expression->kind == Ast_Literal) {
		auto literal = (AstLiteral *)expression;
		if (literal->literal_kind == LiteralKind::integer) {
			if (auto Enum = direct_as<AstEnum>(literal->type)) {
			} else {
				auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, type); });
				if (found_info) {
					auto &info = *found_info;
					if (!ensure_fits(reporter, expression, literal->integer, info)) {
						return false;
					}

					if (apply)
						expression->type = type;
					return true;
				}

				if (types_match(type, builtin_f32) || types_match(type, builtin_f64)) {
					if (apply)
						expression->type = type;
					return true;
				}
			}
		} else if (literal->literal_kind == LiteralKind::Float) {
			if (types_match(type, builtin_f32) || types_match(type, builtin_f64)) {
				if (apply)
					expression->type = type;
				return true;
			}
		} else if (literal->literal_kind == LiteralKind::noinit) {
			if (apply)
				expression->type = type;
			return true;
		} else if (literal->literal_kind == LiteralKind::null) {
			if (is_pointer(type)) {
				if (apply)
					expression->type = type;
				return true;
			}
		}
	} else if (types_match(type, builtin_void.pointer)) {
		if (is_pointer(expression->type)) {
			return true;
		}
	} else if (types_match(expression->type, builtin_unsized_integer)) {
		auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, type); });
		if (found_info) {
			auto &info = *found_info;
			auto integer = get_constant_integer(expression);

			if (integer) {
				if (!ensure_fits(reporter, expression, integer.value_unchecked(), info)) {
					return false;
				}
			} else {
				if (reporter)
					reporter->warning(expression->location, "INTERNAL COMPILER ERROR: Could not optimize constants; will do at runtime.");
				return false;
			}
			if (apply)
				expression->type = type;
			return true;
		}

		if (types_match(type, builtin_f32) || types_match(type, builtin_f64)) {
			if (apply)
				expression->type = type;
			return true;
		}
	} else if (types_match(expression->type, builtin_unsized_float)) {
		if (types_match(type, builtin_f32) || types_match(type, builtin_f64)) {
			if (apply)
				expression->type = type;
			return true;
		}
	} else if (expression->kind == Ast_Lambda) {
		// Lambda literals can be implicitly casted when calling convention does not match
		// That just means that calling convention can be deduced when assigning (or smth else) a lambda literal.
		// This will not work for named lambdas.

		auto src_lambda = (AstLambda *)expression;
		auto dst_lambda = get_lambda(type);
		if (dst_lambda) {
			if (same_argument_and_return_types(src_lambda, dst_lambda)) {
				src_lambda->convention = dst_lambda->convention;
				return true;
			}
		}
	} else {
		CastType request = {direct_as<AstStruct>(expression->type), direct_as<AstStruct>(type)};
		auto found_built_in = find(built_in_casts, request);

		if (found_built_in && found_built_in->implicit) {
			if (apply)
				expression = make_cast(expression, type);
			return true;
		}
	}

	do {
		for (auto lambda : implicit_casts) {
			if (types_match(lambda->return_parameter->type, type) && types_match(lambda->parameters[0]->type, expression->type)) {
				if (apply) {
					auto call = AstCall::create();
					call->location = expression->location;
					call->callable = lambda;
					call->unsorted_arguments.set({{}, expression});
					call->sorted_arguments.set(expression);
					call->type = type;
					assert(lambda->type->kind == Ast_LambdaType);
					call->lambda_type = (AstLambdaType *)lambda->type;
					expression = call;
				}
				state->no_progress_counter = 0;
				return true;
			}
		}
		if (untypechecked_implicit_casts_count == 0) {
			break;
		}

		++state->no_progress_counter;
		yield(TypecheckResult::wait);
	} while (state->no_progress_counter != NO_PROGRESS_THRESHOLD);

	if (reporter) {
		reporter->error(expression->location, "Expression of type {} is not implicitly convertible to {}.", type_to_string(expression->type), type_to_string(type));
	}
	return false;
}

void wait_iteration(TypecheckState *state, String location, KeyString name) {
	state->no_progress_counter++;
	if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) { /* TODO: This is not the best solution */
		if (state->currently_typechecking_definition && state->currently_typechecking_definition->name == name) {
			state->reporter.error(location, "Can't use object while defining it.");
			state->reporter.info(state->currently_typechecking_definition->location, "Declared here:");
		} else {
			state->reporter.error(location, "Identifier {} was not declared.", name);
			// state->reporter.error(location, "Identifier was not declared or compiler failed to infer it's definition's type.");
		}
		yield(TypecheckResult::fail);
	}
	yield(TypecheckResult::wait);
}

DefinitionList wait_for_definitions(TypecheckState *state, String location, KeyString name) {
	while (1) {
		auto definitions = get_definitions(state, name);
		if (definitions.count) {
			for (auto definition : definitions) {
				while (!definition->type) {
					wait_iteration(state, location, name);
				}
			}
			state->no_progress_counter = 0;
			for (auto definition : definitions)
				assert(name == definition->name);
			return definitions;
		}

		wait_iteration(state, location, name);
	}
}

bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
	timed_function(context.profiler);
	switch (expression->kind) {
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition();
			if (definition->is_constant) {
				reporter->error(identifier->location, "Can't assign to '{}' because it is constant.", identifier->location);
				return false;
			}
			if (definition->definition_location == DefinitionLocation::lambda_parameter) {
				reporter->error(identifier->location, "Can't assign to function parameters.");
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
			assert(subscript->expression->kind == Ast_Identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return ensure_assignable(reporter, identifier);
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

bool ensure_subscriptable(TypecheckState *state, AstExpression *expression) {
	if (expression->type->kind == Ast_Subscript || expression->type->kind == Ast_Span || is_pointer(expression->type))
		return true;
	state->reporter.error(expression->location, "Expression is not subscriptable");
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
	assert(lambda->parameters_size == -1, "redundant work");
	s64 parameter_size_accumulator = 0;

	//if (where(lambda->location.data) == "main.tl:52:25"str)
	//	debug_break();

	if (lambda->is_member)
		parameter_size_accumulator += context.stack_word_size;

	for (auto parameter : lambda->parameters) {
		if (parameter->is_constant)
			continue;

		parameter->offset = parameter_size_accumulator;
		parameter_size_accumulator += context.stack_word_size;
	}
	lambda->parameters_size = parameter_size_accumulator;
}

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
AstExpression     *deep_copy(AstExpression     *);
AstIdentifier     *deep_copy(AstIdentifier     *);
AstCall           *deep_copy(AstCall           *);
AstLiteral        *deep_copy(AstLiteral        *);
AstLambda         *deep_copy(AstLambda         *);
AstBinaryOperator *deep_copy(AstBinaryOperator *);
AstUnaryOperator  *deep_copy(AstUnaryOperator  *);
AstStruct         *deep_copy(AstStruct         *);
AstSubscript      *deep_copy(AstSubscript      *);
AstIfx            *deep_copy(AstIfx            *);
AstTest           *deep_copy(AstTest           *);

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
T *copy_statement(T *expression) {
	auto result = copy_node(expression);
	return result;
}

void add_child(Scope *parent, Scope *child) {
	assert(!child->parent);
	child->parent = parent;
	parent->children.add(child);
}

void deep_copy(Scope *d, Scope *s) {
	assert(count_of(d->definitions) == 0);
	assert(d->statements.count == 0);
	for (auto statement : s->statements) {
		auto copied = deep_copy(statement);
		add_to_scope(copied, d);
		switch (copied->kind) {
			case Ast_Definition: {
				auto definition = (AstDefinition *)copied;
				d->definitions.get_or_insert(definition->name).add(definition);
				break;
			}
			case Ast_If: {
				auto If = (AstIf *)copied;
				add_child(d, &If->true_scope);
				add_child(d, &If->false_scope);
				break;
			}
			case Ast_While: {
				auto While = (AstWhile *)copied;
				add_child(d, &While->scope);
				break;
			}
			case Ast_Defer: {
				auto Defer = (AstDefer *)copied;
				add_child(d, &Defer->scope);
				break;
			}
			case Ast_Block: {
				auto Block = (AstBlock *)copied;
				add_child(d, &Block->scope);
				break;
			}
			case Ast_Print:
			case Ast_Return:
			case Ast_ExpressionStatement:
				break;
			default: invalid_code_path();
		}
	}
}

AstExpression *deep_copy(AstExpression *expression) {
	if (!expression)
		return 0;
	switch (expression->kind) {
		case Ast_Identifier     : return deep_copy((AstIdentifier     *)expression);
		case Ast_Call           : return deep_copy((AstCall           *)expression);
		case Ast_Literal        : return deep_copy((AstLiteral        *)expression);
		case Ast_Lambda         : return deep_copy((AstLambda         *)expression);
		case Ast_BinaryOperator: return deep_copy((AstBinaryOperator *)expression);
		case Ast_UnaryOperator : return deep_copy((AstUnaryOperator  *)expression);
		case Ast_Struct         : return deep_copy((AstStruct         *)expression);
		case Ast_Subscript      : return deep_copy((AstSubscript      *)expression);
		case Ast_Ifx            : return deep_copy((AstIfx            *)expression);
		case Ast_Test           : return deep_copy((AstTest           *)expression);
		default: invalid_code_path();
	}
}
AstStatement *deep_copy(AstStatement *statement) {
	assert(statement);
	switch (statement->kind) {
		case Ast_Definition:           return deep_copy((AstDefinition          *)statement);
		case Ast_Defer:                return deep_copy((AstDefer               *)statement);
		case Ast_Assert:               return deep_copy((AstAssert              *)statement);
		case Ast_Block:                return deep_copy((AstBlock               *)statement);
		case Ast_ExpressionStatement: return deep_copy((AstExpressionStatement *)statement);
		case Ast_If:                   return deep_copy((AstIf                  *)statement);
		case Ast_OperatorDefinition:  return deep_copy((AstOperatorDefinition  *)statement);
		case Ast_Parse:                return deep_copy((AstParse               *)statement);
		case Ast_Print:                return deep_copy((AstPrint               *)statement);
		case Ast_Return:               return deep_copy((AstReturn              *)statement);
		case Ast_While:                return deep_copy((AstWhile               *)statement);
		default: invalid_code_path();
	}
}

AstDefinition *deep_copy(AstDefinition *s) {
	auto d = copy_statement(s);
	d->name = s->name;
	d->expression = deep_copy(s->expression);
	d->built_in = s->built_in;
	d->evaluated = deep_copy(s->evaluated);
	d->is_constant = s->is_constant;
	d->definition_location = s->definition_location;
	d->offset = s->offset;
	d->type = deep_copy(s->type);
	return d;
}
AstDefer *deep_copy(AstDefer *s) {
	auto d = copy_statement(s);
	deep_copy(&d->scope, &s->scope);
	return d;
}
AstAssert *deep_copy(AstAssert *s) {
	auto d = copy_statement(s);
	d->condition = deep_copy(s->condition);
	return d;
}
AstBlock *deep_copy(AstBlock *s) {
	auto d = copy_statement(s);
	deep_copy(&d->scope, &s->scope);
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
	deep_copy(&d->true_scope, &s->true_scope);
	deep_copy(&d->false_scope, &s->false_scope);
	return d;
}
AstOperatorDefinition *deep_copy(AstOperatorDefinition *s) {
	auto d = copy_statement(s);
	d->is_implicit = s->is_implicit;
	d->operation = s->operation;
	d->lambda = deep_copy(s->lambda);
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
	deep_copy(&d->scope, &s->scope);
	return d;
}
AstIdentifier *deep_copy(AstIdentifier *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->name = s->name;
	// FIXME: definition is null
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
		case string:	d->string.set(s->string.get());
		case lambda_name: break;
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
AstStruct *deep_copy(AstStruct *s) { not_implemented(); }
AstSubscript *deep_copy(AstSubscript *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->expression       = deep_copy(s->expression      );
	d->index_expression = deep_copy(s->index_expression);
	d->is_prefix = s->is_prefix;
	return d;
}
AstIfx *deep_copy(AstIfx *s) {
	if (!s) return 0;
	auto d = copy_expression(s);
	d->condition        = deep_copy(s->condition       );
	d->true_expression  = deep_copy(s->true_expression );
	d->false_expression = deep_copy(s->false_expression);
	return d;
}
AstTest *deep_copy(AstTest *s) {
	auto d = copy_expression(s);
	deep_copy(&d->scope, &s->scope);
	return d;
}

bool evaluate_if_possible(TypecheckState *state, AstDefinition *definition) {
	if (definition->is_constant && !is_type(definition->expression) && !is_lambda(definition->expression)) {
		evaluate(state, definition);
		return true;
	}
	return false;
}

void typecheck(TypecheckState *state, AstStatement *statement);
void typecheck(TypecheckState *state, AstDefinition *definition);

void typecheck(TypecheckState *state, AstDefinition *definition) {
	if (definition->built_in) {
		return;
	}

	scoped_replace(state->currently_typechecking_definition, definition);

	auto lambda = state->current_lambda;

	bool is_parameter = definition->definition_location == DefinitionLocation::lambda_parameter;
	enum {
		Global,
		Lambda,
		Struct,
		Enum,
	} location;

	if (!definition->parent_lambda_or_struct) {
		definition->parent_lambda_or_struct = state->current_lambda_or_struct_or_enum;
	}

	if (definition->parent_lambda_or_struct && definition->parent_lambda_or_struct->kind == Ast_Lambda && definition->definition_location == DefinitionLocation::unknown)
		definition->definition_location = DefinitionLocation::lambda_body;

	if (state->current_lambda_or_struct_or_enum) {
		switch (state->current_lambda_or_struct_or_enum->kind) {
			case Ast_Struct: location = Struct; break;
			case Ast_Lambda: location = Lambda; break;
			case Ast_Enum:   location = Enum; break;
			default: invalid_code_path();
		}
	} else {
		location = Global;
	}

	if (is_parameter) {
		assert(definition->type);
		assert(!definition->expression, "default parameters are not supported yet");
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

	if (definition->type) {

		// Lambda and struct set definition's type earlier to allow self-referencing, so don't typecheck it
		if (!(definition->expression && definition->expression->kind == Ast_Lambda)) {

			typecheck(state, definition->type);

			if (definition->expression) {
				if (!implicitly_cast(state, &state->reporter, &definition->expression, definition->type)) {
					yield(TypecheckResult::fail);
				}
			}
		}
	} else {
		if (definition->expression->kind == Ast_UnaryOperator && ((AstUnaryOperator *)definition->expression)->operation == UnaryOperation::autocast) {
			state->reporter.error(definition->location, "Can't deduce a type from autocast. You need to explicitly specify the resulting type");
			yield(TypecheckResult::fail);
		}

		if (!definition->is_constant) {
			harden_type(definition->expression);
		}
		definition->type = definition->expression->type;
	}


	if (definition->is_constant) {
		if (definition->expression) {
			if (!is_constant(definition->expression)) {
				state->reporter.error(definition->location, "Definition marked as constant, but assigned expression is not constant");
				yield(TypecheckResult::fail);
			}
		}
	}

	if (definition->type) {
		if (definition->type->kind != Ast_Import) {
			if (!definition->type->type) {
				invalid_code_path();
				state->reporter.error(definition->type->location, "This is not a type");
				yield(TypecheckResult::fail);
			}
			if (!is_type(definition->type)) {
				state->reporter.error(definition->type->location, "This is not a type");
				// definition->type = type_unknown;
				yield(TypecheckResult::fail);
			}
			if (!is_type(definition->type) && get_size(definition->type) == 0) {
				state->reporter.error(definition->location.data ? definition->location : definition->type->location, "Defining a variable with 0 size is not allowed");
				yield(TypecheckResult::fail);
			}
		}
	} else {
		if (!definition->is_constant) {
			state->reporter.error(definition->location, "Can't assign an overload set to a non constant definition.");
			yield(TypecheckResult::fail);
		}
	}

	if (definition->is_constant) {
		if (state->current_lambda_or_struct_or_enum == 0 || state->current_lambda_or_struct_or_enum->kind != Ast_Enum) {
			if (!is_type(definition->expression) && definition->expression->kind != Ast_Lambda) {
				evaluate_and_put_definition_in_section(state, definition, context.constant_section);
			}
		}
	} else {
		switch (location) {
			case Global: {
				if (definition->expression) {
					if (!is_type(definition->expression) && definition->expression->kind != Ast_Lambda) {
						evaluate_and_put_definition_in_section(state, definition, definition->is_constant ? context.constant_section : context.data_section);
					}
				} else {
					auto offset = context.zero_section_size;

					offset = ceil(offset, get_align(definition->type));
					definition->offset = offset;
					offset += get_size(definition->type);

					context.zero_section_size = offset;
				}
				break;
			}
			case Lambda: {
				state->current_lambda->stack_cursor -= ceil(get_size(definition->type), 16ll);
				assert((state->current_lambda->stack_cursor & 0xf) == 0);
				definition->offset = state->current_lambda->stack_cursor;
				break;
			}
		}
	}
}
void typecheck(TypecheckState *state, AstStatement *statement) {
	auto _statement = statement;

	switch (statement->kind) {
		case Ast_Return: {
			auto ret = (AstReturn *)statement;
			if (!ret->lambda) {
				assert(state->current_lambda_or_struct_or_enum->kind == Ast_Lambda);
				ret->lambda = (AstLambda *)state->current_lambda_or_struct_or_enum;
				assert(ret->lambda->original_poly);
			}
			auto lambda = ret->lambda;

			auto &expression = ret->expression;
			if (expression) {
				typecheck(state, expression);

				if (lambda->return_parameter) {
					if (!implicitly_cast(state, &state->reporter, &expression, lambda->return_parameter->type))
						yield(TypecheckResult::fail);
				}
			} else {
				if (lambda->return_parameter) {
					if (!types_match(lambda->return_parameter->type, builtin_void)) {
						state->reporter.warning(ret->location, "No return expression provided. If you did't assign a value to the return parameter, this will return the default value");
					}
				}
			}
			state->current_lambda->return_statements.add(ret);
			break;
		}
		case Ast_Definition: typecheck(state, (AstDefinition *)statement); break;
		case Ast_If: {
			auto If = (AstIf *)statement;

			typecheck(state, If->condition);
			if (!implicitly_cast(state, &state->reporter, &If->condition, builtin_bool.ident)) {
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
				Scope *scope = condition->Bool ? &If->true_scope : &If->false_scope;


				// NOTE: Instead of this:
				//
				//     typecheck_scope(scope);
				//
				// We use this:
				//
				for (auto statement : scope->statements) {
					typecheck(state, statement);
				}
				//
				// Because constant if's true and false scopes are not regular.
				// We want all statements to be outside these scopes as if they were directly
				// in the parent scope.
				// `typecheck_scope` sets the scope as the parent, and this is unwanted in this case.
				//
				// Note that right now this statement remains to be in the parent's statement list.
				// I think a better solution to this problem would be to add all selected statements into parent scope and remove the if.
				//

				auto found = find(scope->parent->children, scope);
				assert(found);
				erase(scope->parent->children, found);
				scope->parent->append(*scope);
			} else {
				typecheck_scope(&If->true_scope);
				typecheck_scope(&If->false_scope);
			}

			break;
		}
		case Ast_While: {
			auto While = (AstWhile *)statement;

			typecheck(state, While->condition);
			if (!implicitly_cast(state, &state->reporter, &While->condition, builtin_bool.ident)) {
				yield(TypecheckResult::fail);
			}

			typecheck_scope(&While->scope);

			break;
		}
		case Ast_ExpressionStatement: {
			auto es = (AstExpressionStatement *)statement;
			typecheck(state, es->expression);
			break;
		}
		case Ast_Block: {
			auto block = (AstBlock *)statement;
			typecheck_scope(&block->scope);
			break;
		}
		case Ast_Assert: {
			auto assert = (AstAssert *)statement;
			typecheck(state, assert->condition);

			if (assert->is_constant) {
				auto result = evaluate(state, assert->condition);
				if (!result) {
					yield(TypecheckResult::fail);
				}

				if (result->literal_kind != LiteralKind::boolean) {
					state->reporter.error(assert->condition->location, "Expression must have bool type");
					yield(TypecheckResult::fail);
				}

				if (result->Bool == false) {
					state->reporter.error(assert->condition->location, "Assertion failed");
					yield(TypecheckResult::fail);
				}
			}

			break;
		}
		case Ast_Defer: {
			auto Defer = (AstDefer *)statement;

			typecheck_scope(&Defer->scope);

			break;
		}
		case Ast_Print: {
			auto print = (AstPrint *)statement;
			typecheck(state, print->expression);

			auto result = evaluate(state, print->expression);
			if (!result) {
				yield(TypecheckResult::fail);
			}

			switch (result->literal_kind) {
				case LiteralKind::type: {
					state->reporter.info(print->expression->location, "{}", type_to_string(result->type_value));
					break;
				}
				case LiteralKind::string: {
					state->reporter.info(print->expression->location, "{}", result->string.get());
					break;
				}
				case LiteralKind::integer: {
					state->reporter.info(print->expression->location, "{}", result->integer);
					break;
				}
				case LiteralKind::Float: {
					state->reporter.info(print->expression->location, "{}", result->Float);
					break;
				}
				case LiteralKind::character: {
					state->reporter.info(print->expression->location, "{}", result->character);
					break;
				}
				default:
					not_implemented();
			}
			break;
		}
		case Ast_OperatorDefinition: {
			auto op = (AstOperatorDefinition *)statement;
			switch (op->operation) {
				using enum BinaryOperation;
				case as: {
					auto old_lambda = op->lambda;
					typecheck(state, (Expression<> &)op->lambda);
					assert(op->lambda == old_lambda);

					if (op->is_implicit) {
						implicit_casts.add(op->lambda);
						untypechecked_implicit_casts_count -= 1;
					} else {
						explicit_casts.add(op->lambda);
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
				{
					typecheck(state, (Expression<> &)op->lambda);
					binary_operators.get_or_insert(op->operation).add(op->lambda);
					break;
				}
				default: invalid_code_path();
			}
			break;
		}
		case Ast_Parse: {
			break;
		}
		default: {
			invalid_code_path("invalid statement kind in typecheck");
		}
	}
}

// Array<Array<Array<bool, built_in_struct_count>, built_in_struct_count>, (u32)BinaryOperation::count> builtin_binary_operator_table;


bool signedness_matches(AstStruct *a, AstStruct *b) {
	assert(::is_integer(a));
	assert(::is_integer(b));
	if (types_match(a, builtin_unsized_integer) ||
		types_match(b, builtin_unsized_integer))
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
	result->type = make_pointer_type(expression->type);
	return result;
}

AstLambda *find_user_defined_binary_operator(TypecheckState *state, BinaryOperation operation, AstExpression *left_type, AstExpression *right_type) {
	while (state->no_progress_counter != NO_PROGRESS_THRESHOLD) {
		auto found = binary_operators.find(operation);
		if (found) {
			state->no_progress_counter = 0;
			auto match = find_if(found->value, [&](auto lambda) {
				return types_match(lambda->parameters[0]->type, left_type)
					&& types_match(lambda->parameters[1]->type, right_type);
			});
			return match ? *match : 0;
		}
		++state->no_progress_counter;
		yield(TypecheckResult::wait);
	}
	return 0;
}

void typecheck(TypecheckState *, Expression<> &);
AstExpression *typecheck(TypecheckState *, AstIdentifier     *);
AstExpression *typecheck(TypecheckState *, AstCall           *);
AstExpression *typecheck(TypecheckState *, AstLiteral        *);
AstExpression *typecheck(TypecheckState *, AstLambda         *);
AstExpression *typecheck(TypecheckState *, AstLambdaType     *);
AstExpression *typecheck(TypecheckState *, AstBinaryOperator *);
AstExpression *typecheck(TypecheckState *, AstUnaryOperator  *);
AstExpression *typecheck(TypecheckState *, AstStruct         *);
AstExpression *typecheck(TypecheckState *, AstSubscript      *);
AstExpression *typecheck(TypecheckState *, AstSpan           *);
AstExpression *typecheck(TypecheckState *, AstIfx            *);
AstExpression *typecheck(TypecheckState *, AstEnum           *);

void typecheck_body(TypecheckState *state, AstLambda *lambda) {
	bool deferred_function_name = true;

	if (lambda->has_body) {
		if (lambda->return_parameter) {
			deferred_function_name = false;
			lambda->type_name = type_to_string(lambda->type);
		} else {
			lambda->type_name = "undefined"str;
		}
		typecheck_scope(&lambda->body_scope);
	}

	if (!lambda->return_parameter) {
		for (auto ret : lambda->return_statements) {
			if (ret->expression) {
				harden_type(ret->expression);
				lambda->return_parameter = make_retparam(ret->expression->type, lambda);
				lambda->return_statement_type_deduced_from = ret;
				break;
			}
		}
		if (!lambda->return_parameter) {
			lambda->return_parameter = make_retparam(builtin_void.ident, lambda);
		}
		ensure_return_types_match(state, lambda);
	}

	if (lambda->has_body) {
		if (lambda->return_parameter->name.is_empty()) {
			if (!do_all_paths_explicitly_return(lambda)) {
				state->reporter.warning(lambda->location, "Not all execution paths explicitly return a value.");
			}
		}
	}

	if (lambda->has_body) {
		context.lambdas_with_body.add(lambda);
	} else {
		context.lambdas_without_body.add(lambda);
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

// Determines if the types are matching and returns an expression that matched the $T.
// For example, match_poly_type([]$T, []int) returns int.
// If there is no match, returns null.
AstExpression *match_poly_type(AstExpression *poly_type, AstExpression *expr_type) {
	if (types_match(poly_type, builtin_poly))
		return expr_type;
	switch (poly_type->kind) {
		case Ast_UnaryOperator: {
			using enum UnaryOperation;
			auto p = (AstUnaryOperator *)poly_type;
			switch (p->operation) {
				case poly: {
					return expr_type;
				}
			}


			assert(poly_type->kind == expr_type->kind);
			auto e = (AstUnaryOperator *)expr_type;

			switch (p->operation) {
				default: {
					assert(p->operation == e->operation);
					return match_poly_type(p, e);
				}
			}
			return 0;
		}
		case Ast_Span: {
			auto p = (AstSpan *)poly_type;
			switch (expr_type->kind) {
				case Ast_Span:      return match_poly_type(p->expression, ((AstSpan *)expr_type)->expression);
				case Ast_Subscript: return match_poly_type(p->expression, ((AstSubscript *)expr_type)->expression);
			}
			return 0;
		}
	}
	return 0;
}

struct StringizePolyTypes {
	AstLambda *lambda;
};

umm append(StringBuilder &builder, StringizePolyTypes s) {
	// NOTE: statements are ordered, but definitions are not.
	umm result = 0;
	for (auto statement : s.lambda->type_scope.statements) {
		assert(statement->kind == Ast_Definition);
		auto definition = (AstDefinition *)statement;
		result += append_format(builder, "\n                                {} = {}", definition->name, definition->expression->location);
	}
	return result;
}

struct PackArgumentInfo {
	AstExpression *expression = 0; // single if set, otherwise pack
	SmallList<AstExpression *> expressions;
	AstExpression *matched_poly_type;
};

struct Resolution {
	Reporter reporter;
	bool success = false;
	AstDefinition *definition = 0;
	AstLambda *lambda = 0;
	AstLambda *instantiated_lambda = 0;
	int distance = 0;
	SmallList<PackArgumentInfo> packs;
	SmallList<AstExpression *> sorted_arguments;
	AstStruct *Struct = 0;
};

// Right now these copy everything.
// Maybe copy only nodes that depend on a poly parameter?
AstLambda *instantiate_head(TypecheckState *state, Reporter *reporter, SmallList<AstExpression *> arguments, Resolution &resolution, AstCall *call, AstLambda *lambda, bool *success) {
	*success = false;
	for (u32 i = 0; i < arguments.count; ++i) {
		harden_type(arguments[i]);
	}

	if (!lambda->has_pack) {
		// TODO: FIXME: implement for packs
		for (auto resolved : lambda->hardened_polys) {
			bool all_types_match = true;
			for (umm i = 0; i < arguments.count; ++i) {
				if (!types_match(arguments[i]->type, resolved.call->unsorted_arguments[i].expression->type)) {
					all_types_match = false;
					break;
				}
				if (resolved.lambda->parameters[i]->is_constant) {
					auto evaluated_arg = evaluate(state, arguments[i]);
					if (!evaluated_arg) {
						all_types_match = false;
						break;
					}

					auto evaluated_param = evaluate(state, resolved.lambda->parameters[i]->expression);
					if (!evaluated_param) {
						all_types_match = false;
						break;
					}

					if (evaluated_arg->literal_kind != evaluated_param->literal_kind) {
						all_types_match = false;
						break;
					}

					switch (evaluated_arg->literal_kind) {
						case LiteralKind::boolean:   all_types_match &= evaluated_arg->Bool         == evaluated_param->Bool; break;
						case LiteralKind::character: all_types_match &= evaluated_arg->character    == evaluated_param->character; break;
						case LiteralKind::Float:     all_types_match &= evaluated_arg->Float        == evaluated_param->Float; break;
						case LiteralKind::integer:   all_types_match &= evaluated_arg->integer      == evaluated_param->integer; break;
						case LiteralKind::string:    all_types_match &= evaluated_arg->string.get() == evaluated_param->string.get(); break;
						case LiteralKind::type:      all_types_match &= types_match(evaluated_arg->type_value, evaluated_param->type_value); break;
					}

					if (!all_types_match)
						break;
				}
			}
			if (all_types_match) {
				*success = true;
				return resolved.lambda;
			}
		}
	}

	// If we're here - we could'n find an exising instantiation, so make a new one.

	state->poly_call_stack.add(call);
	defer { state->poly_call_stack.pop(); };

	// if (!lambda->definition) {
	// 	state->reporter.error(lambda->location, "Poly lambda must have a name.");
	// 	yield(TypecheckResult::fail);
	// }

	auto hardened_lambda = AstLambda::create();
	{
		assert(lambda->definition); // TODO: deal with unnamed poly lambdas
		auto definition = AstDefinition::create();
		definition->expression = hardened_lambda;
		definition->location = hardened_lambda->location;
		definition->name = lambda->definition->name;
		definition->is_constant = true;
		definition->parent_lambda_or_struct = lambda->definition->parent_lambda_or_struct;
		definition->parent_scope = lambda->definition->parent_scope;
		hardened_lambda->definition = definition;
		lambda->hardened_polys.add({.lambda=hardened_lambda, .definition=definition, .call=call});
	}

	// hardened_lambda->is_poly = false;
	hardened_lambda->convention = lambda->convention;
	hardened_lambda->extern_language = lambda->extern_language;
	hardened_lambda->extern_library = lambda->extern_library;
	hardened_lambda->has_body = false;
	hardened_lambda->is_intrinsic = lambda->is_intrinsic;
	hardened_lambda->is_parenthesized = lambda->is_parenthesized;
	hardened_lambda->is_type = lambda->is_type;
	hardened_lambda->location = lambda->location;
	hardened_lambda->extern_library = lambda->extern_library;
	hardened_lambda->parent_lambda = lambda->parent_lambda;
	hardened_lambda->type = create_lambda_type(hardened_lambda);
	hardened_lambda->type->type = builtin_type.ident;
	hardened_lambda->outer_scope()->parent = lambda->outer_scope()->parent;

	if (lambda->has_pack) {
		umm argument_index = 0;
		umm parameter_index = 0;

		resolution.packs.resize(lambda->parameters.count);

		List<AstDefinition *> hardened_params;

		while (1) {
			if (argument_index == arguments.count) {
				while (1) {
					if (parameter_index == lambda->parameters.count) {
						goto break_outer;
					}
					if (lambda->parameters[parameter_index]->is_pack) {
						++parameter_index;
						continue;
					}
				}
			}
			auto &argument = arguments[argument_index];
			auto &parameter = lambda->parameters[parameter_index];

			int distance = 0;
			if (parameter->is_poly) {
				if (parameter->is_pack) {
					assert(parameter->type->kind == Ast_Span);
					auto matched_type = match_poly_type(((AstSpan *)parameter->type)->expression, argument->type);
					if (matched_type) {
						if (!resolution.packs[parameter_index].matched_poly_type) {
							resolution.packs[parameter_index].matched_poly_type = matched_type;

							// COPYPASTA
							if (parameter->poly_ident) {
								auto ident = parameter->poly_ident;
								auto type_def = AstDefinition::create();
								type_def->name = ident->name;
								type_def->expression = matched_type;
								type_def->type = builtin_type.ident;
								type_def->parent_lambda_or_struct = hardened_lambda;
								type_def->location = ident->location;
								add_to_scope(type_def, &hardened_lambda->type_scope);
								// TODO: do at parse time
								if (hardened_lambda->type_scope.definitions.get_or_insert(type_def->name).count != 1) {
									state->reporter.error(ident->location, "Redefinition of polymorpic argument");
									yield(TypecheckResult::fail);
								}
							}
							auto hardened_param = AstDefinition::create();
							hardened_param->location = parameter->location;
							hardened_param->name = parameter->name;
							auto span = AstSpan::create();
							span->expression = argument->type;
							span->type = builtin_type.ident;
							hardened_param->type = span;
							hardened_param->definition_location = DefinitionLocation::lambda_parameter;
							hardened_param->is_pack = parameter->is_pack;
							hardened_param->is_constant = parameter->is_constant;
							hardened_param->parent_lambda_or_struct = hardened_lambda;

							hardened_params.add(hardened_param);
							hardened_lambda->parameters.add(hardened_param);
							add_to_scope(hardened_param, &hardened_lambda->parameter_scope);
							if (parameter->is_constant) {
								hardened_param->expression = argument;
							}
						}
						resolution.packs[parameter_index].expressions.add(argument);

						++argument_index;
					} else {
						++parameter_index;
					}
				} else {
					// COPYPASTA
					// insert the parameter's type when it's in $name form.
					if (parameter->poly_ident) {
						auto ident = parameter->poly_ident;
						auto type_def = AstDefinition::create();
						type_def->location = ident->location;
						type_def->name = ident->name;
						type_def->expression = match_poly_type(parameter->type, argument->type);
						if (!type_def->expression) {
							reporter->error(argument->location, "Could not match the type {} to {}", type_to_string(argument->type), type_to_string(parameter->type));
							reporter->info(parameter->location, "Declared here:");
							return hardened_lambda;
						}
						type_def->type = builtin_type.ident;
						type_def->parent_lambda_or_struct = hardened_lambda;
						add_to_scope(type_def, &hardened_lambda->type_scope);
						// TODO: do at parse time
						if (hardened_lambda->type_scope.definitions.get_or_insert(type_def->name).count != 1) {
							state->reporter.error(ident->location, "Redefinition of polymorpic argument");
							yield(TypecheckResult::fail);
						}
					}
					// insert the parameter
					auto hardened_param = AstDefinition::create();
					hardened_param->location = parameter->location;
					hardened_param->name = parameter->name;
					hardened_param->type = argument->type;
					hardened_param->definition_location = DefinitionLocation::lambda_parameter;
					hardened_param->is_pack = parameter->is_pack;
					hardened_param->is_constant = parameter->is_constant;
					hardened_param->parent_lambda_or_struct = hardened_lambda;
					hardened_params.add(hardened_param);
					hardened_lambda->parameters.add(hardened_param);
					add_to_scope(hardened_param, &hardened_lambda->parameter_scope);
					if (parameter->is_constant) {
						hardened_param->expression = argument;
					}

					resolution.packs[parameter_index].expression = argument;

					++argument_index;
					++parameter_index;
				}
			} else {
				if (parameter->is_pack) {
					assert(parameter->type->kind == Ast_Span);
					if (implicitly_cast(state, &resolution.reporter, &argument, ((AstSpan *)parameter->type)->expression, &distance, false)) {
						resolution.packs[parameter_index].expressions.add(argument);

						++argument_index;
						// total_distance += distance;
					} else {
						++parameter_index;
					}
				} else {
					if (!implicitly_cast(state, &resolution.reporter, &argument, parameter->type, &distance, false)) {
						return hardened_lambda;
					}
					// total_distance += distance;

					resolution.packs[parameter_index].expression = argument;

					++argument_index;
					++parameter_index;
				}
			}
		}
	break_outer:;
		if (hardened_lambda->parameters.count != lambda->parameters.count) {
			reporter->error(call->location, "Could not determine pack type.");
			return hardened_lambda;
		}
	} else {
		List<AstDefinition *> hardened_params;
		hardened_params.allocator = temporary_allocator;
		hardened_params.resize(arguments.count);

		for (u32 i = 0; i < arguments.count; ++i) {
			auto &arg = arguments[i];
			auto &param = lambda->parameters[i];
			auto &hardened_param = hardened_params[i];

			if (param->is_poly) {
				// insert the parameter's type when it's in $name form.
				if (param->poly_ident) {
					auto ident = param->poly_ident;
					auto type_def = AstDefinition::create();
					type_def->location = ident->location;
					type_def->name = ident->name;
					type_def->expression = match_poly_type(param->type, arg->type);
					if (!type_def->expression) {
						reporter->error(arg->location, "Could not match the type {} to {}", type_to_string(arg->type), type_to_string(param->type));
						reporter->info(param->location, "Declared here:");
						return hardened_lambda;
					}
					type_def->type = builtin_type.ident;
					type_def->parent_lambda_or_struct = hardened_lambda;
					add_to_scope(type_def, &hardened_lambda->type_scope);
					// TODO: do at parse time
					if (hardened_lambda->type_scope.definitions.get_or_insert(type_def->name).count != 1) {
						state->reporter.error(ident->location, "Redefinition of polymorpic argument");
						yield(TypecheckResult::fail);
					}
				}
				// insert the parameter
				{
					hardened_param = AstDefinition::create();
					hardened_param->location = param->location;
					hardened_param->name = param->name;
					hardened_param->type = arg->type;
					hardened_param->definition_location = DefinitionLocation::lambda_parameter;
					hardened_param->is_constant = param->is_constant;
					hardened_param->is_pack = param->is_pack;
					hardened_param->parent_lambda_or_struct = hardened_lambda;

					hardened_lambda->parameters.add(hardened_param);
					add_to_scope(hardened_param, &hardened_lambda->parameter_scope);
				}
				if (param->is_constant) {
					hardened_param->expression = arg;
				}
			} else {
				hardened_param = deep_copy(param);
				hardened_param->parent_lambda_or_struct = hardened_lambda;
				hardened_lambda->parameters.add(hardened_param);
				add_to_scope(hardened_param, &hardened_lambda->parameter_scope);
			}
		}

		for (u32 i = 0; i < arguments.count; ++i) {
			auto &arg = arguments[i];
			auto &param = lambda->parameters[i];
			auto &hardened_param = hardened_params[i];

			if (!param->is_poly) {
				{
					scoped_replace(state->current_scope, &hardened_lambda->parameter_scope);
					typecheck(state, hardened_param);


					if (hardened_param->type->kind == Ast_Identifier) {
						auto ident = (AstIdentifier *)hardened_param->type;
						auto def = ident->definition();
						assert(def);
						auto found = hardened_lambda->type_scope.definitions.find(def->name);
						if (found) {
							assert(found->value.count == 1);
							hardened_param->type = found->value.data[0]->expression;
						}
					}
				}

				if (!implicitly_cast(state, reporter, &arg, hardened_param->type)) {
					return hardened_lambda;
				}

				if (param->is_constant) {
					hardened_param->expression = arg;
				}
			}

			if (hardened_param->is_constant) {
				if (!is_constant(arg)) {
					reporter->error(arg->location, "Expected a constant argument.");
					reporter->info(hardened_param->location, "Definition marked as constant.");
					return hardened_lambda;
				}
			}
		}
	}

	scoped_replace(state->current_lambda_or_struct_or_enum, hardened_lambda);
	scoped_replace(state->current_lambda, hardened_lambda);
	scoped_replace(state->current_scope, &hardened_lambda->parameter_scope);

	if (context.debug_poly) {
		state->reporter.info(call->location, "Instantiating poly head: {} with {}", type_to_string(lambda->type), StringizePolyTypes{hardened_lambda});
	}

	if (lambda->return_parameter) {
		hardened_lambda->return_parameter = deep_copy(lambda->return_parameter);
		add_to_scope(hardened_lambda->return_parameter, &hardened_lambda->parameter_scope);
		typecheck(state, hardened_lambda->return_parameter);
	}

	*success = true;
	return hardened_lambda;
}
void instantiate_body(TypecheckState *state, AstLambda *original_lambda, AstLambda *hardened_lambda) {
	if (hardened_lambda->original_poly)
		return;
	hardened_lambda->original_poly = original_lambda;
	hardened_lambda->has_body = original_lambda->has_body;

	if (context.debug_poly) {
		state->reporter.info(hardened_lambda->location, "Instantiating poly body: {} with {}", type_to_string(original_lambda->type), StringizePolyTypes{hardened_lambda});
	}

	{
		scoped_replace(state->current_lambda_or_struct_or_enum, hardened_lambda);
		scoped_replace(state->current_lambda, hardened_lambda);
		scoped_replace(state->current_scope, &hardened_lambda->parameter_scope);

		deep_copy(&hardened_lambda->body_scope, &original_lambda->body_scope);
		typecheck_body(state, hardened_lambda);

		calculate_parameters_size(hardened_lambda);
	}

	hardened_lambda->return_parameter->parent_lambda_or_struct = hardened_lambda;
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

AstExpression *typecheck(TypecheckState *state, AstIdentifier *identifier) {
	if (identifier->possible_definitions.count) {
		// immediate_info(identifier->location, "Redundant typecheck.");
		return identifier;
	}

	// if (identifier->name == "void")
	// 	debug_break();

	auto definitions = wait_for_definitions(state, identifier->location, identifier->name);
	for (auto definition : definitions)
		assert(identifier->name == definition->name);

	if (definitions.count == 1) {
		auto definition = definitions[0];
		assert(definition->type);
#if 1
		if (!definition->type->type) {
			// This is probably due to errors while typecheching definition.
			yield(TypecheckResult::fail);
		}
#else
		while (!definition->type->type) {
			++state->no_progress_counter;
			if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) {
				state->reporter.error(definition->type->location, "Failed to resolve this type.");
				state->reporter.info(identifier->location, "While typechecking this identifier.");
				yield(TypecheckResult::fail);
			}
			yield(TypecheckResult::wait);
		}
#endif
		identifier->possible_definitions.set(definition);
		identifier->type = definition->type;

		// if (definition->evaluated) {
		// 	return definition->evaluated;
		// }
	} else {
		identifier->possible_definitions = definitions;
		identifier->type = builtin_overload_set.ident;
	}
	return identifier;
}
AstExpression *typecheck(TypecheckState *state, AstCall *call) {
	//if (call->location == "f(a=2)")
	//	debug_break();

	//if (call->callable->location == "StringBuilder.create")
	//	debug_break();

	typecheck(state, call->callable);

	for (auto &argument : call->unsorted_arguments) {
		typecheck(state, argument.expression);
	}

	AstExpression *This = 0;
	AstIdentifier *identifier = 0;
	if (call->callable->kind == Ast_Identifier) {
		identifier = (AstIdentifier *)call->callable;
	}

//	if (call->callable->location == "new")
//		debug_break();

	if (identifier) {
	resolve_ident:
		int min_distance = max_value<int>;

		List<Resolution> resolutions;
		resolutions.allocator = temporary_allocator;
		resolutions.resize(identifier->possible_definitions.count);

		List<Resolution *> matches;
		matches.allocator = temporary_allocator;

		for (umm definition_index = 0; definition_index != identifier->possible_definitions.count; ++definition_index) {
			auto &resolution = resolutions[definition_index];
			auto &definition = identifier->possible_definitions[definition_index];
			auto &reporter = resolution.reporter;

			AstLambda *lambda = 0;
			AstStruct *Struct = 0;
			{
				if (definition->expression) {
					auto e = direct(definition->expression);
					if (e->kind == Ast_Lambda) {
						lambda = (AstLambda *)e;
					} else if (e->kind == Ast_Struct) {
						Struct = (AstStruct *)e;
					} else {
						reporter.error("This is not a lambda or struct.");
						continue;
					}
				} else {
					lambda = get_lambda(definition->type);
				}
			}
			// either one must be valid, but not both
			if (!lambda && !Struct) {
				reporter.error(definition->location, "This is not a lambda nor a struct");
				continue;
			}

			resolution.definition = definition;

			if (lambda) {
				// TODO: use temporary allocator
				auto successfully_sorted_arguments = sort_arguments(&reporter, call, lambda->parameters);
				if (!successfully_sorted_arguments) {
					continue;
				}

				resolution.sorted_arguments = successfully_sorted_arguments.value_unchecked();
				auto &arguments = resolution.sorted_arguments;


				if (lambda->is_poly) {
					int total_distance = 2;

					bool success;
					resolution.instantiated_lambda = instantiate_head(state, &reporter, arguments, resolution, call, lambda, &success);
					if (!success)
						continue;

					min_distance = min(min_distance, total_distance);

					resolution.success = true;
					resolution.lambda = lambda;
					resolution.distance = total_distance;
					matches.add(&resolution);
				} else {
					//if (lambda->_uid == 1608)
					//	debug_break();

					wait_for(
						state,
						[&] { return lambda->finished_typechecking_head; },
						[&] {
							state->reporter.error(lambda->location, "Lambda's head couldn't finish typechecking");
							state->reporter.info(call->location, "Waited here:");
						}
					);

					//if (lambda->_uid == 1608)
					//	debug_break();

					if (lambda->has_pack) {
						int total_distance = 1;

						umm argument_index = 0;
						umm parameter_index = 0;

						resolution.packs.resize(lambda->parameters.count);

						while (1) {
							if (argument_index == arguments.count) {
								while (1) {
									if (parameter_index == lambda->parameters.count) {
										goto break_outer;
									}
									if (lambda->parameters[parameter_index]->is_pack) {
										++parameter_index;
										continue;
									}
								}
							}
							auto &argument = arguments[argument_index];
							auto &parameter = lambda->parameters[parameter_index];

							int distance = 0;
							if (parameter->is_pack) {
								assert(parameter->type->kind == Ast_Span);
								if (implicitly_cast(state, &reporter, &argument, ((AstSpan *)parameter->type)->expression, &distance, false)) {
									resolution.packs[parameter_index].expressions.add(argument);

									++argument_index;
									total_distance += distance;
								} else {
									++parameter_index;
								}
							} else {
								if (!implicitly_cast(state, &reporter, &argument, parameter->type, &distance, false)) {
									goto continue_resolution;
								}
								total_distance += distance;

								resolution.packs[parameter_index].expression = argument;

								++argument_index;
								++parameter_index;
							}
						}
					break_outer:;
						if (argument_index != arguments.count) {
							reporter.error(call->location, "Not enough arguments.");
							continue;
						}
						if (parameter_index != lambda->parameters.count) {
							reporter.error(call->location, "Too many arguments.");
							continue;
						}

						min_distance = min(min_distance, total_distance);

						resolution.success = true;
						resolution.lambda = lambda;
						resolution.distance = total_distance;
						matches.add(&resolution);
					} else {
						if (arguments.count != lambda->parameters.count) {
							reporter.error("Argument count does not match.");
							continue;
						}

						int total_distance = 0;

						for (u32 i = 0; i < arguments.count; ++i) {
							auto &argument = arguments[i];
							auto &parameter = lambda->parameters[i];
							int distance = 0;

							assert(!parameter->is_poly);
							if (!implicitly_cast(state, &reporter, &argument, parameter->type, &distance, false)) {
								goto continue_resolution;
							}
							if (parameter->is_constant) {
								if (!is_constant(argument)) {
									reporter.error(argument->location, "Expected a constant argument.");
									reporter.info(parameter->location, "Definition marked as constant.");
									goto continue_resolution;
								}
							}
							total_distance += distance;
						}

						min_distance = min(min_distance, total_distance);

						resolution.success = true;
						resolution.lambda = lambda;
						resolution.distance = total_distance;
						matches.add(&resolution);
					}
				}
			}
			if (Struct) {
				// TODO: use temporary allocator

				wait_for(
					state,
					[&] { return Struct->type; },
					[&] {
						state->reporter.error(Struct->location, "Struct couldn't finish typechecking");
						state->reporter.info(call->location, "Waited here:");
					}
				);

				//for (auto argument : call->unsorted_arguments) {
				//	if (!argument.name.is_empty()) {
				//		reporter.error(argument.expression->location, "Only unnamed arguments are supported for structs right now.");
				//		goto continue_resolution;
				//	}
				//}

				if (call->unsorted_arguments.count > Struct->data_members.count) {
					reporter.error(call->location, "Too many arguments.");
					goto continue_resolution;
				}

				SmallList<NamedArgument> named_arguments;
				SmallList<AstExpression *> unnamed_arguments;
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

				SmallList<AstExpression *> arguments;
				arguments.resize(Struct->data_members.count);

				for (auto arg : named_arguments) {
					auto member = find_if(Struct->data_members, [&](auto member){return arg.name == member->name;});
					assert(member);
					arguments[index_of(Struct->data_members, member)] = arg.expression;
				}

				for (auto arg : unnamed_arguments) {
					*find(arguments, (AstExpression *)nullptr) = arg;
				}

				resolution.sorted_arguments = arguments;

				if (arguments.count != Struct->data_members.count) {
					reporter.error(call->location, "Argument count does not match.");
					goto continue_resolution;
				}

				for (umm i = 0; i < arguments.count; ++i) {
					auto &argument = arguments[i];
					if (argument) {
						auto &member = Struct->data_members[i];

						if (!implicitly_cast(state, &reporter, &argument, member->type, 0, false)) {
							goto continue_resolution;
						}
					}
				}

				resolution.success = true;
				resolution.Struct = Struct;
				matches.add(&resolution);
			}
		continue_resolution:;
		}
		Resolution *match = 0;
		if (matches.count == 0) {
			if (identifier->possible_definitions.count) {
				state->reporter.error(call->location, "No match was found for {}", call->callable->location);
				if (identifier->possible_definitions.count > 1) {
					//state->reporter.info("Here is the list of possible overloads:");
					//for (auto definition : identifier->possible_definitions) {
					//	defer { ++overload_index; };
					//	state->reporter.info(definition->location, "Overload #{}:", overload_index);
					//}
					//state->reporter.info("Here are the reasons of failed overload resolutions:");
					//overload_index = 0;
					for (umm definition_index = 0; definition_index != identifier->possible_definitions.count; ++definition_index) {
						auto &resolution = resolutions[definition_index];
						auto &definition = resolution.definition;
						auto &reporter = resolution.reporter;
						if (resolution.instantiated_lambda) {
							state->reporter.info(definition ? definition->location : call->callable->location, "Match #{} with {}", definition_index, StringizePolyTypes{resolution.instantiated_lambda});
						} else {
							state->reporter.info(definition ? definition->location : call->callable->location, "Match #{}:", definition_index);
						}
						state->reporter.reports.add(reporter.reports);
					}
				} else {
					auto &resolution = resolutions[0];
					auto &definition = resolution.definition;
					auto &reporter = resolution.reporter;
					state->reporter.reports.add(reporter.reports);
					if (resolution.instantiated_lambda) {
						state->reporter.info(definition ? definition->location : call->callable->location, "Couldn't match this with{}", StringizePolyTypes{resolution.instantiated_lambda});
					} else {
						state->reporter.info(definition ? definition->location : call->callable->location, "Couldn't match this:");
					}
				}
			} else {
				state->reporter.error(call->location, "Lambda or struct with that name was not defined");
			}
			yield(TypecheckResult::fail);
		} else if (matches.count == 1) {
			match = matches[0];
		} else {
			List<Resolution *> best_matches;
			for (auto resolution : matches) {
				if (resolution->distance == min_distance)
					best_matches.add(resolution);
			}

			if (best_matches.count == 1) {
				match = best_matches[0];
			} else {
				state->reporter.error(call->location, "Ambiguous matches were found");
				for (auto resolution : matches) {
					state->reporter.info(resolution->definition->location, "Here");
				}
				yield(TypecheckResult::fail);
			}
		}
		call->sorted_arguments = match->sorted_arguments;

		identifier->possible_definitions.set(match->definition);
		identifier->type = call->callable->type = match->definition->type;

		if (match->lambda) {
			if (match->lambda->is_poly) {
				instantiate_body(state, match->lambda, match->instantiated_lambda);
				assert(match->instantiated_lambda->definition); // TODO: deal with unnamed poly lambdas

				match->lambda = match->instantiated_lambda;

				auto ident = AstIdentifier::create();
				ident->name = match->instantiated_lambda->definition->name;
				ident->location = match->instantiated_lambda->location;
				ident->possible_definitions.set(match->instantiated_lambda->definition);

				call->callable = ident;
			}

			if (match->packs.count) {
				SmallList<AstExpression *> packed_arguments;
				for (umm i = 0; i < match->packs.count; ++i) {
					auto info = match->packs[i];
					auto parameter = match->lambda->parameters[i];
					if (info.expression) {
						packed_arguments.add(info.expression);
					} else {
						assert(parameter->type->kind == Ast_Span);
						auto elem_type = ((AstSpan *)parameter->type)->expression;

						for (auto &elem : info.expressions) {
							if (!implicitly_cast(state, &state->reporter, &elem, elem_type, 0)) {
								state->reporter.error(elem->location, "INTERNAL ERROR: implicit cast did not succeed the second time for pack.");
								yield(TypecheckResult::fail);
							}
						}

						auto pack = AstPack::create();
						pack->expressions = info.expressions;
						if (pack->expressions.count)
							pack->location = {pack->expressions.front()->location.begin(), pack->expressions.back()->location.end()};

						auto type = AstSubscript::create();
						type->expression = elem_type;
						type->index_expression = make_integer({}, (u64)pack->expressions.count);
						type->location = pack->location;
						type->type = builtin_type.ident;
						pack->type = type;
						packed_arguments.add(pack);


					}
				}
				call->sorted_arguments = packed_arguments;
			}

			auto lambda = match->lambda;

			for (u32 i = 0; i < call->sorted_arguments.count; ++i) {
				auto &argument = call->sorted_arguments[i];
				auto &parameter = lambda->parameters[i];

				assert(!parameter->is_poly);
				if (!implicitly_cast(state, &state->reporter, &argument, parameter->type, 0)) {
					state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
					yield(TypecheckResult::fail);
				}
			}

			if (This) {
				auto pointer = make_address_of(&state->reporter ,This);
				if (!pointer)
					yield(TypecheckResult::fail);
				call->sorted_arguments.insert_at(pointer, 0);
			}

			// NOTE: not sure if this wait is necessary
			wait_for(
				state,
				[&] { return lambda->return_parameter; },
				[&] {
					state->reporter.error(lambda->location, "Could not deduce the return type.");
					state->reporter.info(call->location, "Called here:");
					yield(TypecheckResult::fail);
				}
			);

			assert(lambda->return_parameter->parent_lambda_or_struct == lambda);
			if (!lambda->return_parameter) {
				state->reporter.error(call->location, "Lambda's return parameter is undefined. This is probably due to errors while hardening a poly.");
				yield(TypecheckResult::fail);
			}
			call->type = lambda->return_parameter->type;

			assert(lambda->type->kind == Ast_LambdaType);
			call->lambda_type = (AstLambdaType *)lambda->type;
		} else {
			assert(match->Struct);
			call->type = identifier;
			for (umm i = 0; i < call->sorted_arguments.count; ++i) {
				auto &argument = call->sorted_arguments[i];
				auto &member = match->Struct->data_members[i];
				if (argument) {
					if (!implicitly_cast(state, &state->reporter, &argument, member->type, 0)) {
						state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
						yield(TypecheckResult::fail);
					}
				} else {
					argument = make_null(member->type, call->location);
				}
			}
		}
	} else if (call->callable->kind == Ast_BinaryOperator) {
		auto binop = (AstBinaryOperator *)call->callable;
		assert(binop->operation == BinaryOperation::dot);
		assert(binop-> left->kind == Ast_Identifier);
		assert(binop->right->kind == Ast_Identifier);
		identifier = (AstIdentifier *)binop->right;
		if (!is_type(binop->left))
			This = binop->left;
		goto resolve_ident;
	} else {
		state->reporter.error(call->location, "NOT IMPLEMENTED: Right now you can call identifiers only");
		yield(TypecheckResult::fail);
	}

	return call;
}
AstExpression *typecheck(TypecheckState *state, AstLiteral *literal) {
	switch (literal->literal_kind) {
		using enum LiteralKind;
		case integer:
			literal->type = builtin_unsized_integer.ident;
			break;
		case Float:
			literal->type = builtin_unsized_float.ident;
			break;
		case boolean:
			literal->type = builtin_bool.ident;
			break;
		case string:
			literal->type = builtin_string.ident;
			break;
		case character:
			literal->type = builtin_u8.ident;
			break;
		case noinit:
			literal->type = builtin_noinit.ident;
			break;
		case null:
			literal->type = builtin_void.pointer;
			break;
		case lambda_name:
			literal->type = builtin_string.ident;
			literal->literal_kind = LiteralKind::string;
			literal->string.set(state->current_lambda->type_name);
			state->current_lambda->function_directives.add(literal);
			break;
		default:
			not_implemented();
	}
	return literal;
}
AstExpression *typecheck(TypecheckState *state, AstLambda *lambda) {
	lambda->parent_lambda = state->current_lambda;

	scoped_replace(state->current_lambda, lambda);
	scoped_replace(state->current_lambda_or_struct_or_enum, lambda);

	lambda->type = create_lambda_type(lambda);
	lambda->type->type = builtin_type.ident;

	if (lambda->is_poly) {
		return lambda;
	}

	if (lambda->return_parameter) {
		typecheck(state, lambda->return_parameter);
	}

	push_scope(&lambda->parameter_scope);
	for (auto parameter : lambda->parameters) {
		typecheck(state, parameter);
	}
	calculate_parameters_size(lambda);

	// lambda->definition can be null in case the lambda is a type pointer to lambda
	if (lambda->definition) {
		lambda->definition->type = lambda->type;
	}

	lambda->finished_typechecking_head = true;

	typecheck_body(state, lambda);

	return lambda;
}
AstExpression *typecheck(TypecheckState *state, AstLambdaType *lambda_type) {
	typecheck(state, lambda_type->lambda);
	lambda_type->type = builtin_type.ident;
	return lambda_type;
}
AstExpression *typecheck(TypecheckState *state, AstBinaryOperator *bin) {
	using enum BinaryOperation;

	typecheck(state, bin->left);
	/*
	if (!bin->left->type) {
		if (bin->left->kind == Ast_Identifier) {
			auto ident = (AstIdentifier *)bin->left;
			if (!ident->definition()) {
				state->reporter.error(bin->left->location, "Ambiguous name.");
				state->reporter.info("Here are definitions:");
				for (auto possible_definition : ident->possible_definitions) {
					state->reporter.info(possible_definition->location, "Here:");
				}
				yield(TypecheckResult::fail);
			}
		}
		state->reporter.error(bin->left->location, "INTERNAL ERROR: Could not determine the type of this expression.");
		yield(TypecheckResult::fail);
	}
	*/

	auto report_type_mismatch = [&] {
		state->reporter.error(bin->location, "Can't use binary {} on types {} and {}", as_string(bin->operation), type_to_string(bin->left->type), type_to_string(bin->right->type));
		yield(TypecheckResult::fail);
	};

	if (bin->operation == dot) {
		if (bin->left->type->kind == Ast_Import) {
			// not_implemented();
			// auto import = (AstImport *)get_definition_expression(bin->left);
			// assert(import->kind == Ast_Import);
			// import->scope;
		} else {
			if (bin->right->kind == Ast_Identifier) {
				auto member_identifier = (AstIdentifier *)bin->right;
				auto name = member_identifier->name;

				//harden_type(&bin->left->type);

				bool left_is_type = is_type(bin->left);
				AstStruct *Struct = 0;

				if (auto pointer = as_pointer(bin->left->type))
					Struct = direct_as<AstStruct>(pointer->expression);
				else
					Struct = direct_as<AstStruct>(left_is_type ? bin->left : bin->left->type);

				//if (bin->location == "default_allocator.func")
				//	debug_break();

				if (Struct) {
					auto found_member = Struct->scope.definitions.find(name);
					if (!found_member) {
						if (left_is_type) {
							state->reporter.error(bin->right->location, "Type '{}' does not contain constant '{}'", Struct->definition->name, bin->right->location);
						} else {
							state->reporter.error(bin->right->location, "'{}' is not a member of '{}'", bin->right->location, Struct->definition->name);
						}
						yield(TypecheckResult::fail);
					}

					assert(found_member->value.count);

					if (found_member->value.count > 1) {
						member_identifier->possible_definitions.set(found_member->value);
						member_identifier->type = bin->type = builtin_overload_set.ident;
						return bin;
					}

					auto definition = found_member->value.data[0];

					if (left_is_type && !definition->is_constant) {
						state->reporter.error(bin->location, "To access {} you need an instance of type {}, not the type itself.", definition->name, type_to_string(Struct));
						yield(TypecheckResult::fail);
					}

					while (1) {
						if (definition->type)
							break;

						++state->no_progress_counter;
						if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) {
							state->reporter.error(definition->location, "Failed to resolve definition's type");
							state->reporter.error(bin->location, "While typechecking this expression:");
							yield(TypecheckResult::fail);
						}
						yield(TypecheckResult::wait);
					}

					member_identifier->possible_definitions.set(definition);
					member_identifier->type = definition->type;
					bin->type = bin->right->type;
				} else if (is_sized_array(bin->left->type)) {
					if (bin->right->kind != Ast_Identifier) {
						state->reporter.error(bin->left->location, "The only members of any array type are 'data' and 'count'");
						yield(TypecheckResult::fail);
					}

					auto array_type = (AstSubscript *)bin->left->type;
					auto identifier = (AstIdentifier *)bin->right;

					if (identifier->name == "data"str) {
						bin->type = make_pointer_type(array_type->expression);

						auto array_address = make_address_of(&state->reporter, bin->left);
						if (!array_address)
							yield(TypecheckResult::fail);

						return make_cast(array_address, bin->type);
					} else if (identifier->name == "count"str) {
						auto size = evaluate(state, array_type->index_expression);
						if (!size) {
							state->reporter.error(array_type->index_expression->location, "INTERNAL ERROR: failed to evaluate index expression");
							yield(TypecheckResult::fail);
						}
						if (size->literal_kind != LiteralKind::integer) {
							state->reporter.error(array_type->index_expression->location, "INTERNAL ERROR: index expression is not an integer");
							yield(TypecheckResult::fail);
						}
						return make_integer(array_type->index_expression->location, size->integer); // unsized
					} else {
						state->reporter.error(bin->left->location, "The only members of any array type are identifiers 'data' and 'count'. You asked for '{}', which does not exist", identifier->name);
						yield(TypecheckResult::fail);
					}
				} else if (auto span = as_span(bin->left->type)) {
					if (bin->right->kind != Ast_Identifier) {
						state->reporter.error(bin->left->location, "The only members of any span type are 'data' and 'count'");
						yield(TypecheckResult::fail);
					}

					auto identifier = (AstIdentifier *)bin->right;

					// TODO: FIXME: HACK:
					// extremely dumb way to access data and count members of span
					if (identifier->name == "data"str) {
						bin->type = make_pointer_type(span->expression);
						return make_cast(bin->left, builtin_void.pointer);
					} else if (identifier->name == "count"str) {
						bin->type = type_int;
						return make_cast(bin->left, type_int);
					} else {
						state->reporter.error(bin->left->location, "The only members of any array type are identifiers 'data' and 'count'. You asked for '{}', which does not exist", identifier->name);
						yield(TypecheckResult::fail);
					}
				} else if (auto Enum = direct_as<AstEnum>(bin->left)) {
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
	} else if (bin->operation == as) {
		auto cast = bin;
		// typecheck(state, cast->left);
		typecheck(state, cast->right);
		cast->type = cast->right;

		auto &src_type = cast->left->type;
		auto &dst_type = cast->type;

#define breakable_scope for(bool CONCAT(_bs_, __LINE__)=true;CONCAT(_bs_, __LINE__);CONCAT(_bs_, __LINE__)=false)

		breakable_scope {
			if (implicitly_cast(state, 0, &cast->left, cast->right)) {
				return cast->left;
			}

			auto found_built_in = find(built_in_casts, {direct_as<AstStruct>(src_type), direct_as<AstStruct>(dst_type)});
			if (found_built_in) {
				//cast->cast_kind = found_built_in->kind;
				break;
			}

			if (::is_integer(src_type)) {
				if (types_match(src_type, builtin_unsized_integer)) {
					if (::is_pointer(dst_type) || ::is_integer(dst_type)) {
						// Just replace cast with literal
						// expression = cast->left;
						// expression->type = dst_type;
						break;
					}
				} else {
					if (::is_pointer(dst_type)) {
						auto built_in_cast = find_if(built_in_casts, [&](auto c) { return types_match(c.from, src_type); });
						assert(built_in_cast);
						//cast->cast_kind = built_in_cast->kind;
						break;
					}
				}
			} else if (::is_pointer(src_type)) {
				if (::is_pointer(dst_type)) {
					cast->left->type = cast->type;
					return cast->left;
				} else if (::is_integer(dst_type)) {
					if (get_size(dst_type) == 8) {
						//cast->cast_kind = CastKind::no_op;
						break;
					}

					auto built_in_cast = find_if(built_in_casts, [&](auto c) {
						return types_match(c.from, builtin_u64) && types_match(c.to, dst_type);
					});
					assert(built_in_cast);
					//cast->cast_kind = built_in_cast->kind;
					break;
				}
			}

			state->reporter.error(
				cast->location,
				"Conversion from {} to {} does not exist",
				type_to_string(src_type),
				type_to_string(dst_type)
			);
			yield(TypecheckResult::fail);
		}
	} else if (bin->operation == ass) {
		typecheck(state, bin->left);
		if (!ensure_assignable(&state->reporter, bin->left)) {
			yield(TypecheckResult::fail);
		}

		typecheck(state, bin->right);

		if (!implicitly_cast(state, &state->reporter, &bin->right, bin->left->type)) {
			yield(TypecheckResult::fail);
		}
		bin->type = builtin_void.ident;
		return bin;
	} else if (bin->operation == lor) {
		typecheck(state, bin->right);
		if (!types_match(bin->right->type, builtin_bool)) {
			state->reporter.error(bin->right->location, "This must be a boolean");
			yield(TypecheckResult::fail);
		}

		typecheck(state, bin->left);
		if (!types_match(bin->left->type, builtin_bool)) {
			state->reporter.error(bin->left->location, "This must be a boolean");
			yield(TypecheckResult::fail);
		}
		bin->type = builtin_bool.ident;
		return bin;
	} else {
		typecheck(state, bin->right);

		auto l = direct(bin->left->type);
		auto r = direct(bin->right->type);

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
				if (direct_as<AstEnum>(l) && direct_as<AstEnum>(r)) {
					bin->type = builtin_bool.ident;
					return bin;
				}
				break;
			}
		}

		if (l->kind == Ast_Struct && r->kind == Ast_Struct) {
			REDECLARE_VAL(l, (AstStruct *)l);
			REDECLARE_VAL(r, (AstStruct *)r);

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
					if (types_match(l, builtin_unsized_integer) && types_match(r, builtin_unsized_integer) ) {
						bin->type = builtin_unsized_integer.ident;
						return bin;
					}
					if (types_match(l, builtin_unsized_integer) && ri) {
						bin->type = bin->left->type = bin->right->type;
						return bin;
					}
					if (li && types_match(r, builtin_unsized_integer) ) {
						bin->type = bin->right->type = bin->left->type;
						return bin;
					}
					if (lf && types_match(r, builtin_unsized_integer) ) {
						bin->type = bin->right->type = bin->left->type;
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
					bin->type = builtin_bool.ident;
					if (types_match(l, builtin_unsized_integer) && types_match(r, builtin_unsized_integer) ) {
						return bin;
					} else if (types_match(l, builtin_unsized_integer) && ri) {
						bin->left->type = bin->right->type;
						return bin;
					} else if (types_match(r, builtin_unsized_integer) && li) {
						bin->right->type = bin->left->type;
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
							bin->type = l->size > r->size ? l : r;
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
					bin->type = builtin_bool.ident;

					if (types_match(l, r)) {
						return bin;
					}

					if ((li && types_match(r, builtin_unsized_integer) ) || (ri && types_match(l, builtin_unsized_integer) )) {
						return bin;
					}

					break;
				}

				case lt:
				case gt:
				case le:
				case ge: {
					bin->type = builtin_bool.ident;

					if (li && ri) {
						if (signedness_matches(l, r)) {
							return bin;
						}
						break;
					}

					if ((li && types_match(r, builtin_unsized_integer) ) || (ri && types_match(l, builtin_unsized_integer) )) {
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
					if ((li && types_match(r, builtin_unsized_integer) ) || (ri && types_match(l, builtin_unsized_integer) )) {
						bin->type = types_match(r, builtin_unsized_integer) ? l : r;
						return bin;
					}

					break;
				}
			}
		} else if (lp && rp) {
			// Both are pointers
			REDECLARE_VAL(l, (AstUnaryOperator *)l);
			REDECLARE_VAL(r, (AstUnaryOperator *)r);
			switch (bin->operation) {
				case eq:
				case ne: {
					bin->type = builtin_bool.ident;
					if (types_match(l->expression, r->expression))
						return bin;
					if ((r == builtin_void.pointer) ||
						(l == builtin_void.pointer))
					{
						return bin;
					}
					break;
				}
			}
		} else if ((lp && ri) || (li && rp)) {
			// One is pointer and other is integer
			if (lp) {
				bin->type = bin->left->type;
				harden_type(bin->right);
			} else {
				bin->type = bin->right->type;
				harden_type(bin->left);
			}
			return bin;
		} else if ((is_lambda_type(l) && is_null_literal(bin->right)) || (is_lambda_type(r) && is_null_literal(bin->left))) {
			bin->type = builtin_bool.ident;
			return bin;
		}

		auto overloaded = find_user_defined_binary_operator(state, bin->operation, bin->left->type, bin->right->type);
		if (overloaded) {
			auto call = AstCall::create();
			call->location = bin->location;
			call->unsorted_arguments.add({{}, bin->left});
			call->unsorted_arguments.add({{}, bin->right});
			call->sorted_arguments.add(bin->left);
			call->sorted_arguments.add(bin->right);
			call->callable = overloaded;
			assert(overloaded->type->kind == Ast_LambdaType);
			call->lambda_type = (AstLambdaType *)overloaded->type;
			call->type = overloaded->return_parameter->type;
			return call;
		}

		report_type_mismatch();
		return 0;
	}
	return bin;
}
AstExpression *typecheck(TypecheckState *state, AstUnaryOperator *unop) {
	using enum UnaryOperation;

	switch (unop->operation) {
		case minus: {
			typecheck(state, unop->expression);
			if (::is_integer(unop->expression->type)) {
				unop->type = unop->expression->type;
			} else if (::is_float(unop->expression->type)) {
				unop->type = unop->expression->type;
			} else {
				state->reporter.error(unop->location, "Unary minus can not be applied to expression of type {}", type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
			}
			break;
		}
		case address_of: {
			typecheck(state, unop->expression);
			if (!ensure_addressable(&state->reporter, unop->expression)) {
				yield(TypecheckResult::fail);
			}
			harden_type(unop->expression);
			unop->type = make_pointer_type(unop->expression->type);
			break;
		}
		case pointer_or_dereference_or_unwrap: {
			typecheck(state, unop->expression);
			unop->operation = is_type(unop->expression) ? pointer : as_option(unop->expression->type) ? unwrap : dereference;
			switch (unop->operation) {
				case pointer: goto _pointer;
				case dereference: goto _dereference;
				case unwrap: goto _unwrap;
			}
			break;
		}
		case pointer: {
		_pointer:
			// done in case pointer_or_dereference_or_unwrap
			//typecheck(state, unop->expression);
			assert(is_type(unop->expression));
			unop->type = builtin_type.ident;
			break;
		}
		case dereference: {
		_dereference:
			// done in case pointer_or_dereference_or_unwrap
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
			// done in case pointer_or_dereference_or_unwrap
			//typecheck(state, unop->expression);
			assert(!is_type(unop->expression));
			auto option = as_option(unop->expression->type);
			assert(option);
			unop->type = option->expression;
			break;
		}
		case bnot: {
			typecheck(state, unop->expression);
			unop->type = unop->expression->type;

			if (types_match(unop->expression->type, builtin_bool)) {
				break;
			}

			if (::is_integer(unop->expression->type)) {
				break;
			}

			state->reporter.error(unop->location, "Can not apply bitwise not to {}", type_to_string(unop->expression->type));
			yield(TypecheckResult::fail);
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

			return make_integer(size_of->expression->location, get_size(size_of->expression));
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
			harden_type(typeof->expression);
			typeof->type = builtin_type.ident;
			//  print("typeof {} is {}\n", typeof->expression->location, type_to_string(typeof->expression->type));
			break;
		}
		case option: {
			typecheck(state, unop->expression);
			if (is_type(unop->expression)) {
				unop->type = builtin_type.ident;
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
			unop->expression->type = builtin_unknown_enum.ident;
			break;
		}
		default: {
			state->reporter.error(unop->location, "INTERNAL ERROR: Unknown unary operation.");
			yield(TypecheckResult::fail);
		}
	}
	return unop;
}
AstExpression *typecheck(TypecheckState *state, AstStruct *Struct) {
	auto definition = Struct->definition;

	Struct->type = builtin_type.ident;
	if (Struct->definition) {
		Struct->definition->type = Struct->type;
	}

	//if (Struct->definition && Struct->definition->name == "Thing")
	//	debug_break();

	scoped_replace(state->current_lambda_or_struct_or_enum, Struct);

	s64 struct_size = 0;
	s64 struct_alignment = 0;

	{
		push_scope(&Struct->scope);

		List<AstDefinition **> members;
		// List<AstDefinition *> struct_constants;
		// List<AstDefinition *> data_members;
		// List<AstDefinition *> lambda_members;
		// List<AstDefinition *> data_constants;
		// List<AstDefinition *> lambda_constants;

		for_each(Struct->scope.definitions, [&] (auto, DefinitionList &m) {
			assert(m.count);
			for (auto &member : m)
				members.add(&member);
		});

		std::sort(members.begin(), members.end(), [](AstDefinition **a, AstDefinition **b) {
			auto get_order = [](AstDefinition *a) -> umm {
				if (a->is_constant) {
					// FIXME: this will not work for aliases
					if (a->expression) {
						switch (a->expression->kind) {
							case Ast_Lambda: return -1; // last
							case Ast_Struct: return  0; // first
						}
					} else {
					}
				} else {
					return (umm)a->location.data;
				}
				return 1;
			};
			return get_order(*a) < get_order(*b);
		});

		for (auto _member : members) {
			auto &member = *_member;
			if (!member->is_constant) {
				for (auto existing_member : Struct->data_members) {
					if (existing_member->name == member->name) {
						state->reporter.error(member->location, "Redefinition.");
						state->reporter.info(existing_member->location, "First is here:");
						yield(TypecheckResult::fail);
					}
				}
				Struct->data_members.add(member);
			}
		}

		for (auto _member : members) {
			auto &member = *_member;
			//if (member->is_constant && member->expression && member->expression->kind == Ast_Lambda) {
			//} else {
			typecheck(state, member);
			//}

			if (!member->is_constant && member->expression) {
				state->reporter.error(member->location, "Default values for struct members are not implemented yet.");
				yield(TypecheckResult::fail);
			}

			member->definition_location = DefinitionLocation::struct_member;
		}

		if (Struct->is_union) {
			for (auto &member : Struct->data_members) {
				switch (Struct->layout) {
					case StructLayout::tlang: {
						member->offset = 0;
						struct_size = max(struct_size, get_size(member->type));
						break;
					}
					case StructLayout::c: {
						auto member_alignment = get_align(member->type);
						struct_alignment = max(struct_alignment, member_alignment);

						member->offset = 0;
						struct_size = max(struct_size, get_size(member->type));
						break;
					}
					default:
						invalid_code_path();
				}
			}
		} else {
			for (auto &member : Struct->data_members) {
				switch (Struct->layout) {
					case StructLayout::tlang: {
						auto member_alignment = get_align(member->type);
						struct_alignment = max(struct_alignment, member_alignment);

						member->offset = struct_size;
						struct_size += get_size(member->type);
						break;
					}
					case StructLayout::c: {
						auto member_alignment = get_align(member->type);
						struct_alignment = max(struct_alignment, member_alignment);
						assert(member_alignment >= 1);

						struct_size = ceil(struct_size, member_alignment);
						member->offset = struct_size;
						struct_size += get_size(member->type);
						break;
					}
					default:
						invalid_code_path();
				}
			}
		}
	}

	Struct->alignment = struct_alignment;
	Struct->size = max(struct_size, struct_alignment);
	return Struct;
}
AstExpression *typecheck(TypecheckState *state, AstSubscript *subscript) {
	if (subscript->index_expression) {
		typecheck(state, subscript->index_expression);
		harden_type(subscript->index_expression);

		if (!::is_integer(subscript->index_expression->type)) {
			state->reporter.error(subscript->index_expression->location, "Expression must be of type integer but is {}", type_to_string(subscript->index_expression->type));
			yield(TypecheckResult::fail);
		}
	}

	typecheck(state, subscript->expression);
	if (is_type(subscript->expression)) {
		subscript->type = builtin_type.ident;

		// if (subscript->is_simd) {
		//
		//     if (!is_constant(subscript->index_expression)) {
		//         state->reporter.error(subscript->index_expression->location, "simd array size must be a constant");
		//         yield(TypecheckResult::fail);
		//     }
		//
		//     auto size = get_constant_integer(subscript->index_expression);
		//     if (!size) {
		//         state->reporter.error(subscript->index_expression->location, "simd array size must be an integer");
		//         yield(TypecheckResult::fail);
		//     }
		//     subscript->simd_size = (u64)size.value();
		// }

	} else {
		if (!ensure_subscriptable(state, subscript->expression)) {
			yield(TypecheckResult::fail);
		}

		auto type = subscript->expression->type;
		if (type->kind == Ast_Subscript) {
			subscript->type = ((AstSubscript *)type)->expression;
		} else if (type->kind == Ast_Span) {
			subscript->type = ((AstSpan *)type)->expression;
		} else if (is_pointer(type)) {
			subscript->type = ((AstUnaryOperator *)type)->expression;
		} else {
			invalid_code_path();
		}
	}
	return subscript;
}
AstExpression *typecheck(TypecheckState *state, AstSpan *span) {
	typecheck(state, span->expression);
	if (!is_type(span->expression)) {
		state->reporter.error(span->expression->location, "Slice expects this to be a type, but it isn't.");
		yield(TypecheckResult::fail);
	}
	span->type = builtin_type.ident;
	return span;
}
AstExpression *typecheck(TypecheckState *state, AstIfx *If) {
	typecheck(state, If->condition);
	if (!implicitly_cast(state, &state->reporter, &If->condition, builtin_bool.ident)) {
		yield(TypecheckResult::fail);
	}

	typecheck(state, If->true_expression);
	harden_type(If->true_expression);

	typecheck(state, If->false_expression);
	harden_type(If->false_expression);

	if (!types_match(If->true_expression->type, If->false_expression->type)) {
		state->reporter.error(If->location, "Both branches of if expression must have the same type, but provided {} and {}", type_to_string(If->true_expression->type), type_to_string(If->false_expression->type));
		yield(TypecheckResult::fail);
	}

	If->type = If->true_expression->type;
	return If;
}
AstExpression *typecheck(TypecheckState *state, AstTest *test) {
	struct TestParams {
		TypecheckState *state;
		AstTest *test;
	} test_params;
	test_params.state = state;
	test_params.test = test;

#if USE_FIBERS
	auto typechecker = [](LPVOID param) -> void {
		auto test_params = (TestParams *)param;
		auto state = test_params->state;
		typecheck_scope(&test_params->test->scope);
		yield(TypecheckResult::success);
	};

	auto fiberstate = CreateFiber(4096, typechecker, &test_params);
	if (!fiberstate) {
		immediate_error(test->location, "INTERNAL COMPILER ERROR: Failed to create fiber");
		exit(-1);
	}
	defer { DeleteFiber(fiberstate); };

	auto original_parent_fiber = state->parent_fiber;
	state->parent_fiber = state->fiber;
	defer { state->parent_fiber = original_parent_fiber; };

	auto original_fiber = state->fiber;
	state->fiber = fiberstate;
	defer { state->fiber = original_fiber; };

	auto original_reporter = state->reporter;
	state->reporter = {};
	defer { state->reporter = original_reporter; };

	bool did_compile = false;

	while (1) {
		SWITCH_TO_FIBER(fiberstate);
		switch ((TypecheckResult)fiber_result) {
			case TypecheckResult::success: {
				did_compile = true;
				goto _break;
			}
			case TypecheckResult::fail: {
				did_compile = false;
				state->no_progress_counter = 0;
				goto _break;
			}
			case TypecheckResult::wait: {
				scoped_replace(state->fiber, original_fiber);
				fiber_result = TypecheckResult::wait;
				SWITCH_TO_FIBER(original_parent_fiber);
				break;
			}
			default: {
				invalid_code_path("something wrong with fiberutines");
			}
		}
	}
_break:
#else
	auto typechecker = [](CoroState *, size_t param) -> size_t {
		auto test_params = (TestParams *)param;
		auto state = test_params->state;
		typecheck_scope(&test_params->test->scope);
		return (size_t)TypecheckResult::success;
	};

	CoroState *corostate;
	auto init_result = coro_init(&corostate, typechecker, 1024*1024);
	if (init_result.is_error) {
		state->reporter.error(test->location, init_result.error.message);
		yield(TypecheckResult::fail);
	}
	defer { coro_free(&corostate); };

	auto original_coro = state->coro;
	state->coro = corostate;
	defer { state->coro = original_coro; };

	auto original_reporter = state->reporter;
	state->reporter = {};
	defer { state->reporter = original_reporter; };

	while (1) {
		switch ((TypecheckResult)coro_yield(corostate, (size_t)&test_params)) {
			case TypecheckResult::success: {
				if (!test->should_compile) {
					original_reporter.error(test->location, "Test compiled, but it should not!");
				}
				goto _break;
			}
			case TypecheckResult::fail: {
				if (test->should_compile) {
					original_reporter.error(test->location, "Test did not compile, but it should!");
					original_reporter.info("Here are reports:");
					original_reporter.reports.add(state->reporter.reports);
				}
				state->no_progress_counter = 0;
				goto _break;
			}
			case TypecheckResult::wait: {
				auto old_coro = state->coro;
				state->coro = original_coro;
				defer { state->coro = old_coro; };
				break;
			}
			default: {
				invalid_code_path("something wrong with coroutines");
			}
		}
	}
_break:
#endif

	auto result = make_bool(did_compile);
	result->location = test->location;
	return result;
}
AstExpression *typecheck(TypecheckState *state, AstEnum *Enum) {
	scoped_replace(state->current_lambda_or_struct_or_enum, Enum);
	typecheck_scope(&Enum->scope);
	Enum->type = builtin_type.ident;
	return Enum;
}

void typecheck(TypecheckState *state, Expression<> &expression) {
	assert(expression);

	defer { if (!throwing) {
		if (expression->kind == Ast_Identifier && ((AstIdentifier *)expression)->possible_definitions.count) {
		} else {
			assert(expression->type);
			if (!SIMPLIFY(&state->reporter, expression))
				yield(TypecheckResult::fail);
			assert(expression->type);
		}
	}};

	switch (expression->kind) {
		case Ast_Identifier:     expression = typecheck(state, (AstIdentifier     *)expression); return;
		case Ast_Call:           expression = typecheck(state, (AstCall           *)expression); return;
		case Ast_Literal:        expression = typecheck(state, (AstLiteral        *)expression); return;
		case Ast_Lambda:         expression = typecheck(state, (AstLambda         *)expression); return;
		case Ast_LambdaType:     expression = typecheck(state, (AstLambdaType     *)expression); return;
		case Ast_BinaryOperator: expression = typecheck(state, (AstBinaryOperator *)expression); return;
		case Ast_UnaryOperator:  expression = typecheck(state, (AstUnaryOperator  *)expression); return;
		case Ast_Struct:         expression = typecheck(state, (AstStruct         *)expression); return;
		case Ast_Subscript:      expression = typecheck(state, (AstSubscript      *)expression); return;
		case Ast_Span:           expression = typecheck(state, (AstSpan           *)expression); return;
		case Ast_Ifx:            expression = typecheck(state, (AstIfx            *)expression); return;
		case Ast_Test:           expression = typecheck(state, (AstTest           *)expression); return;
		case Ast_Enum:           expression = typecheck(state, (AstEnum           *)expression); return;
		case Ast_Import: {
#if 0
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
#endif

			break;
		}
		default: {
			invalid_code_path();
			state->reporter.error(expression->location, "Internal error: typecheck(AstExpression *): unhandled case '{}'", expression->kind);
			yield(TypecheckResult::fail);
		}
	}
}

void typecheck_global(CoroState *corostate, TypecheckState *state) {
	// Keep using pooled fibers
	while (1) {
		throwing = false;
		defer { state = state->next_state; };
		try {
			push_scope(&global_scope);
			typecheck(state, state->statement);
		} catch (int x) {
			continue;
		}
		yield(TypecheckResult::success);
	}
}

umm typecheck_coroutine(CoroState *corostate, umm param) {
	typecheck_global(corostate, (TypecheckState *)param);
	return 0;
}
VOID WINAPI typecheck_fiber(LPVOID lpFiberParameter) {
	typecheck_global(0, (TypecheckState *)lpFiberParameter);
}

bool typecheck_finished;

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

	add_to_scope(d, &destination->scope);
	d->parent_lambda_or_struct = destination;

	if (!constant) {
		destination->data_members.add(d);
	}
}

static void write_test_source() {
	StringBuilder test;
	append(test, R"(
/*
draw :: fn #stdcall (_: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int) {
}

main :: fn () {
    draw(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
}
*/
import "std.tl"
import "opengl.tl"

dc: HDC;

draw :: fn () {
    glColor3f(1, 1, 1);
    glBegin(GL_TRIANGLES);

    glVertex2f(-0.5, -0.5);
    glVertex2f( 0.0,  0.5);
    glVertex2f( 0.5, -0.5);

    glEnd();

    SwapBuffers(dc);
}

main :: fn (): int {
	class_name := "window_class\0".data;
	window_name := "hello window\0".data;

	hInstance := GetModuleHandleA(null);

	wc : WNDCLASSEXA;
	wc.hInstance = hInstance;
	wc.cbSize = #sizeof WNDCLASSEXA;
	wc.lpfnWndProc = fn (hwnd: HWND, uMsg: UINT, wParam: WPARAM, lParam: LPARAM): LRESULT {
		if uMsg == WM_DESTROY {
			PostQuitMessage(0);
		} else if uMsg == WM_SIZE {
            glViewport(0, 0, (lParam & 0xffff) as s32, ((lParam >> 16) & 0xffff) as s32);
            draw();
        } else {
            return DefWindowProcA(hwnd, uMsg, wParam, lParam);
        }
        return 0;
	};
	wc.lpszClassName = class_name;
	wc.hCursor = LoadCursorA(null, IDC_ARROW);

    print(if RegisterClassExA(&wc) != 0 then "Class created!\n" else "Class Failed!\n");

	window := CreateWindowExA(
		0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
	);

	print(if window != null then "Window Success!\n" else "Window Fail!\n");

	dp : PIXELFORMATDESCRIPTOR;
	dp.nSize = #sizeof PIXELFORMATDESCRIPTOR;
	dp.nVersion = 1;
	dp.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER;
	dp.cColorBits = 32;
	dp.cAlphaBits = 8;
	dp.cDepthBits = 24;
	dp.cStencilBits = 8;
	dp.iPixelType = PFD_TYPE_RGBA;
	dp.iLayerType = PFD_MAIN_PLANE;

    dc = GetDC(window);

    index := ChoosePixelFormat(dc, &dp);
	if index == 0 {
		error := GetLastError();
		print("ChoosePixelFormat failed with code ");
		print(error);
		return 1;
	}

	sp : PIXELFORMATDESCRIPTOR;
	DescribePixelFormat(dc, index, #sizeof #typeof sp, &sp);

	SetPixelFormat(dc, index, &sp);

	context := wglCreateContext(dc);
	if wglMakeCurrent(dc, context) != 1 {
		print("wglMakeCurrent failed");
		return 1;
	}

	while true {
        msg : MSG;
		while PeekMessageA(&msg, null, 0, 0, PM_REMOVE) != 0 {
			if msg.message == WM_QUIT
				return 0;
			TranslateMessage(&msg);
			DispatchMessageA(&msg);
		}

        draw();
	}
}
)");
#if 1
	for (u32 i = 0; i < 4096*16; ++i) {
		append_format(test, R"FOOBAR(
/*
draw :: fn #stdcall (_: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int, _: int) {{
}}

main :: fn () {{
    draw(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15);
}}
*/
import "std.tl"
import "opengl.tl"

dc{}: HDC;

draw{} :: fn () {{
    glColor3f(1, 1, 1);
    glBegin(GL_TRIANGLES);

    glVertex2f(-0.5, -0.5);
    glVertex2f( 0.0,  0.5);
    glVertex2f( 0.5, -0.5);

    glEnd();

    SwapBuffers(dc{});
}}

main{} :: fn (): int {{
	class_name := "window_class\0".data;
	window_name := "hello window\0".data;

	hInstance := GetModuleHandleA(null);

	wc : WNDCLASSEXA;
	wc.hInstance = hInstance;
	wc.cbSize = #sizeof WNDCLASSEXA;
	wc.lpfnWndProc = fn (hwnd: HWND, uMsg: UINT, wParam: WPARAM, lParam: LPARAM): LRESULT {{
		if uMsg == WM_DESTROY {{
			PostQuitMessage(0);
		}} else if uMsg == WM_SIZE {{
            glViewport(0, 0, (lParam & 0xffff) as s32, ((lParam >> 16) & 0xffff) as s32);
            draw{}();
        }} else {{
            return DefWindowProcA(hwnd, uMsg, wParam, lParam);
        }}
        return 0;
	}};
	wc.lpszClassName = class_name;
	wc.hCursor = LoadCursorA(null, IDC_ARROW);

    print(if RegisterClassExA(&wc) != 0 then "Class created!\n" else "Class Failed!\n");

	window := CreateWindowExA(
		0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE,
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
	);

	print(if window != null then "Window Success!\n" else "Window Fail!\n");

	dp : PIXELFORMATDESCRIPTOR;
	dp.nSize = #sizeof PIXELFORMATDESCRIPTOR;
	dp.nVersion = 1;
	dp.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER;
	dp.cColorBits = 32;
	dp.cAlphaBits = 8;
	dp.cDepthBits = 24;
	dp.cStencilBits = 8;
	dp.iPixelType = PFD_TYPE_RGBA;
	dp.iLayerType = PFD_MAIN_PLANE;

    dc{} = GetDC(window);

    index := ChoosePixelFormat(dc{}, &dp);
	if index == 0 {{
		error := GetLastError();
		print("ChoosePixelFormat failed with code ");
		print(error);
		return 1;
	}}

	sp : PIXELFORMATDESCRIPTOR;
	DescribePixelFormat(dc{}, index, #sizeof #typeof sp, &sp);

	SetPixelFormat(dc{}, index, &sp);

	context := wglCreateContext(dc{});
	if wglMakeCurrent(dc{}, context) != 1 {{
		print("wglMakeCurrent failed");
		return 1;
	}}

	while true {{
        msg : MSG;
		while PeekMessageA(&msg, null, 0, 0, PM_REMOVE) != 0 {{
			if msg.message == WM_QUIT
				return 0;
			TranslateMessage(&msg);
			DispatchMessageA(&msg);
		}}

        draw{}();
	}}
}}


)FOOBAR", i, i, i, i, i, i, i, i, i, i, i, i);
	}
#else
	append_format(test, u8"♥0 :: fn (☺: s32, ☻: s32) s32 {{ return ☺ + ☻; }} /* ♦♣♠•◘○ this is a /* nested */ comment ♦♣♠•◘○ */\n");
	for (int i = 1; i < 4096; ++i) {
		append_format(test, u8"♥{} :: fn (☺: s32, ☻: s32) s32 {{ return ♥{}(☺, ☻); }} /* ♦♣♠•◘○ this is a /* nested */ comment ♦♣♠•◘○ */\n", i, i - 1);
	}
#endif
	write_entire_file("performance_test.tl"s, as_bytes(to_string(test)));
}

struct ParsedArguments {
	String target;
	String output;

	List<String> source_files;

	bool print_ast_after_parse = false;
	bool print_ast_after_typecheck = false;
	bool no_typecheck = false;
	bool debug_paths = false;
	bool success = false;
	bool stats = false;
};

ParsedArguments parse_arguments(Span<Span<utf8>> arguments) {
	timed_function(context.profiler);

	ParsedArguments result = {};

	context.compiler_path = (String)get_executable_path();

	auto parsed = parse_path(context.compiler_path);
	context.compiler_name = parsed.name;
	context.compiler_directory = parsed.directory;

	result.target = "nasm_x86_64_windows"str;

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
			if (arguments[i] == "parse") result.print_ast_after_parse = true;
			else if (arguments[i] == "type") result.print_ast_after_typecheck = true;
			else immediate_error("Unexpected argument after --print-ast.\n");
		} else if (arguments[i] == "--print-ast-after-typecheck"str) {
			result.print_ast_after_typecheck = true;
		} else if (arguments[i] == "--no-type-check"str) {
			result.no_typecheck = true;
		} else if (arguments[i] == "--debug-paths"str) {
			result.debug_paths = true;
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
			context.do_profile = true;
		} else if (arguments[i] == "--keep-temp"str) {
			context.keep_temp = true;
		} else if (arguments[i] == "--debug-poly"str) {
			context.debug_poly = true;
		} else if (arguments[i] == "--print-lowered"str) {
			context.print_lowered = true;
		} else if (arguments[i] == "--stats"str) {
			result.stats = true;
		} else {
			result.source_files.add(arguments[i]);
		}
	}

	result.success = true;
	return result;
}

#if TRACK_ALLOCATIONS
static HashMap<std::source_location, u32> allocation_sizes;

void print_allocation_count() {
	print("Allocations:\n");
	struct AllocationInfo {
		std::source_location location;
		u32 size;
	};

	List<AllocationInfo> allocations;
	allocations.allocator = temporary_allocator; // otherwise `allocation_sizes` will update inside `for_each`

	for_each(allocation_sizes, [&](std::source_location location, u32 size) {
		allocations.add({location, size});
	});

	std::sort(allocations.begin(), allocations.end(), [](auto a, auto b) {
		return a.size > b.size;
	});

	for (auto a : allocations) {
		print("{}: {}\n", a.location, format_bytes(a.size));
	}
}
#endif

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
#if TRACK_ALLOCATIONS
			allocation_sizes.get_or_insert(location) += new_size;
#endif
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

s32 tl_main(Span<Span<utf8>> arguments) {
	defer { print(""); }; // to reset console color just in case

	strings = strings_en;
	{
		utf16 buffer[256];
		GetUserDefaultLocaleName((wchar *)buffer, sizeof(buffer));
		if (as_span(buffer) == u"ru-RU"s) {
			strings = strings_ru;
		}
		for (auto i = &strings._start_marker + 1; i != &strings._end_marker; ++i) {
			if (*i == 0) {
				*i = *(&strings_en._start_marker + (i - &strings._start_marker));
			}
		}
	}

	construct(context);

	auto global_timer = create_precise_timer();

	set_console_encoding(Encoding_utf8);

	defer {
		if (context.do_profile) {
			print("Execution finished in {} ms\n", reset(global_timer) * 1000);
			print("Peak memory usage: {}\n", format_bytes(get_memory_info().peak_usage));
		}
	};

	// write_test_source();

	init_my_allocator();

#if TRACK_ALLOCATIONS
	allocation_sizes.allocator = os_allocator;
	defer { print_allocation_count(); };
#endif
#if USE_SLABS
	default_allocator = current_allocator = {
		slab_allocator_func,
		0
	};
#else
	default_allocator = current_allocator = {
		.func = [](AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> AllocationResult {
			switch (mode) {
				case Allocator_allocate: {
#if TRACK_ALLOCATIONS
					allocation_sizes.get_or_insert(location) += new_size;
#endif
					return MyAllocator{}.allocate_impl(new_size, align, location);
				}
				case Allocator_reallocate: {
					return MyAllocator{}.reallocate_impl(data, old_size, new_size, align, location);
				}
				case Allocator_free: {
					MyAllocator{}.deallocate_impl(data, new_size, align, location);
					break;
				}
			}
			return {};
		},
		.state = 0
	};
#endif

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
	context.profiler.init();
	defer {
		write_entire_file("profile.tmd"s, context.profiler.output_for_timed());
		context.profiler.deinit();
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

	timed_function(context.profiler);

restart_main:

	auto timer = context.profiler.scoped_timer("setup");

	context.current_directory = get_current_directory();

	auto args = parse_arguments(arguments);

	if (args.debug_paths) print("compiler_directory: {}\n", context.compiler_directory);

	if (args.debug_paths) print("current_directory: {}\n", context.current_directory);

	if (args.source_files.count == 0) {
		immediate_error(strings.no_source_path_received);
		return 1;
	}

	context.source_path = args.source_files[0];
	if (!is_absolute_path(context.source_path)) {
		context.source_path = make_absolute_path(context.source_path);
	}

	if (args.debug_paths) print("source_path: {}\n", context.source_path);

	context.source_path_without_extension = parse_path(context.source_path).path_without_extension();
	if (args.debug_paths) print("source_path_without_extension: {}\n", context.source_path_without_extension);

	if (args.output.count) {
		context.output_path = args.output;
		if (!is_absolute_path(context.output_path)) {
			context.output_path = make_absolute_path(context.output_path);
		}
	} else {
		context.output_path = (String)format("{}\\{}.exe", get_current_directory(), parse_path(context.source_path).name);
	}

	construct(parsed_files);
	construct(global_scope);
	//construct(typechecked_globals);
	//construct(names_not_available_for_globals);
	construct(built_in_casts);

	construct(double_char_tokens);
	construct(triple_char_tokens);
	construct(import_paths);

	construct(implicit_casts);
	construct(explicit_casts);
	construct(binary_operators);

	construct(typeinfo_definitinos);

	Pool32<AstExpression>::init();
	Pool32<AstStatement>::init();

	/*
	construct(Ref<AstAssert>::storage);
	construct(Ref<AstAutocast>::storage);
	construct(Ref<AstBinaryOperator>::storage);
	construct(Ref<AstBlock>::storage);
	construct(Ref<AstCall>::storage);
	construct(Ref<AstCast>::storage);
	construct(Ref<AstDefer>::storage);
	construct(Ref<AstDefinition>::storage);
	construct(Ref<AstExpressionStatement>::storage);
	construct(Ref<AstIdentifier>::storage);
	construct(Ref<AstIf>::storage);
	construct(Ref<AstIfx>::storage);
	construct(Ref<AstImport>::storage);
	construct(Ref<AstLambda>::storage);
	construct(Ref<AstLiteral>::storage);
	construct(Ref<AstPrint>::storage);
	construct(Ref<AstReturn>::storage);
	construct(Ref<AstSizeof>::storage);
	construct(Ref<AstStruct>::storage);
	construct(Ref<AstSubscript>::storage);
	construct(Ref<AstTest>::storage);
	construct(Ref<AstTuple>::storage);
	construct(Ref<AstTypeof>::storage);
	construct(Ref<AstUnaryOperator>::storage);
	construct(Ref<AstWhile>::storage);
	*/

	HMODULE lib = 0;
	if (args.target == "none"str) {
		context.register_size = 8;
		context.stack_word_size = 8;
		context.general_purpose_register_count = 16;
	} else {
		scoped_phase("Collecting target information");

		with(temporary_allocator,
			lib = LoadLibraryW((wchar *)to_utf16(concatenate(context.compiler_directory, "\\targets\\"str, args.target), true).data)
		);

		if (!lib) {
			print("Failed to load target code generator '{}'. Check \"tlang\\bin\\targets\" directory\n", args.target);
			return 1;
		}

		auto get_target_information = (TargetInformationGetter)GetProcAddress(lib, "tlang_get_target_information");
		if (!get_target_information) {
			print("Failed to load target code generator '{}'. There is no `tlang_get_target_information` function\n", args.target);
			return 1;
		}

		get_target_information(context);
	}




	import_paths.add(context.current_directory);
	import_paths.add(concatenate(context.compiler_directory, "\\libs"str));

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

	triple_char_tokens.insert(">>="str);
	triple_char_tokens.insert("<<="str);

	construct(keywords);
#define E(name, value) keywords.get_or_insert((String)u8#name##s) = value;
	ENUMERATE_KEYWORDS(E);
#undef E

	auto add_global_alias = [&](String name, AstExpression *expression) {
		auto defn = AstDefinition::create();
		defn->location = name;
		defn->name = name;
		defn->is_constant = true;
		defn->built_in = true;
		defn->type = expression->type;
		defn->expression = expression;
		defn->definition_location = DefinitionLocation::global;
		// defn->add_to_scope(&global_scope);
		global_scope.definitions.get_or_insert(defn->name).add(defn);
		global_scope.statements.add(defn);

		auto ident = AstIdentifier::create();
		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(defn);
		ident->type = expression->type;
		return ident;
	};
	auto init_builtin = [&](auto &builtin, auto thing, String name) {
		auto definition = thing->definition = AstDefinition::create();
		// NOTE: this identifier has to be created BEFORE doing
		// `thing->type = builtin_type.ident` to allow `type` struct reference itself.
		auto ident = builtin.ident = AstIdentifier::create();

		thing->type = builtin_type.ident;

		definition->is_constant = true;
		definition->expression = thing;
		definition->location = name;
		definition->name = name;
		definition->type = thing->type;
		definition->built_in = true;
		definition->definition_location = DefinitionLocation::global;

		add_to_scope(definition, &global_scope);
		//typechecked_globals.get_or_insert(name) = definition;

		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(definition);
		ident->type = thing->type;

		// NOTE: not every builtin struct requires this, but i'd rather
		// not have to debug for an hour because i forgot to do this.
		init_pointer(builtin);
		init_span(builtin);
	};
	auto init_struct = [&](BuiltinStruct &type, String name, s64 size, s64 align) {
		type.Struct = AstStruct::create();
		type.Struct->size = size;
		type.Struct->alignment = align;
		type.Struct->location = name;
		init_builtin(type, type.Struct, name);

	};
	auto init_enum = [&](BuiltinEnum &type, String name) {
		type.Enum = AstEnum::create();
		type.Enum->location = name;
		init_builtin(type, type.Enum, name);
	};

	init_struct(builtin_type,          "type"str, 8, 8);
	init_struct(builtin_void,          "void"str, 0, 0);
	init_struct(builtin_bool,          "bool"str, 1, 1);
	init_struct(builtin_u8,            "u8"str,   1, 1);
	init_struct(builtin_u16,           "u16"str,  2, 2);
	init_struct(builtin_u32,           "u32"str,  4, 4);
	init_struct(builtin_u64,           "u64"str,  8, 8);
	init_struct(builtin_s8,            "s8"str,   1, 1);
	init_struct(builtin_s16,           "s16"str,  2, 2);
	init_struct(builtin_s32,           "s32"str,  4, 4);
	init_struct(builtin_s64,           "s64"str,  8, 8);
	init_struct(builtin_f32,           "f32"str,  4, 4);
	init_struct(builtin_f64,           "f64"str,  8, 8);
	init_struct(builtin_string,        "string"str, 0, 0);
	init_struct(builtin_struct_member, "struct_member"str, 0, 0);
	init_struct(builtin_typeinfo,      "typeinfo"str, 0, 0);
	init_struct(builtin_any,           "any"str, 0, 0);

	init_enum(builtin_type_kind, "type_kind"str);

	init_struct(builtin_unsized_integer, "<integer>"str, 0, 0);
	init_struct(builtin_unsized_float,   "<float>"str, 0, 0);
	init_struct(builtin_noinit,          "<noinit>"str, 0, 0);
	init_struct(builtin_unknown,         "<unknown>"str, 0, 0);
	init_struct(builtin_unknown_enum,    "<unknown enum>"str, 0, 0);
	init_struct(builtin_poly,            "<poly>"str, 0, 0);
	init_struct(builtin_overload_set,    "<overload set>"str, 0, 0);

	switch (context.register_size) {
		case 4:
			builtin_default_signed_integer = &builtin_s32;
			builtin_default_unsigned_integer = &builtin_u32;
			break;
		case 8:
			builtin_default_signed_integer = &builtin_s64;
			builtin_default_unsigned_integer = &builtin_u64;
			break;
	}
	builtin_default_integer = builtin_default_signed_integer;
	builtin_default_float = &builtin_f64;

	type_sint  = add_global_alias("sint"str,  builtin_default_integer->ident);
	type_uint  = add_global_alias("uint"str,  builtin_default_unsigned_integer->ident);
	type_int   = add_global_alias("int"str,   builtin_default_signed_integer->ident);
	type_float = add_global_alias("float"str, builtin_default_float->ident);

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


		add_to_scope(d, &Struct->scope);
		d->parent_lambda_or_struct = Struct;

		Struct->data_members.add(d);

		return d;
	};
	auto append_value = [](BuiltinEnum &builtin, String name, s64 value) {
		auto Enum = builtin.Enum;

		auto d = AstDefinition::create();
		d->location = name;
		d->name = name;
		d->type = type_int;
		d->expression = make_integer(name, value, type_int);

		add_to_scope(d, &Enum->scope);
		d->parent_lambda_or_struct = Enum;

		return d;
	};

	add_member(builtin_u8 .Struct, builtin_u8 .ident, "min"str, make_integer({}, (u64)min_value<u8 >), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u16.Struct, builtin_u16.ident, "min"str, make_integer({}, (u64)min_value<u16>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u32.Struct, builtin_u32.ident, "min"str, make_integer({}, (u64)min_value<u32>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u64.Struct, builtin_u64.ident, "min"str, make_integer({}, (u64)min_value<u64>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u8 .Struct, builtin_u8 .ident, "max"str, make_integer({}, (u64)max_value<u8 >), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u16.Struct, builtin_u16.ident, "max"str, make_integer({}, (u64)max_value<u16>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u32.Struct, builtin_u32.ident, "max"str, make_integer({}, (u64)max_value<u32>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_u64.Struct, builtin_u64.ident, "max"str, make_integer({}, (u64)max_value<u64>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s8 .Struct, builtin_s8 .ident, "min"str, make_integer({}, (s64)min_value<s8 >), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s16.Struct, builtin_s16.ident, "min"str, make_integer({}, (s64)min_value<s16>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s32.Struct, builtin_s32.ident, "min"str, make_integer({}, (s64)min_value<s32>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s64.Struct, builtin_s64.ident, "min"str, make_integer({}, (s64)min_value<s64>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s8 .Struct, builtin_s8 .ident, "max"str, make_integer({}, (s64)max_value<s8 >), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s16.Struct, builtin_s16.ident, "max"str, make_integer({}, (s64)max_value<s16>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s32.Struct, builtin_s32.ident, "max"str, make_integer({}, (s64)max_value<s32>), true, INVALID_MEMBER_OFFSET);
	add_member(builtin_s64.Struct, builtin_s64.ident, "max"str, make_integer({}, (s64)max_value<s64>), true, INVALID_MEMBER_OFFSET);

	// string
	append_member(builtin_string, "data"str,  builtin_u8.pointer);
	append_member(builtin_string, "count"str, type_uint);

	// type_kind
	append_value(builtin_type_kind, "void"str,    0);
	append_value(builtin_type_kind, "bool"str,    1);
	append_value(builtin_type_kind, "u8"str,      2);
	append_value(builtin_type_kind, "u16"str,     3);
	append_value(builtin_type_kind, "u32"str,     4);
	append_value(builtin_type_kind, "u64"str,     5);
	append_value(builtin_type_kind, "s8"str,      6);
	append_value(builtin_type_kind, "s16"str,     7);
	append_value(builtin_type_kind, "s32"str,     8);
	append_value(builtin_type_kind, "s64"str,     9);
	append_value(builtin_type_kind, "f32"str,     10);
	append_value(builtin_type_kind, "f64"str,     11);
	append_value(builtin_type_kind, "struct"str,  12);
	append_value(builtin_type_kind, "enum"str,    13);
	append_value(builtin_type_kind, "pointer"str, 14);
	append_value(builtin_type_kind, "span"str,    15);

	// struct_member
	append_member(builtin_struct_member, "name"str, builtin_string.ident);
	append_member(builtin_struct_member, "type"str, builtin_typeinfo.pointer);

	// typeinfo
	append_member(builtin_typeinfo, "kind"str, builtin_type_kind.ident);
	append_member(builtin_typeinfo, "name"str, builtin_string.ident);
	append_member(builtin_typeinfo, "members"str, builtin_struct_member.span);
	append_member(builtin_typeinfo, "pointee"str, builtin_typeinfo.pointer);

	// any
	append_member(builtin_any, "pointer"str, builtin_void    .pointer);
	append_member(builtin_any, "type"str,    builtin_typeinfo.pointer);

#if 1
	integer_infos[0] = {builtin_u8 .Struct, 8 };
	integer_infos[1] = {builtin_u16.Struct, 16};
	integer_infos[2] = {builtin_u32.Struct, 32};
	integer_infos[3] = {builtin_u64.Struct, 64};
	integer_infos[4] = {builtin_s8 .Struct, 8 };
	integer_infos[5] = {builtin_s16.Struct, 16};
	integer_infos[6] = {builtin_s32.Struct, 32};
	integer_infos[7] = {builtin_s64.Struct, 64};
#else
	integer_infos[0] = {builtin_u8 .Struct, -make_big_int<BigInteger>((u64)0xff),               make_big_int<BigInteger>((u64)0xff)              };
	integer_infos[1] = {builtin_u16.Struct, -make_big_int<BigInteger>((u64)0xffff),             make_big_int<BigInteger>((u64)0xffff)            };
	integer_infos[2] = {builtin_u32.Struct, -make_big_int<BigInteger>((u64)0xffffffff),         make_big_int<BigInteger>((u64)0xffffffff)        };
	integer_infos[3] = {builtin_u64.Struct, -make_big_int<BigInteger>((u64)0xffffffffffffffff), make_big_int<BigInteger>((u64)0xffffffffffffffff)};
	integer_infos[4] = {builtin_s8 .Struct, -make_big_int<BigInteger>((u64)0xff),               make_big_int<BigInteger>((u64)0xff)              };
	integer_infos[5] = {builtin_s16.Struct, -make_big_int<BigInteger>((u64)0xffff),             make_big_int<BigInteger>((u64)0xffff)            };
	integer_infos[6] = {builtin_s32.Struct, -make_big_int<BigInteger>((u64)0xffffffff),         make_big_int<BigInteger>((u64)0xffffffff)        };
	integer_infos[7] = {builtin_s64.Struct, -make_big_int<BigInteger>((u64)0xffffffffffffffff), make_big_int<BigInteger>((u64)0xffffffffffffffff)};
#endif

	built_in_casts.insert({builtin_u8 .Struct, builtin_s8 .Struct, /*CastKind::u8_s8  , */false});
	built_in_casts.insert({builtin_u8 .Struct, builtin_s16.Struct, /*CastKind::u8_s16 , */true});
	built_in_casts.insert({builtin_u8 .Struct, builtin_s32.Struct, /*CastKind::u8_s32 , */true});
	built_in_casts.insert({builtin_u8 .Struct, builtin_s64.Struct, /*CastKind::u8_s64 , */true});
	built_in_casts.insert({builtin_u8 .Struct, builtin_u16.Struct, /*CastKind::u8_u16 , */true});
	built_in_casts.insert({builtin_u8 .Struct, builtin_u32.Struct, /*CastKind::u8_u32 , */true});
	built_in_casts.insert({builtin_u8 .Struct, builtin_u64.Struct, /*CastKind::u8_u64 , */true});
	built_in_casts.insert({builtin_u16.Struct, builtin_s8 .Struct, /*CastKind::u16_s8 , */false});
	built_in_casts.insert({builtin_u16.Struct, builtin_s16.Struct, /*CastKind::u16_s16, */false});
	built_in_casts.insert({builtin_u16.Struct, builtin_s32.Struct, /*CastKind::u16_s32, */true});
	built_in_casts.insert({builtin_u16.Struct, builtin_s64.Struct, /*CastKind::u16_s64, */true});
	built_in_casts.insert({builtin_u16.Struct, builtin_u8 .Struct, /*CastKind::u16_u8 , */false});
	built_in_casts.insert({builtin_u16.Struct, builtin_u32.Struct, /*CastKind::u16_u32, */true});
	built_in_casts.insert({builtin_u16.Struct, builtin_u64.Struct, /*CastKind::u16_u64, */true});
	built_in_casts.insert({builtin_u32.Struct, builtin_s8 .Struct, /*CastKind::u32_s8 , */false});
	built_in_casts.insert({builtin_u32.Struct, builtin_s16.Struct, /*CastKind::u32_s16, */false});
	built_in_casts.insert({builtin_u32.Struct, builtin_s32.Struct, /*CastKind::u32_s32, */false});
	built_in_casts.insert({builtin_u32.Struct, builtin_s64.Struct, /*CastKind::u32_s64, */true});
	built_in_casts.insert({builtin_u32.Struct, builtin_u8 .Struct, /*CastKind::u32_u8 , */false});
	built_in_casts.insert({builtin_u32.Struct, builtin_u16.Struct, /*CastKind::u32_u16, */false});
	built_in_casts.insert({builtin_u32.Struct, builtin_u64.Struct, /*CastKind::u32_u64, */true});
	built_in_casts.insert({builtin_u64.Struct, builtin_s8 .Struct, /*CastKind::u64_s8 , */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_s16.Struct, /*CastKind::u64_s16, */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_s32.Struct, /*CastKind::u64_s32, */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_s64.Struct, /*CastKind::u64_s64, */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_u8 .Struct, /*CastKind::u64_u8 , */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_u16.Struct, /*CastKind::u64_u16, */false});
	built_in_casts.insert({builtin_u64.Struct, builtin_u32.Struct, /*CastKind::u64_u32, */false});
	built_in_casts.insert({builtin_s8 .Struct, builtin_s16.Struct, /*CastKind::s8_s16 , */true});
	built_in_casts.insert({builtin_s8 .Struct, builtin_s32.Struct, /*CastKind::s8_s32 , */true});
	built_in_casts.insert({builtin_s8 .Struct, builtin_s64.Struct, /*CastKind::s8_s64 , */true});
	built_in_casts.insert({builtin_s8 .Struct, builtin_u8 .Struct, /*CastKind::s8_u8  , */false});
	built_in_casts.insert({builtin_s8 .Struct, builtin_u16.Struct, /*CastKind::s8_u16 , */false});
	built_in_casts.insert({builtin_s8 .Struct, builtin_u32.Struct, /*CastKind::s8_u32 , */false});
	built_in_casts.insert({builtin_s8 .Struct, builtin_u64.Struct, /*CastKind::s8_u64 , */false});
	built_in_casts.insert({builtin_s16.Struct, builtin_s8 .Struct, /*CastKind::s16_s8 , */false});
	built_in_casts.insert({builtin_s16.Struct, builtin_s32.Struct, /*CastKind::s16_s32, */true});
	built_in_casts.insert({builtin_s16.Struct, builtin_s64.Struct, /*CastKind::s16_s64, */true});
	built_in_casts.insert({builtin_s16.Struct, builtin_u8 .Struct, /*CastKind::s16_u8 , */false});
	built_in_casts.insert({builtin_s16.Struct, builtin_u16.Struct, /*CastKind::s16_u16, */false});
	built_in_casts.insert({builtin_s16.Struct, builtin_u32.Struct, /*CastKind::s16_u32, */false});
	built_in_casts.insert({builtin_s16.Struct, builtin_u64.Struct, /*CastKind::s16_u64, */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_s8 .Struct, /*CastKind::s32_s8 , */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_s16.Struct, /*CastKind::s32_s16, */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_s64.Struct, /*CastKind::s32_s64, */true});
	built_in_casts.insert({builtin_s32.Struct, builtin_u8 .Struct, /*CastKind::s32_u8 , */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_u16.Struct, /*CastKind::s32_u16, */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_u32.Struct, /*CastKind::s32_u32, */false});
	built_in_casts.insert({builtin_s32.Struct, builtin_u64.Struct, /*CastKind::s32_u64, */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_s8 .Struct, /*CastKind::s64_s8 , */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_s16.Struct, /*CastKind::s64_s16, */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_s32.Struct, /*CastKind::s64_s32, */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_u8 .Struct, /*CastKind::s64_u8 , */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_u16.Struct, /*CastKind::s64_u16, */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_u32.Struct, /*CastKind::s64_u32, */false});
	built_in_casts.insert({builtin_s64.Struct, builtin_u64.Struct, /*CastKind::s64_u64, */false});

	built_in_casts.insert({builtin_f32.Struct, builtin_s32.Struct, false});
	built_in_casts.insert({builtin_s32.Struct, builtin_f32.Struct, false});

	built_in_casts.insert({builtin_f64.Struct, builtin_s64.Struct, false});
	built_in_casts.insert({builtin_s64.Struct, builtin_f64.Struct, false});

	empty_statement = AstEmptyStatement::create();

	current_printer = standard_output_printer;

	timed_end("setup"str);

	{
		defer {
			if (args.print_ast_after_parse) {
				print_ast();
			}
		};

		scoped_phase("Parsing");

		auto parsed = parse_file(concatenate(context.compiler_directory, "\\libs\\preload.tl"str));
		assert_always(parsed->result != ParseResult::read_error);
		// global_scope.append(parsed->scope);


		for (auto path : args.source_files) {
			parsed = parse_file(path);
			if (parsed->result == ParseResult::read_error) {
				return 1;
			}

			// global_scope.append(parsed->scope);
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
			if (args.print_ast_after_typecheck) {
				print_ast();
			}
			if (context.print_lowered) {
				print_lowered();
			}
		};

		scoped_phase("Typechecking");

		timed_block(context.profiler, "typecheck"str);

		auto main_fiber = ConvertThreadToFiber(0);

		Span<TypecheckState> typecheck_states;
		typecheck_states.count = count(global_scope.statements, [&](AstStatement *statement) { return !(statement->kind == Ast_Definition && ((AstDefinition *)statement)->built_in); });
		if (typecheck_states.count) {
			typecheck_states.data = default_allocator.allocate<TypecheckState>(typecheck_states.count);

			u32 typechecks_finished = 0;
			bool fail = false;
			u32 state_index = 0;

#if !USE_FIBERS
			u64 coro_mem = 0;
			u64 peak_coro_mem = 0;
			defer {
				if (context.do_profile) {
					print("Peak coroutine memory: {}\n", format_bytes(peak_coro_mem));
				}
			};
#endif

			u64 const coro_size = 1*MiB;

			for (auto statement : global_scope.statements)  {
				if (statement->kind == Ast_Definition && ((AstDefinition *)statement)->built_in)
					continue;

				auto &state = typecheck_states[state_index];
				state.statement = statement;
				++state_index;
			}

			struct FiberPoolEntry {
				TypecheckState *state;
				void *fiber;
			};

			List<FiberPoolEntry> fiber_pool;

			while (1) {
				for (u32 i = 0; i < typecheck_states.count; ++i) {
					if (typechecks_finished == typecheck_states.count) {
						goto typecheck_break;
					}

					auto &state = typecheck_states[i];

#if USE_FIBERS
					if (!state.fiber) {
						state.parent_fiber = main_fiber;
						if (fiber_pool.count) {
							auto pooled = fiber_pool.pop();
							state.fiber = pooled.fiber;
							pooled.state->next_state = &state;
						} else {
							state.fiber = CreateFiber(4096, typecheck_fiber, &state);
							if (!state.fiber) {
								immediate_error(state.statement->location, "INTERNAL COMPILER ERROR: Failed to create fiber");
								return -1;
							}
						}
					}
#else
					if (!state.coro) {
						coro_mem += coro_size;
						if (peak_coro_mem < coro_mem)
							peak_coro_mem = coro_mem;
						auto init_result = coro_init(&state.coro, typecheck_coroutine, coro_size);
						if (init_result.is_error) {
							immediate_error(state.statement->location, "INTERNAL COMPILER ERROR: {}", init_result.error.message);
							return -1;
						}
					}
#endif

					if (state.finished)
						continue;
#if USE_FIBERS
					SWITCH_TO_FIBER(state.fiber);
					auto result = fiber_result;
#else
#undef YIELD_STATE
#define YIELD_STATE state.coro
					auto result = (TypecheckResult)(int)yield(&state);
#endif

					switch (result) {
						case TypecheckResult::success:
						case TypecheckResult::fail:
							state.finished = true;
							typechecks_finished++;
							if (result == TypecheckResult::fail) {
								fail = true;
								for (auto call : reverse(state.poly_call_stack)) {
									StringBuilder builder;
									append(builder, call->callable->location);
									append(builder, '(');
									for (auto &arg : call->unsorted_arguments) {
										if (&arg != call->unsorted_arguments.begin()) {
											append(builder, ", ");
										}
										append_type(builder, arg.expression->type, true);
									}
									append(builder, ')');
									state.reporter.info(call->location, "While resolving {}", to_string(builder));
								}
							}
							state.reporter.print_all();
#if USE_FIBERS
							fiber_pool.add({&state, state.fiber});
							state.next_state = 0;
							// DeleteFiber(state.fiber);
#else
							coro_mem -= coro_size;
							coro_free(&state.coro);
#endif
							break;
						case TypecheckResult::wait:
							break;
					}
				}
			}
		typecheck_break:;
			if (fail) {
				print(Print_error, "Typechecking failed.\n");
				return 1;
			}
		}
	}

	typecheck_finished = true;

	if (args.no_typecheck) {
		return 1;
	}

	auto found_build_definitions = global_scope.definitions.find("build"str);
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
			context.build_lambda = get_lambda(build_definition->expression);
			s64 test = 69;
			// invoke(build_lambda, &test);
			print(Print_error, "test is {}\n", test);
		}
	}

	auto found_main_definitions = global_scope.definitions.find("main"str);
	if (found_main_definitions) {
		auto main_definitions = found_main_definitions->value;
		if (main_definitions.count != 1) {
			immediate_error("Found multiple 'main' definitions");
			for (auto definition : main_definitions) {
				immediate_info(definition->location, "Here");
			}
			return 1;
		}
		auto main_definition = main_definitions[0];
		if (!is_lambda(main_definition->expression)) {
			immediate_error(main_definition->location, "'main' is not a lambda");
			return 1;
		}
		context.main_lambda = get_lambda(main_definition->expression);

		if (!::is_integer(context.main_lambda->return_parameter->type) && !types_match(context.main_lambda->return_parameter->type, builtin_void)) {
			immediate_error(context.main_lambda->location, "Main function can return any type of integer or void, but not {}", type_to_string(context.main_lambda->return_parameter->type));
			return 1;
		}
	} else {
		immediate_error("Entry function was not defined.");
		return 1;
	}

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

		build(context, bytecode);
	}

	if (args.stats) {
		print("Total token count: {}\n", total_tokens_parsed);
	}

	return 0;
}
