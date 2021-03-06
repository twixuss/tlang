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
	if (result == TypecheckResult::wait) {
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

void print_help() {
    print(compiler.strings.usage, compiler.compiler_name);
}

u64 printed_reports_count = 0;

struct Reporter {
	List<Report> reports;

	void print_all() {
		printed_reports_count += reports.count;
		for (auto report : reports) {
			compiler.print_report(report);
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
	timed_function(compiler.profiler);

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
	AstNode *container_node = 0;
	Reporter *reporter;
	String extern_language;
	String extern_library;
	Scope *current_scope = &global_scope;
	u32 scope_count = 0;
	CallingConvention current_convention = CallingConvention::tlang;
	StructLayout current_struct_layout = StructLayout::tlang;
	AstIdentifier *poly_identifier = 0;
	AstWhile *current_loop = 0;

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
u32 untypechecked_binary_operators_count;
u32 untypechecked_has_value_overloads_count;

List<AstLambda *> implicit_casts;
List<AstLambda *> explicit_casts;
Map<BinaryOperation, List<AstLambda *>> binary_operators;
List<AstLambda *> has_value_overloads;

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

AstLiteral *make_bool(bool val) {
	auto result = AstLiteral::create();
	result->literal_kind = LiteralKind::boolean;
	result->Bool = val;
	result->type = builtin_bool.ident;
	return result;
}

AstLiteral *make_float(f64 value, AstExpression *type = builtin_unsized_float.ident) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::Float;
	i->Float = value;
	i->type = type;
	return i;
}

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser, int right_precedence = 0);
AstExpression *parse_expression_1(Parser *parser);
AstDefinition *parse_definition(Parser *parser);

void push_scope_check(Scope *scope) {
	assert(scope);
	switch (scope->node->kind) {
		case Ast_Lambda: {
			auto lambda = (AstLambda *)scope->node;
			if (scope == &lambda->body_scope || scope == &lambda->parameter_scope) {
				// these already have their parents set.
				return;
			}
			break;
		}
		case Ast_Struct: {
			auto Struct = (AstStruct *)scope->node;
			if (scope == &Struct->member_scope) {
				// these already have their parents set.
				return;
			}
			break;
		}
	}
	assert(!scope->parent);
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
	retparam->definition_location = LambdaDefinitionLocation::return_parameter;
	retparam->container_node = parent;
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

			name.add(Span(remaining.begin(), slash));

			auto w = slash + 1;
			while (is_whitespace((ascii)*w)) { // NOTE: if non-ascii whitespace is used, this will not work.
				++w;
			}

			remaining.set_begin(w);
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
	definition->type = type;
	definition->container_node = parser->container_node;

	return definition;
}

AstDefinition *create_definition_in_current_scope(Parser *parser, String name, AstExpression *type) {

	auto definition = create_definition(parser, name, type);

	if (name != "_"str) {
		scoped_lock(parser->current_scope);

		add_to_scope(definition, parser->current_scope);
	}

	return definition;
}

AstStruct *parse_struct(Parser *parser, String token) {
	auto Struct = AstStruct::create();
	Struct->location = token;

	scoped_replace(parser->container_node, Struct);

	if (parser->token->kind == '(') {
		parser->next();

		Struct->is_template = true;

		push_scope(&Struct->parameter_scope);

		if (parser->token->kind != ')') {
			for (;;) {
				auto name = parse_identifier(parser);
				if (!name.data)
					return 0;

				auto name_location = parser->token->string;

				parser->next();

				// TODO: remove copypasta

				AstDefinition *definition = 0;
				if (!parser->expect(':'))
					return 0;

				parser->next();

				definition = create_definition_in_current_scope(parser, name, 0);
				// NOTE: don't mark definition as constant yet because we don't know the expression.

				parser->poly_identifier = 0;

				definition->type = parse_expression(parser);
				if (!definition->type)
					return 0;

				definition->location = {name_location.begin(), definition->type->location.end()};

				definition->definition_location = LambdaDefinitionLocation::parameter;
				definition->container_node = Struct;

				for (auto &other_parameter : Struct->parameters) {
					if (definition->name != "_"str && definition->name == other_parameter->name) {
						parser->reporter->error(definition->location, "Can't use identical names for different parameters.");
						return 0;
					}
				}

				Struct->parameters.add(definition);

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

	parser->next();

	push_scope(&Struct->member_scope);

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

inline AstUnaryOperator *make_unary(UnaryOperation operation) {
	auto unop = AstUnaryOperator::create();
	unop->operation = operation;
	return unop;
}
inline AstUnaryOperator *make_sizeof() { return make_unary(UnaryOperation::Sizeof); }
inline AstUnaryOperator *make_typeof() { return make_unary(UnaryOperation::typeof); }

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

	parser->next();

	scoped_replace(parser->container_node, lambda);

	push_scope(&lambda->type_scope);
	push_scope(&lambda->parameter_scope);

	if (parser->token->kind != ')') {
		for (;;) {
			if (parser->token->kind == 'this') {
				lambda->is_member = true;

				parser->next();

				if (parser->token->kind == ':') {
					parser->next();
					if (parser->token->kind != ':') {
						parser->reporter->error(parser->token->string, "Expected ':', but got '{}'", token_kind_to_string(parser->token->kind));
						return 0;
					}
					parser->next();

					lambda->insert_into = parse_expression(parser);
					if (!lambda->insert_into) {
						return 0;
					}
				}

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

			auto name = parse_identifier(parser);
			if (!name.data)
				return 0;

			auto name_location = parser->token->string;

			parser->next();

			AstDefinition *definition = 0;
			if (parser->token->kind == ':') {
				parser->next();

				definition = create_definition_in_current_scope(parser, name, 0);

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
				if (definition->type->kind == Ast_UnaryOperator && ((AstUnaryOperator *)definition->type)->operation == UnaryOperation::pack) {
					definition->is_pack = true;
					definition->type = make_span_type(((AstUnaryOperator *)definition->type)->expression);
					lambda->has_pack = true;
				}
			} else {
				definition = create_definition_in_current_scope(parser, name, builtin_poly.ident);
				definition->location = name_location;
				lambda->is_poly = true;
				definition->is_poly = true;
			}

			definition->definition_location = LambdaDefinitionLocation::parameter;
			definition->container_node = lambda;

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
				lambda->return_parameter->definition_location = LambdaDefinitionLocation::return_parameter;
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
			lambda->return_parameter->name = u8"<ret>"s;
			lambda->return_parameter->location = return_type->location;
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
		} else {
			parser->reporter->error(parser->token->string, "Unknown directive.");
			return 0;
		}
		parser->next();
	}

	if (lambda->convention == CallingConvention::none) {
		lambda->convention = parser->current_convention;
	}

	lambda->location = {lambda->location.begin(), parser->token[-1].string.end()};

	if (!lambda->is_type) {
		if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';') {
			parser->reporter->error(parser->token->string, "Expected '{{' or '=>' or ';' or ':' instead of '{}'.", parser->token->string);
			return 0;
		}
	}

	bool is_short = false;

	if (lambda->is_type) {
		lambda->has_body = false;
		if (parser->token->kind == '{' || parser->token->kind == '=>') {
			parser->reporter->error(lambda->location, "Body of a lambda can not be specified after a #type directive.");
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
	return lambda;
}

bool should_parse_lambda(Parser *parser) {
	return parser->token[1].kind == ')' || parser->token[2].kind == ':';
}

AstExpression *parse_expression_0(Parser *parser) {
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
			identifier->name = parse_identifier(parser);
			assert(identifier->name.data);
			parser->next();
			return identifier;
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
				auto name = parse_identifier(parser);
				if (!name.data)
					return 0;

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
			if (should_parse_lambda(parser)) {
				auto lambda = parse_lambda(parser);
				if (!lambda)
					return 0;

				return lambda;
			}

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
				if (parser->token->string == "#file"str) {
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
				auto kind_string = token_kind_to_string(parser->token->kind);
				if (kind_string == parser->token->string) {
					parser->reporter->error(parser->token->string, "Unexpected token: {}", kind_string);
				} else {
					parser->reporter->error(parser->token->string, "Unexpected token: {} ({})", parser->token->string, kind_string);
				}
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
								case lt:  expression = make_bool(left < right); return true;
								case gt:  expression = make_bool(left > right); return true;
								case le: expression = make_bool(left <= right); return true;
								case ge: expression = make_bool(left >= right); return true;
								case ne: expression = make_bool(left != right); return true;
								case eq: expression = make_bool(left == right); return true;
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
AstExpression *parse_expression(Parser *parser, int right_precedence) {
	//null denotation
	AstExpression *left = parse_expression_1(parser);
	if (!left)
		return 0;

	// left binding power
	Optional<BinaryOperation> operation;
	while ((operation = as_binary_operation(parser->token->kind)) && right_precedence < get_precedence(operation.value())) {
		parser->next();

		auto binop = AstBinaryOperator::create();
		binop->left = left;
		binop->operation = operation.value();

		//left denotation
		if (is_right_associative(operation.value())) {
			binop->right = parse_expression(parser, get_precedence(binop->operation) - 1);
		} else {
			binop->right = parse_expression(parser, get_precedence(binop->operation));
		}
		if (!binop->right)
			return 0;

		update_location(binop);

		left = binop;
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

	if (parser->container_node && parser->container_node->kind == Ast_Lambda) {
		definition->definition_location = LambdaDefinitionLocation::body;
	}

	return definition;
}
AstDefinition *parse_definition(Parser *parser) {
	auto location = parser->token->string;
	auto name = parse_identifier(parser);
	if (!name.data)
		return 0;

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
	timed_function(compiler.profiler);

	bool do_print_ast = false;
	if (parser->token->kind == Token_directive) {
		if (parser->token->string == "#print_ast_parse") {
			do_print_ast = true;
			parser->next();
		}
	}
	defer {
		if (do_print_ast) {
			print_ast(result);
		}
	};

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
			auto name = parse_identifier(parser);
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

			if (parser->container_node->kind != Ast_Lambda) {
				parser->reporter->error(ret->location, "Return statement can only be used in lambda body.");
				return;
			}

			ret->lambda = (AstLambda *)parser->container_node;
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

			scoped_replace(parser->current_loop, While);

			if (!parse_block_or_single_statement(parser, &While->scope)) {
				return;
			}
			result = While;
			return;
		}
		case Token_for: {
			auto For = AstFor::create();
			For->location = parser->token->string;
			parser->next();
			auto condition = parse_expression(parser);
			if (!condition) {
				return;
			}
			For->condition = condition;

			if (!parse_block_or_single_statement(parser, &For->scope)) {
				return;
			}
			result = For;
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
			} else if (parser->token->string == "#insert") {
				parser->reporter->error(parser->token->string, "Inserting code is not implemeted yet.");
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

				result = make_statement(lambda);
				return;
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
					definition->operation = parser->token->kind;

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

					if (definition->lambda->parameters.count != 2) {
						parser->reporter->error(definition->lambda->location, "'{}' operator must have exactly two parameters.", definition->location);
						return;
					}

					untypechecked_binary_operators_count += 1;

					result = definition;
					return;
				}
				case 'as': {
					parser->next();

					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = 'as';
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

					auto lambda = parse_lambda(parser);
					if (!lambda)
						return;
					if (lambda->kind != Ast_Lambda) {
						parser->reporter->error(lambda->location, "Expected a lambda expression.");
						return;
					}

					definition->lambda = (AstLambda *)lambda;

					if (definition->lambda->parameters.count != 1) {
						parser->reporter->error(lambda->location, "'as' operator must have exactly one parameter.");
						return;
					}

					result = definition;
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

					untypechecked_has_value_overloads_count += 1;

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

			if (!parser->expect(';'))
				return;

			parser->next();

			result = assert;
			return;
		}
		case Token_break:
		case Token_continue: {
			if (!parser->current_loop) {
				parser->reporter->error(parser->token->string, "Loop control statement may appear only inside loop body.");
				return;
			}

			auto i = AstLoopControl::create();
			i->control = parser->token->kind == Token_break ? LoopControl::Break : LoopControl::Continue;
			i->location = parser->token->string;
			i->loop = parser->current_loop;

			parser->next();

			if (parser->token->kind == ';') {
				parser->next();
			} else {
				auto ident = parse_identifier(parser);
				if (!ident.data)
					return;
				if (!parser->next_expect(';'))
					return;
				not_implemented("labeled loop control statements are not implemented yet");
				// i->label = ident;
			}

			result = i;
			return;
		}
		case Token_match: {
			auto Match = AstMatch::create();
			Match->location = parser->token->string;
			parser->next();
			Match->expression = parse_expression(parser);
			if (!Match->expression) {
				return;
			}

			if (!parser->expect('{'))
				return;
			parser->next();

			while (parser->token->kind != '}') {
				auto &Case = Match->cases.add();
				Case.scope.node = Match;

				if (parser->token->kind == 'else') {
					if (Match->default_case) {
						parser->reporter->error(parser->token->string, "Redefinition of else block");
						parser->reporter->info(Match->default_case_location, "Previously defined here");
						return;
					}
					Match->default_case_location = parser->token->string;
					Match->default_case = &Case;

					parser->next();
				} else {
					Case.expression = parse_expression(parser);
					if (!Case.expression)
						return;
				}

				if (parser->token->kind == '=>')
					parser->next();

				if (!parse_block_or_single_statement(parser, &Case.scope)) {
					return;
				}
			}
			parser->next();

			result = Match;
			return;
		}
		case Token_using: {
			auto Using = AstUsing::create();
			Using->location = parser->token->string;
			parser->next();
			Using->expression = parse_expression(parser);
			if (!Using->expression) {
				return;
			}
			if (!parser->expect(';'))
				return;

			parser->next();
			result = Using;
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
	timed_function(compiler.profiler);
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
	timed_function(compiler.profiler);

	if (auto found = parsed_files.find(path)) {
		return found->value;
	}

	// print("Parsing {}\n", path);

	auto compiler =
	parsed_files.get_or_insert(path) =
	default_allocator.allocate<SourceFileContext>();

	compiler->lexer.source_buffer = read_entire_file(to_pathchars(path), {.extra_space_before=1, .extra_space_after=1});
	if (!compiler->lexer.source_buffer.data) {
		print("Failed to read '{}'. Exiting.\n", path);
		compiler->result = ParseResult::read_error;
		return compiler;
	}

	compiler->lexer.source_buffer.front() = '\0';
	compiler->lexer.source_buffer.back() = '\0';

	String source = String(compiler->lexer.source_buffer);
	source.data += 1;
	source.count -= 2;

	auto bom = Span(compiler->lexer.source_buffer.data + 1, (umm)3);
	if (bom.end() <= compiler->lexer.source_buffer.end() && bom == "\xef\xbb\xbf"b) {
		bom.back() = '\0';
		source.data += 3;
		source.count -= 3;
	}
	compiler->lexer.source = source;

	// compiler->scope.parent = &global_scope;

	compiler->parser.lexer = &compiler->lexer;
	compiler->parser.reporter = compiler->lexer.reporter = &compiler->reporter;
	// compiler->parser.current_scope = &compiler->scope;

	auto source_info = &::compiler.sources.add({path, source});

	compiler->lexer.source_info = source_info;

	constexpr auto max_token_count = (1*GiB)/sizeof(Token);

	compiler->lexer.tokens_start = (Token *)VirtualAlloc(0, max_token_count*sizeof(Token), MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
	compiler->lexer.tokens_end = compiler->lexer.tokens_start + max_token_count;
	compiler->lexer.token_cursor = compiler->lexer.tokens_start;

	defer {
		compiler->reporter.print_all();
	};

	if (!lexer_function(&compiler->lexer)) {
		atomic_set_if_equals(failed_lexer, &compiler->lexer, (Lexer *)0);
		compiler->result = ParseResult::syntax_error;
		return compiler;
	}
	auto parse_result = parser_function(&compiler->parser);
	if (parse_result != ParseResult::ok) {
		atomic_set_if_equals(failed_parser, &compiler->parser, (Parser *)0);
		compiler->result = parse_result;
		return compiler;
	}

	atomic_add(&total_tokens_parsed, compiler->lexer.token_cursor - compiler->lexer.tokens_start);

	return compiler;
}

ParseResult parser_function(Parser *parser) {
	timed_function(compiler.profiler);

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
			auto child = parse_file((String)concatenate(compiler.compiler_directory, "\\libs\\", libname));
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
	timed_function(compiler.profiler);
	auto result = AstLiteral::create();
	result->literal_kind = LiteralKind::type;
	result->type_value = type;
	result->type = builtin_type.ident;
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
	timed_function(compiler.profiler);
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
	span->location = type->location;
	span->expression = type;
	span->type = builtin_type.ident;
	return span;
}

void typecheck(TypecheckState *state, CExpression auto &expression);

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

bool ensure_addressable(Reporter *reporter, AstExpression *expression) {
	timed_function(compiler.profiler);
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

void wait_for(TypecheckState *state, auto &&predicate, auto &&error_callback) {
	while (1) {
		if (predicate()) {
			state->no_progress_counter = 0;
			break;
		}

		if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) {
			error_callback();
			yield(TypecheckResult::fail);
		}
		state->no_progress_counter += 1;
		yield(TypecheckResult::wait);
	}
}

s64 get_size(TypecheckState *state, AstExpression *type) {
	s64 result;
	wait_for(state,
		[&] {
			result = get_size(type, false);
			return result != -1;
		}, [&] {
			state->reporter.error(type->location, "Failed to compute size.");
		}
	);
	assert(result > 0);
	return result;
}
s64 get_align(TypecheckState *state, AstExpression *type) {
	s64 result;
	wait_for(state,
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

void evaluate_and_put_definition_in_section(TypecheckState *state, AstDefinition *definition, Section &section) {
	definition->evaluated = evaluate(state, definition->expression);
	put_arrays_in_section(definition->evaluated, section);
	definition->offset = put_in_section(definition->evaluated, section);
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
};

TypeKind get_type_kind(AstExpression *type) {
	using enum TypeKind;
	if (types_match(type, builtin_void)) return Void;
	if (types_match(type, builtin_bool)) return Bool;
	if (types_match(type, builtin_u8))   return U8;
	if (types_match(type, builtin_u16))  return U16;
	if (types_match(type, builtin_u32))  return U32;
	if (types_match(type, builtin_u64))  return U64;
	if (types_match(type, builtin_s8))   return S8;
	if (types_match(type, builtin_s16))  return S16;
	if (types_match(type, builtin_s32))  return S32;
	if (types_match(type, builtin_s64))  return S64;
	if (types_match(type, builtin_f32))  return F32;
	if (types_match(type, builtin_f64))  return F64;
	if (direct_as<AstStruct>(type))      return Struct;
	if (direct_as<AstEnum>(type))        return Enum;
	if (auto unop = direct_as<AstUnaryOperator>(type)) {
		switch (unop->operation) {
			using enum UnaryOperation;
			case pointer: return Pointer;
			case pack: return Span;
		}
	}
	if (direct_as<AstSpan>(type)) return Span;
	if (direct_as<AstSubscript>(type)) return Array;
	if (direct_as<AstLambdaType>(type)) return Pointer;

	invalid_code_path();
}

AstCall *make_struct_initializer(AstExpression *type, std::initializer_list<AstExpression *> arguments, String location) {
	auto Struct = direct_as<AstStruct>(type);
	assert(Struct);

	auto create_initializer = [&]() {
		auto initializer = AstCall::create();
		initializer->callable = type;
		initializer->sorted_arguments.resize(Struct->data_members.count);
		initializer->type = type;
		initializer->location = location;
		return initializer;
	};

	auto initializer = create_initializer();

	assert(arguments.size() == initializer->sorted_arguments.count);

	for (umm i = 0; i < arguments.size(); ++i) {
		initializer->sorted_arguments[i] = arguments.begin()[i];
		assert(types_match(arguments.begin()[i]->type, Struct->data_members[i]->type));
	}

	return initializer;
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
		set_member(typeinfo_initializer, "kind", make_integer(location, (s64)get_type_kind(directed)));
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
					set_member(member_initializer, "offset", make_integer(location, (u64)s->offset));

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
		set_member(typeinfo_initializer, "array_count", 0);
		set_member(typeinfo_initializer, "size", make_integer(location, get_size(type)));

		if (auto subscript = (AstSubscript *)directed; directed->kind == Ast_Subscript) {
			if (is_constant(subscript->index_expression)) {
				set_member(typeinfo_initializer, "array_count", subscript->index_expression);
			}
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, subscript->expression, location));
		} else if (auto span = (AstSpan *)directed; directed->kind == Ast_Span) {
			set_member(typeinfo_initializer, "pointee", get_typeinfo(state, span->expression, location));
		}

		definition->expression = typeinfo_initializer;

		evaluate_and_put_definition_in_section(state, definition, compiler.constant_section);
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
bool implicitly_cast(TypecheckState *state, Reporter *reporter, CExpression auto *_expression, AstExpression *type, int *conversion_distance = 0, bool apply = true) {
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

	if (expression->kind == Ast_UnaryOperator) {
		auto unop = (AstUnaryOperator *)expression;
		if (unop->operation == UnaryOperation::pack && types_match(unop->expression->type, type)) {
			return true;
		}
	}

	if (types_match(type, builtin_any)) {
		if (conversion_distance)
			*conversion_distance = 3;

		if (apply) {
			harden_type(expression);

			auto initializer = AstCall::create();
			initializer->callable = builtin_any.ident;
			initializer->location = expression->location;
			initializer->type = builtin_any.ident;

			initializer->sorted_arguments.resize(2);

			if (is_addressable(expression)) {
				initializer->sorted_arguments[0] = make_address_of(reporter, expression);
				if (!initializer->sorted_arguments[0])
					yield(TypecheckResult::fail);
			} else {
				auto temporary = AstUnaryOperator::create();

				temporary->expression = expression;
				temporary->location = expression->location;
				temporary->operation = UnaryOperation::internal_move_to_temporary;
				temporary->type = builtin_void.pointer;

				initializer->sorted_arguments[0] = temporary;
			}

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
		if (expression->kind == Ast_Literal) {
			auto literal = (AstLiteral *)expression;
			if (literal->literal_kind == LiteralKind::null) {
				if (apply)
					expression->type = type;
				return true;
			}
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

bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
	timed_function(compiler.profiler);
	switch (expression->kind) {
		case Ast_Identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition();
			if (definition->is_constant) {
				reporter->error(identifier->location, "Can't assign to '{}' because it is constant.", identifier->location);
				return false;
			}
			if (definition->definition_location == LambdaDefinitionLocation::parameter) {
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
	assert(lambda->parameters_size == -1, "redundant work");
	s64 parameter_size_accumulator = 0;

	//if (where(lambda->location.data) == "main.tl:52:25"str)
	//	debug_break();

	if (lambda->is_member)
		parameter_size_accumulator += compiler.stack_word_size;

	for (auto parameter : lambda->parameters) {
		if (parameter->is_constant)
			continue;

		parameter->offset = parameter_size_accumulator;
		parameter_size_accumulator += compiler.stack_word_size;
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

void set_local_offset(TypecheckState *state, AstDefinition *definition, AstLambda *lambda) {
	assert((lambda->locals_size & 0xf) == 0);
	definition->offset = lambda->locals_size;
	auto size = ceil(get_size(state, definition->type), 16ll);
	lambda->locals_size += size;
}

void typecheck(TypecheckState *state, AstStatement *statement);
void typecheck(TypecheckState *state, AstDefinition *definition);

void typecheck(TypecheckState *state, Scope *scope) {
	push_scope(scope);
	for (auto statement : scope->statements) {
		typecheck(state, statement);
	}
}

void typecheck(TypecheckState *state, AstDefinition *definition) {
	if (definition->built_in) {
		return;
	}

	scoped_replace(state->currently_typechecking_definition, definition);

	auto lambda = state->current_lambda;

	bool is_parameter = definition->definition_location == LambdaDefinitionLocation::parameter;
	enum {
		Global,
		Lambda,
		Struct,
		Enum,
	} location;

	if (!definition->container_node) {
		// assert(!state->current_lambda_or_struct_or_enum, "test");
		definition->container_node = state->current_lambda_or_struct_or_enum;
	}

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
				evaluate_and_put_definition_in_section(state, definition, compiler.constant_section);
			}
		}
	} else {
		switch (location) {
			case Global: {
				if (definition->expression) {
					if (!is_type(definition->expression) && definition->expression->kind != Ast_Lambda) {
						evaluate_and_put_definition_in_section(state, definition, definition->is_constant ? compiler.constant_section : compiler.data_section);
					}
				} else {
					auto offset = compiler.zero_section_size;

					offset = ceil(offset, get_align(definition->type));
					definition->offset = offset;
					offset += get_size(definition->type);

					compiler.zero_section_size = offset;
				}
				break;
			}
			case Lambda: {
				if (definition->definition_location != LambdaDefinitionLocation::parameter &&
					definition->definition_location != LambdaDefinitionLocation::return_parameter
					)
 					set_local_offset(state, definition, state->current_lambda);
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
						state->reporter.warning(ret->location,
							"No return expression provided for an unnamed return parameter. "
							"When you don't assign a value to the return parameter, the default value will be returned. "
							"To remove this warning you can either provide a return expression or give the return parameter a name or just ignore it.");
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
				//     typecheck(state, scope);
				//
				// We use this:
				//
				for (auto statement : scope->statements) {
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

				auto found = find(scope->parent->children, scope);
				assert(found);
				erase(scope->parent->children, found);
				scope->parent->append(*scope);
			} else {
				typecheck(state, &If->true_scope);
				typecheck(state, &If->false_scope);
			}

			break;
		}
		case Ast_While: {
			auto While = (AstWhile *)statement;

			typecheck(state, While->condition);
			if (!implicitly_cast(state, &state->reporter, &While->condition, builtin_bool.ident)) {
				yield(TypecheckResult::fail);
			}

			typecheck(state, &While->scope);

			break;
		}
		case Ast_For: {
			state->reporter.error(statement->location, "For is not implemented");
			yield(TypecheckResult::fail);

			auto For = (AstFor *)statement;

			typecheck(state, For->condition);
			if (!implicitly_cast(state, &state->reporter, &For->condition, builtin_range.ident)) {
				yield(TypecheckResult::fail);
			}

			AstDefinition *it = AstDefinition::create();
			it->name = "it"str;
			it->location = "\0\0it\0\0"str;
			it->location.data += 2;
			it->location.count -= 4;
			set_local_offset(state, it, state->current_lambda);

			For->scope.definitions.get_or_insert(it->name).add(it);
			For->scope.statements.add(it);

			typecheck(state, &For->scope);

			break;
		}
		case Ast_ExpressionStatement: {
			auto es = (AstExpressionStatement *)statement;
			typecheck(state, es->expression);
			break;
		}
		case Ast_Block: {
			auto block = (AstBlock *)statement;
			typecheck(state, &block->scope);
			break;
		}
		case Ast_Assert: {
			auto assert = (AstAssert *)statement;
			typecheck(state, assert->condition);

			if (!implicitly_cast(state, &state->reporter, &assert->condition, builtin_bool.Struct))
				yield(TypecheckResult::fail);

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

			typecheck(state, &Defer->scope);

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
				case 'as': {
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
				case '+':
				case '-':
				case '*':
				case '/':
				case '%':
				case '^':
				case '&':
				case '|':
				case '<<':
				case '>>': {
					typecheck(state, (Expression<> &)op->lambda);
					binary_operators.get_or_insert(as_binary_operation(op->operation).value()).add(op->lambda);
					untypechecked_binary_operators_count -= 1;
					break;
				}
				case '?': {
					typecheck(state, (Expression<> &)op->lambda);
					has_value_overloads.add(op->lambda);
					untypechecked_has_value_overloads_count -= 1;
					break;
				}
				default: invalid_code_path();
			}
			break;
		}
		case Ast_LoopControl: {
			break;
		}
		case Ast_Parse: {
			break;
		}
		case Ast_Match: {
			auto Match = (AstMatch *)statement;
			typecheck(state, Match->expression);
			for (auto &Case : Match->cases) {
				if (Case.expression) {
					typecheck(state, Case.expression);
					if (!implicitly_cast(state, &state->reporter, &Case.expression, Match->expression->type)) {
						// state->reporter.error(Case.expression->location, "Expression has type {}, which does not match the matchable type {}.", type_to_string(Case.expression->type), type_to_string(Match->expression->type));
						yield(TypecheckResult::fail);
					}
				}
				typecheck(state, &Case.scope);
			}
			break;
		}
		case Ast_Using: {
			auto Using = (AstUsing *)statement;
			typecheck(state, Using->expression);

			if (Using->expression->kind != Ast_Identifier) {
				state->reporter.error(statement->location, "Using can be applied to identifiers only.");
				yield(TypecheckResult::fail);
			}
			auto Ident = (AstIdentifier *)Using->expression;

			auto Struct = direct_as<AstStruct>(Ident->type);
			if (!Struct) {
				state->reporter.error(statement->location, "Using can be applied to identifiers with type `struct`.");
				yield(TypecheckResult::fail);
			}

			Using->definition = Ident->definition();

			state->current_scope->usings.add(Using);

			break;
		}
		default: {
			state->reporter.error(statement->location, "INTERNAL ERROR: Invalid statement kind");
			yield(TypecheckResult::fail);
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
		if (untypechecked_binary_operators_count == 0)
			return 0;
		++state->no_progress_counter;
		yield(TypecheckResult::wait);
	}
	return 0;
}

void typecheck(TypecheckState *, CExpression auto &);
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
		typecheck(state, &lambda->body_scope);
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
		compiler.lambdas_with_body.add(lambda);
	} else {
		compiler.lambdas_without_body.add(lambda);
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

			if (arguments.count != resolved.call->unsorted_arguments.count)
				continue;

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
		definition->container_node = lambda->definition->container_node;
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
								type_def->container_node = hardened_lambda;
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
							hardened_param->definition_location = LambdaDefinitionLocation::parameter;
							hardened_param->is_constant = parameter->is_constant;
							hardened_param->container_node = hardened_lambda;

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
						type_def->container_node = hardened_lambda;
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
					hardened_param->definition_location = LambdaDefinitionLocation::parameter;
					hardened_param->is_constant = parameter->is_constant;
					hardened_param->container_node = hardened_lambda;
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
		if (arguments.count != lambda->parameters.count) {
			reporter->error(call->location, "Argument count does not match.");
			*success = false;
			return 0;
		}

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
					type_def->container_node = hardened_lambda;
					add_to_scope(type_def, &hardened_lambda->type_scope);
					// TODO: do at parse time
					if (hardened_lambda->type_scope.definitions.get_or_insert(type_def->name).count != 1) {
						state->reporter.error(ident->location, "Redefinition of polymorpic argument");
						yield(TypecheckResult::fail);
					}
				} else {
					auto type_def = AstDefinition::create();
					type_def->location = param->location;
					type_def->name = to_string(i);
					type_def->expression = arg->type;
					type_def->type = builtin_type.ident;
					type_def->container_node = hardened_lambda;
					add_to_scope(type_def, &hardened_lambda->type_scope);
				}
				// insert the parameter
				{
					hardened_param = AstDefinition::create();
					hardened_param->location = param->location;
					hardened_param->name = param->name;
					hardened_param->type = arg->type;
					hardened_param->definition_location = LambdaDefinitionLocation::parameter;
					hardened_param->is_constant = param->is_constant;
					hardened_param->container_node = hardened_lambda;

					hardened_lambda->parameters.add(hardened_param);
					add_to_scope(hardened_param, &hardened_lambda->parameter_scope);
				}
				if (param->is_constant) {
					hardened_param->expression = arg;
				}
			} else {
				hardened_param = deep_copy(param);
				hardened_param->container_node = hardened_lambda;
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

	if (compiler.debug_poly) {
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

	if (compiler.debug_poly) {
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

	hardened_lambda->return_parameter->container_node = hardened_lambda;
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

	//if (identifier->name == "Block")
	//	debug_break();
	//if (identifier->_uid == 2231)
	//	debug_break();

	DefinitionList definitions, definitions_in_other_scopes;

	while (1) {
		definitions.clear();
		definitions_in_other_scopes.clear();

		auto is_lambda_scope = [&] (Scope *scope) {
			return scope->node && scope->node->kind == Ast_Lambda && scope == ((AstLambda *)scope->node)->outer_scope();
		};

		auto scope = state->current_scope;
		while (1) {
			if (auto found_local = scope->definitions.find(identifier->name)) {
				for (auto definition : found_local->value) {
					if (definition->container_node == 0 || definition->is_constant || state->current_lambda_or_struct_or_enum == definition->container_node) {
						definitions.add(definition);
					} else {
						definitions_in_other_scopes.add(definition);
					}
				}
			}

			struct SuitableMember {
				AstUsing *Using;
				AstDefinition *member;
			};

			List<SuitableMember> suitable_members;
			for (auto Using : scope->usings) {
				if (auto Struct = direct_as<AstStruct>(Using->definition->type)) {
					for (auto member : Struct->data_members) {
						if (member->name == identifier->name) {
							suitable_members.add({
								.Using = Using,
								.member = member,
							});
						}
					}
				} else {
					invalid_code_path("using expects a struct");
				}
			}

			if (suitable_members.count) {
				if (suitable_members.count == 1) {
					auto Using = suitable_members[0].Using;
					auto member = suitable_members[0].member;

					identifier->possible_definitions.set(member);

					auto base = AstIdentifier::create();
					base->name = Using->definition->name;
					base->location = identifier->location;
					base->possible_definitions.set(Using->definition);
					base->type = Using->definition->type;

					auto bin = AstBinaryOperator::create();
					bin->operation = BinaryOperation::dot;
					bin->location = identifier->location;
					bin->left  = base;
					bin->right = identifier;
					bin->type = member->type;
					return bin;
				}

				state->reporter.error(identifier->location, "Ambiguous identifier", identifier->name);
				for (auto member : suitable_members) {
					state->reporter.info(member.Using->location, "Imported from here:");
					state->reporter.info(member.member->location, "Declared here:");
				}
				yield(TypecheckResult::fail);
			}

			if (definitions.count) {
				break;
			}

			if (!scope->parent) {
				assert(scope == &global_scope, "incomplete relationships between scopes. probably you forgot to set scope's parent.");
				break;
			}
			scope = scope->parent;
		}
		for (auto definition : definitions)
			assert(identifier->name == definition->name);

		auto wait_iteration = [&] {
			if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) { /* TODO: This is not the best solution */
				if (state->currently_typechecking_definition && state->currently_typechecking_definition->name == identifier->name) {
					state->reporter.error(identifier->location, "Can't use object while defining it.");
					state->reporter.info(state->currently_typechecking_definition->location, "Declared here:");
				} else {
					if (definitions_in_other_scopes.count) {
						state->reporter.error(identifier->location, "Can't access {} from here.", identifier->name);
						for (auto definition : definitions_in_other_scopes) {
							state->reporter.info(definition->location, "Definition:");
						}
					} else {
						state->reporter.error(identifier->location, "Identifier {} was not declared.", identifier->name);
					}
					// state->reporter.error(location, "Identifier was not declared or compiler failed to infer it's definition's type.");
				}
				yield(TypecheckResult::fail);
			}
			state->no_progress_counter++;
			yield(TypecheckResult::wait);
		};

		if (definitions.count) {
			for (auto definition : definitions) {
				while (!definition->type) {
					wait_iteration();
				}
			}
			state->no_progress_counter = 0;
			for (auto definition : definitions)
				assert(identifier->name == definition->name);
			break;
		}

		wait_iteration();
	}

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
		identifier->directed = definition->expression ? definition->expression->directed : 0;

		// if (definition->evaluated) {
		// 	return definition->evaluated;
		// }
	} else {
		identifier->possible_definitions = definitions;
		identifier->type = builtin_overload_set.ident;
		identifier->directed = 0;
	}
	return identifier;
}
AstExpression *typecheck(TypecheckState *state, AstCall *call) {
	//if (call->location == "f(a=2)")
	//	debug_break();

	typecheck(state, call->callable);

	for (auto &argument : call->unsorted_arguments) {
		typecheck(state, argument.expression);
	}

	AstExpression *probably_this = 0;
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
					wait_for(
						state,
						[&] { return lambda->finished_typechecking_head; },
						[&] {
							state->reporter.error(lambda->location, "Lambda's head couldn't finish typechecking");
							state->reporter.info(call->location, "Waited here:");
						}
					);

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
							if (parameter_index == lambda->parameters.count) {
								goto break_outer;
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

				if (Struct->parameters.count) {
					if (call->unsorted_arguments.count > Struct->parameters.count) {
						reporter.error(call->location, "Too many parameters.");
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
					arguments.resize(Struct->parameters.count);

					for (auto arg : named_arguments) {
						auto member = find_if(Struct->parameters, [&](auto member){return arg.name == member->name;});
						assert(member);
						arguments[index_of(Struct->parameters, member)] = arg.expression;
					}

					for (auto arg : unnamed_arguments) {
						*find(arguments, (AstExpression *)nullptr) = arg;
					}

					resolution.sorted_arguments = arguments;

					if (arguments.count != Struct->parameters.count) {
						reporter.error(call->location, "Argument count does not match.");
						goto continue_resolution;
					}

					for (umm i = 0; i < arguments.count; ++i) {
						auto &argument = arguments[i];
						if (argument) {
							auto &member = Struct->parameters[i];

							if (!implicitly_cast(state, &reporter, &argument, member->type, 0, false)) {
								goto continue_resolution;
							}
						}
					}

					resolution.success = true;
					resolution.Struct = Struct;
					matches.add(&resolution);
				} else {
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

						// Don'n repack the argument if unpack is passed to a pack.
						if (info.expressions.count == 1) {
							auto expr = info.expressions[0];
							if (expr->kind == Ast_UnaryOperator && ((AstUnaryOperator *)expr)->operation == UnaryOperation::pack) {
								packed_arguments.add(expr);
								continue;
							}
						}

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

			if (lambda->is_member) {
				assert(probably_this);
				if (!is_type(probably_this)) {
					auto pointer = make_address_of(&state->reporter, probably_this);
					if (!pointer)
						yield(TypecheckResult::fail);
					call->sorted_arguments.insert_at(pointer, 0);
				}
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

			assert(lambda->return_parameter->container_node == lambda);
			if (!lambda->return_parameter) {
				state->reporter.error(call->location, "Lambda's return parameter is undefined. This is probably due to errors while hardening a poly.");
				yield(TypecheckResult::fail);
			}
			call->type = lambda->return_parameter->type;

			assert(lambda->type->kind == Ast_LambdaType);
			call->lambda_type = (AstLambdaType *)lambda->type;
		} else {
			assert(match->Struct);
			if (match->Struct->parameters.count) {
				// TODO: cache instantiations

				// Make sure arguments are constant

				for (auto &argument : call->sorted_arguments) {
					if (!is_constant(argument)) {
						state->reporter.error(argument->location, "Only constants can be here.");
						yield(TypecheckResult::fail);
					}
				}

				// Instantiate a struct with specified arguments

				auto instantiated_struct = AstStruct::create();

				instantiated_struct->is_template = false;
				instantiated_struct->is_union = match->Struct->is_union;
				instantiated_struct->layout = match->Struct->layout;
				for (umm i = 0; i < call->sorted_arguments.count; ++i) {
					auto &argument = call->sorted_arguments[i];
					auto &template_parameter = match->Struct->parameters[i];
					auto parameter = AstDefinition::create();
					parameter->name = template_parameter->name;
					parameter->container_node = instantiated_struct;
					parameter->expression = argument;
					parameter->is_constant = true;
					parameter->type = argument->type;
					instantiated_struct->parameters.add(parameter);
					add_to_scope(parameter, &instantiated_struct->parameter_scope);
				}

				for (umm i = 0; i < call->sorted_arguments.count; ++i) {
					auto &argument = call->sorted_arguments[i];
					auto &member = instantiated_struct->parameters[i];
					if (argument) {
						if (!implicitly_cast(state, &state->reporter, &argument, member->type, 0)) {
							state->reporter.error(argument->location, "INTERNAL ERROR: implicit cast did not succeed the second time.");
							yield(TypecheckResult::fail);
						}
					} else {
						argument = make_null(member->type, call->location);
					}
				}

				// Typecheck instantiated body.

				deep_copy(&instantiated_struct->member_scope, &match->Struct->member_scope);

				typecheck(state, instantiated_struct);

				return instantiated_struct;
			} else {
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
		}
	} else if (call->callable->kind == Ast_BinaryOperator) {
		auto binop = (AstBinaryOperator *)call->callable;
		assert(binop->operation == BinaryOperation::dot);
		assert(binop-> left->kind == Ast_Identifier);
		assert(binop->right->kind == Ast_Identifier);
		identifier = (AstIdentifier *)binop->right;
		probably_this = binop->left;
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
	//if (lambda->definition && lambda->definition->name == "create")
	//	debug_break();

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

	if (lambda->insert_into) {
		typecheck(state, lambda->insert_into);
		if (lambda->insert_into->kind != Ast_Identifier) {
			state->reporter.error(lambda->insert_into->location, "This is expected to be an identifier.");
			yield(TypecheckResult::fail);
		}
		auto ident = (AstIdentifier *)lambda->insert_into;
		if (auto Struct = direct_as<AstStruct>(ident)) {
			assert(lambda->definition);
			Struct->member_scope.definitions.get_or_insert(lambda->definition->name).add(lambda->definition);
			Struct->member_scope.statements.add(lambda->definition);
		} else {
			state->reporter.error(lambda->insert_into->location, "This is expected to be an identifier for a struct.");
			yield(TypecheckResult::fail);
		}
 	}

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

	//if (bin->_uid == 2317)
	//	debug_break();

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

	switch (bin->operation) {
		case dot: {
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
						Map<KeyString, DefinitionList>::KeyValue *found_member = 0;
						wait_for(state,
							[&] {
								return found_member = Struct->member_scope.definitions.find(name);
							},
							[&] {
								if (left_is_type) {
									state->reporter.error(bin->right->location, "Type '{}' does not contain constant '{}'", Struct->definition->name, bin->right->location);
								} else {
									state->reporter.error(bin->right->location, "'{}' is not a member of '{}'", bin->right->location, Struct->definition->name);
									StringBuilder available_members;
									available_members.allocator = temporary_allocator;

									append(available_members, "Available members:\n");

									// TODO: these are not printed in declaration order
									for_each(Struct->member_scope.definitions, [&](auto name, auto definitions) {
										for (auto definition : definitions) {
											append_format(available_members, "    {}: {}\n", name, type_name(definition->type));
										}
									});

									state->reporter.info("{}", available_members);
								}
							}
						);

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

						wait_for(state,
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
							return make_cast(bin->left, bin->type);
						} else if (identifier->name == "count"str) {
							bin->type = type_uint;
							return make_cast(bin->left, type_uint);
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
			break;
		}
		case as: {
			auto cast = bin;
			typecheck(state, cast->right);
			cast->type = cast->right;

			auto &src_type = cast->left->type;
			auto &dst_type = cast->type;

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
			break;
		}
		case ass: {
			if (!ensure_assignable(&state->reporter, bin->left)) {
				yield(TypecheckResult::fail);
			}

			typecheck(state, bin->right);

			if (!implicitly_cast(state, &state->reporter, &bin->right, bin->left->type)) {
				yield(TypecheckResult::fail);
			}
			bin->type = builtin_void.ident;
			return bin;
		}
		case lor:
		case land: {
			if (!types_match(bin->left->type, builtin_bool)) {
				state->reporter.error(bin->left->location, "This must be a boolean");
				yield(TypecheckResult::fail);
			}

			typecheck(state, bin->right);
			if (!types_match(bin->right->type, builtin_bool)) {
				state->reporter.error(bin->right->location, "This must be a boolean");
				yield(TypecheckResult::fail);
			}
			bin->type = builtin_bool.ident;
			break;
		}
		case range: {
			harden_type(bin->left);
			if (!::is_integer(bin->left->type)) {
				state->reporter.error(bin->left->location, "This must be an integer");
				yield(TypecheckResult::fail);
			}

			typecheck(state, bin->right);
			harden_type(bin->right);
			if (!::is_integer(bin->right->type)) {
				state->reporter.error(bin->right->location, "This must be an integer");
				yield(TypecheckResult::fail);
			}

			return make_struct_initializer(builtin_range.ident, {bin->left, bin->right}, bin->location);
		}
		default: {
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
					case sub: {
						if (!types_match(l->expression, r->expression)) {
							state->reporter.error(bin->location, "Can't subtract pointers to different types.");
							yield(TypecheckResult::fail);
						}
						if (types_match(l->expression, builtin_void)) {
							state->reporter.error(bin->location, "Can't subtract pointers to void.");
							yield(TypecheckResult::fail);
						}
						bin->type = type_int;
						return bin;
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
		case pack: {
			typecheck(state, unop->expression);
			if (is_type(unop->expression)) {
				unop->type = builtin_type.ident;
			} else {
				if (!direct_as<AstSpan>(unop->expression->type)) {
					state->reporter.error(unop->expression->location, "Type {} is not unpackable. Only span type can be unpacked.", type_to_string(unop->expression->type));
					yield(TypecheckResult::fail);
				}
				unop->type = unop->expression->type;
			}
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

	scoped_replace(state->current_lambda_or_struct_or_enum, Struct);

	push_scope(&Struct->parameter_scope);

	if (Struct->is_template) {
		for (auto &parameter : Struct->parameters) {
			typecheck(state, parameter);
		}
	} else {
		push_scope(&Struct->member_scope);

		s64 struct_size = 0;
		s64 struct_alignment = 1;


		List<AstDefinition **> members;
		// List<AstDefinition *> struct_constants;
		// List<AstDefinition *> data_members;
		// List<AstDefinition *> lambda_members;
		// List<AstDefinition *> data_constants;
		// List<AstDefinition *> lambda_constants;

		for_each(Struct->member_scope.definitions, [&] (auto, DefinitionList &m) {
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
		}

		//if (Struct->definition && Struct->definition->name == "WNDCLASSEXA")
		//	debug_break();

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

						struct_alignment = max(struct_alignment, member_align);

						struct_size = ceil(struct_size, member_align);
						member->offset = struct_size;
						struct_size += member_size;
						break;
					}
					default:
						invalid_code_path();
				}
			}
		}
		Struct->alignment = struct_alignment;
		Struct->size = max(1, ceil(struct_size, struct_alignment));
	}
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
		auto type = subscript->expression->type;
		if (type->kind == Ast_Subscript) {
			subscript->type = ((AstSubscript *)type)->expression;
		} else if (type->kind == Ast_Span) {
			subscript->type = ((AstSpan *)type)->expression;
		} else if (is_pointer(type)) {
			subscript->type = ((AstUnaryOperator *)type)->expression;
		} else if (types_match(type, builtin_string)) {
			subscript->type = builtin_u8.ident;
		} else {
			state->reporter.error(subscript->location, "Expression is not subscriptable");
			yield(TypecheckResult::fail);
		}
	}
	return subscript;
}
AstExpression *typecheck(TypecheckState *state, AstSpan *span) {
	typecheck(state, span->expression);
	if (!is_type(span->expression)) {
		state->reporter.error(span->expression->location, "Span expects this to be a type, but it isn't.");
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
		typecheck(state, &test_params->test->scope);
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
		typecheck(state, &test_params->test->scope);
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
	typecheck(state, &Enum->scope);
	Enum->type = builtin_type.ident;
	return Enum;
}

void typecheck(TypecheckState *state, CExpression auto &expression) {
	assert(expression);

	switch (expression->kind) {
		case Ast_Identifier:     expression = typecheck(state, (AstIdentifier     *)expression); break;
		case Ast_Call:           expression = typecheck(state, (AstCall           *)expression); break;
		case Ast_Literal:        expression = typecheck(state, (AstLiteral        *)expression); break;
		case Ast_Lambda:         expression = typecheck(state, (AstLambda         *)expression); break;
		case Ast_LambdaType:     expression = typecheck(state, (AstLambdaType     *)expression); break;
		case Ast_BinaryOperator: expression = typecheck(state, (AstBinaryOperator *)expression); break;
		case Ast_UnaryOperator:  expression = typecheck(state, (AstUnaryOperator  *)expression); break;
		case Ast_Struct:         expression = typecheck(state, (AstStruct         *)expression); break;
		case Ast_Subscript:      expression = typecheck(state, (AstSubscript      *)expression); break;
		case Ast_Span:           expression = typecheck(state, (AstSpan           *)expression); break;
		case Ast_Ifx:            expression = typecheck(state, (AstIfx            *)expression); break;
		case Ast_Test:           expression = typecheck(state, (AstTest           *)expression); break;
		case Ast_Enum:           expression = typecheck(state, (AstEnum           *)expression); break;
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
	if (!throwing) {
		if (expression->kind == Ast_Identifier && ((AstIdentifier *)expression)->possible_definitions.count) {
		} else {
			assert(expression->type);
			if (!simplify(&state->reporter, &expression))
				yield(TypecheckResult::fail);
			assert(expression->type);

			if (!direct(expression->type))
				yield(TypecheckResult::fail);

			// assert(expression->type->directed == direct(expression->type));
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

	add_to_scope(d, &destination->member_scope);
	d->container_node = destination;

	if (!constant) {
		destination->data_members.add(d);
	}
}

static void write_test_source() {
	StringBuilder test;
	append_format(test, R"FOOBAR(
main :: () {
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
}
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
	timed_function(compiler.profiler);

	ParsedArguments result = {};

	compiler.compiler_path = (String)get_executable_path();

	auto parsed = parse_path(compiler.compiler_path);
	compiler.compiler_name = parsed.name;
	compiler.compiler_directory = parsed.directory;

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
			compiler.do_profile = true;
		} else if (arguments[i] == "--keep-temp"str) {
			compiler.keep_temp = true;
		} else if (arguments[i] == "--debug-poly"str) {
			compiler.debug_poly = true;
		} else if (arguments[i] == "--print-lowered"str) {
			compiler.print_lowered = true;
		} else if (arguments[i] == "--optimize"str) {
			compiler.optimize = true;
		} else if (arguments[i] == "--passes"str) {
			++i;
			if (i >= arguments.count) {
				immediate_error("Expected a number of optimization passes after --passes.\n");
				return result;
			}
			compiler.optimization_pass_count = parse_u64(arguments[i]).value_or(4);
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

void init_strings();

s32 tl_main(Span<Span<utf8>> arguments) {
	defer { print(""); }; // to reset console color just in case

	construct(compiler);

	init_strings();

	auto global_timer = create_precise_timer();

	set_console_encoding(Encoding_utf8);

	defer {
		if (compiler.do_profile) {
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
	compiler.profiler.init();
	defer {
		write_entire_file("profile.tmd"s, compiler.profiler.output_for_timed());
		compiler.profiler.deinit();
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

	timed_function(compiler.profiler);

restart_main:

	auto timer = compiler.profiler.scoped_timer("setup");

	compiler.current_directory = get_current_directory();

	auto args = parse_arguments(arguments);

	if (args.debug_paths) print("compiler_directory: {}\n", compiler.compiler_directory);

	if (args.debug_paths) print("current_directory: {}\n", compiler.current_directory);

	if (args.source_files.count == 0) {
		immediate_error(compiler.strings.no_source_path_received);
		return 1;
	}

	compiler.source_path = args.source_files[0];
	if (!is_absolute_path(compiler.source_path)) {
		compiler.source_path = make_absolute_path(compiler.source_path);
	}

	if (args.debug_paths) print("source_path: {}\n", compiler.source_path);

	compiler.source_path_without_extension = parse_path(compiler.source_path).path_without_extension();
	if (args.debug_paths) print("source_path_without_extension: {}\n", compiler.source_path_without_extension);

	if (args.output.count) {
		compiler.output_path = args.output;
		if (!is_absolute_path(compiler.output_path)) {
			compiler.output_path = make_absolute_path(compiler.output_path);
		}
	} else {
		compiler.output_path = (String)format("{}\\{}.exe", get_current_directory(), parse_path(compiler.source_path).name);
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
	construct(has_value_overloads);

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
		compiler.register_size = 8;
		compiler.stack_word_size = 8;
		compiler.general_purpose_register_count = 16;
	} else {
		scoped_phase("Collecting target information");

		with(temporary_allocator,
			lib = LoadLibraryW((wchar *)to_utf16(concatenate(compiler.compiler_directory, "\\targets\\"str, args.target), true).data)
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

		get_target_information(compiler);
	}




	import_paths.add(compiler.current_directory);
	import_paths.add(concatenate(compiler.compiler_directory, "\\libs"str));

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
		// defn->add_to_scope(&global_scope);
		global_scope.definitions.get_or_insert(defn->name).add(defn);
		global_scope.statements.add(defn);

		auto ident = AstIdentifier::create();
		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(defn);
		ident->type = expression->type;
		ident->directed = expression->directed;

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

		add_to_scope(definition, &global_scope);
		//typechecked_globals.get_or_insert(name) = definition;

		ident->location = name;
		ident->name = name;
		ident->possible_definitions.set(definition);
		ident->type = thing->type;
		ident->directed = thing->directed;

		// NOTE: not every builtin struct requires this, but i'd rather
		// not have to debug for an hour because i forgot to do this.
		builtin.pointer = make_pointer_type(builtin.ident);
		builtin.span = make_span_type(builtin.ident);
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

	// Type:
	init_struct(builtin_type, "Type"str, 8, 8);

	// Primitives:
	init_struct(builtin_void, "Void"str, 0, 0);
	init_struct(builtin_bool, "Bool"str, 1, 1);
	init_struct(builtin_u8,   "U8"str,   1, 1);
	init_struct(builtin_u16,  "U16"str,  2, 2);
	init_struct(builtin_u32,  "U32"str,  4, 4);
	init_struct(builtin_u64,  "U64"str,  8, 8);
	init_struct(builtin_s8,   "S8"str,   1, 1);
	init_struct(builtin_s16,  "S16"str,  2, 2);
	init_struct(builtin_s32,  "S32"str,  4, 4);
	init_struct(builtin_s64,  "S64"str,  8, 8);
	init_struct(builtin_f32,  "F32"str,  4, 4);
	init_struct(builtin_f64,  "F64"str,  8, 8);

	// Structs:  NOTE: members are appended after every builtin type was initialized.
	init_struct(builtin_string,        "String"str, 0, 0);
	init_struct(builtin_struct_member, "StructMember"str, 0, 0);
	init_struct(builtin_typeinfo,      "TypeInfo"str, 0, 0);
	init_struct(builtin_any,           "Any"str, 0, 0);
	init_struct(builtin_range,         "Range"str, 0, 0);

	// Enums:
	init_enum(builtin_type_kind, "TypeKind"str);

	// Internal types:
	init_struct(builtin_unsized_integer, "<integer>"str, 0, 0);
	init_struct(builtin_unsized_float,   "<float>"str, 0, 0);
	init_struct(builtin_noinit,          "<noinit>"str, 0, 0);
	init_struct(builtin_unknown,         "<unknown>"str, 0, 0);
	init_struct(builtin_unknown_enum,    "<unknown enum>"str, 0, 0);
	init_struct(builtin_poly,            "<poly>"str, 0, 0);
	init_struct(builtin_overload_set,    "<overload set>"str, 0, 0);

	switch (compiler.register_size) {
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

	type_sint  = add_global_alias("SInt"str,  builtin_default_integer->ident);
	type_uint  = add_global_alias("UInt"str,  builtin_default_unsigned_integer->ident);
	type_int   = add_global_alias("Int"str,   builtin_default_signed_integer->ident);
	type_float = add_global_alias("Float"str, builtin_default_float->ident);
	add_global_alias("SInt8"str,  builtin_s8.ident);
	add_global_alias("SInt16"str, builtin_s16.ident);
	add_global_alias("SInt32"str, builtin_s32.ident);
	add_global_alias("SInt64"str, builtin_s64.ident);
	add_global_alias("UInt8"str,  builtin_u8.ident);
	add_global_alias("UInt16"str, builtin_u16.ident);
	add_global_alias("UInt32"str, builtin_u32.ident);
	add_global_alias("UInt64"str, builtin_u64.ident);
	add_global_alias("Float32"str, builtin_f32.ident);
	add_global_alias("Float64"str, builtin_f64.ident);

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


		add_to_scope(d, &Struct->member_scope);
		d->container_node = Struct;

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
		d->container_node = Enum;

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
	{
		using enum TypeKind;
		append_value(builtin_type_kind, "Void"str,    (s64)Void   );
		append_value(builtin_type_kind, "Bool"str,    (s64)Bool   );
		append_value(builtin_type_kind, "U8"str,      (s64)U8     );
		append_value(builtin_type_kind, "U16"str,     (s64)U16    );
		append_value(builtin_type_kind, "U32"str,     (s64)U32    );
		append_value(builtin_type_kind, "U64"str,     (s64)U64    );
		append_value(builtin_type_kind, "S8"str,      (s64)S8     );
		append_value(builtin_type_kind, "S16"str,     (s64)S16    );
		append_value(builtin_type_kind, "S32"str,     (s64)S32    );
		append_value(builtin_type_kind, "S64"str,     (s64)S64    );
		append_value(builtin_type_kind, "F32"str,     (s64)F32    );
		append_value(builtin_type_kind, "F64"str,     (s64)F64    );
		append_value(builtin_type_kind, "struct"str,  (s64)Struct );
		append_value(builtin_type_kind, "enum"str,    (s64)Enum   );
		append_value(builtin_type_kind, "pointer"str, (s64)Pointer);
		append_value(builtin_type_kind, "span"str,    (s64)Span   );
		append_value(builtin_type_kind, "array"str,   (s64)Array  );
	}

	// struct_member
	append_member(builtin_struct_member, "name"str,   builtin_string.ident);
	append_member(builtin_struct_member, "type"str,   builtin_typeinfo.pointer);
	append_member(builtin_struct_member, "offset"str, type_uint);

	// typeinfo
	append_member(builtin_typeinfo, "kind"str, builtin_type_kind.ident);
	append_member(builtin_typeinfo, "name"str, builtin_string.ident);
	append_member(builtin_typeinfo, "size"str, type_uint);
	append_member(builtin_typeinfo, "members"str, builtin_struct_member.span);
	append_member(builtin_typeinfo, "pointee"str, builtin_typeinfo.pointer);
	append_member(builtin_typeinfo, "array_count"str, type_uint);

	// any
	append_member(builtin_any, "pointer"str, builtin_u8      .pointer);
	append_member(builtin_any, "type"str,    builtin_typeinfo.pointer);

	// range
	// TODO: make this a template. might be useful for aabb
	append_member(builtin_range, "min"str, type_int);
	append_member(builtin_range, "max"str, type_int);

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

		auto parsed = parse_file(concatenate(compiler.compiler_directory, "\\libs\\preload.tl"str));
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
			if (compiler.print_lowered) {
				print_lowered();
			}
		};

		scoped_phase("Typechecking");

		timed_block(compiler.profiler, "typecheck"str);

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
				if (compiler.do_profile) {
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
							auto pooled = fiber_pool.pop().value();
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
								for (auto call : reverse_iterate(state.poly_call_stack)) {
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
				with(ConsoleColor::red, print("Typechecking failed.\n"));
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
			compiler.build_lambda = get_lambda(build_definition->expression);
			s64 test = 69;
			// invoke(build_lambda, &test);
			with(ConsoleColor::red, print("test is {}\n", test));
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
		compiler.main_lambda = get_lambda(main_definition->expression);

		if (!::is_integer(compiler.main_lambda->return_parameter->type) && !types_match(compiler.main_lambda->return_parameter->type, builtin_void)) {
			immediate_error(compiler.main_lambda->location, "Main function can return any type of integer or void, but not {}", type_to_string(compiler.main_lambda->return_parameter->type));
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

		build(compiler, bytecode);
	}

	if (args.stats) {
		print("Total token count: {}\n", total_tokens_parsed);
	}

	return 0;
}
