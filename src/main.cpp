﻿#define TL_IMPL
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

// Fibers are nice for automatic stack expansion.
// But they are EXTREMELY slow. Typechecking is about 30% slower on performance_test.tl.
// SwitchFiber:   3.1s  5446  20.88%
// coro_yield:    2.8s        49.08%
// t_SwitchFiber: 2.4s
#define USE_FIBERS 1
#define SWITCH_TO_FIBER t_SwitchToFiber

extern "C" void t_SwitchToFiber(void *);

#if USE_FIBERS
size_t fiber_result;
#define PARENT_FIBER state->parent_fiber
#define yield(x) (_ReadWriteBarrier(), fiber_result = (size_t)(x), SWITCH_TO_FIBER(PARENT_FIBER))
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

static List<String> import_paths;

AstDefinition *parse_definition(String name, Parser *parser);
AstDefinition *parse_definition(Parser *parser);
AstExpression *make_pointer_type(AstExpression *type);
BinaryOperation binary_operation_from_token(TokenKind kind);

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
			case '\\':
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
			case '.':
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
			{
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
			default: {
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
	bool reached_end = false;
	AstLambda *current_lambda = 0;
	Reporter *reporter;
	String extern_language;
	String extern_library;
	Scope *current_scope = &global_scope;
	u32 scope_count = 0;
	CallingConvention current_convention = CallingConvention::tlang;
	StructLayout current_struct_layout = StructLayout::tlang;

	bool next() {
		auto old_token = token;
		++token;
		if (token == lexer->end()) {
			token = old_token;
			reached_end = true;
			return false;
		}
		return true;
	}

	bool expect(TokenKind expected_kind) {
		if (token->kind != expected_kind) {
			reporter->error(token->string, "Expected '{}', but got {}.", token_kind_to_string(expected_kind), token_kind_to_string(token->kind));
			return false;
		}
		return true;
	}
	bool next_not_end() {
		if (!next()) {
			reporter->error(token->string, "Unexpected end of file.");
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

AstLiteral *make_string(String value) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::string;
	i->string.set(value);
	i->type = type_string;
	return i;
}

AstLiteral *make_integer(BigInteger value, AstExpression *type = type_unsized_integer) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::integer;
	i->integer = value;
	i->type = type;
	return i;
}

AstLiteral *make_integer(u64 value, AstExpression *type = type_unsized_integer) {
	return make_integer(make_big_int<BigInteger>(value), type);
}

AstLiteral *make_boolean(bool value) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::boolean;
	i->Bool = value;
	i->type = type_bool;
	return i;
}

AstLiteral *make_float(f64 value, AstExpression *type = type_unsized_float) {
	auto i = AstLiteral::create();
	i->literal_kind = LiteralKind::Float;
	i->Float = value;
	i->type = type;
	return i;
}

bool is_binary_operator(TokenKind kind) {
	switch (kind) {
		case '+':
		case '-':
		case '*':
		case '/':
		case '%':
		case '.':
		case '&':
		case '|':
		case '^':
		case '>':
		case '<':
		case '==':
		case '!=':
		case '>=':
		case '<=':
		case '<<':
		case '>>':
		case Token_as:
		case '=':
		case '+=':
		case '-=':
		case '*=':
		case '/=':
		case '%=':
		case '^=':
		case '&=':
		case '|=':
		case '<<=':
		case '>>=':
			return true;
	}
	return false;
}

s32 get_precedence(BinaryOperation op) {
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
			return 3;

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

// TODO FIXME right now `!func()` parses as `(!func)()`

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser);
AstExpression *parse_expression_1(Parser *parser);
AstDefinition *parse_definition(Parser *parser);

#define push_scope(scope) \
	auto CONCAT(new_scope, __LINE__) = scope; \
	assert(CONCAT(new_scope, __LINE__)); \
	assert(!CONCAT(new_scope, __LINE__)->parent); \
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

AstDefinition *make_retparam(AstExpression *type, AstLambda *parent) {
	auto retparam = AstDefinition::create();

	retparam->type = type;
	retparam->is_return_parameter = true;
	retparam->parent_block = parent;
	// retparam->set_parent_scope(parser->current_scope);

	return retparam;
}

bool ensure_return_is_not_in_defer(Parser *parser, String location) {
	auto scope = parser->current_scope;
	// Global scope will have `node` set to null
	while (scope->node && scope->node->kind != Ast_lambda) {
		if (scope->node->kind == Ast_defer) {
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

	if (!parser->next_not_end())
		return 0;

	push_scope(&Struct->scope);

	while (parser->token->kind != '}') {
		auto definition = parse_definition(parser);
		if (!definition)
			return 0;

		definition->parent_block = Struct;

		if (!definition->expression || needs_semicolon(definition->expression)) {
			if (!parser->expect(';')) {
				return 0;
			}
			if (!parser->next_not_end())
				return 0;
		}

		Struct->members.add(definition);
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
inline AstUnaryOperator *make_autocast() { return make_unary(UnaryOperation::autocast); }
inline AstUnaryOperator *make_sizeof() { return make_unary(UnaryOperation::Sizeof); }
inline AstUnaryOperator *make_typeof() { return make_unary(UnaryOperation::typeof); }

// if `lambda` is 0, `parse_lambda` will create a new one.
// otherwise it fills the passed one.
AstLambda *parse_lambda(Parser *parser, bool is_parsing_type) {
	auto lambda = AstLambda::create();
	if (!parser->next_not_end())  return 0;

	if (parser->token->kind == Token_directive) {
		if (parser->token->string == "#stdcall"str) {
			lambda->convention = CallingConvention::stdcall;
		} else if (parser->token->string == "#intrinsic"str) {
			lambda->is_intrinsic = true;
		} else {
			parser->reporter->error(parser->token->string, "Unknown directive.");
			return 0;
		}
		if (!parser->next_not_end())
			return 0;
	}

	if (lambda->convention == CallingConvention::none) {
		lambda->convention = parser->current_convention;
	}

	if (!parser->expect('('))  return 0;

	lambda->location = parser->token->string;

	if (!parser->next_not_end())  return 0;

	push_scope(&lambda->parameter_scope);

	if (parser->token->kind != ')') {
		for (;;) {
			auto definition = parse_definition(parser);
			if (!definition)
				return 0;

			definition->is_parameter = true;
			definition->parent_block = lambda;

			for (auto &other_parameter : lambda->parameters) {
				if (definition->name != "_"str && definition->name == other_parameter->name) {
					parser->reporter->error(definition->name, "Can't use identical names for different parameters.");
					return 0;
				}
			}

			lambda->parameters.add(definition);

			if (parser->token->kind == ')') {
				break;
			}

			if (!parser->expect(','))
				return 0;

			if (!parser->next_not_end())
				return 0;
		}
	}

	if (!parser->next_not_end())  return 0;

	if (parser->token->kind == ':') {
		if (!parser->next_not_end())
			return 0;

		//
		// Parse first identifier manually to reduce parser resets
		//
		auto first_retparam_token = parser->token;
		if (parser->token->kind == Token_identifier) {
			auto ident = parser->token->string;
			if (!parser->next_not_end()) {
				return 0;
			}

			if (parser->token->kind == ':') {
				lambda->return_parameter = parse_definition(ident, parser);
				if (!lambda->return_parameter) {
					return 0;
				}
				lambda->return_parameter->is_return_parameter = true;
				lambda->return_parameter->parent_block = lambda;
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
		}
	}


	if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';') {
		parser->reporter->error(parser->token->string, "Expected '{{' or '=>' or ';' or ':' instead of '{}'.", parser->token->string);
		return 0;
	}

	bool is_short = false;

	if (is_parsing_type) {
		lambda->has_body = false;
		lambda->is_type = true;
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

	auto previous_lambda = parser->current_lambda;
	parser->current_lambda = lambda;
	defer { parser->current_lambda = previous_lambda; };

	push_scope(&lambda->body_scope);

	if (lambda->has_body) {
		auto opening_token = parser->token;

		if (!parser->next_not_end())
			return 0;

		if (is_short) {

			auto expression = parse_expression(parser);
			if (!expression)
				return 0;

			if (!parser->expect(';'))
				return 0;

			auto ret = AstReturn::create();
			ret->expression = expression;
			ret->location = opening_token->string;
			ret->lambda = lambda;
			lambda->body_scope.statements.add(ret);
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
		case Token_autocast: {
			auto cast = make_autocast();
			cast->location = parser->token->string;
			if (!parser->next_not_end())
				return 0;
			cast->expression = parse_expression_1(parser);
			return cast;
		}
		case Token_simd: {
			if (!parser->next_not_end())
				return 0;

			auto expr = parse_expression(parser);
			if (!expr)
				return 0;

			if (expr->kind != Ast_subscript) {
				parser->reporter->error(expr->location, "Expected an array type after simd keyword, but got {}.", expr->kind);
				return 0;
			}

			auto array = (AstSubscript *)expr;

			// array->is_simd = true;

			return array;
		}
		case Token_string_literal: {
			auto string = AstLiteral::create();
			string->literal_kind = LiteralKind::string;
			string->location = parser->token->string;


			auto str = parser->token->string;
			assert(str.front() == '"');
			assert(str.back() == '"');
			str.data += 1;
			str.count -= 2;

			string->string = erase_all<HeapString::Allocator>(str, u8'\r');

			if (starts_with(string->string, "\n"s)) {
				string->string.data += 1;
				string->string.count -= 1;
			}

			if (ends_with(string->string, "\n"s)) {
				string->string.count -= 1;
			}

			if (auto unescaped = unescape_string(string->string)) {
				string->string = unescaped.value_unchecked();
				parser->next();
				return string;
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
			auto result = make_integer(0);
			result->location = parser->token->string;
			parser->next();
			result->type = type_pointer_to_void;
			return result;
		}
		case Token_float_literal: {
			auto result = AstLiteral::create();
			result->literal_kind = LiteralKind::Float;
			if (std::from_chars((char *)parser->token->string.begin(), (char *)parser->token->string.end(), result->Float).ec == std::errc::invalid_argument) {
				parser->reporter->error(parser->token->string, "Failed to parse floating point number.");
				return 0;
			}
			result->type = type_unsized_float;
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
			auto result = make_integer(value);
			result->location = location;
			return result;
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
		case Token_identifier: {
			auto identifier_token = parser->token;
			parser->next();
			auto identifier = AstIdentifier::create();
			identifier->location = identifier->name = identifier_token->string;
			return identifier;
		}
		case Token_fn: {

		parse_function:

			return parse_lambda(parser, is_parsing_type);
		}
		case Token_struct: {
			auto token = parser->token->string;
			if (!parser->next_not_end())
				return 0;
			return parse_struct(parser, token);
		}
		case Token_union: {
			auto token = parser->token->string;
			if (!parser->next_not_end())
				return 0;
			auto Struct = parse_struct(parser, token);
			if (!Struct)
				return 0;

			Struct->is_union = true;
			return Struct;
		}
		case Token_if: {
			auto If = AstIfx::create();
			If->location = parser->token->string;
			if (!parser->next_not_end())
				return 0;

			If->condition = parse_expression(parser);
			if (!If->condition)
				return 0;

			if (parser->token->kind == Token_then && !parser->next_not_end()) {
				return 0;
			}

			If->true_expression = parse_expression(parser);
			if (!If->true_expression)
				return 0;

			if (!parser->expect(Token_else)) {
				return 0;
			}

			if (!parser->next_not_end())
				return 0;

			If->false_expression = parse_expression(parser);
			if (!If->false_expression)
				return 0;

			return If;
		}
					 /*
		case Token_import: {
			auto import = AstImport::create();
			import->location = parser->token->string;
			if (!parser->next_not_end())
				return 0;

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
			expression->is_parenthesized = true;

			if (!parser->expect(')')) {
				return 0;
			}
			auto end_token = parser->token->string;

			expression->location = {start_token.begin(), end_token.end()};

			parser->next();

			return expression;
		}
		case '?': {
			auto noinit = AstLiteral::create();
			noinit->location = parser->token->string;
			noinit->literal_kind = LiteralKind::noinit;
			parser->next();
			return noinit;
		}
		case '[': {
			auto subscript = AstSubscript::create();
			subscript->location = parser->token->string;
			subscript->is_prefix = true;

			if (!parser->next_not_end())
				return 0;

			subscript->index_expression = parse_expression(parser);
			if (!subscript->index_expression)
				return 0;

			if (!parser->expect(']'))
				return 0;

			parser->next();

			subscript->location = {subscript->location.begin(), parser->token->string.end()};
			subscript->expression = parse_expression_0(parser);
			if (!subscript->expression)
				return 0;

			return subscript;
		}
		default: {
			auto operation = as_unary_operation(*parser->token);
			if (operation) {
				auto unop = AstUnaryOperator::create();
				unop->location = parser->token->string;
				unop->operation = operation.value_unchecked();
				if (!parser->next_not_end())
					return 0;

				unop->expression = parse_expression_1(parser);
				if (!unop->expression)
					return 0;

				unop->location = {unop->location.begin(), unop->expression->location.end()};

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
					auto result = make_integer(get_line_number(parser->token->string.data));
					result->location = parser->token->string;
					parser->next();
					return result;
				} else if (parser->token->string == "#location"str) {
					auto result = make_string(where(parser->token->string.data));
					result->location = parser->token->string;
					parser->next();
					return result;
				} else {
					parser->reporter->error(parser->token->string, "Unexpected directive (expression).");
					return 0;
				}
			} else {
				parser->reporter->error(parser->token->string, "Unexpected token '{}'.", parser->token->string);
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

			if (!parser->next_not_end())
				return 0;

			binop->right = parse_expression_0(parser);
			if (!binop->right)
				return 0;

			update_location(binop);
			expression = binop;
		}
		while (parser->token->kind == '(') {
			auto open_paren = parser->token->string;
			if (!parser->next_not_end())  return 0;

			List<CallArgument> arguments;
			if (parser->token->kind != ')') {
				for (;;) {
					CallArgument argument = {};

					argument.expression = parse_expression(parser);
					if (!argument.expression)
						return 0;

					if (argument.expression->kind == Ast_binary_operator) {
						auto bin = (AstBinaryOperator *)argument.expression;
						if (bin->operation == BinaryOperation::ass) {

							auto left = bin->left;
							if (left->kind != Ast_identifier) {
								parser->reporter->error(left->location, "Invalid use of named argument: expected an identifier for left operand.");
								return 0;
							}
							auto ident = (AstIdentifier *)left;

							argument.name = ident->name;
							argument.expression = bin->right;
						}
					}
					if (!argument.name.empty()) {
						for (auto &other_argument : arguments) {
							if (argument.name == other_argument.name) {
								parser->reporter->error(argument.name, "Can't use the same named parameter more than once.");
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

					if (!parser->next_not_end())
						return 0;
				}
			}

			u32 named_argument_count = 0;
			for (auto argument : arguments) {
				named_argument_count += argument.name.data != 0;
			}

			auto close_paren = parser->token->string;

			auto call = AstCall::create();
			call->callable = expression;
			call->arguments = arguments;

			call->location = parser->token->string;
			update_location(call);

			if (!parser->next_not_end())  return 0;
			expression = call;
		}

		while (parser->token->kind == '[') {
			auto subscript = AstSubscript::create();
			subscript->is_prefix = false;
			subscript->expression = expression;

			if (!parser->next_not_end())
				return 0;

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
	defer { *_expression = expression; };

	bool is_type = (expression->type == type_type) || (expression->type && expression->type->kind == Ast_lambda);

	if (!is_type) {
		switch (expression->kind) {
			case Ast_binary_operator: {
				auto binop = (AstBinaryOperator *)expression;

				using enum BinaryOperation;

				switch (binop->operation) {
					case dot: {
						// HACK HACK HACK TODO TODO TODO FIXME FIXME FIXME
						if (binop->left->kind == Ast_literal) {
							auto left = (AstLiteral *)binop->left;
							if (left->literal_kind == LiteralKind::string) {
								if (binop->right->kind == Ast_identifier) {
									auto right = (AstIdentifier *)binop->right;
									if (right->name == "count"str) {
										expression = make_integer(left->string.count, right->type);
										expression->location = binop->location;
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

							defer {
								if (expression)
									expression->location = binop->location;
							};
							switch (binop->operation) {
								case add: expression = make_integer(left + right, binop->type); return true;
								case sub: expression = make_integer(left - right, binop->type); return true;
								case mul: expression = make_integer(left * right, binop->type); return true;
								case div:
									if (right == 0) {
										reporter->error(expression->location, "Integer division by zero.");
										expression = 0;
										return false;
									}
									expression = make_integer(left / right, binop->type);
									return true;
								case mod: expression = make_integer(left % right, binop->type); return true;
								case band: expression = make_integer(left & right, binop->type); return true;
								case bor: expression = make_integer(left | right, binop->type); return true;
								case bxor: expression = make_integer(left ^ right, binop->type); return true;
								case bsl:
									if (right.msb) {
										reporter->error(expression->location, "Can't shift left by negative amount.");
										expression = 0;
										return false;
									}
									if (right.parts.count > 1) {
										reporter->error(expression->location, "Can't shift left by amount that big. Resulting number will now fit in memory.");
										expression = 0;
										return false;
									}
									expression = make_integer(left << right, binop->type);
									return true;
								case bsr: expression = make_integer(left >> right, binop->type); return true;
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
							if (types_match(binop->right, type_u8)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
							} else if (types_match(binop->right, type_u16)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
							} else if (types_match(binop->right, type_u32)) {
								result.msb = 0;
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
							} else if (types_match(binop->right, type_u64) || ::is_pointer(binop->right)) {
								result.msb = 0;
								result.parts.resize(1);
							} else if (types_match(binop->right, type_s8)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFF;
								result.msb = result.parts.data[0] & 0x80;
								result.parts.data[0] |= result.msb ? ~0xFF : 0;
							} else if (types_match(binop->right, type_s16)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFF;
								result.msb = result.parts.data[0] & 0x8000;
								result.parts.data[0] |= result.msb ? ~0xFFFF : 0;
							} else if (types_match(binop->right, type_s32)) {
								result.parts.resize(1);
								result.parts.data[0] &= 0xFFFFFFFF;
								result.msb = result.parts.data[0] & 0x80000000;
								result.parts.data[0] |= result.msb ? ~0xFFFFFFFF : 0;
							} else if (types_match(binop->right, type_s64)) {
								result.parts.resize(1);
								result.msb = result.parts.data[0] & 0x8000000000000000;
							} else {
								invalid_code_path();
							}
							expression = make_integer(result, binop->right);
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
						invalid_code_path();
				}
				break;
			}
			case Ast_unary_operator: {
				using enum UnaryOperation;
				auto unop = (AstUnaryOperator *)expression;
				defer {
					if (expression)
						expression->location = unop->location;
				};

				if (unop->expression->kind == Ast_literal) {
					auto literal = (AstLiteral *)unop->expression;
					switch (literal->literal_kind) {
						case LiteralKind::integer: {
							auto integer = literal->integer;
							switch (unop->operation) {
								case plus: return true;
								case minus: expression = make_integer(-integer); return true;
								case bnot:  expression = make_integer(~integer); return true;
								default: invalid_code_path(); break;
							}
							break;
						}
						case LiteralKind::Float: {
							auto Float = literal->Float;
							switch (unop->operation) {
								case plus: return true;
								case minus: expression = make_float(-Float); return true;
								default: invalid_code_path(); break;
							}
						}
					}
				}

				break;
			}
			case Ast_identifier: {
				auto identifier = (AstIdentifier *)expression;
				if (identifier->definition) {
					if (identifier->definition->is_constant) {
						expression = identifier->definition->expression;
						assert(expression->kind == Ast_literal);
					}
				}
				break;
			}
			case Ast_lambda:
			case Ast_struct:
			case Ast_literal:
			case Ast_call:
			case Ast_subscript:
			case Ast_ifx:
				break;
			default:
				invalid_code_path();
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
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;
			open();
			print(ident->name);
			close();
			break;
		}
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)expression;
			open();
			print_parsed_expression(bin->left);
			print(operator_string(bin->operation));
			print_parsed_expression(bin->right);
			close();
			break;
		}
		case Ast_unary_operator: {
			auto un = (AstUnaryOperator *)expression;
			open();
			print(as_string(un->operation));
			print_parsed_expression(un->expression);
			close();
			break;
		}
		case Ast_subscript: {
			auto s = (AstSubscript *)expression;
			open();
			print_parsed_expression(s->expression);
			open('[');
			print_parsed_expression(s->index_expression);
			close(']');
			close();
			break;
		}
		case Ast_call: {
			auto s = (AstCall *)expression;
			open();
			print_parsed_expression(s->callable);
			open();
			for (int i = 0; i < s->arguments.count; ++i) {
				if (i)
					print(", ");
				print_parsed_expression(s->arguments[i].expression);
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

	if (parser->reached_end) {
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

	while (is_binary_operator(parser->token->kind)) {
		auto binop = AstBinaryOperator::create();

		defer { previous_binop = binop; };

		binop->left = expression;

		binop->operation = binary_operation_from_token(parser->token->kind);

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
				case Ast_call: {
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
				case Ast_subscript: {
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
				case Ast_binary_operator: {
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
AstDefinition *parse_definition(String name, Parser *parser) {
	assert(parser->token->kind == ':');

	if (!parser->next_not_end())  return 0;

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

	auto definition = AstDefinition::create();

	definition->location = definition->name = name;
	definition->type = type;
	definition->parent_block = parser->current_lambda;
	definition->is_constant = is_constant;
	// definition->set_parent_scope(parser->current_scope);

#if 1
	// Redefinition checks now are impossible at parsing time because of function overloading
	if (definition->name != "_"str) {
		scoped_lock(parser->current_scope);

		if (!definition->name.count) {
			debug_break();
		}
		auto &list = parser->current_scope->definitions.get_or_insert(definition->name);

		list.add(definition);
	}
#else
	if (definition->name != "_"str) {
		bool check_redefinition_in_parent_scopes = true;
		if (definition->parent_scope->node && definition->parent_scope->node->kind == Ast_struct) {
			check_redefinition_in_parent_scopes = false;
		}

		{
			auto report_redefinition_error = [&] (AstDefinition *_new, AstDefinition *existing) {
				parser->reporter->error(_new->name, "Redefinition of '{}'.", _new->name);
				parser->reporter->info(existing->name, "Top declaration is here:");
			};

			Scope *scope = parser->current_scope;
			while (scope) {
				auto to_lock = scope;
				scoped_lock(to_lock);
				auto found = scope->definitions.find(definition->name);
				if (found) {
					auto x = found->back();
					if (!is_redefinable(x)) {
						report_redefinition_error(definition, x);
					}
					return 0;
				}


				if (!check_redefinition_in_parent_scopes) {
					break;
				}

				scope = scope->parent;
			}

			{
				scoped_lock(parser->current_scope);
				if (parser->current_scope == &global_scope) {
					auto found = names_not_available_for_globals.find(definition->name);
					if (found) {
						report_redefinition_error(*found, definition);
						return 0;
					}
				}
				auto added = parser->current_scope->definitions.try_insert(definition->name, definition);
				assert(added);
			}
			if (check_redefinition_in_parent_scopes) {
				scoped_lock(&global_scope);
				names_not_available_for_globals.try_insert(definition->name, definition); // May fail, don't care. Error will show the first one
			}
		}
	}
#endif

	if (has_expression) {
		if (!parser->next_not_end())  return 0;

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		definition->expression = expression;
		switch (expression->kind) {
			case Ast_lambda: {
				auto lambda = (AstLambda *)expression;
				lambda->definition = definition;
				break;
			}
			case Ast_struct: {
				auto Struct = (AstStruct *)expression;
				Struct->definition = definition;
				break;
			}
		}
	}

	return definition;
}
AstDefinition *parse_definition(Parser *parser) {
	if (parser->token->kind == Token_identifier) {
		auto name = parser->token->string;
		if (!parser->next_expect(':'))
			return 0;
		return parse_definition(name, parser);
	}
	parser->reporter->error(parser->token->string, "Failed to parse definition.");
	return 0;
}

bool is_statement(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_call:
		case Ast_import:
			return true;
		case Ast_binary_operator:
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

	if (parser->token->kind == Token_then && !parser->next_not_end())
		return 0;

	if (!parse_block_or_single_statement(parser, &If->true_scope))
		return 0;

	if (parser->token->kind == Token_else) {
		if (!parser->next_not_end())
			return 0;

		if (!parse_block_or_single_statement(parser, &If->false_scope))
			return 0;
	}
	return If;
}
void parse_statement(Parser *parser, AstStatement *&result) {
	timed_function(context.profiler);

	result = 0;
	defer {
		if (result) {
			scoped_lock(parser->current_scope);
			parser->current_scope->statements.add(result);
		}
	};

	switch (parser->token->kind) {
		case Token_identifier: {
			auto name = parser->token->string;

			parser->next();

			if (parser->token->kind == ':') {
				// Definition

				auto definition = parse_definition(name, parser);

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

			if (!parser->next_not_end())
				return;

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
			if (!parser->next_not_end())
				return;

			result = parse_if_statement(parser, token);
			return;
		}
		case Token_while: {
			auto While = AstWhile::create();
			While->location = parser->token->string;
			if (!parser->next_not_end())
				return;
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
				if (!parser->next_not_end())
					return;

				auto If = parse_if_statement(parser, token);
				if (!If)
					return;

				If->is_constant = true;
				result = If;
				return;
			} else if (parser->token->string == "#test"str) {
				auto test = AstTest::create();
				test->location = parser->token->string;

				if (!parser->next_not_end())
					return;

				if (parser->token->kind == Token_true) {
					test->should_compile = true;
				} else if (parser->token->kind == Token_false) {
					test->should_compile = false;
				} else {
					parser->reporter->error(parser->token->string, "Unexpected token after #test directive, expected true or false.");
					return;
				}

				if (!parser->next_not_end())
					return;

				if (!parse_block_or_single_statement(parser, &test->scope)) {
					return;
				}

				result = test;
				return;
			} else if (parser->token->string == "#assert"str) {
				if (!parser->next_not_end())
					return;

				auto expression = parse_expression(parser);
				if (!expression) {
					return;
				}
				if (!parser->expect(';'))
					return;
				parser->next();

				auto assert = AstAssert::create();
				assert->condition = expression;
				result = assert;
				return;
			} else if (parser->token->string == "#print"str) {
				auto print = AstPrint::create();
				print->location = parser->token->string;
				if (!parser->next_not_end()) {
					return;
				}
				print->expression = parse_expression(parser);
				if (!parser->expect(';'))
					return;
				parser->next();

				result = print;
				return;
			} else if (parser->token->string == "#parse"str) {
				auto parse = AstParse::create();
				parse->location = parser->token->string;
				if (!parser->next_not_end()) {
					return;
				}
				parse->expression = parse_expression(parser);
				if (!parser->expect(';'))
					return;
				parser->next();

				print_parsed_expression(parse->expression);
				print('\n');

				result = parse;
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
			if (!parser->next_not_end())
				return;
			if (!parse_block_or_single_statement(parser, &Defer->scope)) {
				return;
			}

			result = Defer;
			return;
		}
		case Token_operator: {
			if (!parser->next_not_end())
				return;

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
					if (!parser->next_not_end())
						return;

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
				case Token_as: {
					if (!parser->next_not_end())
						return;

					auto definition = AstOperatorDefinition::create();
					definition->location = parser->token->string;
					definition->operation = Token_as;
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
					if (!parser->next_not_end())
						return;

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
	while (!parser->reached_end) {
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

enum class TypecheckResult {
	wait,
	fail,
	success,
};

struct TypecheckState {
	TypecheckState() = default;
	TypecheckState(TypecheckState const &) = delete;

	AstStatement *statement = 0;
	AstLambda *lambda = 0;
	AstDefinition *definition = 0;
	AstExpression *waiting_for = 0;
	String waiting_for_name;

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

DefinitionList get_definitions(TypecheckState *state, String name) {
	timed_function(context.profiler);
	DefinitionList result;
	auto scope = state->current_scope;

	auto is_lambda_scope = [&] (Scope *scope) {
		// `body_scope` is inside `parameter_scope`, so parameter_scope is outermost.
		return scope->node && scope->node->kind == Ast_lambda && scope == &((AstLambda *)scope->node)->parameter_scope;
	};

	while (scope) {
		auto found_local = scope->definitions.find(name);
		if (found_local)
			result.add(found_local->value);
		if (is_lambda_scope(scope)) {
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
		scope = scope->parent;
	}
	return result;
}

struct IntegerInfo {
	AstStruct *type;
	BigInteger min_value;
	BigInteger max_value;
};

IntegerInfo integer_infos[8];

void harden_type(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			 // Literals may already have their type set by, for example, cast expressions
			if (literal->type == type_unsized_integer) {
				literal->type = type_int;
			} else if (literal->type == type_unsized_float) {
				literal->type = type_default_float;
			}
			break;
		}
		case Ast_identifier: // TODO: unsized identifiers?
		case Ast_binary_operator:
		case Ast_call:
		case Ast_subscript:
		case Ast_unary_operator:
		case Ast_struct:
		case Ast_lambda:
			break;
		default:
			invalid_code_path();
	}
}

Box<AstLiteral> make_type_literal(AstExpression *type) {
	timed_function(context.profiler);
	AstLiteral result;
	result.literal_kind = LiteralKind::type;
	result.type_value = type;
	result.type = type_type;
	return result;
}

Box<AstLiteral> make_bool(bool val) {
	timed_function(context.profiler);
	AstLiteral result;
	result.literal_kind = LiteralKind::boolean;
	result.Bool = val;
	result.type = type_bool;
	return result;
}

void ensure_definition_is_resolved(TypecheckState *state, AstIdentifier *identifier) {
	if (!identifier->definition) {
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

Box<AstLiteral> evaluate(TypecheckState *state, AstExpression *expression) {
	timed_function(context.profiler);
	switch (expression->kind) {
		case Ast_literal: return (AstLiteral *)expression;
		case Ast_identifier: {
			auto ident = (AstIdentifier *)expression;

			ensure_definition_is_resolved(state, ident);

			auto definition = ident->definition;

			if (!definition->is_constant) {
				state->reporter.error(expression->location, "Can't evaluate expression at compile time: definition is not constant");
				return nullptr;
			}

			if (definition->evaluated)
				return definition->evaluated;

			return definition->evaluated = evaluate(state, definition->expression);
		}
		case Ast_binary_operator: {
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
						case integer: return make_integer(l->integer + r->integer, bin->type);
						case Float:   return make_float  (l->Float   + r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case sub: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer(l->integer - r->integer, bin->type);
						case Float:   return make_float  (l->Float   - r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case mul: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer(l->integer * r->integer, bin->type);
						case Float:   return make_float  (l->Float   * r->Float  , bin->type);
						default: invalid_code_path();
					}
				}
				case div: {
					assert(l->literal_kind == r->literal_kind);
					switch (l->literal_kind) {
						using enum LiteralKind;
						case integer: return make_integer(l->integer / r->integer, bin->type);
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
		case Ast_struct: {
			return make_type_literal(expression);
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;

			switch (unop->operation) {
				using enum UnaryOperation;
				case typeof:
					return make_type_literal(unop->expression->type);
				default: {
					if (types_match(expression->type, type_type)) {
						auto child = evaluate(state, unop->expression);
						if (!child)
							return nullptr;

						if (child->literal_kind == LiteralKind::type) {
							return make_type_literal(make_pointer_type(child->type_value));
						}

						invalid_code_path();
					}
				}
			}

			break;
		}
		case Ast_call: {
			// TODO: this is very limited right now
			auto call = (AstCall *)expression;
			auto lambda = get_lambda(call->callable);
			if (lambda->body_scope.statements.count != 1) {
				state->reporter.error(expression->location, "Can't evaluate expression at compile time: definition is not constant");
				return nullptr;
			}
			assert(lambda->body_scope.statements[0]->kind == Ast_return);
			return evaluate(state, ((AstReturn *)lambda->body_scope.statements[0])->expression);
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)expression;
			assert(!lambda->has_body);
			assert(lambda->is_type);
			return make_type_literal(lambda);
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(is_type(subscript->expression));
			return make_type_literal(subscript);
		}
		default:
			invalid_code_path();
	}
	return nullptr;
}



/*
bool do_all_paths_return(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_return:
			return true;
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)statement;
			if (bin->operation == '=') {
			}
			break;
		}
		case Ast_if: {
			auto If = (AstIf *)statement;

			bool true_returns  = false;
			bool false_returns = false;

			for (auto statement : If->true_statements) {
				if (do_all_paths_return(statement)) {
					true_returns = true;
					break;
				}
			}
			for (auto statement : If->false_statements) {
				if (do_all_paths_return(statement)) {
					false_returns = true;
					break;
				}
			}

			return true_returns && false_returns;
		}
	}
	return false;
}
bool do_all_paths_return(AstLambda *lambda) {
	if (types_match(lambda->return_parameter->type, type_void))
		return true;

	for (auto statement : lambda->body_scope.statements) {
		if (do_all_paths_return(statement)) {
			return true;
		}
	}
	return false;
}
*/

AstExpression *make_pointer_type(AstExpression *type) {
	using enum UnaryOperation;
	timed_function(context.profiler);
	auto unop = AstUnaryOperator::create();
	unop->expression = type;
	unop->type = type_type;
	unop->operation = pointer;
	return unop;
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
	if (integer >= info.min_value && integer <= info.max_value)
		return true;
	if (reporter) {
		auto type_string = type_name(info.type);
		reporter->error(expression->location, "{} does not fit into {}. You can explicitly write '{} as {}' to perform lossy conversion.", integer, type_string, expression->location, type_string);
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

bool implicitly_cast(TypecheckState *state, Reporter *reporter, Expression<> *_expression, AstExpression *type, int *conversion_distance = 0) {
	timed_function(context.profiler);
	auto expression = *_expression;
	defer { *_expression = expression; };

	if (conversion_distance)
		*conversion_distance = 0;

	if (types_match(expression->type, type)) {
		return true;
	}

	if (conversion_distance)
		*conversion_distance = 1;

	if (expression->kind == Ast_unary_operator) {
		auto unop = (AstUnaryOperator *)expression;
		if (unop->operation == UnaryOperation::autocast) {
			auto autocast = unop;
			CastType request = {get_struct(expression->type), get_struct(type)};
			auto found_built_in = find(built_in_casts, request);

			auto allow_autocast = [&] {
				expression = make_cast(autocast->expression, type);
				return true;
			};

			if (found_built_in) {
				return allow_autocast();
			}

			if (::is_pointer(expression->type) && type->kind == Ast_lambda) {
				// Pointer can be converted to a lambda.
				return allow_autocast();
			}

			if (reporter) {
				reporter->error(expression->location, "Expression of type {} is not convertible to {}.", type_to_string(expression->type), type_to_string(type));
			}
			return false;
		}
	}

	if (expression->kind == Ast_literal) {
		auto literal = (AstLiteral *)expression;
		if (literal->literal_kind == LiteralKind::integer) {
			if (types_match(literal->type, type_pointer_to_void)) {
				if (is_pointer(type)) {
					return true;
				}
			} else {
				auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, type); });
				if (found_info) {
					auto &info = *found_info;
					if (!ensure_fits(reporter, expression, literal->integer, info)) {
						return false;
					}

					expression->type = type;
					return true;
				}

				if (types_match(type, type_f32) || types_match(type, type_f64)) {
					expression->type = type;
					return true;
				}
			}
		} else if (literal->literal_kind == LiteralKind::Float) {
			if (types_match(type, type_f32) || types_match(type, type_f64)) {
				expression->type = type;
				return true;
			}
		} else if (literal->literal_kind == LiteralKind::noinit) {
			literal->type = type;
			return true;
		}
	} else if (types_match(type, type_pointer_to_void)) {
		if (is_pointer(expression->type)) {
			return true;
		}
	} else if (expression->type == type_unsized_integer) {
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
			expression->type = type;
			return true;
		}

		if (types_match(type, type_f32) || types_match(type, type_f64)) {
			expression->type = type;
			return true;
		}
	} else if (expression->type == type_unsized_float) {
		if (types_match(type, type_f32) || types_match(type, type_f64)) {
			expression->type = type;
			return true;
		}
	} else if (expression->kind == Ast_lambda) {
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
		CastType request = {get_struct(expression->type), get_struct(type)};
		auto found_built_in = find(built_in_casts, request);

		if (found_built_in && found_built_in->implicit) {
			expression = make_cast(expression, type);
			return true;
		}
	}

	if (untypechecked_implicit_casts_count) {
		while (state->no_progress_counter != NO_PROGRESS_THRESHOLD) {
			for (auto lambda : implicit_casts) {
				if (types_match(lambda->return_parameter->type, type) && types_match(lambda->parameters[0]->type, expression->type)) {
					auto call = AstCall::create();
					call->location = expression->location;
					call->callable = lambda;
					call->arguments = make_list({CallArgument{expression}});
					call->type = type;
					expression = call;
					state->no_progress_counter = 0;
					return true;
				}
			}
			++state->no_progress_counter;
			yield(TypecheckResult::wait);
		}
	}

	if (reporter) {
		reporter->error(expression->location, "Expression of type {} is not implicitly convertible to {}.", type_to_string(expression->type), type_to_string(type));
	}
	return false;
}

void wait_iteration(TypecheckState *state, String name) {
	state->no_progress_counter++;
	if (state->no_progress_counter == NO_PROGRESS_THRESHOLD) { /* TODO: This is not the best solution */
		if (state->currently_typechecking_definition && state->currently_typechecking_definition->name == name) {
			state->reporter.error(name, "Can't use object while defining it.");
			state->reporter.info(state->currently_typechecking_definition->location, "Declared here:");
		} else {
			state->reporter.error(name, "Undeclared identifier");
		}
		yield(TypecheckResult::fail);
	}
	yield(TypecheckResult::wait);
}

DefinitionList wait_for_definitions(TypecheckState *state, String name) {
	while (1) {
		auto definitions = get_definitions(state, name);
		if (definitions.count) {
			for (auto definition : definitions) {
				while (!definition->type) {
					wait_iteration(state, name);
				}
			}
			state->no_progress_counter = 0;
			return definitions;
		}

		wait_iteration(state, name);
	}
}

bool ensure_addressable(Reporter *reporter, AstExpression *expression) {
	timed_function(context.profiler);
	switch (expression->kind) {
		case Ast_identifier: {
			return true;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != BinaryOperation::dot)
				break;

			return ensure_addressable(reporter, binop->right);
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(subscript->expression->kind == Ast_identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return ensure_addressable(reporter, identifier);
		}
						  /*
		case Ast_unary_operator: {
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
bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
	timed_function(context.profiler);
	switch (expression->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			auto definition = identifier->definition;
			if (definition->is_constant) {
				reporter->error(identifier->location, "Can't assign to '{}' because it is constant.", identifier->location);
				return false;
			}
			if (definition->is_parameter) {
				reporter->error(identifier->location, "Can't assign to function parameters.");
				return false;
			}

			return true;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != BinaryOperation::dot)
				break;

			return ensure_assignable(reporter, binop->left) && ensure_assignable(reporter, binop->right);
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(subscript->expression->kind == Ast_identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return ensure_assignable(reporter, identifier);
		}
		case Ast_unary_operator: {
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
	if (expression->type->kind == Ast_subscript || is_pointer(expression->type))
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

void typecheck(TypecheckState *state, AstStatement *statement) {
	auto _statement = statement;

	switch (statement->kind) {
		case Ast_return: {
			auto ret = (AstReturn *)statement;
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
					if (!types_match(lambda->return_parameter->type, type_void)) {
						state->reporter.warning(ret->location, "No return expression provided. If you did't assign a value to the return parameter, this will return the default value");
					}
				}
			}
			state->current_lambda->return_statements.add(ret);
			break;
		}
		case Ast_definition: {
			auto definition = (AstDefinition *)statement;
			if (definition->built_in) {
				break;
			}

			auto previous_definition = state->currently_typechecking_definition;
			state->currently_typechecking_definition = definition;
			defer { state->currently_typechecking_definition = previous_definition; };

			auto lambda = state->current_lambda;

			bool is_parameter = definition->is_parameter;
			enum {
				Global,
				Lambda,
				Struct,
			} location;

			if (definition->parent_block) {
				switch (definition->parent_block->kind) {
					case Ast_struct: location = Struct; break;
					case Ast_lambda: location = Lambda; break;
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
				if (location == Global && definition->expression->kind == Ast_lambda) {
					state->lambda = (AstLambda *)definition->expression;
				}

				typecheck(state, definition->expression);
			}

			if (definition->type) {

				// Lambda has already set it's type, so don't typecheck it
				if (!(definition->expression && definition->expression->kind == Ast_lambda)) {

					typecheck(state, definition->type);

					// auto evaluated_type = evaluate(state, definition->type);
					// assert(evaluated_type);
					// assert(evaluated_type->literal_kind == LiteralKind::type);
					// definition->type = evaluated_type->type_value;

					if (definition->expression) {
						if (!implicitly_cast(state, &state->reporter, &definition->expression, definition->type)) {
							yield(TypecheckResult::fail);
						}
					}
				}
			} else {
				if (definition->expression->kind == Ast_unary_operator && ((AstUnaryOperator *)definition->expression)->operation == UnaryOperation::autocast) {
					state->reporter.error(definition->location, "Can't deduce a type from autocast. You need to explicitly specify the resulting type");
					yield(TypecheckResult::fail);
				}

				if (!definition->is_constant) {
					harden_type(definition->expression);
				}
				definition->type = definition->expression->type;
			}


			if (definition->is_constant) {
				if (!is_constant(definition->expression)) {
					state->reporter.error(definition->location, "Definition marked as constant, but assigned expression is not constant");
					yield(TypecheckResult::fail);
				}
			}

			assert(definition->type);

			if (definition->type->kind != Ast_import) {
				if (!is_type(definition->type) && get_size(definition->type) == 0) {
					state->reporter.error(definition->location.data ? definition->location : definition->type->location, "Defining a variable with 0 size is not allowed");
					yield(TypecheckResult::fail);
				}
			}

			break;
		}
		case Ast_if: {
			auto If = (AstIf *)statement;

			typecheck(state, If->condition);
			if (!implicitly_cast(state, &state->reporter, &If->condition, type_bool)) {
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


				scope->parent->children.erase(*find(scope->parent->children, scope));
				scope->parent->append(*scope);
			} else {
				typecheck_scope(&If->true_scope);
				typecheck_scope(&If->false_scope);
			}

			break;
		}
		case Ast_while: {
			auto While = (AstWhile *)statement;

			typecheck(state, While->condition);
			if (!implicitly_cast(state, &state->reporter, &While->condition, type_bool)) {
				yield(TypecheckResult::fail);
			}

			typecheck_scope(&While->scope);

			break;
		}
		case Ast_expression_statement: {
			auto es = (AstExpressionStatement *)statement;
			typecheck(state, es->expression);
			break;
		}
		case Ast_block: {
			auto block = (AstBlock *)statement;
			typecheck_scope(&block->scope);
			break;
		}
		case Ast_test: {
			auto test = (AstTest *)statement;

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
				fiber_result = (int)TypecheckResult::success;
			};

			auto fiberstate = CreateFiber(4096, typechecker, &test_params);
			if (!fiberstate) {
				immediate_error(statement->location, "INTERNAL COMPILER ERROR: Failed to create fiber");
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

			while (1) {
				SWITCH_TO_FIBER(fiberstate);
				switch ((TypecheckResult)fiber_result) {
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
						auto old_fiber = state->fiber;
						state->fiber = original_fiber;
						defer { state->fiber = old_fiber; };
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

			break;
		}
		case Ast_assert: {
			auto assert = (AstAssert *)statement;
			typecheck(state, assert->condition);

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

			break;
		}
		case Ast_defer: {
			auto Defer = (AstDefer *)statement;

			typecheck_scope(&Defer->scope);

			break;
		}
		case Ast_print: {
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
					state->reporter.info(print->expression->location, "{}", result->string);
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
					invalid_code_path("not implemented");
			}
			break;
		}
		case Ast_operator_definition: {
			auto op = (AstOperatorDefinition *)statement;
			switch (op->operation) {
				case Token_as: {
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
				{
					typecheck(state, (Expression<> &)op->lambda);
					binary_operators.get_or_insert(binary_operation_from_token(op->operation)).add(op->lambda);
					break;
				}
				default: invalid_code_path();
			}
			break;
		}
		case Ast_parse: {
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
	if (a == type_unsized_integer || b == type_unsized_integer)
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


void typecheck(TypecheckState *state, Expression<> &expression);
AstExpression *typecheck(TypecheckState *state, AstIdentifier *identifier) {
	// if (identifier->location == "HDC"str)
	// 	debug_break();

	if (identifier->definition)
		return identifier;

	auto definitions = wait_for_definitions(state, identifier->name);

	if (definitions.count == 1) {
		auto definition = definitions[0];
		assert(definition->type);
		identifier->definition = (definition);
		identifier->type = definition->type;
	} else {
		identifier->possible_definitions = definitions;
	}
	return identifier;
}
AstExpression *typecheck(TypecheckState *state, AstCall *call) {
	typecheck(state, call->callable);

	auto &arguments = call->arguments;
	for (auto &argument : arguments) {
		typecheck(state, argument.expression);
	}

	struct Match {
		AstDefinition *definition = 0;
		AstLambda *lambda = 0;
		int distance = 0;
	};

	List<Match> matches;
	matches.allocator = temporary_allocator;

	if (call->callable->kind == Ast_identifier) {
		auto identifier = (AstIdentifier *)call->callable;
		Match match;
		// TODO: These branches have similar code
		if (identifier->definition) {
			auto definition = identifier->definition;
			if (definition->expression)
				match = {definition, get_lambda(definition->expression)};
			else
				match = {definition, get_lambda(definition->type)};

			if (!match.lambda) {
				state->reporter.error(call->location, "No lambda with that name was found");
				state->reporter.info (match.definition->location, "Found a definition");
				yield(TypecheckResult::fail);
			}

			auto lambda = match.lambda;
			while (!lambda->finished_typechecking_head)
				yield(TypecheckResult::wait);

			if (arguments.count != lambda->parameters.count) {
				state->reporter.error(call->location, "Argument count does not match");
				state->reporter.info(match.definition->location, "Definition is here:");
				yield(TypecheckResult::fail);
			}


			List<CallArgument *> sorted_arguments;
			sorted_arguments.resize(arguments.count);
			for (int argument_index = 0; argument_index < arguments.count; ++argument_index) {
				auto &argument = arguments[argument_index];
				if (!argument.name.empty()) {
					bool found = false;
					for (int parameter_index = 0; parameter_index < arguments.count; ++parameter_index) {
						auto &parameter = lambda->parameters[parameter_index];
						if (argument.name == parameter->name) {
							sorted_arguments[parameter_index] = &argument;
							found = true;
							break;
						}
					}
					if (!found) {
						state->reporter.error(argument.expression->location, "'{}' does not have a parameter named '{}'.", call->callable->location, argument.name);
						yield(TypecheckResult::fail);
					}
				}
			}
			for (auto &arg : arguments) {
				if (arg.name.empty()) {
					for (auto &sorted_arg : sorted_arguments) {
						if (!sorted_arg) {
							sorted_arg = &arg;
							break;
						}
					}
				}
			}

			auto old = arguments;
			arguments = map(sorted_arguments, [](auto x) { return *x; });
			free(old);

			for (u32 i = 0; i < arguments.count; ++i) {
				auto &argument = arguments[i].expression;
				auto &parameter = lambda->parameters[i];
				if (!implicitly_cast(state, &state->reporter, &argument, parameter->type)) {
					yield(TypecheckResult::fail);
				}
			}
		} else {
			auto named_argument_count = count(call->arguments, [&](auto arg){ return arg.name.data != 0; });
			if (named_argument_count != 0) {
				state->reporter.error(call->location, "INTERNAL COMPILER ERROR: Named arguments are not implemented for overloaded functions for now.");
				yield(TypecheckResult::fail);
			}

			for (auto definition : identifier->possible_definitions) {
				auto e = direct(definition->expression);
				if (e->kind != Ast_lambda)
					continue;

				auto lambda = (AstLambda *)e;
				while (!lambda->finished_typechecking_head)
					yield(TypecheckResult::wait);

				if (arguments.count != lambda->parameters.count)
					continue;

				int total_distance = 0;

				for (u32 i = 0; i < arguments.count; ++i) {
					auto &argument = arguments[i].expression;
					auto &parameter = lambda->parameters[i];
					int distance = 0;
					if (!implicitly_cast(state, 0, &argument, parameter->type, &distance)) { // no reports here, just checking
						goto _continue_definition0;
					}
					total_distance += distance;
				}

				matches.add({definition, lambda, total_distance});
			_continue_definition0:;
			}
			if (matches.count == 0) {
				if (identifier->possible_definitions.count) {
					state->reporter.error(call->location, "No matching lambda was found");
					state->reporter.info("Here is the list of possible overloads:");
					u64 overload_index = 0;
					for (auto definition : identifier->possible_definitions) {
						defer { ++overload_index; };
						state->reporter.info(definition->location, "Overload #{}:", overload_index);
					}
					state->reporter.info("Here are the reasons of failed overload resolutions:");
					overload_index = 0;
					for (auto definition : identifier->possible_definitions) {
						defer { ++overload_index; };
						state->reporter.info(definition->location, "Overload #{}:", overload_index);
						auto e = direct(definition->expression);
						if (e->kind != Ast_lambda) {
							state->reporter.info("This is not a lambda.");
							continue;
						}

						auto lambda = (AstLambda *)e;
						assert(lambda->finished_typechecking_head);
						// How this can happen?
						// while (!lambda->finished_typechecking_head)
						// 	yield(TypecheckResult::wait);

						if (arguments.count != lambda->parameters.count) {
							state->reporter.info("Argument count does not match.");
							continue;
						}

						for (u32 i = 0; i < arguments.count; ++i) {
							auto &argument = arguments[i].expression;
							auto &parameter = lambda->parameters[i];
							if (!implicitly_cast(state, &state->reporter, &argument, parameter->type)) { // implicitly_cast will add a report
								state->reporter.info(parameter->location, "Invalid conversion for parameter #{}:", i);
								goto _continue_definition1;
							}
						}
					_continue_definition1:;
					}
				} else {
					state->reporter.error(call->location, "Lambda with that name was not defined");
				}
				yield(TypecheckResult::fail);
			} else if (matches.count == 1) {
				match = matches[0];
			} else {
				List<Match> ideal_matches;
				for (auto match : matches) {
					if (match.distance == 0)
						ideal_matches.add(match);
				}

				if (ideal_matches.count == 1) {
					match = ideal_matches[0];
				} else {
					state->reporter.error(call->location, "Ambiguous overloads were found");
					for (auto match : matches) {
						state->reporter.info(match.definition->location, "Here");
					}
					yield(TypecheckResult::fail);
				}
			}

			identifier->definition = match.definition;
			identifier->type = match.definition->type;
			//identifier->possible_definitions = {}; // actually this is redundant
		}

		auto lambda = match.lambda;
		call->type = lambda->return_parameter->type;
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
			literal->type = type_unsized_integer;
			break;
		case boolean:
			literal->type = type_bool;
			break;
		case string:
			literal->type = type_string;
			break;
		case character:
			literal->type = type_u8;
			break;
		case noinit:
			literal->type = type_noinit;
			break;
		default:
			invalid_code_path("not implemented");
	}
	return literal;
}
AstExpression *typecheck(TypecheckState *state, AstLambda *lambda) {
	lambda->parent_lambda = state->current_lambda;

	auto old_current_lamdda = state->current_lambda;
	defer { assert(state->current_lambda == old_current_lamdda); };

	state->current_lambda = lambda;
	assert(state->current_lambda);
	defer { state->current_lambda = old_current_lamdda; };

	if (lambda->return_parameter) {
		typecheck(state, lambda->return_parameter);
	}

	push_scope(&lambda->parameter_scope);
	for (auto parameter : lambda->parameters) {
		typecheck(state, parameter);
	}

	lambda->type = lambda; // MYTYPEISME

	// lambda->definition can be null in case the lambda is a type pointer to lambda
	if (lambda->definition) {
		lambda->definition->type = lambda->type;
	}

	lambda->finished_typechecking_head = true;

	if (lambda->has_body) {
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
			lambda->return_parameter = make_retparam(type_void, lambda);
		}
		ensure_return_types_match(state, lambda);
	}
	return lambda;
}
AstExpression *typecheck(TypecheckState *state, AstBinaryOperator *bin) {
	using enum BinaryOperation;

	typecheck(state, bin->left);
	if (!bin->left->type) {
		if (bin->left->kind == Ast_identifier) {
			auto ident = (AstIdentifier *)bin->left;
			if (!ident->definition) {
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

	auto report_type_mismatch = [&] {
		state->reporter.error(bin->location, "Can't use binary {} on types {} and {}", operator_string(bin->operation), type_to_string(bin->left->type), type_to_string(bin->right->type));
		yield(TypecheckResult::fail);
	};

	if (bin->operation == dot) {
		if (bin->left->type->kind == Ast_import) {
			// invalid_code_path("not implemented");
			// auto import = (AstImport *)get_definition_expression(bin->left);
			// assert(import->kind == Ast_import);
			// import->scope;
		} else {
			assert(bin->right->kind == Ast_identifier);
			auto member_identifier = (AstIdentifier *)bin->right;
			auto name = member_identifier->name;

			//harden_type(&bin->left->type);

			bool left_is_type = is_type(bin->left);
			auto Struct = get_struct(left_is_type ? bin->left : bin->left->type);

			if (Struct) {
				auto found_member = find_if(left_is_type ? Struct->constants : Struct->members, [&](AstDefinition *member) { return member->name == name; });
				if (!found_member) {
					if (left_is_type) {
						state->reporter.error(bin->right->location, "Type '{}' does not contain constant '{}'", Struct->definition->name, bin->right->location);
					} else {
						state->reporter.error(bin->right->location, "'{}' is not a member of '{}'", bin->right->location, Struct->definition->name);
					}
					yield(TypecheckResult::fail);
				}

				member_identifier->definition = (*found_member);
				member_identifier->type = (*found_member)->type;
				bin->type = bin->right->type;
			} else if (is_sized_array(bin->left->type)) {
				if (bin->right->kind != Ast_identifier) {
					state->reporter.error(bin->left->location, "The only members of any array type are identifiers 'data' and 'count'");
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
					return make_integer(size->integer); // unsized
				} else {
					state->reporter.error(bin->left->location, "The only members of any array type are identifiers 'data' and 'count'. You asked for '{}', which does not exist", identifier->name);
					yield(TypecheckResult::fail);
				}
			} else {
				state->reporter.error(bin->left->location, "Dot operator can not be applied to an expression of type {}", type_to_string(bin->left->type));
				yield(TypecheckResult::fail);
			}
		}
	} else if (bin->operation == as) {
		auto cast = bin;
		typecheck(state, cast->left);
		typecheck(state, cast->right);
		cast->type = cast->right;

		auto &src_type = cast->left->type;
		auto &dst_type = cast->type;

#define breakable_scope for(bool CONCAT(_bs_, __LINE__)=true;CONCAT(_bs_, __LINE__);CONCAT(_bs_, __LINE__)=false)

		breakable_scope {
			auto found_built_in = find(built_in_casts, {get_struct(src_type), get_struct(dst_type)});
			if (found_built_in) {
				//cast->cast_kind = found_built_in->kind;
				break;
			}

			if (::is_integer(src_type)) {
				if (src_type == type_unsized_integer) {
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
						return c.from == type_u64 && types_match(c.to, dst_type);
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
		bin->type = type_void;
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
		}

		if (l->kind == Ast_struct && r->kind == Ast_struct) {
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
					if (l == type_unsized_integer && r == type_unsized_integer) {
						bin->type = type_unsized_integer;
						return bin;
					}
					if (l == type_unsized_integer && ri) {
						bin->type = bin->left->type = r;
						return bin;
					}
					if (li && r == type_unsized_integer) {
						bin->type = bin->right->type = l;
						return bin;
					}
					if (lf && r == type_unsized_integer) {
						bin->type = bin->right->type = l;
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
					bin->type = type_bool;
					if (l == type_unsized_integer && r == type_unsized_integer) {
						return bin;
					} else if (l == type_unsized_integer && ri) {
						bin->left->type = r;
						return bin;
					} else if (r == type_unsized_integer && li) {
						bin->right->type = l;
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
								bin->type = l;
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
					bin->type = type_bool;

					if (types_match(l, r)) {
						return bin;
					}

					if ((li && r == type_unsized_integer) || (ri && l == type_unsized_integer)) {
						return bin;
					}

					break;
				}

				case lt:
				case gt:
				case le:
				case ge: {
					bin->type = type_bool;

					if (li && ri) {
						if (signedness_matches(l, r)) {
							return bin;
						}
						break;
					}

					if ((li && r == type_unsized_integer) || (ri && l == type_unsized_integer)) {
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
					if ((li && r == type_unsized_integer) || (ri && l == type_unsized_integer)) {
						bin->type = r == type_unsized_integer ? l : r;
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
				case ne: {
					bin->type = type_bool;
					if (types_match(l->expression, r->expression))
						return bin;
					break;
				}
			}
		} else if ((lp && ri) || (li && rp)) {
			// One is pointer and other is integer
			if (lp) {
				bin->type = l;
				harden_type(bin->right);
			} else {
				bin->type = r;
				harden_type(bin->left);
			}
			return bin;
		}

		auto overloaded = find_user_defined_binary_operator(state, bin->operation, bin->left->type, bin->right->type);
		if (overloaded) {
			auto call = AstCall::create();
			call->location = bin->location;
			call->arguments.add({bin->left});
			call->arguments.add({bin->right});
			call->callable = overloaded;
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

	typecheck(state, unop->expression);

	if (unop->operation == pointer_or_dereference) {
		unop->operation = is_type(unop->expression) ? pointer : dereference;
	}

	switch (unop->operation) {
		case minus: {
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
			if (!ensure_addressable(&state->reporter, unop->expression)) {
				yield(TypecheckResult::fail);
			}
			unop->type = make_pointer_type(unop->expression->type);
			break;
		}
		case pointer: {
			assert(is_type(unop->expression));
			unop->type = type_type;
			break;
		}
		case dereference: {
			assert(!is_type(unop->expression));
			if (!is_pointer(unop->expression->type)) {
				state->reporter.error(unop->location, "{} is not a pointer type, can't dereference it", type_to_string(unop->expression->type));
				yield(TypecheckResult::fail);
			}
			unop->type = ((AstUnaryOperator *)unop->expression->type)->expression;
			break;
		}
		case bnot: {
			unop->type = unop->expression->type;

			if (types_match(unop->expression->type, type_bool)) {
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

			return make_integer(get_size(size_of->expression));
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
			typeof->type = type_type;
			//  print("typeof {} is {}\n", typeof->expression->location, type_to_string(typeof->expression->type));
			break;
		}
		default: {
			invalid_code_path();
			yield(TypecheckResult::fail);
		}
	}
	return unop;
}
AstExpression *typecheck(TypecheckState *state, AstStruct *Struct) {
	auto definition = Struct->definition;

	//assert(!definition->type);
	//definition->type = type_type;
	Struct->type = type_type;


	s64 struct_size = 0;
	s64 struct_alignment = 0;

	{
		push_scope(&Struct->scope);
		for (auto member : Struct->members) {
			typecheck(state, member);
		}

		if (Struct->is_union) {
			for (auto member : Struct->members) {
				switch (Struct->layout) {
					case StructLayout::tlang: {
						member->offset_in_struct = 0;
						struct_size = max(struct_size, get_size(member->type));
						break;
					}
					case StructLayout::c: {
						auto member_alignment = get_align(member->type);
						struct_alignment = max(struct_alignment, member_alignment);

						member->offset_in_struct = 0;
						struct_size = max(struct_size, get_size(member->type));
						break;
					}
					default:
						invalid_code_path();
				}
			}
		} else {
			for (auto member : Struct->members) {
				switch (Struct->layout) {
					case StructLayout::tlang: {
						member->offset_in_struct = struct_size;
						struct_size += get_size(member->type);
						break;
					}
					case StructLayout::c: {
						auto member_alignment = get_align(member->type);
						struct_alignment = max(struct_alignment, member_alignment);
						assert(member_alignment >= 1);

						struct_size = ceil(struct_size, member_alignment);
						member->offset_in_struct = struct_size;
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
	typecheck(state, subscript->index_expression);

	harden_type(subscript->index_expression);

	if (!::is_integer(subscript->index_expression->type)) {
		state->reporter.error(subscript->index_expression->location, "Expression must be of type integer but is {}", type_to_string(subscript->index_expression->type));
		yield(TypecheckResult::fail);
	}

	typecheck(state, subscript->expression);
	if (is_type(subscript->expression)) {
		subscript->type = type_type;

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
		if (type->kind == Ast_subscript) {
			// subscript->is_simd = ((AstSubscript *)type)->is_simd;
			subscript->type = ((AstSubscript *)type)->expression;
		} else if (is_pointer(type)) {
			subscript->type = ((AstUnaryOperator *)type)->expression;
		} else {
			invalid_code_path();
		}
	}
	return subscript;
}
AstExpression *typecheck(TypecheckState *state, AstIfx *If) {
	typecheck(state, If->condition);
	if (!implicitly_cast(state, &state->reporter, &If->condition, type_bool)) {
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
void typecheck(TypecheckState *state, Expression<> &expression) {
	assert(expression);

	// Skip typechecking builtin expressions
	if (expression->type && expression->type->type) {
		return;
	}
	defer {
		if (expression->kind == Ast_identifier && ((AstIdentifier *)expression)->possible_definitions.count) {
		} else {
			assert(expression->type);
			if (!SIMPLIFY(&state->reporter, expression))
				yield(TypecheckResult::fail);
			assert(expression->type);
		}
	};

	switch (expression->kind) {
		case Ast_identifier:      expression = typecheck(state, (AstIdentifier     *)expression); return;
		case Ast_call:            expression = typecheck(state, (AstCall           *)expression); return;
		case Ast_literal:         expression = typecheck(state, (AstLiteral        *)expression); return;
		case Ast_lambda:          expression = typecheck(state, (AstLambda         *)expression); return;
		case Ast_binary_operator: expression = typecheck(state, (AstBinaryOperator *)expression); return;
		case Ast_unary_operator:  expression = typecheck(state, (AstUnaryOperator  *)expression); return;
		case Ast_struct:          expression = typecheck(state, (AstStruct         *)expression); return;
		case Ast_subscript:       expression = typecheck(state, (AstSubscript      *)expression); return;
		case Ast_ifx:             expression = typecheck(state, (AstIfx            *)expression); return;
		case Ast_import: {
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

void stack_overflow() {
	stack_overflow();
}

void typecheck_global(CoroState *corostate, TypecheckState *state) {
	while (1) {
		{
			push_scope(&global_scope);
			typecheck(state, state->statement);
		}
		yield(TypecheckResult::success);

		//
		// Now the statement is fully typechecked.
		// If we got past the above yield, this means that this
		// coroutine was pooled and now is used for another statement.
		//

		state = state->next_state;
	}
}
void layer(CoroState *corostate, TypecheckState *state) {
	CORO_BEGIN
	// stack_overflow();
	typecheck_global(corostate, state);
	CORO_END
}
umm typecheck_coroutine(CoroState *corostate, umm param) {
	layer(corostate, (TypecheckState *)param);
	return 0;
}


VOID WINAPI typecheck_fiber(LPVOID lpFiberParameter) {
	// stack_overflow();
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
	d->offset_in_struct = offset;
	d->parent_block = destination;
	if (value) {
		value->type = type;
	}

	(constant ? destination->constants : destination->members).add(d);
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
};

ParsedArguments parse_arguments(Span<Span<utf8>> arguments) {
	timed_function(context.profiler);

	ParsedArguments result = {};

	context.compiler_path = (String)get_executable_path();

	auto parsed = parse_path(context.compiler_path);
	context.compiler_name = parsed.name;
	context.compiler_directory = parsed.directory;

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
	{
		Map<int, float> map;
		for (int i = 0; i < 1024*1024; ++i) {
			map.get_or_insert(i) = i;
		}
	}


	defer { print(""); }; // to reset console color

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

	triple_char_tokens.insert(">>="str);
	triple_char_tokens.insert("<<="str);

	construct(keywords);
#define E(name, value) keywords.get_or_insert((String)u8#name##s) = value;
	ENUMERATE_KEYWORDS(E);
#undef E

	auto init_type = [&](AstStruct *&s, String name, s64 size, s64 align) {
		s = AstStruct::create();
		s->members.allocator = {};
		s->constants.allocator = {};
		s->size = size;
		s->alignment = align;
		s->type = type_type;
		s->location = name;

		auto definition = AstDefinition::create();
		definition->is_constant = true;
		definition->expression = s;
		definition->location = definition->name = name;
		definition->type = type_type;

		definition->built_in = true;

		s->definition = definition;

		global_scope.statements.add(definition);
		global_scope.definitions.get_or_insert(name).add(definition);
		//typechecked_globals.get_or_insert(name) = definition;
	};

	init_type(type_type,   "type"str, 8, 8);
	init_type(type_void,   "void"str, 0, 0);
	init_type(type_bool,   "bool"str, 1, 1);
	init_type(type_u8,     "u8"str,   1, 1);
	init_type(type_u16,    "u16"str,  2, 2);
	init_type(type_u32,    "u32"str,  4, 4);
	init_type(type_u64,    "u64"str,  8, 8);
	init_type(type_s8,     "s8"str,   1, 1);
	init_type(type_s16,    "s16"str,  2, 2);
	init_type(type_s32,    "s32"str,  4, 4);
	init_type(type_s64,    "s64"str,  8, 8);
	init_type(type_f32,    "f32"str,  4, 4);
	init_type(type_f64,    "f64"str,  8, 8);
	init_type(type_string, "string"str, context.register_size * 2, 8);
	init_type(type_unsized_integer,  "unsized integer"str, 0, 0);
	init_type(type_unsized_float,  "unsized float"str, 0, 0);
	init_type(type_noinit, "(noinit)"str, 0, 0);

	type_pointer_to_void = AstUnaryOperator::create();
	type_pointer_to_void->expression = type_void;
	type_pointer_to_void->operation = UnaryOperation::pointer;
	type_pointer_to_void->type = type_type;

	switch (context.register_size) {
		case 4:
			type_default_signed_integer = type_s32;
			type_default_unsigned_integer = type_u32;
			break;
		case 8:
			type_default_signed_integer = type_s64;
			type_default_unsigned_integer = type_u64;
			break;
	}
	type_default_integer = type_default_signed_integer;
	type_default_float = type_f64;

	add_member(type_u8,  type_u8,  "min"str, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u16, type_u16, "min"str, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u32, type_u32, "min"str, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u64, type_u64, "min"str, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u8,  type_u8,  "max"str, make_integer(0xff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u16, type_u16, "max"str, make_integer(0xffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u32, type_u32, "max"str, make_integer(0xffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u64, type_u64, "max"str, make_integer(0xffffffffffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s8,  type_s8,  "min"str, make_integer(0x80), true, INVALID_MEMBER_OFFSET);
	add_member(type_s16, type_s16, "min"str, make_integer(0x8000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s32, type_s32, "min"str, make_integer(0x80000000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s64, type_s64, "min"str, make_integer(0x8000000000000000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s8,  type_s8,  "max"str, make_integer(0x7f), true, INVALID_MEMBER_OFFSET);
	add_member(type_s16, type_s16, "max"str, make_integer(0x7fff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s32, type_s32, "max"str, make_integer(0x7fffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s64, type_s64, "max"str, make_integer(0x7fffffffffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_string, make_pointer_type(type_u8), "data"str, 0, false, 0);
	add_member(type_string, type_default_unsigned_integer, "count"str, 0, false, 8);

	auto add_global_alias = [&](String name, AstExpression *expression) {
		auto defn = AstDefinition::create();
		defn->location = defn->name = name;
		defn->is_constant = true;
		defn->built_in = true;
		defn->type = expression->type;
		defn->expression = expression;
		// defn->set_parent_scope(&global_scope);
		global_scope.definitions.get_or_insert(defn->name).add(defn);
		global_scope.statements.add(defn);

		auto ident = AstIdentifier::create();
		ident->location =ident->name = name;
		ident->definition = defn;
		ident->type = expression->type;
		return ident;
	};
	type_sint = add_global_alias("sint"str, type_default_integer);
	type_uint = add_global_alias("uint"str, type_default_unsigned_integer);
	type_int  = add_global_alias("int"str, type_default_signed_integer);

	integer_infos[0] = {type_u8,  -make_big_int<BigInteger>(0xff),               make_big_int<BigInteger>(0xff)              };
	integer_infos[1] = {type_u16, -make_big_int<BigInteger>(0xffff),             make_big_int<BigInteger>(0xffff)            };
	integer_infos[2] = {type_u32, -make_big_int<BigInteger>(0xffffffff),         make_big_int<BigInteger>(0xffffffff)        };
	integer_infos[3] = {type_u64, -make_big_int<BigInteger>(0xffffffffffffffff), make_big_int<BigInteger>(0xffffffffffffffff)};
	integer_infos[4] = {type_s8,  -make_big_int<BigInteger>(0xff),               make_big_int<BigInteger>(0xff)              };
	integer_infos[5] = {type_s16, -make_big_int<BigInteger>(0xffff),             make_big_int<BigInteger>(0xffff)            };
	integer_infos[6] = {type_s32, -make_big_int<BigInteger>(0xffffffff),         make_big_int<BigInteger>(0xffffffff)        };
	integer_infos[7] = {type_s64, -make_big_int<BigInteger>(0xffffffffffffffff), make_big_int<BigInteger>(0xffffffffffffffff)};

	built_in_casts.insert({type_u8 , type_s8 , /*CastKind::u8_s8  , */false});
	built_in_casts.insert({type_u8 , type_s16, /*CastKind::u8_s16 , */true});
	built_in_casts.insert({type_u8 , type_s32, /*CastKind::u8_s32 , */true});
	built_in_casts.insert({type_u8 , type_s64, /*CastKind::u8_s64 , */true});
	built_in_casts.insert({type_u8 , type_u16, /*CastKind::u8_u16 , */true});
	built_in_casts.insert({type_u8 , type_u32, /*CastKind::u8_u32 , */true});
	built_in_casts.insert({type_u8 , type_u64, /*CastKind::u8_u64 , */true});
	built_in_casts.insert({type_u16, type_s8 , /*CastKind::u16_s8 , */false});
	built_in_casts.insert({type_u16, type_s16, /*CastKind::u16_s16, */false});
	built_in_casts.insert({type_u16, type_s32, /*CastKind::u16_s32, */true});
	built_in_casts.insert({type_u16, type_s64, /*CastKind::u16_s64, */true});
	built_in_casts.insert({type_u16, type_u8 , /*CastKind::u16_u8 , */false});
	built_in_casts.insert({type_u16, type_u32, /*CastKind::u16_u32, */true});
	built_in_casts.insert({type_u16, type_u64, /*CastKind::u16_u64, */true});
	built_in_casts.insert({type_u32, type_s8 , /*CastKind::u32_s8 , */false});
	built_in_casts.insert({type_u32, type_s16, /*CastKind::u32_s16, */false});
	built_in_casts.insert({type_u32, type_s32, /*CastKind::u32_s32, */false});
	built_in_casts.insert({type_u32, type_s64, /*CastKind::u32_s64, */true});
	built_in_casts.insert({type_u32, type_u8 , /*CastKind::u32_u8 , */false});
	built_in_casts.insert({type_u32, type_u16, /*CastKind::u32_u16, */false});
	built_in_casts.insert({type_u32, type_u64, /*CastKind::u32_u64, */true});
	built_in_casts.insert({type_u64, type_s8 , /*CastKind::u64_s8 , */false});
	built_in_casts.insert({type_u64, type_s16, /*CastKind::u64_s16, */false});
	built_in_casts.insert({type_u64, type_s32, /*CastKind::u64_s32, */false});
	built_in_casts.insert({type_u64, type_s64, /*CastKind::u64_s64, */false});
	built_in_casts.insert({type_u64, type_u8 , /*CastKind::u64_u8 , */false});
	built_in_casts.insert({type_u64, type_u16, /*CastKind::u64_u16, */false});
	built_in_casts.insert({type_u64, type_u32, /*CastKind::u64_u32, */false});
	built_in_casts.insert({type_s8 , type_s16, /*CastKind::s8_s16 , */true});
	built_in_casts.insert({type_s8 , type_s32, /*CastKind::s8_s32 , */true});
	built_in_casts.insert({type_s8 , type_s64, /*CastKind::s8_s64 , */true});
	built_in_casts.insert({type_s8 , type_u8 , /*CastKind::s8_u8  , */false});
	built_in_casts.insert({type_s8 , type_u16, /*CastKind::s8_u16 , */false});
	built_in_casts.insert({type_s8 , type_u32, /*CastKind::s8_u32 , */false});
	built_in_casts.insert({type_s8 , type_u64, /*CastKind::s8_u64 , */false});
	built_in_casts.insert({type_s16, type_s8 , /*CastKind::s16_s8 , */false});
	built_in_casts.insert({type_s16, type_s32, /*CastKind::s16_s32, */true});
	built_in_casts.insert({type_s16, type_s64, /*CastKind::s16_s64, */true});
	built_in_casts.insert({type_s16, type_u8 , /*CastKind::s16_u8 , */false});
	built_in_casts.insert({type_s16, type_u16, /*CastKind::s16_u16, */false});
	built_in_casts.insert({type_s16, type_u32, /*CastKind::s16_u32, */false});
	built_in_casts.insert({type_s16, type_u64, /*CastKind::s16_u64, */false});
	built_in_casts.insert({type_s32, type_s8 , /*CastKind::s32_s8 , */false});
	built_in_casts.insert({type_s32, type_s16, /*CastKind::s32_s16, */false});
	built_in_casts.insert({type_s32, type_s64, /*CastKind::s32_s64, */true});
	built_in_casts.insert({type_s32, type_u8 , /*CastKind::s32_u8 , */false});
	built_in_casts.insert({type_s32, type_u16, /*CastKind::s32_u16, */false});
	built_in_casts.insert({type_s32, type_u32, /*CastKind::s32_u32, */false});
	built_in_casts.insert({type_s32, type_u64, /*CastKind::s32_u64, */false});
	built_in_casts.insert({type_s64, type_s8 , /*CastKind::s64_s8 , */false});
	built_in_casts.insert({type_s64, type_s16, /*CastKind::s64_s16, */false});
	built_in_casts.insert({type_s64, type_s32, /*CastKind::s64_s32, */false});
	built_in_casts.insert({type_s64, type_u8 , /*CastKind::s64_u8 , */false});
	built_in_casts.insert({type_s64, type_u16, /*CastKind::s64_u16, */false});
	built_in_casts.insert({type_s64, type_u32, /*CastKind::s64_u32, */false});
	built_in_casts.insert({type_s64, type_u64, /*CastKind::s64_u64, */false});

	built_in_casts.insert({type_f32, type_s32, false});
	built_in_casts.insert({type_s32, type_f32, false});

	built_in_casts.insert({type_f64, type_s64, false});
	built_in_casts.insert({type_s64, type_f64, false});

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
			failed_lexer->reporter->print_all();
			if (!printed_reports_count) {
				immediate_error("Lexer failed. Exiting.\n");
			}
			return 1;
		}
		if (failed_parser) {
			failed_parser->reporter->print_all();
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
		};

		scoped_phase("Typechecking");

		timed_block(context.profiler, "typecheck"str);

		auto main_fiber = ConvertThreadToFiber(0);

		Span<TypecheckState> typecheck_states;
		typecheck_states.count = count(global_scope.statements, [&](AstStatement *statement) { return !(statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in); });
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
				if (statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in)
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
					auto result = (TypecheckResult)fiber_result;
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
							state.reporter.print_all();
							if (result == TypecheckResult::fail) {
								fail = true;
							}
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
				if (!printed_reports_count) {
					immediate_error("Typechecking failed.");
				}
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

		if (!::is_integer(context.main_lambda->return_parameter->type) && !types_match(context.main_lambda->return_parameter->type, type_void)) {
			immediate_error(context.main_lambda->return_parameter->type->location.data ? context.main_lambda->return_parameter->type->location : context.main_lambda->location, "Main function can return any type of integer or void, but not {}", type_to_string(context.main_lambda->return_parameter->type));
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

	return 0;
}
