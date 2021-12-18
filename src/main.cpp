﻿#define TL_ENABLE_PROFILER 1
#define TL_IMPL
#include <tl/main.h>
#include <tl/cpu.h>
#include <tl/time.h>
#include <tl/hash_set.h>
#include <ast.h>
#include <output/c.h>
#include <output/nasm.h>
#include "../dep/cppcoro/include/cppcoro/generator.hpp"
#include "extern.h"
#include "bytecode.h"
#include "print_ast.h"
#include <string>

ThreadPool *thread_pool;

bool ensure_addressable(struct Reporter *reporter, AstExpression *expression);
AstExpression *make_pointer_type(AstExpression *type);

void print_help() {
	print(R"(Usage:
	% <path>
)", executable_name);
}

u32 get_line_number(utf8 *from) {
	u32 result = 1;
	while (*--from != 0)
		result += (*from == '\n');
	return result;
}

u32 get_column_number(utf8 *from) {
	u32 result = 0;
	while (1) {
		if (*from == '\n' || *from == '\0')
			break;

		result += 1;
		from -= 1;
	}
	return result;
}

void print_replacing_tabs_with_4_spaces(PrintKind kind, Span<utf8> string) {
	for (auto c : string) {
		if (c == '\t') {
			print(kind, "    ");
		} else {
			print(kind, c);
		}
	}
}

void print_source_line(Span<utf8> location) {

	if (location.data == nullptr) {
		// print("(null location)\n\n");
		return;
	}


	auto error_line_begin = location.begin();
	if (*error_line_begin != 0) {
		while (1) {
			error_line_begin--;
			if (*error_line_begin == 0 || *error_line_begin == '\n') {
				if (*error_line_begin != 0) {
					error_line_begin++;
				}
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
	auto error_line_number = get_line_number(error_line_begin);

	if (*error_line.data != 0) {
		auto prev_line_end = error_line.data - 1;
		auto prev_line_begin = prev_line_end - 1;

		while (1) {
			if (*prev_line_begin == 0)
				break;

			if (*prev_line_begin == '\n') {
				++prev_line_begin;
				break;
			}

			--prev_line_begin;
		}
		auto prev_line = Span(prev_line_begin, prev_line_end);
		auto prev_line_number = get_line_number(prev_line_begin);

		print(Print_warning, "%| ", prev_line_number);
		print("%\n", prev_line);
	}

	auto line_start = Span(error_line.begin(), location.begin());
	auto line_end   = Span(location.end(), error_line.end());
	auto offset = print(Print_warning, "%| ", error_line_number);
	print_replacing_tabs_with_4_spaces(Print_info,  line_start);
	print_replacing_tabs_with_4_spaces(Print_error, location);
	print_replacing_tabs_with_4_spaces(Print_info,  line_end);
	print('\n');

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
	for (auto c : location) {
		if (c == '\t') {
			print("^^^^");
		} else {
			print('^');
		}
	}
	print("\n\n");
}

struct SourceFileInfo {
	Span<utf8> path;
	Span<utf8> source;
};

List<SourceFileInfo> sources;

Span<utf8> get_source_path(Span<utf8> location) {
	for (auto source : sources) {
		if (source.source.begin() <= location.begin() && location.end() <= source.source.end()) {
			return source.path;
		}
	}
	return u8"unknown source"s;
}

struct Report {
	Span<utf8> location;
	Span<ascii> message;
};

template <class ...Args>
Report make_report(Span<utf8> location, Span<utf8> severity, char const *format_string, Args const &...args) {
	Report r;
	r.location = location;
	if (location.data) {
		r.message = format("%:%:%: %: %", get_source_path(location), get_line_number(location.begin()), get_column_number(location.begin()), severity, format(format_string, args...));
	} else {
		r.message = format("%: %", severity, format(format_string, args...));
	}
	return r;
}

template <class ...Args>
Report make_info_report(Span<utf8> location, char const *format_string, Args const &...args) {
	return make_report(location, u8"Info"s, format_string, args...);
}
template <class ...Args>
Report make_info_report(char const *format_string, Args const &...args) {
	return make_info_report(Span<utf8>{}, format_string, args...);
}
template <class ...Args>
Report make_error_report(Span<utf8> location, char const *format_string, Args const &...args) {
	return make_report(location, u8"Error"s, format_string, args...);
}

template <class ...Args>
Report make_error_report(char const *format_string, Args const &...args) {
	return make_error_report(Span<utf8>{}, format_string, args...);
}

void print_report(Report r) {
	print(r.message);
	print('\n');
	print_source_line(r.location);
}

template <class ...Args>
void immediate_info(char const *format_string, Args const &...args) {
	print_report(make_info_report(format_string, args...));
}
template <class ...Args>
void immediate_error(Span<utf8> location, char const *format_string, Args const &...args) {
	print_report(make_error_report(location, format_string, args...));
}
template <class ...Args>
void immediate_error(char const *format_string, Args const &...args) {
	immediate_error(Span<utf8>{}, format_string, args...);
}

struct Reporter {
	List<Report> reports;

	u32 checkpoint() {
		return (u32)reports.count;
	}
	void reset(u32 count) {
		reports.count = count;
	}

	void print_all() {
		for (auto report : reports) {
			print_report(report);
		}
	}
	template <class ...Args>
	void info(Span<utf8> location, char const *format_string, Args const &...args) {
		reports.add(make_info_report(location, format_string, args...));
	}
	template <class ...Args>
	void info(char const *format_string, Args const &...args) {
		reports.add(make_info_report(format_string, args...));
	}
	template <class ...Args>
	void error(char const *format_string, Args const &...args) {
		error(Span<utf8>{}, format_string, args...);
	}
	template <class ...Args>
	void error(Span<utf8> location, char const *format_string, Args const &...args) {
		reports.add(make_error_report(location, format_string, args...));
	}
};


struct Lexer {
	static constexpr u32 block_capacity = 4096;
	struct Block {
		Token tokens[block_capacity];
		umm size = 0;
		Block *next = 0;
	};

	struct Iterator {
		Lexer *lexer;
		Block *block;
		umm token_index;
		umm local_token_index;

		Iterator &operator++() {
			// It's critical to see if lexer is finished first, only then check `last` and `tokens_lexed`.
			// Lexer could set `finished` to true when we are at :LABEL:. In that case parser could get not every token
			bool lexer_finished = lexer->finished;

			_ReadWriteBarrier();
			_mm_mfence();

			if (block == lexer->last) {
				Spinner spinner;
				while (1) {
					if (token_index + 1 < lexer->tokens_lexed) {
						break;
					}

					// :LABEL:

					if (lexer_finished) {
						block = 0;
						return *this;
					}
					iteration(spinner);
				}
			}

			local_token_index += 1;
			token_index += 1;
			if (local_token_index == block_capacity) {
				local_token_index = 0;
				block = block->next;
			}

			return *this;
		}
		explicit operator bool() { return block; }

		Token *operator->() { return &block->tokens[local_token_index]; }
		Token &operator*() { return block->tokens[local_token_index]; }
	};

	Block first;
	Block *last = &first;
	Block *pop_last = &first;

	void add(Token token) {
		if (last->size == count_of(last->tokens)) {
			auto new_block = default_allocator.allocate<Block>();

			last->next = new_block;
			last = new_block;
		}

		last->tokens[last->size] = token;

		_ReadWriteBarrier();
		_mm_mfence();

		atomic_increment(&last->size);
		atomic_increment(&tokens_lexed);
	}

	Iterator begin() {
		return {
			this,
			&first,
			0
		};
	}

	u32 tokens_lexed = 0;
	bool finished = false;
	bool success = false;
	Reporter *reporter;
	Token *popped_token = 0;


	Buffer source_buffer;
	Span<utf8> source;
};

f32 lexer_time;
bool lexer_function(Lexer *lexer) {
	timed_function();

	auto timer = create_precise_timer();
	defer {
		lexer_time = reset(timer);
	};


	defer { lexer->finished = true; };

	HashMap<Span<utf8>, TokenKind> keywords;
#define E(name, value) keywords.get_or_insert(u8#name##s) = value;
	ENUMERATE_KEYWORDS(E);
#undef E

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

	const auto double_char_tokens = []() { // this shit cannot be constexpr...
		constexpr Span<ascii> tokens[] = {
			"=="s,
			"=>"s,
			"!="s,
			">="s,
			"<="s,
			"+="s,
			"-="s,
			"*="s,
			"/="s,
			"%="s,
			"|="s,
			"&="s,
			"^="s,
			"->"s,
			">>"s,
			"<<"s,
		};

		Array<StaticList<u8, 4>, 256> result = {{}};
		for (auto token : tokens) {
			result[token[0]].add(token[1]);
		}
		return result;
	}();

	while (1) {
	_continue:
		token.string.data = current_p;
		token.string.count = 0;

	nc:
		switch (c) {
			case '\0':
				lexer->success = true;
				return true;
			case ' ':
			case '\n':
			case '\r':
			case '\t':
				token.string.data += 1;
				if (!next_char()) {
					lexer->success = true;
					return true;
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
					lexer->success = true;
					return true;
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
				auto first_char = c;
				if (next_char()) {
					for (auto possibility : double_char_tokens[first_char]) {
						if (possibility == c) {
							token.kind = (first_char << 8) | possibility;
							next_char();
							token.string.count = 2;
							goto _push_token;
						}
					}
					goto single_lt;
				} else {
				single_lt:
					token.kind = first_char;
					token.string.count = 1;
				}

			_push_token:
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
							if (c == '\n')
								break;
						}
						continue;
					} else if (c == '*') {
						u32 deepness = 1;

						if (!next_char()) {
							token.string.count = 2;
							lexer->reporter->error(token.string, "Unclosed comment block (end of file)");
							return false;
						}

					continue_search:
						auto comment_begin_or_end = find(Span(current_p, lexer->source.end()), {u8"*/"s, u8"/*"s});
						if (!comment_begin_or_end) {
							token.string.count = 2;
							lexer->reporter->error(token.string, "Unclosed comment block");
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
				if (c == '0') {
					if (next_char()) {
						if (c == 'x') {
							if (!next_char()) {
								lexer->reporter->error(token.string, "Unexpected end when parsing hex number");
								return false;
							}

						next_hex_digit:
							switch (c) {
								case '0': case '1': case '2': case '3':
								case '4': case '5': case '6': case '7':
								case '8': case '9': case 'a': case 'b':
								case 'c': case 'd': case 'e': case 'f':
								case 'A': case 'B': case 'C': case 'D':
								case 'E': case 'F':
									if (!next_char()) {
										break;
									}
									goto next_hex_digit;
								default:
									break;
							}

							if (current_p - token.string.data <= 2) {
								lexer->reporter->error(token.string, "Invalid hex number");
								return false;
							}
						}
					}
				} else {
					while (is_digit(c)) {
						if (!next_char()) {
							break;
						}
					}
				}

				token.kind = Token_integer_literal;
				token.string.count = current_p - token.string.data;
				push_token();
				break;
			}
			case '#': {
				if (!next_char()) {
					lexer->reporter->error(token.string, "Unexpected end when parsing directive");
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
				while (1) {
					if (!next_char()) {
						goto stop_identifier;
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
							goto stop_identifier;
						}
						default: {
							continue;
						}
					}
				}
			stop_identifier:
				token.string.count = current_p - token.string.data;

				auto found_keyword = keywords.find(token.string);
				if (found_keyword) {
					token.kind = *found_keyword;
				} else {
					token.kind = Token_identifier;
				}

				push_token();
				break;
			}
		}
	}
	lexer->success = true;
	return true;
}

u32 main_return_value = 0;

struct Parser {
	Lexer *lexer = 0;
	Lexer::Iterator token;
	u32 token_index = 0;
	bool reached_end = false;
	AstLambda *current_lambda = 0;
	Reporter *reporter;
	u32 reporter_checkpoint = 0;
	Span<utf8> extern_language;
	Span<utf8> extern_library;
	Scope *current_scope = &global_scope;
	u32 scope_count = 0;
	CallingConvention current_convention = CallingConvention::tlang;
	StructLayout current_struct_layout = StructLayout::tlang;

	Parser checkpoint() {
		reporter_checkpoint = reporter->checkpoint();
		return *this;
	}

	[[nodiscard]] bool reset(Parser checkpoint) {
		if (scope_count != checkpoint.scope_count) {
			reporter->error("Failed to recover parser state!");
			return false;
		}
		*this = checkpoint;
		reporter->reset(reporter_checkpoint);
		return true;
	}

	bool next() {
		if (reached_end) {
			return false;
		}

		auto old_token = token;
		++token;
		if (!token) {
			token = old_token;
			reached_end = true;
			return false;
		}
		return true;
	}

	bool expect(TokenKind expected_kind) {
		if (token->kind != expected_kind) {
			reporter->error(token->string, "Expected '%', but got %", token_kind_to_string(expected_kind), token_kind_to_string(token->kind));
			return false;
		}
		return true;
	}
	bool next_not_end() {
		if (!next()) {
			reporter->error(token->string, "Unexpected end of file");
			return false;
		}
		return true;
	}
	bool next_expect(TokenKind expected_kind) {
		if (!next()) {
			reporter->error(token->string, "Unexpected end of file");
			return false;
		}
		if (!expect(expected_kind))
			return false;
		return true;
	}
};

#if 1

AstLiteral *make_integer(BigInt value) {
	auto i = new_ast<AstLiteral>();
	i->literal_kind = LiteralKind::integer;
	i->integer = value;
	return i;
}

AstLiteral *make_integer(u64 value) {
	return make_integer(make_big_int(value));
}
#else
AstLiteral *make_integer(u64 value) {
	auto i = new_ast<AstLiteral>();
	i->literal_kind = LiteralKind::integer;
	i->integer = value;
	return i;
}

#endif

AstLiteral *make_boolean(bool value) {
	auto i = new_ast<AstLiteral>();
	i->literal_kind = LiteralKind::boolean;
	i->Bool = value;
	return i;
}

bool is_unary_operator(TokenKind kind) {
	switch (kind) {
		case '+':
		case '-':
		case '*':
		case '&':
		case '!':
			return true;
	}
	return false;
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
			return true;
	}
	return false;
}

s32 get_precedence(BinaryOperation op) {
	// a + b * c == d
	switch (op) {
		case '.': return 100;

		case '*':
		case '/':
		case '%': return 20;

		case '+':
		case '-': return 10;

		case '&':
		case '|':
		case '^':
		case '<<':
		case '>>': return 5;

		case '>':
		case '<':
		case '==':
		case '!=':
		case '>=':
		case '<=': return 3;
	}

	invalid_code_path();
	return 0;
}

Span<utf8> unescape_string(Span<utf8> string) {
	string.data  += 1;
	string.count -= 2;

	if (!string.count)
		return string;

	List<utf8> new_string;
	new_string.reserve(string.count);

	auto p = string.data;
	utf32 c = 0;
	utf32 prev = 0;

	while (1) {
		if (p >= string.end())
			break;
		auto got_char = get_char_and_advance_utf8(&p);
		if (!got_char) {
			return {};
		}

		prev = c;
		c = got_char.value_unchecked();

		if (prev == '\\') {
			switch (c) {
				case 'n': { new_string.back() = '\n'; break; }
				case 'r': { new_string.back() = '\r'; break; }
				case 't': { new_string.back() = '\t'; break; }
				case '0': { new_string.back() = '\0'; break; }
				case '\\': { new_string.back() = '\\'; c = 0; break; }
				default: { new_string.back() = c; break; }
			}
		} else {
			new_string.add(c);
		}
	}
	return new_string;
}

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser);
AstDefinition *parse_definition(Parser *parser);

#define push_scope(scope) \
	auto CONCAT(new_scope, __LINE__) = scope; \
	assert(!CONCAT(new_scope, __LINE__)->parent); \
	auto CONCAT(old_scope, __LINE__) = (CONCAT(new_scope, __LINE__)->parent = parser->current_scope); \
	parser->current_scope->children.add(CONCAT(new_scope, __LINE__)); \
	parser->current_scope = CONCAT(new_scope, __LINE__); \
	parser->current_scope->level = CONCAT(old_scope, __LINE__)->level + 1; \
	defer { parser->current_scope = CONCAT(old_scope, __LINE__); }; \
	parser->scope_count += 1;

ExternLanguage extern_language_from_string(Span<utf8> string) {
	if (string == u8"C"s) return ExternLanguage::c;

	return ExternLanguage::none;
}

AstDefinition *make_retparam(Parser *parser, AstExpression *type) {
	auto retparam = new_ast<AstDefinition>();

	retparam->type = type;
	retparam->is_return_parameter = true;
	retparam->parent_block = parser->current_lambda;
	retparam->parent_scope = parser->current_scope;

	return retparam;
}

AstExpression *parse_sub_expression(Parser *parser) {
	bool is_parsing_type = false;

	switch (parser->token->kind) {
		case Token_string_literal: {
			auto string = new_ast<AstLiteral>();
			string->literal_kind = LiteralKind::string;
			string->location = parser->token->string;
			string->string = unescape_string(parser->token->string);
			if (!string->string.data) {
				parser->reporter->error(parser->token->string, "Bad escape sequence in string literal");
				return 0;
			}
			parser->next();
			return string;
		}
		case Token_character_literal: {
			auto character = new_ast<AstLiteral>();
			character->literal_kind = LiteralKind::character;
			character->location = parser->token->string;
			auto character_string = unescape_string(parser->token->string);
			if (!character_string.data) {
				parser->reporter->error(parser->token->string, "Bad escape sequence in string literal");
				return 0;
			}
			if (character_string.count != 1) {
				parser->reporter->error(parser->token->string, "Character literal can not contain more than one character");
				return 0;
			}
			character->character = character_string.data[0];
			parser->next();
			return character;
		}
		case Token_null: {
			auto result = make_integer(0);
			result->location = parser->token->string;
			parser->next();
			result->type = &type_pointer_to_void;
			return result;
		}
		case Token_integer_literal: {
			BigInt value = 0ib;
			if (parser->token->string.count >= 2 && parser->token->string.data[1] == 'x') {
				for (u32 i = 2; i != parser->token->string.count; ++i) {
					u8 quart;
					switch (parser->token->string.data[i]) {
						case '0': quart = 0; break;
						case '1': quart = 1; break;
						case '2': quart = 2; break;
						case '3': quart = 3; break;
						case '4': quart = 4; break;
						case '5': quart = 5; break;
						case '6': quart = 6; break;
						case '7': quart = 7; break;
						case '8': quart = 8; break;
						case '9': quart = 9; break;
						case 'a': case 'A': quart = 10; break;
						case 'b': case 'B': quart = 11; break;
						case 'c': case 'C': quart = 12; break;
						case 'd': case 'D': quart = 13; break;
						case 'e': case 'E': quart = 14; break;
						case 'f': case 'F': quart = 15; break;
					}

					value <<= 4;
					value |= make_big_int(quart);
				}
			} else {
				for (auto character : parser->token->string) {
					u64 digit = (u64)character - '0';
					if (digit >= 10) {
						parser->reporter->error(parser->token->string, "Failed to parse integer.");
						return 0;
					}
					value = value * 10ib + make_big_int(digit);
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
			auto boolean = new_ast<AstLiteral>();
			boolean->literal_kind = LiteralKind::boolean;
			boolean->Bool = parser->token->kind == Token_true;
			boolean->location = parser->token->string;
			parser->next();
			return boolean;
		}
		case Token_identifier: {
			auto identifier_token = parser->token;
			parser->next();
			if (parser->token->kind == '(') {
				if (!parser->next_not_end())  return 0;

				List<AstExpression *> arguments;
				if (parser->token->kind != ')') {
					for (;;) {
						auto expression = parse_expression(parser);
						if (!expression)
							return 0;

						arguments.add(expression);

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

				auto call = new_ast<AstCall>();
				call->location = call->name = identifier_token->string;

				call->arguments = arguments;

				return call;
			} else {
				auto identifier = new_ast<AstIdentifier>();
				identifier->location = identifier->name = identifier_token->string;
				return identifier;
			}
		}
		case Token_directive: {
			if (parser->token->string == u8"#type"s) {
				if (!parser->next_expect(Token_fn))
					return 0;

				is_parsing_type = true;
				goto parse_function;
			} else if (parser->token->string == u8"#sizeof"s) {
				auto size_of = new_ast<AstSizeof>();
				size_of->location = parser->token->string;

				if (!parser->next_not_end())
					return 0;

				size_of->expression = parse_sub_expression(parser);
				if (!size_of->expression)
					return 0;

				return size_of;
			} else {
				parser->reporter->error(parser->token->string, "Unexpected directive");
				return 0;
			}
		}
		case Token_fn: {

		parse_function:

			auto lambda = new_ast<AstLambda>();

			auto start_token = parser->token;
			if (!parser->next_not_end())  return 0;

			if (parser->token->kind == Token_directive) {
				if (parser->token->string == u8"#stdcall"s) {
					lambda->convention = CallingConvention::stdcall;
					if (!parser->next_not_end())
						return 0;
				} else {
					parser->reporter->error(parser->token->string, "Unknown directive");
					return 0;
				}
			}

			if (lambda->convention == CallingConvention::none) {
				lambda->convention = parser->current_convention;
			}

			if (!parser->expect('('))  return 0;

			if (!parser->next_not_end())  return 0;

			lambda->location = start_token->string;

			push_scope(&lambda->parameter_scope);

			if (parser->token->kind != ')') {
				for (;;) {
					auto definition = parse_definition(parser);
					if (!definition)
						return 0;

					definition->is_parameter = true;
					definition->parent_block = lambda;

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

			if (parser->token->kind == '->') {
				if (!parser->next_not_end())
					return 0;

	#if 1
				auto checkpoint = parser->checkpoint();
				lambda->return_parameter = parse_definition(parser);
				if (lambda->return_parameter) {
					lambda->return_parameter->is_return_parameter = true;
					lambda->return_parameter->parent_block = lambda;
				} else {
					if (!parser->reset(checkpoint))
						return 0;

					auto return_type = parse_expression(parser);
					if (!return_type)
						return 0;

					lambda->return_parameter = make_retparam(parser, return_type);
				}
	#else
				// Parse multiple return parameters
				for (;;) {
					auto definition = parse_definition(parser);
					if (!definition)
						return 0;

					definition->is_return_parameter = true;
					definition->parent_block = lambda;

					lambda->return_parameters.add(definition);

					if (parser->token->kind == '{' || parser->token->kind == '=>') {
						break;
					}

					if (!parser->expect(','))
						return 0;

					if (!parser->next_not_end())
						return 0;
				}
				if (!parser->next_not_end())
					return 0;
	#endif
			} else {
				// TODO: this probably will be executed a lot, so maybe this should be cached ???
				lambda->return_parameter = make_retparam(parser, &type_void);
			}


			if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';') {
				parser->reporter->error(parser->token->string, "Expected '{' or '=>' or ';' or '->' instead of '%'", parser->token->string);
				return 0;
			}

			bool is_short = false;

			if (is_parsing_type) {
				lambda->has_body = false;
				if (parser->token->kind == '{' || parser->token->kind == '=>') {
					parser->reporter->error(lambda->location, "Body of a lambda can not be specified after a #type directive");
					return 0;
				} else if (parser->token->kind != ';') {
					parser->reporter->error(parser->token->string, "Expected ';' or return type instead of '%'", parser->token->string);
					return 0;
				}
			} else {

				if (parser->token->kind == '{') {
				} else if (parser->token->kind == '=>') {
					is_short = true;
				} else if (parser->token->kind == ';') {
					lambda->has_body = false;
				} else {
					parser->reporter->error(parser->token->string, "Expected '{' or '=>' or ';' or return type instead of '%'", parser->token->string);
					return 0;
				}
			}

			lambda->name = format(u8"unnamed%", lambda->uid);

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

					auto ret = new_ast<AstReturn>();
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
			} else {

				if (!is_parsing_type) {
					// Extern functions

					auto print_example = [&]{
						parser->reporter->info("For example, if you want to link with C library you can do this:\nextern \"C\" \"library.lib\" {\n\t<Library's functions>\n}");
					};
					/*
					if (parser->extern_language.count == 0) {
						parser->reporter->error(lambda->location, "Lambda has no body, but extern language was not provided");
						print_example();
						return 0;
					}
					*/
					if (parser->extern_library.count == 0) {
						parser->reporter->error(lambda->location, "Lambda has no body, but extern library was not provided");
						print_example();
						return 0;
					}

					/*
					lambda->extern_language = extern_language_from_string(parser->extern_language);
					if (lambda->extern_language == ExternLanguage::none) {
						parser->reporter->error(lambda->location, "Unsupported language: %", parser->extern_language);
						print_example();
						return 0;
					}
					*/

					lambda->extern_library = parser->extern_library;

				}
			}

			parser->next();

			return lambda;
		}
		case Token_struct: {
			auto Struct = new_ast<AstStruct>();
			Struct->location = parser->token->string;

			if (!parser->next_expect('{'))
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
		case '(': {
			if (!parser->next()) {
				parser->reporter->error(parser->token->string, "Unexpected end of file. Unclosed ')'");
				return 0;
			}

			auto expression = parse_expression(parser);
			if (!expression) {
				return 0;
			}

			if (!parser->expect(')')) {
				return 0;
			}
			if (!parser->next()) {
				parser->reporter->error("Unexpected end of file while parsing parenthesized expression");
				return 0;
			}
			return expression;
		}
		case '[': {
			auto subscript = new_ast<AstSubscript>();
			subscript->location = parser->token->string;

			if (!parser->next_not_end())
				return 0;

			subscript->index_expression = parse_expression(parser);
			if (!subscript->index_expression)
				return 0;

			if (!parser->expect(']'))
				return 0;

			parser->next();

			subscript->location = {subscript->location.begin(), parser->token->string.end()};
			subscript->expression = parse_sub_expression(parser);
			if (!subscript->expression)
				return 0;

			return subscript;
		}
		default: {
			if (is_unary_operator(parser->token->kind)) {
				auto unop = new_ast<AstUnaryOperator>();
				unop->location = parser->token->string;
				unop->operation = parser->token->kind;
				if (!parser->next_not_end())
					return 0;

				unop->expression = parse_sub_expression(parser);
				if (!unop->expression)
					return 0;

				return unop;
			} else {
				parser->reporter->error(parser->token->string, "Unexpected token '%'", parser->token->string);
				return 0;
			}
		}
	}


	invalid_code_path();
}

#if 0
AstExpression *simplify(AstExpression *expression);

void simplify(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_definition: {
			auto definition = (AstDefinition *)statement;
			if (definition->expression) {
				definition->expression = simplify(definition->expression);
			}
			break;
		}
		case Ast_return: {
			auto ret = (AstReturn *)statement;
			ret->expression = simplify(ret->expression);
			break;
		}
		case Ast_if: {
			break;
		}
		case Ast_while: {
			break;
		}
		case Ast_expression_statement: {
			auto es = (AstExpressionStatement *)statement;
			es->expression = simplify(es->expression);
			break;
		}
		default: {
			invalid_code_path("not implemented");
		}
	}
}

AstExpression *simplify(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;

			if (binop->operation == '.') {

				switch (binop->right->kind) {
					case Ast_identifier: {

						auto ident = (AstIdentifier *)binop->right;

						if (ident->definition) {
							if (ident->definition->is_constant) {
								return make_integer(get_constant_integer(ident->definition->expression).value());
							}
						}

						break;
					}
					default: {
						invalid_code_path();
						break;
					}
				}

			} else {
			retry_substitute:

				auto left_value  = get_constant_integer(binop->left);
				auto right_value = get_constant_integer(binop->right);

				if (left_value && right_value) {
					auto left  = left_value.value_unchecked();
					auto right = right_value.value_unchecked();

					BigInt value;

					switch (binop->operation) {
						case '+': value = left + right; break;
						case '-': value = left - right; break;
						case '*': value = left * right; break;
						case '/': invalid_code_path(); // value = left / right; break;
						case '&': value = left & right; break;
						case '|': value = left | right; break;
						case '^': value = left ^ right; break;
						case '<':  return make_boolean(left < right);
						case '>':  return make_boolean(left > right);
						case '<=': return make_boolean(left <= right);
						case '>=': return make_boolean(left >= right);
						case '!=': return make_boolean(left != right);
						case '==': return make_boolean(left == right);
						default: invalid_code_path(); break;
					}

					return make_integer(value);
				} else {
					auto new_left  = simplify(binop->left);
					auto new_right = simplify(binop->right);

					if (binop->left != new_left || binop->right != new_right) {
						binop->left  = new_left;
						binop->right = new_right;
						goto retry_substitute;
					} else {
						return binop;
					}
				}

				return binop;
			}
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)expression;
			for (auto &statement : lambda->statements) {
				simplify(statement);
			}
			break;
		}
	}

	return expression;
}
#endif
AstExpression *simplify(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;

			binop->left  = simplify(binop->left);
			binop->right = simplify(binop->right);

			if (binop->operation == '+' ||
				binop->operation == '-' ||
				binop->operation == '*' ||
				binop->operation == '/' ||
				binop->operation == '%' ||
				binop->operation == '^' ||
				binop->operation == '&' ||
				binop->operation == '|' ||
				binop->operation == '==' ||
				binop->operation == '!=' ||
				binop->operation == '>=' ||
				binop->operation == '<=' ||
				binop->operation == '>>' ||
				binop->operation == '<<' ||
				binop->operation == '>' ||
				binop->operation == '<'
			) {
				auto left_value  = get_constant_integer(binop->left);
				auto right_value = get_constant_integer(binop->right);

				if (left_value && right_value) {
					auto left  = left_value.value_unchecked();
					auto right = right_value.value_unchecked();

					BigInt value;

					switch (binop->operation) {
						case '+': value = left + right; break;
						case '-': value = left - right; break;
						case '*': value = left * right; break;
						case '/': invalid_code_path("not implemented"); // value = left / right; break;
						case '&': value = left & right; break;
						case '|': value = left | right; break;
						case '^': value = left ^ right; break;
						case '<<': invalid_code_path("not implemented"); // value = left << right; break;
						case '>>': invalid_code_path("not implemented"); // value = left >> right; break;
						case '<':  return make_boolean(left < right);
						case '>':  return make_boolean(left > right);
						case '<=': return make_boolean(left <= right);
						case '>=': return make_boolean(left >= right);
						case '!=': return make_boolean(left != right);
						case '==': return make_boolean(left == right);
						default: invalid_code_path(); break;
					}
					return make_integer(value);
				}
			}
			break;
		}
	}
	return expression;
}

void combine_location(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;
			combine_location(unop->expression);
			unop->location = Span(unop->location.begin(), unop->expression->location.end());
			break;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			combine_location(binop->left);
			combine_location(binop->right);
			binop->location = Span(binop->left->location.begin(), binop->right->location.end());
			break;
		}
	}
}

AstExpression *parse_expression(Parser *parser) {
	timed_function();
	auto sub = parse_sub_expression(parser);

	if (parser->reached_end) {
		if (sub) {
			combine_location(sub);
			return sub;
		}
		return 0;
	}

#if 0
parse_subscript:
	while (parser->token->kind == '[') {
		auto subscript = new_ast<AstSubscript>();

		if (!parser->next_not_end())
			return 0;

		subscript->index_expression = parse_expression(parser);

		if (!parser->expect(']'))
			return 0;

		subscript->location = {sub->location.begin(), parser->token->string.end()};

		parser->next();
		subscript->expression = sub;

		sub = subscript;
	}
#endif

	AstBinaryOperator *top_binop = 0;
	AstBinaryOperator *previous_binop = 0;
	s32 previous_precedence = 0;
	while (is_binary_operator(parser->token->kind)) {
		auto binop = new_ast<AstBinaryOperator>();
		binop->left = sub;

		binop->operation = parser->token->kind;

		binop->location = parser->token->string;

		auto precedence = get_precedence(binop->operation);

		if (!sub) {
			return 0;
		}

		if (!parser->next()) {
			parser->reporter->error("Unexpected end of file after binary operator");
			return 0;
		}

		binop->right = parse_sub_expression(parser);
		if (!binop->right) {
			parser->reporter->info("While parsing binary operator '%'", binary_operator_string(binop->operation));
			return 0;
		}

		if (binop->operation == '.') {
			if (binop->right->kind != Ast_identifier && binop->right->kind != Ast_call) {
				parser->reporter->error(binop->right->location, "This expression can not follow a dot. Only identifiers are allowed here.");
				return 0;
			}
		}

		if (previous_binop) {
			if (precedence > previous_precedence) {
				binop->left = previous_binop->right;
				previous_binop->right = binop;
			} else {
				binop->left = previous_binop;
				top_binop = binop;
			}
		} else {
			top_binop = binop;
		}

		previous_precedence = precedence;
		previous_binop = binop;
	}

	if (top_binop) {
		sub = top_binop;
	}

	if (sub) {
#if 0
		if (parser->token->kind == '[') {
			sub = top_binop;
			goto parse_subscript;
		}
#endif
		combine_location(sub);
		sub = simplify(sub);
		return sub;
	}

	return 0;
}

//
// Use this if name token is already taken from parser.
//
AstDefinition *parse_definition(Span<utf8> name, Parser *parser) {
	assert(parser->token->kind == ':');

	if (!parser->next_not_end())  return 0;

	AstExpression *type = 0;
	if (parser->token->kind != ':' && parser->token->kind != '=' ) {
		type = parse_expression(parser);
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

	auto definition = new_ast<AstDefinition>();

	definition->location = definition->name = name;
	definition->type = type;
	definition->parent_block = parser->current_lambda;
	definition->is_constant = is_constant;
	definition->parent_scope = parser->current_scope;


	{

		auto report_redefinition_error = [&] (AstDefinition *_new, AstDefinition *existing) {
			parser->reporter->error(_new->name, "Redefinition of '%'", _new->name);
			parser->reporter->info(existing->name, "Top declaration is here");
		};

		Scope *scope = parser->current_scope;
		while (scope) {
			auto to_lock = scope;
			scoped_lock(to_lock);
			auto found = scope->definitions.find(definition->name);
			if (found) {
				report_redefinition_error(definition, *found);
				return 0;
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
			assert(parser->current_scope->definitions.try_insert(definition->name, definition));
		}
		{
			scoped_lock(&global_scope);
			names_not_available_for_globals.try_insert(definition->name, definition); // May fail, don't care. Error will show the first one
		}

		/*
		AstDefinition **existing;
		if (!parser->current_scope->definitions.try_insert(definition->name, definition, &existing)) {
			report_redefinition_error(definition, *existing);
			return 0;
		}

		Scope *scope = parser->current_scope;
		while (scope) {
			if (!scope->taken_names.try_insert(definition->name, definition, &existing)) {
				report_redefinition_error(definition, *existing);
				return 0;
			}
			scope = scope->parent;
		}
		*/
	}

	if (has_expression) {
		if (!parser->next_not_end())  return 0;

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		definition->expression = expression;
		switch (expression->kind) {
			case Ast_lambda: {
				auto lambda = (AstLambda *)expression;
				lambda->name.set(definition->name);
				lambda->definition = definition;

				scoped_lock(parser->current_scope);

				parser->current_scope->functions.insert_or(definition->name, lambda, [&] (Span<utf8> existing_name, AstLambda *existing) {
					invalid_code_path();
				});
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
	parser->reporter->error(parser->token->string, "Failed to parse definition");
	return 0;
}

bool is_statement(AstExpression *expression) {
	return expression->kind == Ast_call;
}

AstExpressionStatement *make_statement(AstExpression *expression) {
	if (!expression)
		return 0;

	auto statement = new_ast<AstExpressionStatement>();
	statement->expression = expression;
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
#if 1
void parse_statement(Parser *parser, AstStatement *&result) {
	timed_function();

	result = 0;
	defer {
		if (result) {
			scoped_lock(parser->current_scope);
			parser->current_scope->statements.add(result);
		}
	};

	if (parser->token->kind == Token_identifier) {
		auto checkpoint = parser->checkpoint();

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

			if (!parser->reset(checkpoint)) {
				return;
			}
		}
	} else if (parser->token->kind == Token_return) {

		auto return_token = parser->token;

		if (!parser->next_not_end())
			return;

		auto ret = new_ast<AstReturn>();

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
	} else if (parser->token->kind == Token_if) {
		auto If = new_ast<AstIf>();
		If->location = parser->token->string;
		if (!parser->next_not_end())
			return;
		auto condition = parse_expression(parser);
		if (!condition) {
			return;
		}
		If->condition = condition;

		if (!parse_block_or_single_statement(parser, &If->true_scope)) {
			return;
		}

		if (parser->token->kind == Token_else) {
			if (!parser->next_not_end())
				return;

			if (!parse_block_or_single_statement(parser, &If->false_scope)) {
				return;
			}
		}
		result = If;
		return;
	} else if (parser->token->kind == Token_while) {
		auto While = new_ast<AstWhile>();
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
	} else if (parser->token->kind == '{') {

		auto block = new_ast<AstBlock>();

		push_scope(&block->scope);

		parser->next();
		while (parser->token->kind != '}') {
			auto statement = parse_statement(parser);
			if (!statement)
				return;

			block->scope.statements.add(statement);
		}
		parser->next();

		result = block;
		return;
	}

	auto checkpoint = parser->checkpoint();

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
		} else if (
			parser->token->kind == '=' ||
			parser->token->kind == '+=' ||
			parser->token->kind == '-=' ||
			parser->token->kind == '*=' ||
			parser->token->kind == '/=' ||
			parser->token->kind == '%=' ||
			parser->token->kind == '|=' ||
			parser->token->kind == '&=' ||
			parser->token->kind == '^='
		) {
			auto ass = new_ast<AstBinaryOperator>();
			ass->location = parser->token->string;
			ass->left = expression;
			ass->operation = parser->token->kind;

			if (!parser->next_not_end())
				return;

			ass->right = parse_expression(parser);
			if (!ass->right)
				return;

			if (!parser->expect(';'))
				return;
			parser->next();

			combine_location(ass);

			result = make_statement(ass);
			return;
		}
	}

	parser->reset(checkpoint);
	parser->reporter->error(parser->token->string, "Failed to parse statement or expression.");
}
AstStatement *parse_statement(Parser *parser) {
	AstStatement *result;
	parse_statement(parser, result);
	return result;
}
#else
AstStatement *parse_statement(Parser *parser) {
	timed_function();
	auto try_parse_non_expression = [&]() -> AstStatement * {
		if (parser->token->kind == Token_identifier) {
			auto checkpoint = parser->checkpoint();

			parser->next();

			if (parser->token->kind == '(') {
				if (!parser->reset(checkpoint)) {
					return 0;
				}
				auto expression = parse_expression(parser);
				if (!expression) {
					return 0;
				}
				return make_statement(expression);
			} else {
				if (!parser->reset(checkpoint)) {
					return 0;
				}

				auto definition = parse_definition(parser);

				if (!definition) {
					return 0;
				}

				if (!definition->expression || needs_semicolon(definition->expression)) {
					if (!parser->expect(';'))  return 0;
					parser->next();
				}

				return definition;
			}
		} else if (parser->token->kind == Token_return) {

			auto return_token = parser->token;

			if (!parser->next_not_end()) return 0;

			auto ret = new_ast<AstReturn>();

			if (parser->token->kind != ';') {
				auto expression = parse_expression(parser);
				if (!expression)  return 0;
				if (!parser->expect(';'))  return 0;
				ret->expression = expression;
			}

			parser->next();

			ret->location = return_token->string;
			return ret;
		} else if (parser->token->kind == Token_if) {
			auto If = new_ast<AstIf>();
			If->location = parser->token->string;
			if (!parser->next_not_end())
				return 0;
			auto condition = parse_expression(parser);
			if (!condition) {
				return 0;
			}
			If->condition = condition;

			auto parsed_block = parse_block_or_single_statement(parser);
			if (!parsed_block) {
				return 0;
			}
			If->true_statements = parsed_block.value_unchecked();

			if (parser->token->kind == Token_else) {
				if (!parser->next_not_end())
					return 0;

				parsed_block = parse_block_or_single_statement(parser);
				if (!parsed_block) {
					return 0;
				}
				If->false_statements = parsed_block.value_unchecked();
			}
			return If;
		} else if (parser->token->kind == Token_while) {
			auto While = new_ast<AstWhile>();
			While->location = parser->token->string;
			if (!parser->next_not_end())
				return 0;
			auto condition = parse_expression(parser);
			if (!condition) {
				return 0;
			}
			While->condition = condition;

			auto parsed_block = parse_block_or_single_statement(parser);
			if (!parsed_block) {
				return 0;
			}
			While->statements = parsed_block.value_unchecked();
			return While;
		} else if (parser->token->kind == '{') {

			auto block = new_ast<AstBlock>();

			parser->next();
			while (parser->token->kind != '}') {
				auto statement = parse_statement(parser);
				if (!statement)
					return {};

				block->statements.add(statement);
			}
			parser->next();

			return block;
		}
		return (AstStatement *)1;
	};

	auto checkpoint = parser->checkpoint();

	auto expression = parse_expression(parser);
	if (expression) {
		if (parser->token->kind == ';') {
			if (!is_statement(expression)) {
				parser->reporter->error(expression->location, "This expression is not a statement.");
				return 0;
			}
			parser->next();
			auto statement = make_statement(expression);
			parser->current_scope->statements.add(statement);
			return statement;
		} else if (
			parser->token->kind == '=' ||
			parser->token->kind == '+=' ||
			parser->token->kind == '-=' ||
			parser->token->kind == '*=' ||
			parser->token->kind == '/=' ||
			parser->token->kind == '%=' ||
			parser->token->kind == '|=' ||
			parser->token->kind == '&=' ||
			parser->token->kind == '^='
		) {
			auto ass = new_ast<AstBinaryOperator>();
			ass->location = parser->token->string;
			ass->left = expression;
			ass->operation = parser->token->kind;

			if (!parser->next_not_end())
				return 0;

			ass->right = parse_expression(parser);
			if (!ass->right)
				return 0;

			if (!parser->expect(';'))
				return 0;
			parser->next();

			combine_location(ass);

			auto statement = make_statement(ass);
			parser->current_scope->statements.add(statement);
			return statement;
		} else {
			// Maybe free `expression` ?
		}
	}

	if (!parser->reset(checkpoint)) {
		return 0;
	}

	auto statement = try_parse_non_expression();
	if (statement == 0) {
		parser->reporter->error("Failed to parse statement or expression.");
		return 0;
	}

	if (statement != (AstStatement *)1) {
		parser->current_scope->statements.add(statement);
		return statement;
	}


	parser->reporter->error(parser->token->string, "Failed to parse statement. Unexpected token '%'", parser->token->string);
	return 0;
}
#endif
AstStatement *parse_global_statement(Parser *parser) {
	timed_function();
	auto statement = parse_statement(parser);
	if (!statement) {
		return 0;
	}
	if (!can_be_global(statement)) {
		parser->reporter->error(statement->location, "This statement can not be global.");
		return 0;
	}

	/*
	bool insert_fail = false;

	global_statements.insert_or(statement->location, statement, [&] (Span<utf8> found_key, AstStatement *found) {
		if (found->kind == Ast_definition) {
			auto defn = (AstDefinition *)found;
			if (defn->expression && defn->expression->kind == Ast_lambda) {
			} else {
				parser->reporter->error(statement->location, "Redefinition of '%'", statement->location);
				parser->reporter->error(found->location, "Previous declaration is here");
				insert_fail = true;
			}
		}
	});

	if (insert_fail) {
		return 0;
	}

	assert(*global_statements.find(statement->location) != 0);
	*/

	return statement;
}

bool parser_function(Parser *parser);

struct SourceFileContext {
	Reporter reporter;
	Lexer lexer;
	Parser parser;
	WorkQueue work_queue;
};

Lexer *failed_lexer;
Parser *failed_parser;

LinearSet<Span<utf8>> parsed_files;

void parse_file(Span<utf8> path) {
	timed_function();

	// TODO: normalize path
	if (find(parsed_files, path)) {
		return;
	}
	parsed_files.insert(path);

	auto context = default_allocator.allocate<SourceFileContext>();

	context->lexer.source_buffer = read_entire_file(to_pathchars(path), {.extra_space_before=1, .extra_space_after=1});
	if (!context->lexer.source_buffer.data) {
		print("Failed to read '%'. Exiting.\n", path);
		return;
	}

	context->lexer.source_buffer.front() = '\0';
	context->lexer.source_buffer.back() = '\0';

	Span<utf8> source = as_utf8(context->lexer.source_buffer);
	source.data += 1;
	source.count -= 2;

	auto bom = Span(context->lexer.source_buffer.data + 1, 3);
	if (bom.end() <= context->lexer.source_buffer.end() && bom == "\xef\xbb\xbf"b) {
		bom.back() = '\0';
		source.data += 3;
		source.count -= 3;
	}
	context->lexer.source = source;

	context->parser.lexer = &context->lexer;

	context->parser.reporter = context->lexer.reporter = &context->reporter;

	sources.add({path, source});

	context->work_queue = make_work_queue(*thread_pool);

	context->work_queue.push([context]() {
		if (!lexer_function(&context->lexer)) {
			atomic_set_if_equals(failed_lexer, &context->lexer, (Lexer *)0);
		}
	});

	context->work_queue.push([context]() {
		if (!parser_function(&context->parser)) {
			atomic_set_if_equals(failed_parser, &context->parser, (Parser *)0);
		}
	});

}

f64 parser_time;
bool parser_function(Parser *parser) {
	timed_function();

	auto timer = create_precise_timer();
	defer { parser_time = reset(timer); };

	auto lexer = parser->lexer;

	while (lexer->tokens_lexed == 0 && !lexer->finished) {} // Wait for tokens

	if (lexer->tokens_lexed == 0) {
		return true;
	}

	parser->token = lexer->begin();
	while (!parser->reached_end) {
		if (parser->token->kind == Token_directive) {
			if (parser->token->string == u8"#extern_language"s) {
				if (!parser->next_expect(Token_string_literal)) {
					parser->reporter->error("Expected language name. Currently only \"C\" is available.");
					return false;
				}
				parser->extern_language = unescape_string(parser->token->string);
				if (parser->extern_language != u8"C"s) {
					parser->reporter->error(parser->token->string, "Only \"C\" is supported.");
					return false;
				}
				parser->next();
			} else if (parser->token->string == u8"#extern_library"s) {
				if (!parser->next_expect(Token_string_literal)) {
					parser->reporter->error("Expected library name.");
					return false;
				}
				parser->extern_library = unescape_string(parser->token->string);
				extern_libraries.insert(parser->extern_library);
				parser->next();
			} else if (parser->token->string == u8"#stdcall"s) {
				parser->current_convention = CallingConvention::stdcall;
				parser->next();
			} else if (parser->token->string == u8"#tlangcall"s) {
				parser->current_convention = CallingConvention::tlang;
				parser->next();
			} else if (parser->token->string == u8"#layout_c"s) {
				parser->current_struct_layout = StructLayout::c;
				parser->next();
			} else {
				parser->reporter->error(parser->token->string, "Unknown directive");
				return false;
			}
		} else if (parser->token->kind == Token_import) {
			if (!parser->next_expect(Token_string_literal)) {
				parser->reporter->error("Expected library path.");
				return false;
			}
			auto libname = unescape_string(parser->token->string);
			parse_file(concatenate(executable_directory, "\\libs\\", libname));
			parser->next();
		} else {
			auto statement = parse_global_statement(parser);
			if (!statement) {
				return false;
			}
		}
	}
	return true;
}

HashMap<Span<utf8>, AstStatement *> typechecked_globals;
RecursiveMutex typechecked_globals_mutex;
u32 typechecked_globals_mutex_lock_count;

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
	Span<utf8> waiting_for_name;

	AstLambda *current_lambda = 0;

	Scope *current_scope = 0;

	Reporter reporter;

	bool finished = false;

	cppcoro::generator<TypecheckResult> generator;
	cppcoro::generator<TypecheckResult>::iterator generator_iterator;

	u32 no_progress_counter = 0;
};

#undef push_scope
#define push_scope(scope) \
	auto CONCAT(old_scope, __LINE__) = state->current_scope; \
	state->current_scope = scope; \
	defer { state->current_scope = CONCAT(old_scope, __LINE__); };

#define TYPECHECK(arg) \
	for (auto ret : typecheck(state, arg)) { \
		if (ret == TypecheckResult::success) { \
			break; \
		} \
		co_yield ret; \
	}

#define typecheck_scope(scope) \
	{ \
		push_scope(scope); \
		for (auto statement : (scope)->statements) { \
			TYPECHECK(statement); \
		} \
	}

AstDefinition *get_definition(TypecheckState *state, Span<utf8> name) {
	if (state->definition->name == name)
		return state->definition;

	if (state->current_lambda) {
		auto found_param = find_if(state->current_lambda->parameters, [&](AstDefinition *d) { return d->name == name; });
		if (found_param)
			return *found_param;

		//auto found_retparam = find_if(state->current_lambda->return_parameters, [&](AstDefinition *d) { return d->name == name; });
		//if (found_retparam)
		//	return *found_retparam;

		if (state->current_lambda->return_parameter->name == name)
			return state->current_lambda->return_parameter;

		auto scope = state->current_scope;
		while (scope) {
			auto found_local = scope->definitions.find(name);
			if (found_local)
				return *found_local;
			scope = scope->parent;
			if (scope == &global_scope)
				break;
		}
	}

	lock(typechecked_globals_mutex);
	defer { unlock(typechecked_globals_mutex); };
	typechecked_globals_mutex_lock_count += 1;
	auto found = typechecked_globals.find(name);
	if (found) {
		auto statement = *found;
		if (statement->kind == Ast_definition) {
			return (AstDefinition *)statement;
		}
		// Is this possible?
		invalid_code_path();
	} else {
		return 0;
	}
}

struct IntegerInfo {
	AstStruct *type;
	BigInt min_value;
	BigInt max_value;
	u64 mask;
};

IntegerInfo integer_infos[8];

void report_not_convertible(Reporter *reporter, AstExpression *expression, AstExpression *type) {
	reporter->error(expression->location, "Expression of type '%' is not implicitly convertible to '%'", type_to_string(expression->type), type_to_string(type));
}

bool convertible(AstExpression *expression, AstExpression *type) {
	if (type->kind == Ast_unary_operator) {
		auto unop = (AstUnaryOperator *)type;
		if (unop->operation == '*') {
			if (expression->kind == Ast_literal) {
				auto literal = (AstLiteral *)expression;
				if (literal->literal_kind == LiteralKind::integer) {
					if (literal->integer == 0ib) {
						return true;
					}
				}
			}
		}
	} else {
		auto Struct = get_struct(type);
		if (Struct) {
			if (Struct == type) {
				return true;
			} else if (Struct == &type_string) {
				return types_match(expression->type, &type_string);
			} else if (
				Struct == &type_u8 ||
				Struct == &type_u16 ||
				Struct == &type_u32 ||
				Struct == &type_u64 ||
				Struct == &type_s8 ||
				Struct == &type_s16 ||
				Struct == &type_s32 ||
				Struct == &type_s64
			) {
				return expression->type == &type_unsized_integer;
			}
		}
	}
	return false;
}

bool is_integer(AstExpression *type) {
	return
		types_match(type, &type_unsized_integer) ||
		types_match(type, &type_u8) ||
		types_match(type, &type_u16) ||
		types_match(type, &type_u32) ||
		types_match(type, &type_u64) ||
		types_match(type, &type_s8) ||
		types_match(type, &type_s16) ||
		types_match(type, &type_s32) ||
		types_match(type, &type_s64);
}

// TODO: i didn't think when i wrote this
// Resolves undecided types like 'unsized_integer' into sized ones
// If `target_type` is not null, it tries to implicitly convert `expression` to `target_type`
bool harden_type(TypecheckState *state, AstExpression **expression_pointer, AstExpression *target_type = 0) {
	auto expression = *expression_pointer;
	defer { *expression_pointer = expression; };
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			if (literal->type == &type_pointer_to_void) {
				// This is `null` literal.

				if (target_type) {
					if (!convertible(literal, target_type)) {
						report_not_convertible(&state->reporter, literal, target_type);
						return false;
					}
				}

				return true;
			}
			switch (literal->literal_kind) {
				using enum LiteralKind;
				case integer: {
					AstExpression *pointer_type = 0;
					if (target_type) {
						if (target_type->kind == Ast_unary_operator && ((AstUnaryOperator*)target_type)->operation == '*') {
							pointer_type = target_type;
							target_type = &type_u64;
						}
					} else {
						target_type = type_default_integer;
					}

					auto found_info = find_if(integer_infos, [&](IntegerInfo const &i) { return types_match(target_type, i.type); });

					if (found_info) {
						auto info = *found_info;

						if (literal->integer < info.min_value || literal->integer > info.max_value) {
							state->reporter.error(expression->location, "Computed value (%) does not fit into destination type % [%; %]. You can explicitly bitwise-and this expression with 0x% to discard higher bits", literal->integer, type_to_string(target_type), info.min_value, info.max_value, FormatInt{.value=info.mask,.radix=16});
							return false;
						}

						if (pointer_type) {
							literal->type = pointer_type;
						} else {
							literal->type = target_type;
						}
					} else {
						report_not_convertible(&state->reporter, expression, target_type);
						return false;
					}
					break;
				}
				case boolean: {
					if (!target_type) {
						target_type = &type_bool;
					}
					if (!types_match(expression->type, target_type)) {
						report_not_convertible(&state->reporter, expression, target_type);
						return false;
					}
					return true;
				}
				case string: {
					if (!target_type) {
						target_type = &type_string;
					}
					if (!types_match(expression->type, target_type)) {
						report_not_convertible(&state->reporter, expression, target_type);
						return false;
					}
					return true;
				}
				case character: {
					if (!target_type) {
						target_type = &type_u8;
					}
					if (!types_match(expression->type, target_type)) {
						report_not_convertible(&state->reporter, expression, target_type);
						return false;
					}
					return true;
				}
				default: {
					invalid_code_path();
				}
			}
			break;
		}
		case Ast_identifier: {
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;

			//if (unop->expression->kind == Ast_literal) {
			//
			//}

			if (!harden_type(state, &unop->expression, target_type)) {
				return false;
			}

			//if (is_type(unop->expression)) {
			//	if (unop->operation == '&') {
			//		unop->type = unop->expression->type;
			//	}
			//} else {
			//	unop->type = unop->expression->type;
			//}

			break;
		}
		case Ast_call: {
			break;
		}
		case Ast_binary_operator: {

			auto binop = (AstBinaryOperator *)expression;

			if (binop->operation == '.')
				break;

			assert(binop->operation != '=');
			auto &left  = binop->left;
			auto &right = binop->right;

			if (left->type == &type_unsized_integer && right->type == &type_unsized_integer) {
				binop->type = target_type;
			} else {
				if (target_type) {
					binop->type = target_type;
				} else {
					if (left->type == right->type) {
						binop->type = left->type;
					} else {
						invalid_code_path();
					}
				}
			}

			break;


			switch (binop->operation) {
				case '+':
				case '-':
				case '*':
				case '/':
				case '%':
				case '|':
				case '&':
				case '^': {
					defer { binop->type = binop->left->type; };
					auto val = get_constant_integer(binop);
					if (val.has_value()) {
						expression = make_integer(val.value_unchecked());
						return harden_type(state, &expression, target_type);
					}

					if (left->type == &type_unsized_integer) {
						return harden_type(state, &left, right->type);
					}
					if (right->type == &type_unsized_integer) {
						return harden_type(state, &right, left->type);
					}

					/*
						i : int;
						a := (i+1) * (i+1);
					*/

					if (!harden_type(state, &left))  return false;
					if (!harden_type(state, &right)) return false;

					if (!types_match(left->type, right->type)) {
						state->reporter.error(binop->location, "Can not apply binary '%' to expressions of types '%' and '%'", binary_operator_string(binop->operation), type_to_string(left->type), type_to_string(right->type));
						return false;
					}
					if (target_type) {
						if (!types_match(left->type, target_type)) {
							report_not_convertible(&state->reporter, left, target_type);
							return false;
						}
						if (!types_match(right->type, target_type)) {
							report_not_convertible(&state->reporter, right, target_type);
							return false;
						}
					}
					return true;
				}
				case '<' :
				case '>' :
				case '<=':
				case '>=':
				case '==':
				case '!=': {
					// TODO: implement
					if (!harden_type(state, &left))
						return false;
					if (!harden_type(state, &right))
						return false;

					if (!::is_integer(left->type) || !::is_integer(right->type)) {
						state->reporter.error(binop->location, "Types '%' and '%' are not comparable", type_to_string(left->type), type_to_string(right->type));
						return false;
					}

					return true;
				}
				case '.':
					return true;
			}
			invalid_code_path();
		}
		case Ast_struct: {
			auto Struct = (AstStruct *)expression;
			for (auto member : Struct->members) {
				if (member->expression) {
					if (!harden_type(state, &member->expression, member->type)) {
						return false;
					}
				}
			}
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			if (!harden_type(state, &subscript->index_expression)) {
				return false;
			}
			if (!harden_type(state, &subscript->expression)) {
				return false;
			}
			break;
		}
		case Ast_cast: {
			break;
		}
		default: {
			invalid_code_path("unhandled case in harden_type");
		}
	}
	return true;
}

//
// Replaces identifiers with structs
// Example: *void parses into:
//   AstUnary(*) -> AstIdentifier(void)
// This will be replaced with:
//   AstUnary(*) -> AstStruct(void)
//
// Reason for this is getting rid of extra pointer dereferences in later use
//
void shorten_type(AstExpression **_type) {
	auto type = *_type;

	if (type->type != &type_type)
		return; // Don't shorten expressions!!

	defer { *_type = type; };
	switch (type->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)type;
			type = identifier->definition->expression;
			shorten_type(&type);
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)type;
			assert(unop->operation == '*');
			shorten_type(&unop->expression);
			break;
		}
		case Ast_struct: {
			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)type;
			shorten_type(&subscript->expression);
			shorten_type(&subscript->index_expression);
			break;
		}
		default: {
			invalid_code_path();
			break;
		}
	}
}

bool is_constant(AstExpression *expression) {
	if (expression->kind == Ast_literal)
		return true;

	if (expression->kind == Ast_identifier) {
		return ((AstIdentifier *)expression)->definition->is_constant;
	}

	if (expression->kind == Ast_binary_operator) {
		auto binop = (AstBinaryOperator *)expression;
		return is_constant(binop->left) && is_constant(binop->right);
	}

	return false;
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
	if (types_match(lambda->return_parameter->type, &type_void))
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
	auto unop = new_ast<AstUnaryOperator>();
	unop->expression = type;
	unop->type = &type_type;
	unop->operation = '*';
	return unop;
}

cppcoro::generator<TypecheckResult> typecheck(TypecheckState *state, AstExpression *&expression);

AstExpression *get_type_from_type_expression(AstExpression *type) {
	if (type->kind == Ast_subscript || type->kind == Ast_unary_operator) {
		return type;
	}

	return get_struct(type);
}

struct CastType {
	AstStruct *from;
	AstStruct *to;
	CastKind kind;
	bool implicit;
};


bool operator==(CastType a, CastType b) {
	return a.from == b.from && a.to == b.to;
}

// TODO: this can be HashSet
LinearSet<CastType> built_in_casts;

bool implicitly_cast(TypecheckState *state, AstExpression **_expression, AstExpression *type) {
	auto expression = *_expression;
	defer { *_expression = expression; };

	if (types_match(expression->type, type)) {
		return true;
	}

	auto found_built_in = find(built_in_casts, {get_struct(expression->type), get_struct(type)});

	if (found_built_in) {
		auto cast = new_ast<AstCast>();
		cast->expression = expression;
		cast->type = type;
		cast->cast_kind = found_built_in->kind;
		expression = cast;
		return true;
	}

	state->reporter.error(expression->location, "Expression of type '%' is not implicitly convertible to '%'", type_to_string(expression->type), type_to_string(type));
	return false;
}

cppcoro::generator<TypecheckResult> typecheck(TypecheckState *state, AstStatement *statement) {
	timed_function();

	auto _statement = statement;

	switch (statement->kind) {
		case Ast_return: {
			auto ret = (AstReturn *)statement;
			auto lambda = ret->lambda;

			auto &expression = ret->expression;
			if (expression) {
				TYPECHECK(expression);
				assert(expression->type);

				if (!harden_type(state, &expression, lambda->return_parameter->type))
					co_yield TypecheckResult::fail;

				if (!implicitly_cast(state, &expression, lambda->return_parameter->type))
					co_yield TypecheckResult::fail;
			}
			state->current_lambda->return_statements.add(ret);
			break;
		}
		case Ast_definition: {
			auto definition = (AstDefinition *)statement;

			if (definition->is_parameter || (definition->parent_block && definition->parent_block->kind == Ast_struct)) {
				assert(definition->type);
				TYPECHECK(definition->type);
				shorten_type(&definition->type);
			} else if (state->current_lambda) {

				//
				// Lambda local definition
				//

				auto lambda = state->current_lambda;

				if (definition->expression) {
					TYPECHECK(definition->expression);
				}

				if (definition->type) {
					TYPECHECK(definition->type);
					shorten_type(&definition->type);

					if (definition->expression) {
						if (!convertible(definition->expression, definition->type)) {
							report_not_convertible(&state->reporter, definition->expression, definition->type);
							co_yield TypecheckResult::fail;
						}
						if (!harden_type(state, &definition->expression, definition->type)) {
							co_yield TypecheckResult::fail;
						}
					}

					definition->type = get_type_from_type_expression(definition->type);
					assert(definition->type);
				} else {
					if (!harden_type(state, &definition->expression)) {
						co_yield TypecheckResult::fail;
					}
					definition->type = definition->expression->type;
				}


				if (definition->is_constant) {
					if (!is_constant(definition->expression)) {
						state->reporter.error(definition->location, "Definition marked as constant, but assigned expression is not constant");
						co_yield TypecheckResult::fail;
					}
				}
			} else {

				//
				// Global definition
				//

				if (definition->built_in) {
					break;
				}

				state->definition = definition;

				if (definition->expression) {
					if (definition->expression->kind == Ast_lambda) {
						state->lambda = (AstLambda *)definition->expression;
					}

					TYPECHECK(definition->expression);
				}

				// This will check if specified type matches expression's type.
				// If the type is not specified then it will be null, except for lambdas:
				// Lambda will set the type right after typechecking it's head
				if (definition->type && !(definition->expression && definition->expression->kind == Ast_lambda)) {
					TYPECHECK(definition->type);

					if (definition->expression) {
						if (!convertible(definition->expression, definition->type)) {
							report_not_convertible(&state->reporter, definition->expression, definition->type);
							co_yield TypecheckResult::fail;
						}
					}
				} else {
					definition->type = definition->expression->type;
					if (definition->type == &type_unsized_integer) {
						definition->type = &type_s64;
					}
				}
				if (definition->expression) {
					switch (definition->expression->kind) {
						case Ast_lambda: {
							definition->type = definition->expression;
							break;
						}
						default: {
							if (!harden_type(state, &definition->expression, definition->type)) {
								co_yield TypecheckResult::fail;
							}
							break;
						}
					}

					// definition->expression = simplify(definition->expression);
				}


				// Lambda will insert itself into typechecked_globals
				// after typechecking arguments and return type
				// and before typechecking it's body
				if (!definition->expression || definition->expression->kind != Ast_lambda) {
					scoped_lock(typechecked_globals_mutex);
					typechecked_globals_mutex_lock_count += 1;
					typechecked_globals.get_or_insert(definition->name) = definition;
				}
			}
			assert(definition->type);

			break;
		}
		case Ast_if: {
			auto If = (AstIf *)statement;

			auto _state = state;

			TYPECHECK(If->condition);
			if (!harden_type(state, &If->condition, &type_bool)) {
				co_yield TypecheckResult::fail;
			}

			if (!types_match(If->condition->type, &type_bool)) {
				state->reporter.error(If->condition->location, "Expression with type % can not be used as a condition in if statement. Only expressions with type bool are allowed here.", type_to_string(If->condition->type));
				co_yield TypecheckResult::fail;
			}

			typecheck_scope(&If->true_scope);
			typecheck_scope(&If->false_scope);

			break;
		}
		case Ast_while: {
			auto While = (AstWhile *)statement;

			TYPECHECK(While->condition);
			if (!harden_type(state, &While->condition, &type_bool))
				co_yield TypecheckResult::fail;

			if (!types_match(While->condition->type, &type_bool)) {
				state->reporter.error(While->condition->location, "Expression with type % can not be used as a condition in while statement. Only expressions with type bool are allowed here.", type_to_string(While->condition->type));
				co_yield TypecheckResult::fail;
			}

			typecheck_scope(&While->scope);

			break;
		}
		case Ast_expression_statement: {
			auto es = (AstExpressionStatement *)statement;
			TYPECHECK(es->expression);
			break;
		}
		case Ast_block: {
			auto block = (AstBlock *)statement;
			typecheck_scope(&block->scope);
			break;
		}
		default: {
			invalid_code_path("invalid statement kind in typecheck");
		}
	}
	co_yield TypecheckResult::success;
}

#define wait_iteration \
	state->no_progress_counter++; \
	if (state->no_progress_counter == 256) { /* TODO: This is not the best solution */ \
		auto _definition = definition; \
		state->reporter.error(name, "Undeclared identifier"); \
		co_yield TypecheckResult::fail; \
	} \
	co_yield TypecheckResult::wait

cppcoro::generator<TypecheckResult> _wait_for_definition(AstDefinition *&definition, TypecheckState *state, Span<utf8> name) {
	while (1) {
		definition = get_definition(state, name);
		if (definition) {
			while (!definition->type) {
				wait_iteration;
			}
			state->no_progress_counter = 0;
			break;
		}

		wait_iteration;
	}
	co_yield TypecheckResult::success;
}

#define wait_for_definition(definition, name) \
	AstDefinition *definition = 0; \
	for (auto result : _wait_for_definition(definition, state, name)) { \
		if (result == TypecheckResult::success) { \
			break; \
		} \
		co_yield result; \
	}

bool ensure_addressable(Reporter *reporter, AstExpression *expression) {
	switch (expression->kind) {
		case Ast_identifier: {
			return true;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != '.')
				break;

			return ensure_addressable(reporter, binop->right);
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(subscript->expression->kind == Ast_identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return ensure_addressable(reporter, identifier);
		}
	}

	reporter->error(expression->location, "Expression is not addressable");
	return false;
}
bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
	switch (expression->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;
			if (identifier->definition->is_constant) {
				reporter->error(identifier->location, "Can't assign to '%' because it is constant", identifier->location);
				return false;
			}
			if (identifier->definition->is_parameter) {
				reporter->error(identifier->location, "Can't assign to function parameters");
				return false;
			}

			return true;
		}
		case Ast_binary_operator: {
			auto binop = (AstBinaryOperator *)expression;
			if (binop->operation != '.')
				break;

			return ensure_assignable(reporter, binop->left) && ensure_assignable(reporter, binop->right);
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;
			assert(subscript->expression->kind == Ast_identifier);
			auto identifier = (AstIdentifier *)subscript->expression;

			return ensure_assignable(reporter, identifier);
		}
	}

	reporter->error(expression->location, "Expression is not assignable");
	return false;
}

bool ensure_subscriptable(TypecheckState *state, AstExpression *expression) {
	if (expression->type->kind != Ast_subscript) {
		state->reporter.error(expression->location, "Expression is not subscriptable");
		return false;
	}
	return true;
}

cppcoro::generator<TypecheckResult> typecheck(TypecheckState *state, AstExpression *&expression) {
	timed_function();

	assert(expression);

	if (expression->type)
		co_yield TypecheckResult::success;

	switch (expression->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;

			if (identifier->definition)
				break;

			wait_for_definition(definition, identifier->name);

			assert(definition->type);
			identifier->definition = definition;
			identifier->type = definition->type;
			break;
		}
		case Ast_call: {
			auto call = (AstCall *)expression;
			wait_for_definition(definition, call->name);
			call->definition = definition;
			if (definition->expression->kind != Ast_lambda) {
				state->reporter.error(call->location, "Expression in not callable");
				co_yield TypecheckResult::fail;
			}
			auto lambda = (AstLambda *)definition->expression;
			call->lambda = lambda;

			while (!lambda->finished_typechecking_head) {
				co_yield TypecheckResult::wait;
			}

			if (call->arguments.count != lambda->parameters.count) {
				state->reporter.error(call->location, "Argument count does not match");
				co_yield TypecheckResult::fail;
			}

			for (u32 i = 0; i < call->arguments.count; ++i) {
				auto &argument = call->arguments[i];
				auto &parameter = lambda->parameters[i];

				TYPECHECK(argument);

				if (!convertible(argument, parameter->type)) {
				}

				if (!harden_type(state, &argument, parameter->type)) {
					co_yield TypecheckResult::fail;
				}

			}

			call->type = lambda->return_parameter->type;
			break;
		}
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			switch (literal->literal_kind) {
				using enum LiteralKind;
				case integer:
					expression->type = &type_unsized_integer;
					break;
				case boolean:
					expression->type = &type_bool;
					break;
				case string:
					expression->type = &type_string;
					break;
				case character:
					expression->type = &type_u8;
					break;
				default:
					invalid_code_path("not implemented");
			}
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)expression;

			lambda->parent_lambda = state->current_lambda;

			auto old_current_lamdda = state->current_lambda;
			defer { assert(state->current_lambda == old_current_lamdda); };

			state->current_lambda = lambda;
			assert(state->current_lambda);
			defer { state->current_lambda = old_current_lamdda; };

			assert(lambda->return_parameter);
			TYPECHECK(lambda->return_parameter);

			for (auto parameter : lambda->parameters) {
				TYPECHECK(parameter);
			}

			// Weird way to check if lambda is global
			if (lambda == state->lambda) {
				scoped_lock(typechecked_globals_mutex);
				typechecked_globals_mutex_lock_count += 1;
				typechecked_globals.get_or_insert(state->definition->name) = state->definition;
			}

			lambda->type = lambda;

			lambda->definition->type = lambda->type;

			lambda->finished_typechecking_head = true;

			if (lambda->has_body) {
				typecheck_scope(&lambda->body_scope);

				/*
				for (auto ret : lambda->return_statements) {
					if (ret->expression) {
						if (!harden_type(state, &ret->expression, lambda->return_parameter->type)) {
							state->reporter.info(lambda->location, "When hardening return statement expression's type (lambda's return type is '%')", type_to_string(lambda->return_parameter->type));
							co_yield TypecheckResult::fail;
						}
						if (!convertible(ret->expression, lambda->return_parameter->type)) {
							report_not_convertible(&state->reporter, ret->expression, lambda->return_parameter->type);
							co_yield TypecheckResult::fail;
						}
					} else {
						if (!types_match(lambda->return_parameter->type, &type_void)) {
							if (!lambda->return_parameter->name.count) {
								state->reporter.error(ret->location, "Attempt to return nothing when lambda's return type is '%' and return parameter is unnamed", type_to_string(lambda->return_parameter->type));
								co_yield TypecheckResult::fail;
							}
						}
					}
				}
				*/

				// assert(lambda->return_parameter->type);

				//if (!do_all_paths_return(lambda)) {
				//	state->reporter.error(lambda->location, "Not all paths return a value");
				//	co_yield TypecheckResult::fail;
				//}
			}

			break;
		}
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)expression;

			TYPECHECK(bin->left);

			auto report_type_mismatch = [&] {
				state->reporter.error(bin->location, "Can't use binary '%' on types '%' and '%'", binary_operator_string(bin->operation), type_to_string(bin->left->type), type_to_string(bin->right->type));
			};

			if (bin->operation == '.') {
				Span<utf8> name;
				switch (bin->right->kind) {
					case Ast_identifier: name = ((AstIdentifier *)bin->right)->name; break;
					case Ast_call:       name = ((AstCall       *)bin->right)->name; break;
					default: invalid_code_path("Typecheck: Right expression of binary '.' was not 'identifier' nor 'call'"); break;
				}

				//harden_type(&bin->left->type);

				bool left_is_type = is_type(bin->left);
				auto Struct = get_struct(left_is_type ? bin->left : bin->left->type);

				if (Struct) {
					auto found_member = find_if(left_is_type ? Struct->constants : Struct->members, [&](AstDefinition *member) { return member->name == name; });
					if (!found_member) {
						if (left_is_type) {
							state->reporter.error(bin->right->location, "Type '%' does not contain constant '%'", Struct->definition->name, bin->right->location);
						} else {
							state->reporter.error(bin->right->location, "'%' is not a member of '%'", bin->right->location, Struct->definition->name);
						}
						co_yield TypecheckResult::fail;
					}

					switch (bin->right->kind) {
						case Ast_identifier: {
							auto identifier = (AstIdentifier *)bin->right;
							identifier->definition = *found_member;
							identifier->type = identifier->definition->type;
							break;
						}
						default: invalid_code_path(); break;
					}
				} else {
					state->reporter.error(bin->left->location, "Dot operator can not be applied to an expression of type '%'", type_to_string(bin->left->type));
					co_yield TypecheckResult::fail;
				}
				bin->type = bin->right->type;

			} else if (bin->operation == '<' || bin->operation == '>' || bin->operation == '<=' || bin->operation == '>='|| bin->operation == '=='|| bin->operation == '!=') {
				TYPECHECK(bin->right);

				if (!types_match(bin->left->type, bin->right->type)) {
					if (bin->left->type == &type_unsized_integer) {
						     if (bin->right->type == &type_u8 ) bin->left->type = &type_u8;
						else if (bin->right->type == &type_u16) bin->left->type = &type_u16;
						else if (bin->right->type == &type_u32) bin->left->type = &type_u32;
						else if (bin->right->type == &type_u64) bin->left->type = &type_u64;
						else if (bin->right->type == &type_s8 ) bin->left->type = &type_s8;
						else if (bin->right->type == &type_s16) bin->left->type = &type_s16;
						else if (bin->right->type == &type_s32) bin->left->type = &type_s32;
						else if (bin->right->type == &type_s64) bin->left->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else if (bin->right->type == &type_unsized_integer) {
						     if (bin->left->type == &type_u8 ) bin->right->type = &type_u8;
						else if (bin->left->type == &type_u16) bin->right->type = &type_u16;
						else if (bin->left->type == &type_u32) bin->right->type = &type_u32;
						else if (bin->left->type == &type_u64) bin->right->type = &type_u64;
						else if (bin->left->type == &type_s8 ) bin->right->type = &type_s8;
						else if (bin->left->type == &type_s16) bin->right->type = &type_s16;
						else if (bin->left->type == &type_s32) bin->right->type = &type_s32;
						else if (bin->left->type == &type_s64) bin->right->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else {
						// report_type_mismatch();
						// co_yield TypecheckResult::fail;
					}
				}

				bin->type = &type_bool;

			} else if (bin->operation == '=') {
				TYPECHECK(bin->right);
				if (!ensure_assignable(&state->reporter, bin->left)) {
					co_yield TypecheckResult::fail;
				}

				if (!harden_type(state, &bin->right, bin->left->type)) {
					co_yield TypecheckResult::fail;
				}
				bin->type = bin->right->type;
				assert(get_size(bin->left->type) == get_size(bin->right->type));

			} else if (
				bin->operation == '+=' ||
				bin->operation == '-=' ||
				bin->operation == '*=' ||
				bin->operation == '/=' ||
				bin->operation == '%=' ||
				bin->operation == '|=' ||
				bin->operation == '&=' ||
				bin->operation == '^='
			) {
				TYPECHECK(bin->right);
				if (!ensure_assignable(&state->reporter, bin->left)) {
					co_yield TypecheckResult::fail;
				}

				if (types_match(bin->left->type, bin->right->type)) {
					bin->type = bin->left->type;
				} else {
					if (bin->left->type == &type_unsized_integer) {
								if (bin->right->type == &type_u8 ) bin->type = bin->left->type = &type_u8;
						else if (bin->right->type == &type_u16) bin->type = bin->left->type = &type_u16;
						else if (bin->right->type == &type_u32) bin->type = bin->left->type = &type_u32;
						else if (bin->right->type == &type_u64) bin->type = bin->left->type = &type_u64;
						else if (bin->right->type == &type_s8 ) bin->type = bin->left->type = &type_s8;
						else if (bin->right->type == &type_s16) bin->type = bin->left->type = &type_s16;
						else if (bin->right->type == &type_s32) bin->type = bin->left->type = &type_s32;
						else if (bin->right->type == &type_s64) bin->type = bin->left->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else if (bin->right->type == &type_unsized_integer) {
								if (bin->left->type == &type_u8 ) bin->type = bin->right->type = &type_u8;
						else if (bin->left->type == &type_u16) bin->type = bin->right->type = &type_u16;
						else if (bin->left->type == &type_u32) bin->type = bin->right->type = &type_u32;
						else if (bin->left->type == &type_u64) bin->type = bin->right->type = &type_u64;
						else if (bin->left->type == &type_s8 ) bin->type = bin->right->type = &type_s8;
						else if (bin->left->type == &type_s16) bin->type = bin->right->type = &type_s16;
						else if (bin->left->type == &type_s32) bin->type = bin->right->type = &type_s32;
						else if (bin->left->type == &type_s64) bin->type = bin->right->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else {
						report_type_mismatch();
						co_yield TypecheckResult::fail;
					}
				}
				if (!harden_type(state, &bin->right, bin->left->type)) {
					co_yield TypecheckResult::fail;
				}
				bin->type = bin->right->type;
				assert(get_size(bin->left->type) == get_size(bin->right->type));
			} else if (
				bin->operation == '+' ||
				bin->operation == '-' ||
				bin->operation == '*' ||
				bin->operation == '/' ||
				bin->operation == '%' ||
				bin->operation == '|' ||
				bin->operation == '&' ||
				bin->operation == '>>' ||
				bin->operation == '<<' ||
				bin->operation == '^'
			) {
				TYPECHECK(bin->right);

				if (types_match(bin->left->type, bin->right->type)) {
					bin->type = bin->left->type;
				} else {
					if (bin->left->type == &type_unsized_integer) {
						     if (bin->right->type == &type_u8 ) bin->type = bin->left->type = &type_u8;
						else if (bin->right->type == &type_u16) bin->type = bin->left->type = &type_u16;
						else if (bin->right->type == &type_u32) bin->type = bin->left->type = &type_u32;
						else if (bin->right->type == &type_u64) bin->type = bin->left->type = &type_u64;
						else if (bin->right->type == &type_s8 ) bin->type = bin->left->type = &type_s8;
						else if (bin->right->type == &type_s16) bin->type = bin->left->type = &type_s16;
						else if (bin->right->type == &type_s32) bin->type = bin->left->type = &type_s32;
						else if (bin->right->type == &type_s64) bin->type = bin->left->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else if (bin->right->type == &type_unsized_integer) {
						     if (bin->left->type == &type_u8 ) bin->type = bin->right->type = &type_u8;
						else if (bin->left->type == &type_u16) bin->type = bin->right->type = &type_u16;
						else if (bin->left->type == &type_u32) bin->type = bin->right->type = &type_u32;
						else if (bin->left->type == &type_u64) bin->type = bin->right->type = &type_u64;
						else if (bin->left->type == &type_s8 ) bin->type = bin->right->type = &type_s8;
						else if (bin->left->type == &type_s16) bin->type = bin->right->type = &type_s16;
						else if (bin->left->type == &type_s32) bin->type = bin->right->type = &type_s32;
						else if (bin->left->type == &type_s64) bin->type = bin->right->type = &type_s64;
						else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;
						}
					} else {
						auto left_size = get_size(bin->left->type);
						auto right_size = get_size(bin->right->type);
						if (left_size != right_size) {
							if (left_size > right_size) {
								bin->type = bin->left->type;
							} else {
								bin->type = bin->right->type;
							}
						} else {
							report_type_mismatch();
							co_yield TypecheckResult::fail;

						}
					}
					/* else if (bin->left->type == &type_u8) {
						     if (bin->right->type == &type_u8 ) bin->type = &type_u8;
						else if (bin->right->type == &type_u16) bin->type = &type_u16;
						else if (bin->right->type == &type_u32) bin->type = &type_u32;
						else if (bin->right->type == &type_u64) bin->type = &type_u64;
						else if (bin->right->type == &type_s8 ) { state->reporter.error(bin->location, "Can't use binary '%' on types"); }
						else if (bin->right->type == &type_s16) bin->type = &type_s16;
						else if (bin->right->type == &type_s32) bin->type = &type_s32;
						else if (bin->right->type == &type_s64) bin->type = &type_s64;
						else {
							invalid_code_path();
						}
					} else {
						invalid_code_path();
					}
					*/
				}

				/*
				if (!types_match(bin->left->type, bin->right->type)) {
					if (bin->left->type == &type_unsized_integer && bin->right->type == &type_unsized_integer) {
						if (!harden_type(state, &bin->left) || !harden_type(state, &bin->right)) {
							co_yield TypecheckResult::fail;
						}
					} else if (bin->left->type == &type_unsized_integer) {
						if (!harden_type(state, &bin->left, bin->right->type)) {
							co_yield TypecheckResult::fail;
						}
					} else if (bin->right->type == &type_unsized_integer) {
						if (!harden_type(state, &bin->right, bin->left->type)) {
							co_yield TypecheckResult::fail;
						}
					} else {
						state->reporter.error(bin->location, "Invalid binary operator");
						co_yield TypecheckResult::fail;
					}
				}
				bin->type = bin->right->type;
				*/
			} else {
				invalid_code_path();
			}

			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;

			TYPECHECK(unop->expression);

			if (is_type(unop->expression)) {
				if (unop->operation != '*') {
					state->reporter.error(unop->location, "Unary operator '%' can not be applied to a type expression", unop->location);
					co_yield TypecheckResult::fail;
				}
				unop->type = &type_type;
			} else {
				switch (unop->operation) {
					case '-': {
						if (::is_integer(unop->expression->type)) {
							unop->type = unop->expression->type;
						} else {
							state->reporter.error(unop->location, "Unary minus can not be applied to expression of type '%'", type_to_string(unop->expression->type));
							co_yield TypecheckResult::fail;
						}
						break;
					}
					case '&': {
						if (!ensure_addressable(&state->reporter, unop->expression)) {
							co_yield TypecheckResult::fail;
						}
						unop->type = make_pointer_type(unop->expression->type);
						break;
					}
					default: {
						invalid_code_path();
						co_yield TypecheckResult::fail;
					}
				}
			}
			break;
		}
		case Ast_struct: {
			auto Struct = (AstStruct *)expression;
			auto definition = Struct->definition;

			//assert(!definition->type);
			//definition->type = &type_type;
			Struct->type = &type_type;


			s64 struct_size = 0;
			s64 struct_alignment = 0;

			{
				push_scope(&Struct->scope);
				for (auto member : Struct->members) {
					TYPECHECK(member);

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

			Struct->size = struct_size;
			Struct->alignment = struct_alignment;

			break;
		}
		case Ast_subscript: {
			auto subscript = (AstSubscript *)expression;

			TYPECHECK(subscript->index_expression);
			if (!harden_type(state, &subscript->index_expression, 0)) {
				co_yield TypecheckResult::fail;
			}
			if (!::is_integer(subscript->index_expression->type)) {
				state->reporter.error(subscript->index_expression->location, "Expression must be of type 'integer' but is '%'", type_to_string(subscript->index_expression->type));
				co_yield TypecheckResult::fail;
			}

			TYPECHECK(subscript->expression);
			if (is_type(subscript->expression)) {
				subscript->type = &type_type;
			} else {
				if (!ensure_subscriptable(state, subscript->expression)) {
					co_yield TypecheckResult::fail;
				}

				auto type = subscript->expression->type;
				assert(type->kind == Ast_subscript);

				subscript->type = ((AstSubscript *)type)->expression;
			}
			break;
		}
		case Ast_sizeof: {
			auto size_of = (AstSizeof *)expression;

			TYPECHECK(size_of->expression);

			expression = make_integer(get_size(size_of->expression));
			expression->type = &type_unsized_integer;

			break;
		}
		default: {
			invalid_code_path();
			state->reporter.error(expression->location, "Internal error: typecheck(AstExpression *): unhandled case '%'", expression->kind);
			co_yield TypecheckResult::fail;
		}
	}
	assert(expression->type);
	co_yield TypecheckResult::success;
}

cppcoro::generator<TypecheckResult> typecheck_global(TypecheckState *state) {
	timed_function();
	TYPECHECK(state->statement);
	co_yield TypecheckResult::success;
}

bool typecheck_finished;

void add_member(AstStruct &destination, AstExpression *type, Span<utf8> name, AstLiteral *value, bool constant, s32 offset) {
	auto d = new_ast<AstDefinition>();
	d->location = name;
	d->name = name;
	d->expression = value;
	d->is_constant = constant;
	d->type = type;
	d->offset_in_struct = offset;
	d->parent_block = &destination;
	if (value) {
		value->type = type;
	}

	(constant ? destination.constants : destination.members).add(d);
}

static void write_test_source() {
	StringBuilder test;
	append_format(test, u8"♥0 :: fn (☺: s32, ☻: s32) s32 { return ☺ + ☻; } /* ♦♣♠•◘○ this is a /* nested */ comment ♦♣♠•◘○ */\n");
	for (int i = 1; i < 4096; ++i) {
		append_format(test, u8"♥% :: fn (☺: s32, ☻: s32) s32 { return ♥%(☺, ☻); } /* ♦♣♠•◘○ this is a /* nested */ comment ♦♣♠•◘○ */\n", i, i - 1);
	}
	write_entire_file("test.tl"s, as_bytes(to_string(test)));
}
enum class Output {
	none,
	c,
	nasm,
};
struct ParsedArguments {
	Output output = {};

	List<Span<utf8>> source_files;

	bool print_ast = false;
	bool no_typecheck = false;

	bool success = false;
};

ParsedArguments parse_arguments(Span<Span<utf8>> arguments) {
	timed_function();

	ParsedArguments result = {};

	executable_path = arguments[0];

	if (!is_absolute_path(executable_path)) {
		executable_path = concatenate(to_utf8(get_current_directory()), '\\', executable_path);
	}

	auto parsed = parse_path(executable_path);
	executable_name = parsed.name;
	executable_directory = parsed.directory;

	//print("executable_path: %\nexecutable_name: %\nexecutable_directory: %\n", executable_path, executable_name, executable_directory);

	if (arguments.count == 1) {
		print_help();
		return result;
	}

	for (int i = 1; i < arguments.count; ++i) {
		if (arguments[i] == u8"--print-ast"s) {
			result.print_ast = true;
		} else if (arguments[i] == u8"--no-type-check"s) {
			result.no_typecheck = true;
		} else if (arguments[i] == u8"--output"s) {
			++i;
			if (i >= arguments.count) {
				print("Expected an argument after --output.\n");
				return result;
			}
			using enum Output;
				 if (arguments[i] == u8"none"s) result.output = none;
			else if (arguments[i] == u8"c"s   ) result.output = c;
			else if (arguments[i] == u8"nasm"s) result.output = nasm;
			else {
				print(Print_error, "Unknown output type '%'\n", arguments[i]);
			}
		} else {
			result.source_files.add(arguments[i]);
		}
	}

	result.success = true;
	return result;
}

s32 tl_main(Span<Span<utf8>> arguments) {
	//CreateWindowExA(0, (char *)1, (char *)2, 3, 4, 5, 6, 7, (HWND)8, (HMENU)9, (HINSTANCE)10, (void *)11);

	set_console_encoding(Encoding_utf8);

	defer { print("Peak memory usage: %\n", format_bytes(get_memory_info().peak_usage)); };

	//write_test_source();

	init_ast_allocator();
	extern_init();

	default_allocator = current_allocator = {
		[](AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> void * {
			switch (mode) {
				case Allocator_allocate: {
					return my_allocate(new_size, align);
				}
				case Allocator_reallocate: {
					auto result = my_allocate(new_size, align);
					memcpy(result, data, old_size);
					return result;
				}
				case Allocator_free: {
					return 0;
				}
			}
			return 0;
		},
		0
	};

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
	Profiler::init();
	defer { Profiler::deinit(); };
#endif

	defer { write_entire_file("profile.tmd"s, Profiler::output_for_timed()); };

	timed_function();

restart_main:

	timed_begin("setup"s);

	auto args = parse_arguments(arguments);

	if (args.source_files.count == 0) {
		print("No source path received. Exiting.\n");
		return 1;
	}
	source_path = args.source_files[0];
	if (!is_absolute_path(source_path)) {
		source_path = make_absolute_path(source_path);
	}
	source_path_without_extension = parse_path(source_path).path_without_extension();

	construct(parsed_files);
	construct(sources);
	construct(global_scope);
	construct(typechecked_globals);
	construct(names_not_available_for_globals);
	construct(built_in_casts);

	integer_infos[0] = {&type_u8,  0ib, 0xffib,               0xffllu};
	integer_infos[1] = {&type_u16, 0ib, 0xffffib,             0xffffllu};
	integer_infos[2] = {&type_u32, 0ib, 0xffffffffib,         0xffffffffllu};
	integer_infos[3] = {&type_u64, 0ib, 0xffffffffffffffffib, 0xffffffffffffffffllu};
	integer_infos[4] = {&type_s8,  -0x80ib,               0x7fib,               0xffllu};
	integer_infos[5] = {&type_s16, -0x8000ib,             0x7fffib,             0xffffllu};
	integer_infos[6] = {&type_s32, -0x80000000ib,         0x7fffffffib,         0xffffffffllu};
	integer_infos[7] = {&type_s64, -0x8000000000000000ib, 0x7fffffffffffffffib, 0xffffffffffffffffllu};

	auto init_type = [&](AstStruct &s, Span<utf8> name, s64 size, s64 align) {
		s.members.allocator = default_allocator;
		s.constants.allocator = default_allocator;
		s.size = size;
		s.alignment = align;
		s.type = &type_type;

		auto definition = new_ast<AstDefinition>();
		definition->is_constant = true;
		definition->expression = &s;
		definition->location = definition->name = name;
		definition->type = &type_type;

		definition->built_in = true;

		s.definition = definition;

		global_scope.statements.add(definition);
		global_scope.definitions.get_or_insert(name) = definition;
		typechecked_globals.get_or_insert(name) = definition;
	};

	init_type(type_void,   u8"void"s, 0, 0);
	init_type(type_type,   u8"Type"s, 0, 0);
	init_type(type_bool,   u8"bool"s, 1, 1);
	init_type(type_u8,     u8"u8"s,   1, 1);
	init_type(type_u16,    u8"u16"s,  2, 2);
	init_type(type_u32,    u8"u32"s,  4, 4);
	init_type(type_u64,    u8"u64"s,  8, 8);
	init_type(type_s8,     u8"s8"s,   1, 1);
	init_type(type_s16,    u8"s16"s,  2, 2);
	init_type(type_s32,    u8"s32"s,  4, 4);
	init_type(type_s64,    u8"s64"s,  8, 8);
	init_type(type_string, u8"string"s, 16, 8);
	init_type(type_unsized_integer,  u8"integer"s, 0, 0);

	type_pointer_to_void.expression = &type_void;
	type_pointer_to_void.operation = '*';
	type_pointer_to_void.type = &type_type;

	type_default_integer = &type_s64;

	add_member(type_u8,  &type_u8,  u8"min"s, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u16, &type_u16, u8"min"s, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u32, &type_u32, u8"min"s, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u64, &type_u64, u8"min"s, make_integer(0), true, INVALID_MEMBER_OFFSET);
	add_member(type_u8,  &type_u8,  u8"max"s, make_integer(0xff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u16, &type_u16, u8"max"s, make_integer(0xffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u32, &type_u32, u8"max"s, make_integer(0xffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_u64, &type_u64, u8"max"s, make_integer(0xffffffffffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s8,  &type_s8,  u8"min"s, make_integer(0x80), true, INVALID_MEMBER_OFFSET);
	add_member(type_s16, &type_s16, u8"min"s, make_integer(0x8000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s32, &type_s32, u8"min"s, make_integer(0x80000000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s64, &type_s64, u8"min"s, make_integer(0x8000000000000000), true, INVALID_MEMBER_OFFSET);
	add_member(type_s8,  &type_s8,  u8"max"s, make_integer(0x7f), true, INVALID_MEMBER_OFFSET);
	add_member(type_s16, &type_s16, u8"max"s, make_integer(0x7fff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s32, &type_s32, u8"max"s, make_integer(0x7fffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_s64, &type_s64, u8"max"s, make_integer(0x7fffffffffffffff), true, INVALID_MEMBER_OFFSET);
	add_member(type_string, make_pointer_type(&type_void), u8"data"s, 0, false, 0);
	add_member(type_string, &type_u64, u8"count"s, 0, false, 8);

	built_in_casts.insert({&type_u8 , &type_s8 , CastKind::u8_s8  , false});
	built_in_casts.insert({&type_u8 , &type_s16, CastKind::u8_s16 , true});
	built_in_casts.insert({&type_u8 , &type_s32, CastKind::u8_s32 , true});
	built_in_casts.insert({&type_u8 , &type_s64, CastKind::u8_s64 , true});
	built_in_casts.insert({&type_u8 , &type_u16, CastKind::u8_u16 , true});
	built_in_casts.insert({&type_u8 , &type_u32, CastKind::u8_u32 , true});
	built_in_casts.insert({&type_u8 , &type_u64, CastKind::u8_u64 , true});
	built_in_casts.insert({&type_u16, &type_s8 , CastKind::u16_s8 , false});
	built_in_casts.insert({&type_u16, &type_s16, CastKind::u16_s16, false});
	built_in_casts.insert({&type_u16, &type_s32, CastKind::u16_s32, true});
	built_in_casts.insert({&type_u16, &type_s64, CastKind::u16_s64, true});
	built_in_casts.insert({&type_u16, &type_u8 , CastKind::u16_u8 , false});
	built_in_casts.insert({&type_u16, &type_u32, CastKind::u16_u32, true});
	built_in_casts.insert({&type_u16, &type_u64, CastKind::u16_u64, true});
	built_in_casts.insert({&type_u32, &type_s8 , CastKind::u32_s8 , false});
	built_in_casts.insert({&type_u32, &type_s16, CastKind::u32_s16, false});
	built_in_casts.insert({&type_u32, &type_s32, CastKind::u32_s32, false});
	built_in_casts.insert({&type_u32, &type_s64, CastKind::u32_s64, true});
	built_in_casts.insert({&type_u32, &type_u8 , CastKind::u32_u8 , false});
	built_in_casts.insert({&type_u32, &type_u16, CastKind::u32_u16, false});
	built_in_casts.insert({&type_u32, &type_u64, CastKind::u32_u64, true});
	built_in_casts.insert({&type_u64, &type_s8 , CastKind::u64_s8 , false});
	built_in_casts.insert({&type_u64, &type_s16, CastKind::u64_s16, false});
	built_in_casts.insert({&type_u64, &type_s32, CastKind::u64_s32, false});
	built_in_casts.insert({&type_u64, &type_s64, CastKind::u64_s64, false});
	built_in_casts.insert({&type_u64, &type_u8 , CastKind::u64_u8 , false});
	built_in_casts.insert({&type_u64, &type_u16, CastKind::u64_u16, false});
	built_in_casts.insert({&type_u64, &type_u32, CastKind::u64_u32, false});
	built_in_casts.insert({&type_s8 , &type_s16, CastKind::s8_s16 , true});
	built_in_casts.insert({&type_s8 , &type_s32, CastKind::s8_s32 , true});
	built_in_casts.insert({&type_s8 , &type_s64, CastKind::s8_s64 , true});
	built_in_casts.insert({&type_s8 , &type_u8 , CastKind::s8_u8  , false});
	built_in_casts.insert({&type_s8 , &type_u16, CastKind::s8_u16 , false});
	built_in_casts.insert({&type_s8 , &type_u32, CastKind::s8_u32 , false});
	built_in_casts.insert({&type_s8 , &type_u64, CastKind::s8_u64 , false});
	built_in_casts.insert({&type_s16, &type_s8 , CastKind::s16_s8 , false});
	built_in_casts.insert({&type_s16, &type_s32, CastKind::s16_s32, true});
	built_in_casts.insert({&type_s16, &type_s64, CastKind::s16_s64, true});
	built_in_casts.insert({&type_s16, &type_u8 , CastKind::s16_u8 , false});
	built_in_casts.insert({&type_s16, &type_u16, CastKind::s16_u16, false});
	built_in_casts.insert({&type_s16, &type_u32, CastKind::s16_u32, false});
	built_in_casts.insert({&type_s16, &type_u64, CastKind::s16_u64, false});
	built_in_casts.insert({&type_s32, &type_s8 , CastKind::s32_s8 , false});
	built_in_casts.insert({&type_s32, &type_s16, CastKind::s32_s16, false});
	built_in_casts.insert({&type_s32, &type_s64, CastKind::s32_s64, true});
	built_in_casts.insert({&type_s32, &type_u8 , CastKind::s32_u8 , false});
	built_in_casts.insert({&type_s32, &type_u16, CastKind::s32_u16, false});
	built_in_casts.insert({&type_s32, &type_u32, CastKind::s32_u32, false});
	built_in_casts.insert({&type_s32, &type_u64, CastKind::s32_u64, false});
	built_in_casts.insert({&type_s64, &type_s8 , CastKind::s64_s8 , false});
	built_in_casts.insert({&type_s64, &type_s16, CastKind::s64_s16, false});
	built_in_casts.insert({&type_s64, &type_s32, CastKind::s64_s32, false});
	built_in_casts.insert({&type_s64, &type_u8 , CastKind::s64_u8 , false});
	built_in_casts.insert({&type_s64, &type_u16, CastKind::s64_u16, false});
	built_in_casts.insert({&type_s64, &type_u32, CastKind::s64_u32, false});
	built_in_casts.insert({&type_s64, &type_u64, CastKind::s64_u64, false});

	current_printer = standard_output_printer;

	ThreadPool thread_pool;
	::thread_pool = &thread_pool;

	init_thread_pool(thread_pool, get_cpu_info().logical_processor_count - 1);

	defer {
		if (args.print_ast) {
			print_ast();
		}
	};

	timed_end("setup"s);

	parse_file(concatenate(executable_directory, u8"\\libs\\preload.tl"s));

	for (auto path : args.source_files) {
		parse_file(path);
	}

	wait_for_completion(thread_pool);

	if (failed_lexer) {
		failed_lexer->reporter->print_all();
		print("Lexer failed. Exiting.\n");
		return 1;
	}
	if (failed_parser) {
		failed_parser->reporter->print_all();
		print("Parser failed. Exiting.\n");
		return 1;
	}


	f32 typecheck_time;
	if (!args.no_typecheck) {
		auto timer = create_precise_timer();
		defer { typecheck_time = reset(timer); };

		Span<TypecheckState> typecheck_states;
		typecheck_states.count = count(global_scope.statements, [&](AstStatement *statement) { return !(statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in); });
		if (typecheck_states.count) {
			typecheck_states.data = default_allocator.allocate<TypecheckState>(typecheck_states.count);

			u32 typechecks_finished = 0;
			bool fail = false;
			u32 state_index = 0;

			auto process_coroutine_result = [&](auto &state) {
				auto result = *state.generator_iterator;
				switch (result) {
					case TypecheckResult::success:
						state.finished = true;
						typechecks_finished++;
						destruct(state); // This must be done to run all defers/destructors in the coroutine
						break;
					case TypecheckResult::fail:
						state.finished = true;
						typechecks_finished++;
						state.reporter.print_all();
						fail = true;
						destruct(state); // This must be done to run all defers/destructors in the coroutine
						break;
					case TypecheckResult::wait:
						break;
				}
				return result;
			};

			for (auto statement : global_scope.statements)  {
				if (statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in)
					continue;

				auto &state = typecheck_states[state_index];
				state.statement = statement;
				construct(state.generator, typecheck_global(&state));
				construct(state.generator_iterator, state.generator.begin());
				++state_index;

				if (process_coroutine_result(state) == TypecheckResult::fail) {
					break;
				}
			}
			if (fail)
				return 1;

			while (1) {
				for (u32 i = 0; i < typecheck_states.count; ++i) {
					if (typechecks_finished == typecheck_states.count) {
						goto typecheck_break;
					}

					auto &state = typecheck_states[i];
					if (state.finished)
						continue;

					++state.generator_iterator;
					switch (process_coroutine_result(state)) {
						//case TypecheckResult::fail:
						//	goto typecheck_break;
					}
				}
			}
		typecheck_break:;
			if (fail)
				return 1;
		}
	}

	typecheck_finished = true;

	if (args.no_typecheck) {
		return 1;
	}

	auto found_main = global_scope.functions.find(u8"main"s);
	if (found_main) {
		main_lambda = *found_main;

		if (!::is_integer(main_lambda->return_parameter->type) && !types_match(main_lambda->return_parameter->type, &type_void)) {
			immediate_error(main_lambda->return_parameter->type->location.data ? main_lambda->return_parameter->type->location : main_lambda->location, "Main function can return any type of integer or void, but not '%'.", type_to_string(main_lambda->return_parameter->type));
			return 1;
		}
	} else {
		print("Main function was not defined. Exiting.\n");
		return 1;
	}

	if (args.output != Output::none) {

		auto bytecode = build_bytecode();

		switch (args.output) {
			using enum Output;
			//case c:    output_c();    break;
			case nasm: output_nasm(bytecode); break;
		}
	}

	//print("Lexing took % ms. Tokens processed: %. Bytes processed: %.\n", lexer_time * 1000, lexer.tokens_lexed, format_bytes(source.count));
	print("Parsing took % ms.\n", parser_time * 1000);
	print("Type checking took % ms.\n", typecheck_time * 1000);
	print("typechecked_globals_mutex_lock_count: %\n", typechecked_globals_mutex_lock_count);

	return 0;
}

