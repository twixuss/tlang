#define TL_IMPL
#include <tl/main.h>
#include <tl/cpu.h>
#include <ast.h>
#include <output/c.h>
#include <output/nasm.h>

void print_help() {
	print(R"(Usage:
	% <path>
)", executable_name);
}

enum RingQueuePopStatus {
	RingQueuePop_ok,
	RingQueuePop_reached_end,
	RingQueuePop_user_condition,
};

template <class T, umm capacity_>
struct RingQueue {
	inline static constexpr umm capacity = capacity_;
	union {
		T data[capacity];
	};
	u32 current = 0;
	u32 end = 0;
	bool ended_write = false;
	void add(T value) {
		loop_while([&] { return full(); });
		data[atomic_increment(&end) % capacity] = value;
	}
	Optional<T> pop() {
		loop_while([&] {
			if (ended_write)
				return false;
			return empty();
		});
		if (ended_write && current == end) {
			return {};
		}
		return data[atomic_increment(&current) % capacity];
	}
	void end_write() {
		ended_write = true;
	}
	bool full() { return end - current == capacity; }
	bool empty() { return end - current == 0; }

	RingQueue() {}
};

Buffer source_buffer;
Span<utf8> source;

[[noreturn]] void exit() {
	::exit(-1);
}



u32 get_line_number(utf8 *from) {
	if (from == source.data)
		return 1;

	u32 result = 1;
	while (--from != source.data)
		result += (*from == '\n');
	return result;
}

void print_source_line(Span<utf8> location) {

	if (location.data == nullptr) {
		print("(null location)\n\n");
		return;
	}


	auto error_line_begin = location.begin();
	if (error_line_begin != source.begin()) {
		while (1) {
			error_line_begin--;
			if (error_line_begin == source.begin() || *error_line_begin == '\n') {
				if (error_line_begin != source.begin()) {
					error_line_begin++;
				}
				break;
			}
		}
	}

	auto error_line_end = location.end();
	while (1) {
		if (error_line_end == source.end() || *error_line_end == '\n') {
			break;
		}
		error_line_end++;
	}


	auto error_line = Span(error_line_begin, error_line_end);
	auto error_line_number = get_line_number(error_line_begin);

	if (error_line.data != source.data) {
		auto prev_line_end = error_line.data - 1;
		auto prev_line_begin = prev_line_end - 1;

		while (1) {
			if (prev_line_begin == source.data)
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
	print(Print_info,  line_start);
	print(Print_error, location);
	print(Print_info,  line_end);
	//print('\n');
	//
	//for (u32 i = 0; i < line_start.count + offset; ++i) print(' ');
	//for (u32 i = 0; i <   location.count; ++i) print('^');
	//for (u32 i = 0; i <   line_end.count; ++i) print(' ');

	print("\n\n");
}

struct Report {
	Span<utf8> location;
	Span<ascii> message;
};

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
			print(report.message);
			print_source_line(report.location);
		}
	}
	template <class ...Args>
	void error(char const *format_string, Args const &...args) {
		error(Span<utf8>{}, format_string, args...);
	}
	template <class ...Args>
	void error(Span<utf8> location, char const *format_string, Args const &...args) {
		Report r;
		r.location = location;
		r.message = format("Error: %\n\n", format(format_string, args...));
		reports.add(r);
	}
};

//RingQueue<Token, 16384> tokens;
BlockList<Token, 4096> tokens;
Mutex tokens_mutex;
u32 tokens_lexed = 0;
bool lexer_finished;
bool lexer_success;

bool lexer_function(Reporter *reporter) {
	timed_function();

	defer { lexer_finished = true; };

	HashMap<Span<utf8>, TokenKind> keywords;
#define E(name, value) keywords.get_or_insert(u8#name##s) = value;
	ENUMERATE_KEYWORDS(E);
#undef E

	auto current_p = source.begin();
	auto next_p    = source.begin();
	utf32 c;

	auto next_char = [&] {
		current_p = next_p;
		if (current_p >= source.end()) {
			return false;
		}
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
		scoped_lock(tokens_mutex);
		tokens.add(token);
		atomic_increment(&tokens_lexed);
	};

	while (1) {
		token.string.data = current_p;
		token.string.count = 0;

	nc:
		switch (c) {
			case '\0':
				lexer_success = true;
				return true;
			case ' ':
			case '\n':
			case '\r':
			case '\t':
				token.string.data += 1;
				if (!next_char()) {
					lexer_success = true;
					return true;
				}
				goto nc;
			case '&':
			case '|':
			case '^':
			case '!':
			case '~':
			case '+':
			case '-':
			case '*':
			case ':':
			case '(':
			case ')':
			case '{':
			case '}':
			case '>':
			case '<':
			case '.':
			case ',':
			case ';': {
				token.kind = (TokenKind)c;
				token.string.count = 1;
				next_char();
				push_token();
				break;
			}
			case '=': {
				if (next_char() && c == '>') {
					next_char();
					token.kind = '=>';
					token.string.count = 2;
				} else {
					token.kind = '=';
					token.string.count = 1;
				}
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
			case '/': {
				if (next_char()) {
					if (c == '/') {
						while (next_char()) {
							if (c == '\n')
								break;
						}
					} else if (c == '*') {
						u32 deepness = 1;

						if (!next_char()) {
							reporter->error(Span(token.string.data, 2), "Unclosed comment block (end of file)");
							return false;
						}

					continue_search:
						auto comment_begin_or_end = find(Span(current_p, source.end()), {u8"*/"s, u8"/*"s});
						if (!comment_begin_or_end) {
							reporter->error(Span(token.string.data, 2), "Unclosed comment block");
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
					} else {
						token.kind = '/';
						token.string.count = 1;
						push_token();
					}
				} else {
					token.kind = '/';
					token.string.count = 1;
					push_token();
				}
				break;
			}
			default: {
				if (is_alpha(c) || c == '_') {
					while (is_alpha(c) || is_digit(c) || c == '_') {
						if (!next_char()) {
							break;
						}
					}
					token.string.count = current_p - token.string.data;

					auto found_keyword = keywords.find(token.string);
					if (found_keyword) {
						token.kind = *found_keyword;
					} else {
						token.kind = Token_identifier;
					}

					push_token();
				} else if (is_digit(c)) {
					if (c == '0') {
						if (next_char()) {
							if (c == 'x') {
								if (!next_char()) {
									reporter->error(token.string, "Unexpected end when parsing hex number");
									return false;
								}
								while (is_hex_digit(c)) {
									if (!next_char()) {
										break;
									}
								}
								if (current_p - token.string.data <= 2) {
									reporter->error(token.string, "Invalid hex number");
									return false;
								}
								if (current_p - token.string.data > 18 ) {
									reporter->error(token.string, "Hex number too big to store in 64 bits");
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
				} else {
					token.string.count = 1;
					reporter->error(token.string, "Invalid character '%'", c);
					return false;
				}
			}
		}
	}
	lexer_success = true;
	return true;
}

u32 main_return_value = 0;

struct Parser {
	decltype(tokens)::Iterator token = {};
	u32 token_index = 0;
	bool reached_end = false;
	AstLambda *current_lambda = 0;
	Reporter *reporter;
	u32 reporter_checkpoint;

	Parser checkpoint() {
		reporter_checkpoint = reporter->checkpoint();
		return *this;
	}

	void reset(Parser checkpoint) {
		*this = checkpoint;
		reporter->reset(reporter_checkpoint);
	}

	bool next() {
		Spinner spinner;
		while (1) {
			if (token_index < tokens_lexed - 1) {
				break;
			}

			if (lexer_finished) {
				reached_end = true;
				break;
			}
			iteration(spinner);
		}

		if (reached_end)
			return false;

		{
			scoped_lock(tokens_mutex);
			++token;
			++token_index;
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
		if (!expect(expected_kind))  return false;
		return true;
	}
};

AstLiteral *make_integer(u64 value) {
	auto i = new_ast<AstLiteral>();
	i->literal_kind = LiteralKind::integer;
	i->u64 = value;
	return i;
}

UnaryOperation token_to_unary_operation(TokenKind kind) {
	switch (kind) {
		using enum UnaryOperation;
		case '+': return plus;
		case '-': return minus;
		case '*': return star;
		case '&': return _and;
		case '!': return _not;
	}
	invalid_code_path("attempt to convert bad token to unary operation");
}

BinaryOperation token_to_binary_operation(TokenKind kind) {
	switch (kind) {
		using enum BinaryOperation;
		case '+': return add;
		case '-': return subtract;
		case '*': return multiply;
		case '/': return divide;
		case '.': return member_access;
		case '&': return _and;
		case '|': return _or;
		case '^': return _xor;
	}
	invalid_code_path("attempt to convert bad token to binary operation");
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
		case '.':
		case '&':
		case '|':
		case '^':
			return true;
	}
	return false;
}

s32 get_precedence(BinaryOperation op) {
	switch (op) {
		using enum BinaryOperation;

		case member_access: return 100;

		case multiply:
		case divide: return 20;

		case add:
		case subtract: return 10;

		case _and:
		case _or:
		case _xor: return 5;
	}

	invalid_code_path();
	return 0;
}

Span<utf8> unescape_string(Span<utf8> string) {
	string.data  += 1;
	string.count -= 2;

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

		if (c == '"' && prev == '\\') {
			new_string.back() = '"';
		} else {
			new_string.add(c);
		}
	}
	return new_string;
}

AstStatement *parse_statement(Parser &parser);
AstExpression *parse_expression(Parser &parser);
AstDefinition *parse_definition(Parser &parser);

AstExpression *parse_sub_expression(Parser &parser) {
	//if (parser.token->kind & keyword_built_in_type_flag) {
	//	auto identifier_token = parser.token;
	//	parser.next();
	//	auto identifier = new_ast<AstIdentifier>();
	//	identifier->location = identifier->name = identifier_token->string;
	//	identifier->definition = find_built_in_definition_from_token(identifier_token->kind);
	//	assert(identifier->definition);
	//
	//	return identifier;
	//} else
	if (parser.token->kind == Token_string_literal) {
		auto string = new_ast<AstLiteral>();
		string->literal_kind = LiteralKind::string;
		string->location = parser.token->string;
		string->string = unescape_string(parser.token->string);
		if (!string->string.data) {
			parser.reporter->error(parser.token->string, "Bad escape sequence in string literal");
			return 0;
		}
		parser.next();
		return string;
	} else if (parser.token->kind == Token_integer_literal) {
		u64 value = 0;
		if (parser.token->string.count >= 2 && parser.token->string.data[1] == 'x') {
			for (u32 i = parser.token->string.count - 1; i != 1; --i) {
				u8 quart;
				switch (parser.token->string.data[i]) {
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
				value |= quart;
			}
		} else {
			auto parsed = parse_u64(parser.token->string);
			if (!parsed) {
				parser.reporter->error(parser.token->string, "Failed to parse integer.");
				return 0;
			}
			value = parsed.value();
		}
		auto location = parser.token->string;
		parser.next();
		return make_integer(value);
	} else if (parser.token->kind == Token_true || parser.token->kind == Token_false) {
		auto boolean = new_ast<AstLiteral>();
		boolean->literal_kind = LiteralKind::boolean;
		boolean->Bool = parser.token->kind == Token_true;
		boolean->location = parser.token->string;
		parser.next();
		return boolean;
	} else if (parser.token->kind == Token_identifier) {
		auto identifier_token = parser.token;
		parser.next();
		if (parser.token->kind == '(') {
			if (!parser.next_not_end())  return 0;

			List<AstExpression *> arguments;
			if (parser.token->kind != ')') {
				for (;;) {
					auto expression = parse_expression(parser);
					if (!expression)
						return 0;

					arguments.add(expression);

					if (parser.token->kind == ')') {
						break;
					}

					if (!parser.expect(','))
						return 0;

					if (!parser.next_not_end())
						return 0;
				}
			}

			if (!parser.next_not_end())  return 0;

			auto call = new_ast<AstCall>();
			call->location = call->name = identifier_token->string;

			call->arguments = arguments;

			return call;
		} else {
			auto identifier = new_ast<AstIdentifier>();
			identifier->location = identifier->name = identifier_token->string;
			return identifier;
		}
	} else if (parser.token->kind == Token_fn) {
		auto start_token = parser.token;
		if (!parser.next_expect('('))  return 0;
		if (!parser.next_not_end())  return 0;

		auto lambda = new_ast<AstLambda>();

		if (parser.token->kind != ')') {
			for (;;) {
				auto definition = parse_definition(parser);
				if (!definition)
					return 0;

				definition->is_parameter = true;
				definition->parent_lambda = lambda;

				lambda->parameters.add(definition);

				if (parser.token->kind == ')') {
					break;
				}

				if (!parser.expect(','))
					return 0;

				if (!parser.next_not_end())
					return 0;
			}
		}

		if (!parser.next_not_end())  return 0;

		AstExpression *return_type = 0;
		if (parser.token->kind != '{' && parser.token->kind != '=>' && parser.token->kind != ';') {
			return_type = parse_expression(parser);
		}

		bool parse_return_type = true;
		bool is_short = false;
		if (parser.token->kind == '{') {
			parse_return_type = false;
		} else if (parser.token->kind == '=>') {
			parse_return_type = false;
			is_short = true;
		} else if (parser.token->kind == ';') {
			lambda->has_body = false;
		} else {
			parser.reporter->error(parser.token->string, "Expected '{' or '=>' or ';' or return type instead of '%'", parser.token->string);
			return 0;
		}

		auto opening_token = parser.token;

		if (!parser.next_not_end())
			return 0;

		lambda->name = format(u8"unnamed%", lambda->uid);

		auto previous_lambda = parser.current_lambda;
		parser.current_lambda = lambda;
		defer { parser.current_lambda = previous_lambda; };

		if (lambda->has_body) {
			if (is_short) {

				auto expression = parse_expression(parser);
				if (!expression)
					return 0;

				if (!parser.expect(';'))
					return 0;

				auto ret = new_ast<AstReturn>();
				ret->expression = expression;
				ret->location = opening_token->string;
				lambda->statements.add(ret);
			} else {
				while (parser.token->kind != '}') {
					auto statement = parse_statement(parser);
					if (!statement) {
						return 0;
					}
					lambda->statements.add(statement);
				}
			}
			parser.next();
		}

		lambda->location = start_token->string;
		lambda->return_type = return_type;

		return lambda;
	} else if (parser.token->kind == '(') {
		if (!parser.next()) {
			parser.reporter->error(parser.token->string, "Unexpected end of file. Unclosed ')'");
			return 0;
		}

		auto expression = parse_expression(parser);
		if (!expression) {
			return 0;
		}

		if (!parser.expect(')')) {
			return 0;
		}
		if (!parser.next()) {
			parser.reporter->error("Unexpected end of file while parsing parenthesized expression");
			return 0;
		}
		return expression;
	} else if (is_unary_operator(parser.token->kind)) {
		auto unop = new_ast<AstUnaryOperator>();
		unop->location = parser.token->string;
		unop->operation = token_to_unary_operation(parser.token->kind);
		if (!parser.next_not_end())
			return 0;

		unop->expression = parse_sub_expression(parser);
		if (!unop->expression)
			return 0;

		return unop;
	} else {
		parser.reporter->error(parser.token->string, "Failed to parse expression.");
		return 0;
	}
	invalid_code_path();
}

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

			if (binop->operation == BinaryOperation::member_access) {

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

					u64 value;

					switch (binop->operation) {
						using enum BinaryOperation;
						case add:      value = left + right; break;
						case subtract: value = left - right; break;
						case multiply: value = left * right; break;
						case divide:   value = left / right; break;
						case _and:     value = left & right; break;
						case _or:      value = left | right; break;
						case _xor:     value = left ^ right; break;
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

AstExpression *parse_expression(Parser &parser) {
	auto sub = parse_sub_expression(parser);

	if (parser.reached_end)
		return sub;

	AstBinaryOperator *top_binop = 0;
	AstBinaryOperator *previous_binop = 0;
	s32 previous_precedence = 0;
	while (is_binary_operator(parser.token->kind)) {
		auto binop = new_ast<AstBinaryOperator>();
		binop->left = sub;

		binop->operation = token_to_binary_operation(parser.token->kind);

		binop->location = parser.token->string;

		auto precedence = get_precedence(binop->operation);

		if (!sub) {
			return 0;
		}

		if (!parser.next()) {
			parser.reporter->error("Unexpected end of file after binary operator");
			return 0;
		}

		binop->right = parse_sub_expression(parser);
		if (!binop->right) {
			return 0;
		}

		if (binop->operation == BinaryOperation::member_access) {
			if (binop->right->kind != Ast_identifier && binop->right->kind != Ast_call) {
				parser.reporter->error(binop->right->location, "This expression can not follow a dot. Only identifiers are allowed here.");
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
		return simplify(top_binop);
	}

	return sub;
}

AstDefinition *parse_definition(Parser &parser) {
	if (parser.token->kind == Token_identifier) {
		auto name_token = parser.token;
		if (!parser.next_expect(':'))  return 0;
		if (!parser.next_not_end())  return 0;

		AstExpression *type = 0;
		if (parser.token->kind != ':' && parser.token->kind != '=' ) {
			type = parse_expression(parser);
			if (!type)  return 0;
		}

		bool is_constant = false;
		bool has_expression = false;
		switch (parser.token->kind) {
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

		definition->location = definition->name = name_token->string;
		definition->type = type;
		definition->parent_lambda = parser.current_lambda;
		definition->is_constant = is_constant;

		if (has_expression) {
			if (!parser.next_not_end())  return 0;

			auto expression = parse_expression(parser);
			if (!expression)  return 0;

			definition->expression = expression;
			if (expression->kind == Ast_lambda) {
				auto lambda = (AstLambda *)expression;
				lambda->name.set(definition->name);
			}
		}

		return definition;
	}
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

AstStatement *parse_statement(Parser &parser) {
	auto try_parse_non_expression = [&]() -> AstStatement * {
		if (parser.token->kind == Token_identifier) {
			auto checkpoint = parser.checkpoint();

			parser.next();

			if (parser.token->kind == '(') {
				parser.reset(checkpoint);
				auto expression = parse_expression(parser);
				if (!expression) {
					return 0;
				}
				return make_statement(expression);
			} else {
				parser.reset(checkpoint);

				auto definition = parse_definition(parser);

				if (!definition) {
					return 0;
				}

				if (!definition->expression || needs_semicolon(definition->expression)) {
					if (!parser.expect(';'))  return 0;
					parser.next();
				}

				return definition;
			}
		} else if (parser.token->kind == Token_return) {

			auto return_token = parser.token;

			if (!parser.next_not_end()) return 0;

			auto expression = parse_expression(parser);
			if (!expression)  return 0;

			if (!parser.expect(';'))  return 0;
			parser.next();

			auto ret = new_ast<AstReturn>();
			ret->location = return_token->string;
			ret->expression = expression;
			return ret;
		} else if (parser.token->kind == Token_if) {
			auto If = new_ast<AstIf>();
			If->location = parser.token->string;
			if (!parser.next_not_end())
				return 0;
			auto condition = parse_expression(parser);
			if (!condition) {
				return 0;
			}
			If->condition = condition;

			if (!parser.expect('{'))
				return 0;

			parser.next();
			while (parser.token->kind != '}') {
				auto statement = parse_statement(parser);
				if (!statement)
					return 0;

				If->true_statements.add(statement);
			}
			parser.next();
			if (parser.token->kind == Token_else) {
				if (!parser.next_expect('{'))
					return 0;
				parser.next();

				while (parser.token->kind != '}') {
					auto statement = parse_statement(parser);
					if (!statement)
						return 0;

					If->false_statements.add(statement);
				}
				parser.next();
			}
			return If;
		}
		return (AstStatement *)1;
	};

	auto checkpoint = parser.checkpoint();

	auto expression = parse_expression(parser);
	if (expression) {
		if (parser.token->kind == ';') {
			if (!is_statement(expression)) {
				parser.reporter->error(expression->location, "This expression is not a statement.");
				return 0;
			}
			parser.next();
			return make_statement(expression);
		} else {
			// Maybe free `expression` ?
		}
	}

	parser.reset(checkpoint);

	auto statement = try_parse_non_expression();
	if (statement == 0)
		return 0;

	if (statement != (AstStatement *)1)
		return statement;


	parser.reporter->error(parser.token->string, "Failed to parse statement. Unexpected token '%'", parser.token->string);
	return 0;
}

bool parser_success;
bool parser_function(Reporter *reporter) {
	timed_function();

	Parser parser = {};
	parser.reporter = reporter;
	loop_while([]{return tokens_lexed == 0;});
	parser.token = tokens.begin();
	while (!parser.reached_end) {
		auto statement = parse_statement(parser);
		if (!statement) {
			return false;
		}
		if (!can_be_global(statement)) {
			parser.reporter->error(statement->location, "This statement can not be global.");
			return false;
		}
		auto found = global_statements.find(statement->location);
		if (found) {
			parser.reporter->error(statement->location, "Redefinition of '%'", statement->location);
			parser.reporter->error((*found)->location, "Previous declaration is here");
			return false;
		}
		global_statements.get_or_insert(statement->location) = statement;
	}
	parser_success = true;
	return true;
}

HashMap<Span<utf8>, AstStatement *> typechecked_globals;
RecursiveMutex typechecked_globals_mutex;

umm append(StringBuilder &b, AstDefinition *definition) {
	if (definition == nullptr)  return append(b, "(null)");
	return append(b, definition->name);
}

struct TypecheckState {
	AstStatement *statement = 0;
	AstLambda *lambda = 0;
	AstDefinition *definition = 0;
	AstNode *waiting_for = 0;

	AstLambda *current_lambda = 0;

	Reporter reporter;
};

thread_local TypecheckState *typecheck_state;

template <class Result>
struct Waiter {
	Result volatile result = {};
};

HashMap<Span<utf8>, LinkedList<Waiter<AstDefinition *>>> definitions_waiters;
Mutex definitions_waiters_mutex;

u32 typecheck_thread_count_waiting_for_definition = 0;
u32 typecheck_thread_count_finished = 0;

AstDefinition *wait_for_definition(AstNode *node, Span<utf8> name) {
	if (typecheck_state->definition->name == name)
		return typecheck_state->definition;

	if (typecheck_state->current_lambda) {
		auto found_local = typecheck_state->current_lambda->local_definitions.find(name);
		if (found_local)
			return *found_local;

		auto found_param = find_if(typecheck_state->current_lambda->parameters, [&](AstDefinition *d) { return d->name == name; });
		if (found_param)
			return *found_param;
	}

	lock(typechecked_globals_mutex);
	auto found = typechecked_globals.find(name);
	if (found) {
		auto statement = *found;
		if (statement->kind == Ast_definition) {
			unlock(typechecked_globals_mutex);
			return (AstDefinition *)statement;
		}
		// Is this possible?
		invalid_code_path();
	} else {

		// YIELD
#if 1
		lock(definitions_waiters_mutex);
		auto &waiters = definitions_waiters.get_or_insert(name);
		auto &waiter = waiters.add();
		unlock(definitions_waiters_mutex);

		unlock(typechecked_globals_mutex);

		typecheck_state->waiting_for = node;
		atomic_increment(&typecheck_thread_count_waiting_for_definition);
		loop_while([&]{ return waiter.result == 0; });
		atomic_decrement(&typecheck_thread_count_waiting_for_definition);
		typecheck_state->waiting_for = 0;

		auto result = waiter.result;

		lock(definitions_waiters_mutex);
		erase(waiters, &waiter);
		unlock(definitions_waiters_mutex);
#endif
		return result;
	}
}

struct IntegerInfo {
	AstStruct *type;
	s64 min_value;
	s64 max_value;
	u64 mask;
};

constexpr IntegerInfo integer_infos[] {
	{&type_u8,  0llu, 0xffllu,               0xffllu},
	{&type_u16, 0llu, 0xffffllu,             0xffffllu},
	{&type_u32, 0llu, 0xffffffffllu,         0xffffffffllu},
	{&type_u64, 0llu, 0xffffffffffffffffllu, 0xffffffffffffffffllu},
	{&type_s8,  (s8 )0x80llu,               0x7fllu,               0xffllu},
	{&type_s16, (s16)0x8000llu,             0x7fffllu,             0xffffllu},
	{&type_s32, (s32)0x80000000llu,         0x7fffffffllu,         0xffffffffllu},
	{&type_s64, (s64)0x8000000000000000llu, 0x7fffffffffffffffllu, 0xffffffffffffffffllu},
};

bool harden_type(AstExpression **expression_pointer, AstExpression *target_type = 0) {
	auto expression = *expression_pointer;
	defer { *expression_pointer = expression; };
	switch (expression->kind) {
		case Ast_literal: {
			auto literal = (AstLiteral *)expression;
			switch (literal->literal_kind) {
				using enum LiteralKind;
				case integer: {
					if (target_type) {
						if (expression->type == &type_unsized_integer) {

							auto got_value = get_constant_integer(expression);
							if (got_value) {
								auto value = got_value.value();

								auto found_info = find_if(integer_infos, [&](IntegerInfo const &i) { return types_match(target_type, i.type); });

								if (found_info) {
									auto info = *found_info;

									// TODO: FIXME:
									// right now i'm assuming that any value will fit into u64 or s64
									// i think that literal simplification should be done in arbitrarily large integers
									if ((info.type != &type_s64 && info.type != &type_u64) && (value < info.min_value || value > info.max_value)) {
										typecheck_state->reporter.error(expression->location, "Computed value (%) does not fit into destination type % [%; %]. You can explicitly bitwise-and this expression with 0x% to discard higher bits", value, type_to_string(target_type), info.min_value, info.max_value, FormatInt{.value=info.mask,.radix=16});
										return false;
									}

									expression = make_integer(value);
									expression->type = target_type;
								}

							} else {
								invalid_code_path();
							}
						} else {
							invalid_code_path();
						}
					} else {
						invalid_code_path();
					}
					break;
				}
				case boolean: {
					return true;
				}
				case string: {
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

			// TODO: this is probably wrong.
			// we should evaluate first, then harden
			if (!harden_type(&unop->expression, target_type)) {
				return false;
			}
			unop->type = unop->expression->type;
			break;
		}
		case Ast_call: {
			break;
		}
		case Ast_binary_operator: {
			break;
		}
		default: {
			invalid_code_path("unhandled case in harden_type");
		}
	}
	return true;
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

bool do_all_paths_return(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_return:
			return true;
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
	for (auto statement : lambda->statements) {
		if (do_all_paths_return(statement)) {
			return true;
		}
	}
	return false;
}

AstExpression *make_pointer_type(AstExpression *type) {
	auto unop = new_ast<AstUnaryOperator>();
	unop->expression = type;
	unop->type = &type_type;
	unop->operation = UnaryOperation::star;
	return unop;
}

bool typecheck(AstExpression *expression);

bool convertible(AstExpression *expression, AstExpression *type) {
	auto Struct = get_struct(type);
	if (Struct) {
		if (Struct == &type_string) {
			return types_match(expression->type, &type_string);
		}
	}
	return false;
}

bool typecheck(AstStatement *statement) {
	switch (statement->kind) {
		case Ast_return: {
			auto ret = (AstReturn *)statement;
			if (typecheck_state->current_lambda) {
				auto expression = ret->expression;
				if (!typecheck(expression))
					return false;
				assert(expression->type);
				typecheck_state->current_lambda->return_statements.add(ret);
			} else {
				invalid_code_path("got return statement in global scope when typechecking.");
			}
			break;
		}
		case Ast_definition: {
			auto definition = (AstDefinition *)statement;

			if (definition->is_parameter) {
				assert(definition->type);
				if (!typecheck(definition->type)) {
					return false;
				}
			} else if (typecheck_state->current_lambda) {

				//
				// Lambda local definition
				//

				auto lambda = typecheck_state->current_lambda;
				auto where = lambda;

				while (where) {
					auto found_definition = where->local_definitions.find(definition->name);
					if (found_definition) {
						typecheck_state->reporter.error(definition->location, "Redeclaration of '%'", definition->location);
						typecheck_state->reporter.error((*found_definition)->location, "previous is here");
						return false;
					}
					where = where->parent_lambda;
				}

				lambda->local_definitions.get_or_insert(definition->name) = definition;
				if (definition->expression) {
					if (!typecheck(definition->expression)) {
						return false;
					}
				}

				if (definition->type) {
					typecheck(definition->type);

					if (!convertible(definition->expression, definition->type)) {
						typecheck_state->reporter.error(definition->expression->location, "Expression is not implicitly convertible to '%'", type_to_string(definition->type));
						return false;
					}
				} else {
					definition->type = definition->expression->type;
				}

				if (definition->expression) {
					if (!harden_type(&definition->expression, definition->type)) {
						return false;
					}
				}

				if (definition->is_constant) {
					if (!is_constant(definition->expression)) {
						typecheck_state->reporter.error(definition->location, "Definition marked as constant, but assigned expression is not constant");
						return false;
					}
				}
			} else {

				//
				// Global definition
				//

				if (definition->built_in) {
					break;
				}

				typecheck_state->definition = definition;

				if (definition->expression->kind == Ast_lambda) {
					typecheck_state->lambda = (AstLambda *)definition->expression;
				}

				if (!typecheck(definition->expression))
					return false;

				if (definition->type) {
					typecheck(definition->type);

					//if (!convertible(definition->expression, definition->type)) {
					//	typecheck_state->reporter.error(definition->expression->location, "Expression has type '%', which is not implicitly convertible to '%'", definition->expression->type);
					//	return false;
					//}
				} else {
					definition->type = definition->expression->type;
				}

				switch (definition->expression->kind) {
					case Ast_lambda: {
						definition->type = definition->expression;
						break;
					}
					default: {
						if (!harden_type(&definition->expression, definition->type)) {
							return false;
						}
						break;
					}
				}

				definition->expression = simplify(definition->expression);

				assert(definition->type);

				{
					scoped_lock(typechecked_globals_mutex);
					auto found_definition = typechecked_globals.find(definition->name);
					if (found_definition) {
						typecheck_state->reporter.error(definition->location, "Redefinition of '%'", definition->location);
						typecheck_state->reporter.error((*found_definition)->location, "prev is here");
						return false;
					}
					typechecked_globals.get_or_insert(definition->name) = definition;
				}

				lock(definitions_waiters_mutex);
				auto found_waiters = definitions_waiters.find(definition->name);
				unlock(definitions_waiters_mutex);

				if (found_waiters) {
					for_each(*found_waiters, [&](Waiter<AstDefinition *> &waiter) {
						atomic_set(&waiter.result, definition);
					});
				}

				lock(typechecked_globals_mutex);
				typechecked_globals.get_or_insert(definition->name) = definition;
				unlock(typechecked_globals_mutex);
			}

			break;
		}
		case Ast_if: {
			auto If = (AstIf *)statement;

			if (!typecheck(If->condition))
				return false;

			if (!types_match(If->condition->type, &type_bool)) {
				typecheck_state->reporter.error(If->condition->location, "Expression with type % can not be used as a condition in if statement. Only expressions with type bool are allowed here.", type_to_string(If->condition->type));
				return false;
			}

			for (auto statement : If->true_statements) {
				if (!typecheck(statement)) {
					return false;
				}
			}

			for (auto statement : If->false_statements) {
				if (!typecheck(statement)) {
					return false;
				}
			}

			break;
		}
		case Ast_expression_statement: {
			auto es = (AstExpressionStatement *)statement;
			if (!typecheck(es->expression)) {
				return false;
			}
			break;
		}
		default: {
			invalid_code_path("invalid statement kind in typecheck");
		}
	}
	return true;
}

bool typecheck(AstExpression *expression) {
	assert(expression);

	if (expression->type)
		return true;

	switch (expression->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;

			if (identifier->definition)
				break;

			auto definition = wait_for_definition(identifier, identifier->name);
			assert(definition->type);
			identifier->definition = definition;
			identifier->type = definition->type;
			break;
		}
		case Ast_call: {
			auto call = (AstCall *)expression;
			auto definition = wait_for_definition(call, call->name);
			call->definition = definition;
			if (definition->expression->kind != Ast_lambda) {
				typecheck_state->reporter.error(call->location, "Expression in not callable");
				return false;
			}
			auto lambda = (AstLambda *)definition->expression;
			call->lambda = lambda;

			if (call->arguments.count != lambda->parameters.count) {
				typecheck_state->reporter.error(call->location, "Argument count does not match");
				return false;
			}

			for (u32 i = 0; i < call->arguments.count; ++i) {
				auto &argument = call->arguments[i];
				auto &parameter = lambda->parameters[i];

				if (!typecheck(argument)) {
					return false;
				}
				if (!harden_type(&argument, parameter->type)) {
					return false;
				}
			}

			if (lambda->return_type) {
				call->type = lambda->return_type;
			} else {
				call->type = &type_void;
			}
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
				default:
					invalid_code_path("not implemented");
			}
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)expression;

			lambda->parent_lambda = typecheck_state->current_lambda;

			auto old_current_lamdda = typecheck_state->current_lambda;
			defer { assert(typecheck_state->current_lambda == old_current_lamdda); };

			typecheck_state->current_lambda = lambda;
			assert(typecheck_state->current_lambda);
			defer { typecheck_state->current_lambda = old_current_lamdda; };

			if (lambda->return_type) {
				typecheck(lambda->return_type);
			}

			for (auto parameter : lambda->parameters) {
				if (!typecheck(parameter)) {
					return false;
				}
			}

			if (lambda->has_body) {
				for (auto statement : lambda->statements) {
					if (!typecheck(statement)) {
						return false;
					}
				}

				if (lambda->return_type) {
					for (auto ret : lambda->return_statements) {
						if (!harden_type(&ret->expression, lambda->return_type)) {
							return false;
						}
					}
				} else {
					if (lambda->return_statements.count == 0) {
						lambda->return_type = &type_void;
					} else if (lambda->return_statements.count == 1) {
						lambda->return_type = lambda->return_statements[0]->expression->type;
					} else {
						if (!lambda->return_type) {
							typecheck_state->reporter.error(lambda->location, "Deducing return type from multiple statements is not implemented yet. You have to explicitly specify return type");
							return false;
						}
					}
				}

				assert(lambda->return_type);

				if (!do_all_paths_return(lambda)) {
					typecheck_state->reporter.error(lambda->location, "Not all paths return a value");
					return false;
				}
			} else {
				if (!lambda->return_type) {
					lambda->return_type = &type_void;
				}
			}

			lambda->type = lambda;

			break;
		}
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)expression;
			if (!typecheck(bin->left))
				return false;


			if (bin->operation == BinaryOperation::member_access) {
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
							typecheck_state->reporter.error(bin->right->location, "Type '%' does not contain constant '%'", Struct->name, bin->right->location);
						} else {
							typecheck_state->reporter.error(bin->right->location, "'%' is not a member of '%'", bin->right->location, Struct->name);
						}
						return false;
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
					typecheck_state->reporter.error(bin->left->location, "Dot operator can not be applied to an expression of type '%'", type_to_string(bin->left->type));
					return false;
				}

			} else {
				if (!typecheck(bin->right))
					return false;
			}

			bin->type = bin->left->type;
			break;
		}
		case Ast_unary_operator: {
			auto unop = (AstUnaryOperator *)expression;

			if (!typecheck(unop->expression)) {
				return false;
			}

			if (is_type(unop->expression)) {
				if (unop->operation != UnaryOperation::star) {
					typecheck_state->reporter.error(unop->location, "Unary operator '%' can not be applied to a type expression", unop->location);
					return false;
				}
				unop->type = &type_type;
			} else {
				switch (unop->operation) {
					case UnaryOperation::minus: {
						if (types_match(unop->expression->type, &type_u8) ||
							types_match(unop->expression->type, &type_u16) ||
							types_match(unop->expression->type, &type_u32) ||
							types_match(unop->expression->type, &type_u64) ||
							types_match(unop->expression->type, &type_s8) ||
							types_match(unop->expression->type, &type_s16) ||
							types_match(unop->expression->type, &type_s32) ||
							types_match(unop->expression->type, &type_s64) ||
							types_match(unop->expression->type, &type_unsized_integer))
						{
							unop->type = unop->expression->type;
						} else {
							typecheck_state->reporter.error(unop->location, "Unary minus can not be applied to expression of type '%'", type_to_string(unop->expression->type));
							return false;
						}
						break;
					}
					case UnaryOperation::_and: {
						// TODO: make sure we can take address of expression
						unop->type = make_pointer_type(unop->expression->type);
						break;
					}
					default: {
						invalid_code_path();
						return false;
					}
				}
			}


			break;
		}
		default: {
			typecheck_state->reporter.error(expression->location, "Internal error: typecheck(AstExpression *): unhandled case '%'", expression->kind);
			return false;
		}
	}
	assert(expression->type);
	return true;
}

List<TypecheckState *> typechecks_failed;
Mutex typechecks_failed_mutex;
bool typecheck_global(TypecheckState *state) {
	timed_function();

	typecheck_state = state;
	current_printer = console_printer;
	assert(typecheck_state->current_lambda == 0); // WTF!!???
	if (!typecheck(typecheck_state->statement)) {
		scoped_lock(typechecks_failed_mutex);
		typechecks_failed.add(state);
		return false;
	}
	atomic_increment(&typecheck_thread_count_finished);
	return true;
}

u32 tab_count = 0;

void print_tabs() {
	for (u32 i = tab_count; i--;) {
		print("  ");
	}
}

void print_ast(AstBinaryOperator *node);
void print_ast(AstDefinition *node);
void print_ast(AstLambda *node);
void print_ast(AstIdentifier *node);
void print_ast(AstLiteral *node);
void print_ast(AstReturn *node);
void print_ast(AstCall *node);
void print_ast(AstStruct *node);
void print_ast(AstIf *node);
void print_ast(AstExpressionStatement *node);
void print_ast(AstUnaryOperator *node);
void print_ast(AstNode *node) {
	switch (node->kind) {
		case Ast_definition: return print_ast((AstDefinition *)node);
		case Ast_lambda:     return print_ast((AstLambda     *)node);
		case Ast_identifier: return print_ast((AstIdentifier *)node);
		case Ast_literal:    return print_ast((AstLiteral    *)node);
		case Ast_return:     return print_ast((AstReturn     *)node);
		case Ast_call:       return print_ast((AstCall       *)node);
		case Ast_if:         return print_ast((AstIf         *)node);
		case Ast_expression_statement: return print_ast((AstExpressionStatement *)node);
		case Ast_binary_operator: return print_ast((AstBinaryOperator *)node);
		case Ast_struct: return print_ast((AstStruct *)node);
		case Ast_unary_operator: return print_ast((AstUnaryOperator *)node);
		default:
			print_tabs();
			print("unknown - uid: %\n", node->uid);
			break;
	}
}
void print_ast(AstDefinition *node) {
	if (node->built_in)
		return;

	print_tabs();
	print("definition - name: %, type: %, uid: %\n", node->name, type_to_string(node->type), node->uid);
	if (node->expression) {
		tab_count += 1;
		print_ast(node->expression);
		tab_count -= 1;
	}
}
void print_ast(AstLambda *node) {
	print_tabs();
	print("lambda - return_type: %, uid: %\n", type_to_string(node->return_type), node->uid);
	tab_count += 1;
	for (auto statement : node->statements) {
		print_ast(statement);
	}
	tab_count -= 1;
}
void print_ast(AstIdentifier *node) {
	print_tabs();
	print("identifier - name: %, type: %, uid: %, definition.uid: %\n", node->name, type_to_string(node->type), node->uid, node->definition ? node->definition->uid : -1);
}
void print_ast(AstCall *node) {
	print_tabs();
	print("call - name: %, type: %, uid: %, definition.uid: %\n", node->name, type_to_string(node->type), node->uid, node->definition ? node->definition->uid : -1);
	tab_count += 1;
	print_tabs();
	print("arguments:\n");
	tab_count += 1;
	for (auto argument : node->arguments) {
		print_ast(argument);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstBinaryOperator *node) {
	print_tabs();
	print("binary - operation: %, type: %, uid: %\n", node->operation, type_to_string(node->type), node->uid);
	tab_count += 1;

	print_tabs();
	print("left:\n");
	tab_count += 1;
	print_ast(node->left);
	tab_count -= 1;

	print_tabs();
	print("right:\n");
	tab_count += 1;
	print_ast(node->right);
	tab_count -= 1;

	tab_count -= 1;
}
void print_ast(AstUnaryOperator *node) {
	print_tabs();
	print("unary - operation: %, type: %, uid: %\n", node->operation, type_to_string(node->type), node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstLiteral *node) {
	print_tabs();
	     if (types_match(node->type, &type_u8  )) print("u8  literal - value: %, uid: %\n", node->u8 , node->uid);
	else if (types_match(node->type, &type_u16 )) print("u16 literal - value: %, uid: %\n", node->u16, node->uid);
	else if (types_match(node->type, &type_u32 )) print("u32 literal - value: %, uid: %\n", node->u32, node->uid);
	else if (types_match(node->type, &type_u64 )) print("u64 literal - value: %, uid: %\n", node->u64, node->uid);
	else if (types_match(node->type, &type_s8  )) print("s8  literal - value: %, uid: %\n", node->s8 , node->uid);
	else if (types_match(node->type, &type_s16 )) print("s16 literal - value: %, uid: %\n", node->s16, node->uid);
	else if (types_match(node->type, &type_s32 )) print("s32 literal - value: %, uid: %\n", node->s32, node->uid);
	else if (types_match(node->type, &type_s64 )) print("s64 literal - value: %, uid: %\n", node->s64, node->uid);
	else if (types_match(node->type, &type_unsized_integer)) print("unsized integer literal - value: %, uid: %\n", node->u64, node->uid);
	else if (types_match(node->type, &type_bool)) print("bool literal - value: %, uid: %\n", node->Bool, node->uid);
	else if (types_match(node->type, &type_string)) print("string literal - value: \"%\", uid: %\n", node->string, node->uid);
	else invalid_code_path();
}
void print_ast(AstReturn *node) {
	print_tabs();
	print("return - uid: %\n", node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstStruct *node) {
	print_tabs();
	print("struct - name: %, uid: %\n", node->name, node->uid);
	tab_count += 1;
	for (auto member : node->members) {
		print_ast(member);
	}
	tab_count -= 1;
}
void print_ast(AstIf *node) {
	print_tabs(); print("if - uid: %\n", node->uid);
	tab_count += 1;
	print_tabs(); print("condition:\n");
	tab_count += 1;
	print_ast(node->condition);
	tab_count -= 1;
	print_tabs(); print("true statements:\n");
	tab_count += 1;
	for (auto statement : node->true_statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	print_tabs(); print("false statements:\n");
	tab_count += 1;
	for (auto statement : node->false_statements) {
		print_ast(statement);
	}
	tab_count -= 1;
	tab_count -= 1;
}
void print_ast(AstExpressionStatement *node) {
	print_ast(node->expression);
}

void add_member(AstStruct &destination, AstExpression *type, Span<utf8> name, AstLiteral *value, bool constant) {
	auto d = new_ast<AstDefinition>();
	d->location = name;
	d->name = name;
	d->expression = value;
	d->is_constant = constant;
	d->type = type;
	if (value) {
		value->type = type;
	}

	(constant ? destination.constants : destination.members).add(d);
}

ThreadPool thread_pool;

s32 tl_main(Span<Span<utf8>> arguments) {
	Profiler::init();
	defer { Profiler::deinit(); };

	defer { write_entire_file("profile.tmd"s, Profiler::output_for_timed()); };

	{
		StringBuilder test;
		for (int i = 0; i < 1024*16; ++i) {
			append_format(test, "_% :: fn (a: s32, b: s32) s32 { return a + b; } /* this is a /* nested */ comment */\n", i);
		}
		write_entire_file("test.tl"s, as_bytes(to_string(test)));
	}

	timed_function();

	executable_path = arguments[0];
	auto parsed = parse_path(executable_path);
	executable_name = parsed.name;
	executable_directory = parsed.directory;

	if (arguments.count == 1) {
		print_help();
		return 1;
	}

	enum class Output {
		none,
		c,
		nasm,
	} output = {};

	bool do_print_ast = false;
	for (int i = 0; i < arguments.count; ++i) {
		if (arguments[i] == u8"--print-ast"s) {
			do_print_ast = true;
		} else if (arguments[i] == u8"--output"s) {
			++i;
			if (i >= arguments.count) {
				print("Expected an argument after --output.\n");
				return 1;
			}
			using enum Output;
				 if (arguments[i] == u8"none"s) output = none;
			else if (arguments[i] == u8"c"s   ) output = c;
			else if (arguments[i] == u8"nasm"s) output = nasm;
			else {
				print(Print_error, "Unknown output type '%'\n", arguments[i]);
			}
		} else {
			source_path = arguments[i];
		}
	}

	if (!source_path.data) {
		print("No source path received. Exiting.\n");
		return 1;
	}
	if (!is_absolute_path(source_path)) {
		source_path = make_absolute_path(source_path);
	}
	source_path_without_extension = parse_path(source_path).path_without_extension();

	source_buffer = read_entire_file(to_pathchars(source_path), {.extra_space_after=1});
	if (!source_buffer.data) {
		print("No failed to read '%'. Exiting.\n", source_path);
		return 1;
	}
	source_buffer.back() = '\0';
	source = as_utf8(source_buffer);

	construct(tokens);
	construct(global_statements);
	construct(definitions_waiters);
	construct(typechecked_globals);

	//{
	//	timed_block("init_thread_pool"s);
	//	construct(thread_pool);
	//	init_thread_pool(thread_pool, get_cpu_info().logical_processor_count);
	//}
	//
	//auto work_queue = make_work_queue(thread_pool);

	auto init_type = [&](AstStruct &s, Span<utf8> name, u32 size) {
		s.members.allocator = default_allocator;
		s.constants.allocator = default_allocator;
		s.name = name;
		s.size = size;
		s.type = &type_type;

		auto definition = new_ast<AstDefinition>();
		definition->is_constant = true;
		definition->expression = &s;
		definition->location = definition->name = name;
		definition->type = &type_type;

		definition->built_in = true;

		global_statements.get_or_insert(name) = definition;
		typechecked_globals.get_or_insert(name) = definition;
	};

	init_type(type_void,   u8"void"s, 0);
	init_type(type_type,   u8"Type"s, 0);
	init_type(type_bool,   u8"bool"s, 1);
	init_type(type_u8,     u8"u8"s,   1);
	init_type(type_u16,    u8"u16"s,  2);
	init_type(type_u32,    u8"u32"s,  4);
	init_type(type_u64,    u8"u64"s,  8);
	init_type(type_s8,     u8"s8"s,   1);
	init_type(type_s16,    u8"s16"s,  2);
	init_type(type_s32,    u8"s32"s,  4);
	init_type(type_s64,    u8"s64"s,  8);
	init_type(type_string, u8"string"s, 16);
	init_type(type_unsized_integer,  u8"(unsized_integer)"s,  0);

	type_default_integer = &type_s64;

	add_member(type_u8,  &type_u8,  u8"min"s, make_integer(0), true);
	add_member(type_u16, &type_u16, u8"min"s, make_integer(0), true);
	add_member(type_u32, &type_u32, u8"min"s, make_integer(0), true);
	add_member(type_u64, &type_u64, u8"min"s, make_integer(0), true);
	add_member(type_u8,  &type_u8,  u8"max"s, make_integer(0xff), true);
	add_member(type_u16, &type_u16, u8"max"s, make_integer(0xffff), true);
	add_member(type_u32, &type_u32, u8"max"s, make_integer(0xffffffff), true);
	add_member(type_u64, &type_u64, u8"max"s, make_integer(0xffffffffffffffff), true);
	add_member(type_s8,  &type_s8,  u8"min"s, make_integer(0x80), true);
	add_member(type_s16, &type_s16, u8"min"s, make_integer(0x8000), true);
	add_member(type_s32, &type_s32, u8"min"s, make_integer(0x80000000), true);
	add_member(type_s64, &type_s64, u8"min"s, make_integer(0x8000000000000000), true);
	add_member(type_s8,  &type_s8,  u8"max"s, make_integer(0x7f), true);
	add_member(type_s16, &type_s16, u8"max"s, make_integer(0x7fff), true);
	add_member(type_s32, &type_s32, u8"max"s, make_integer(0x7fffffff), true);
	add_member(type_s64, &type_s64, u8"max"s, make_integer(0x7fffffffffffffff), true);
	add_member(type_string, make_pointer_type(&type_void), u8"data"s, 0, false);
	add_member(type_string, &type_u64, u8"count"s, 0, false);

#if 0
	Reporter lexer_reporter;
	Reporter parser_reporter;

	current_printer = console_printer;

	work_queue.push(lexer_function, &lexer_reporter);
	work_queue.push(parser_function, &parser_reporter);
	work_queue.wait_for_completion();

	if (!lexer_success) {
		lexer_reporter.print_all();
		print("Lexer failed. Exiting.\n");
		return 1;
	}

	if (!parser_success) {
		parser_reporter.print_all();
		print("Parser failed. Exiting.\n");
		return 1;
	}
#elif 1
	Reporter lexer_reporter;
	Reporter parser_reporter;

	current_printer = console_printer;

	auto lexer_thread = create_thread(lexer_function, &lexer_reporter);
	auto parser_thread = create_thread(parser_function, &parser_reporter);

	if (!join(lexer_thread)) {
		lexer_reporter.print_all();
		print("Lexer failed. Exiting.\n");
		return 1;
	}

	if (!join(parser_thread)) {
		parser_reporter.print_all();
		print("Parser failed. Exiting.\n");
		return 1;
	}
#else
	Reporter lexer_reporter;
	Reporter parser_reporter;

	current_printer = console_printer;

	if (!lexer_function(&lexer_reporter)) {
		lexer_reporter.print_all();
		print("Lexer failed. Exiting.\n");
		return 1;
	}

	if (!parser_function(&parser_reporter)) {
		parser_reporter.print_all();
		print("Parser failed. Exiting.\n");
		return 1;
	}
#endif

	typechecks_failed.allocator = default_allocator;

		// TODO FIXME
		// TODO FIXME
		// TODO FIXME
		// This is extremely bad
	struct ThreadContext {
		ThreadRet<bool> thread;
		TypecheckState *typecheck_state;
	};

	List<ThreadContext> typer_threads;
	for_each(global_statements, [&](Span<utf8> key, AstStatement *statement) {
		if (statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in)
			return;
		timed_block("create_thread"s);
		auto typecheck_state = default_allocator.allocate<TypecheckState>();
		assert(typecheck_state->current_lambda == 0);
		typecheck_state->statement = statement;
		//typer_threads.add({create_thread(typecheck_global, typecheck_state), typecheck_state});

		CreateThread(0, 0, [](void *param) noexcept -> DWORD {
			typecheck_global((TypecheckState *)param);
		}, typecheck_state, 0, 0);
	});

	while (1) {
		spin_iteration();

		if (typechecks_failed.count) {
			scoped_lock(typechecks_failed_mutex);

			for (auto state : typechecks_failed) {
				state->reporter.print_all();
			}

			print("Typecheck has failed. Exiting.\n");
			return 1;
		}



		// TODO FIXME
		// TODO FIXME
		// TODO FIXME
		// This is extremely bad
		bool should_wait = true;
	recheck:
		if (typecheck_thread_count_waiting_for_definition + typecheck_thread_count_finished == typer_threads.count) {
			if (typecheck_thread_count_waiting_for_definition == 0) {
				break;
			} else {
				if (should_wait) {
					should_wait = false;
					sleep_milliseconds(1);
					goto recheck;
				}
				for (auto thread : typer_threads) {
					if (thread.typecheck_state->waiting_for) {
						thread.typecheck_state->reporter.error(thread.typecheck_state->waiting_for->location, "Undeclared identifier '%'", thread.typecheck_state->waiting_for->location);
						thread.typecheck_state->reporter.print_all();
					}
				}
				print("Typecheck failed due to unresolved names. Exiting.\n");
				return 1;
			}
		}
	}

	{
		timed_block("typecheck join"s);
		for (auto &typer_thread : typer_threads) {
			if (!join(typer_thread.thread)) {
				print("Typecheck failed. Exiting.\n");
				return 1;
			}
		}
	}

	if (do_print_ast) {
		timed_block("ast print"s);
		for_each(global_statements, [&](auto key, auto statement) {
			print_ast(statement);
		});
	}

	switch (output) {
		using enum Output;
		//case c:    output_c();    break;
		case nasm: output_nasm(); break;
	}

	print("Peak memory usage: %\n", format_bytes(get_memory_info().peak_usage));

	return 0;
}
