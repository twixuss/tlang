#define TL_IMPL
#include <tl/main.h>
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
	void add(T token) {
		loop_while([&] { return end - current == capacity; });
		data[atomic_increment(&end) % capacity] = token;
	}
	Optional<T> pop() {
		loop_while([&] {
			if (ended_write)
				return false;
			return end - current == 0;
		});
		if (ended_write && current == end) {
			return {};
		}
		return data[atomic_increment(&current) % capacity];
	}
	void end_write() {
		ended_write = true;
	}

	RingQueue() {}
};

Buffer source_buffer;
Span<utf8> source;

[[noreturn]] void exit() {
	::exit(-1);
}

template <class ...Args>
void report_error(char const *format, Args const &...args) {
	print("Error: ");
	print(format, args...);
	print("\n\n");
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

template <class ...Args>
void report_error(Span<utf8> location, char const *format, Args const &...args) {
	print("Error: ");
	print(format, args...);
	print("\n\n");

	print_source_line(location);
}

template <class ...Args>
void report_error(Span<utf8> location1, Span<utf8> location2, char const *format, Args const &...args) {
	print("Error: ");
	print(format, args...);
	print("\n\n");

	print_source_line(location1);
	print_source_line(location2);
}

//RingQueue<Token, 16384> tokens;
RingQueue<Token, 4096> tokens;

bool lexer_function() {
	defer { tokens.end_write(); };

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

	while (current_p < source.end()) {
		while (is_whitespace(c)) {
			if (!next_char()) {
				return true;
			}
		}

		Token token = {};
		token.string.data = current_p;

		auto push_token = [&] {
			tokens.add(token);
		};

		switch (c) {
			case '+':
			case '-':
			case '*':
			case ':':
			case '(':
			case ')':
			case '{':
			case '}':
			case ';': {
			one_char_token:
				token.kind = (TokenKind)c;
				token.string.count = 1;
				next_char();
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
							report_error(Span(token.string.data, 2), "Unclosed comment block (end of file)");
							return false;
						}

					continue_search:
						auto comment_begin_or_end = find(Span(current_p, source.end()), {u8"*/"s, u8"/*"s});
						if (!comment_begin_or_end) {
							report_error(Span(token.string.data, 2), "Unclosed comment block");
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
						goto one_char_token;
					}
				} else {
					goto one_char_token;
				}
				break;
			}
			default: {
				if (is_alpha(c)) {
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
					while (is_digit(c)) {
						if (!next_char()) {
							break;
						}
					}
					token.string.count = current_p - token.string.data;
					token.kind = Token_integer_literal;

					push_token();
				} else {
					token.string.count = 1;
					report_error(token.string, "Invalid character");
					return false;
				}
			}
		}
	}
	return true;
}

u32 main_return_value = 0;

struct Parser {
	Token token = {};
	bool reached_end = false;
	AstLambda *current_lambda = 0;


	bool next() {
		auto popped = tokens.pop();
		if (!popped) {
			reached_end = true;
			return false;
		}
		token = popped.value();
		return true;
	}

	bool expect(TokenKind expected_kind) {
		if (token.kind != expected_kind) {
			report_error(token.string, "Expected '%', but got %", token_kind_to_string(expected_kind), token_kind_to_string(token.kind));
			return false;
		}
		return true;
	}
	bool next_not_end() {
		if (!next()) {
			report_error(token.string, "Unexpected end of file");
			return false;
		}
		return true;
	}
	bool next_expect(TokenKind expected_kind) {
		if (!next()) {
			report_error(token.string, "Unexpected end of file");
			return false;
		}
		if (!expect(expected_kind))  return false;
		return true;
	}
};

AstStatement *parse_statement(Parser &parser);
AstExpression *parse_expression(Parser &parser);

AstExpression *parse_sub_expression(Parser &parser) {
	if (parser.token.kind & keyword_built_in_type_flag) {
		auto identifier_token = parser.token;
		parser.next();
		auto identifier = new_ast<AstIdentifier>();
		identifier->location = identifier->name = identifier_token.string;
		identifier->definition = find_built_in_definition_from_token(identifier_token.kind);
		assert(identifier->definition);

		return identifier;
	} else if (parser.token.kind == Token_integer_literal) {
		auto parsed = parse_u64(parser.token.string);
		if (!parsed) {
			report_error(parser.token.string, "Failed to parse integer.");
			return 0;
		}
		auto integer = new_ast<AstInteger>();
		integer->value = parsed.value();
		integer->location = parser.token.string;
		parser.next();
		return integer;
	} else if (parser.token.kind == Token_identifier) {
		auto identifier_token = parser.token;
		parser.next();
		if (parser.token.kind == '(') {
			if (!parser.next_expect(')'))  return 0;

			auto call = new_ast<AstCall>();
			call->location = call->name = identifier_token.string;

			parser.next();

			return call;
		} else {
			auto identifier = new_ast<AstIdentifier>();
			identifier->location = identifier->name = identifier_token.string;
			return identifier;
		}
	} else if (parser.token.kind == '(') {
		auto start_token = parser.token;

		if (!parser.next_expect(')'))  return 0;
		if (!parser.next_not_end())  return 0;

		AstExpression *return_type_expression = 0;
		if (parser.token.kind != '{') {
			return_type_expression = parse_expression(parser);
		}
		if (!parser.next_not_end())    return 0;

		auto lambda = new_ast<AstLambda>();

		lambda->name = format(u8"unnamed%", lambda->uid);

		auto previous_lambda = parser.current_lambda;
		parser.current_lambda = lambda;
		defer { parser.current_lambda = previous_lambda; };

		while (parser.token.kind != '}') {
			auto statement = parse_statement(parser);
			if (!statement) {
				return 0;
			}
			lambda->statements.add(statement);
		}

		lambda->location = start_token.string;
		lambda->return_type_expression = return_type_expression;

		parser.next();
		return lambda;
	} else {
		report_error(parser.token.string, "Failed to parse expression.");
		return 0;
	}
	invalid_code_path();
}

BinaryOperation token_to_binary_operation(TokenKind kind) {
	switch (kind) {
		using enum BinaryOperation;
		case '+': return add;
	}
	invalid_code_path("attempt to convert bad token to binary operation");
}

AstExpression *parse_expression(Parser &parser) {
	auto sub = parse_sub_expression(parser);

	if (parser.reached_end)
		return sub;

	while (1) {
		if (parser.token.kind == '+') {
			auto binop = new_ast<AstBinaryOperator>();
			binop->left = sub;

			binop->operation = token_to_binary_operation(parser.token.kind);

			if (!sub) {
				return 0;
			}

			if (!parser.next()) {
				report_error("Unexpected end of file after binary operator");
				return 0;
			}

			binop->right = parse_sub_expression(parser);

			sub = binop;
			continue;
		} else {
			return sub;
		}
	}
}

AstStatement *parse_statement(Parser &parser) {
	if (parser.token.kind == Token_identifier) {
		auto name_token = parser.token;
		if (!parser.next_expect(':'))  return 0;
		if (!parser.next_not_end())  return 0;

		AstExpression *type_expression = 0;
		if (parser.token.kind != ':') {
			type_expression = parse_expression(parser);
			if (!type_expression)  return 0;
		}

		if (!parser.expect(':'))  return 0;
		if (!parser.next_not_end())  return 0;

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		if (needs_semicolon(expression)) {
			if (!parser.expect(';'))  return 0;
			parser.next();
		}

		auto definition = new_ast<AstDefinition>();
		definition->location = definition->name = name_token.string;
		definition->expression = expression;
		definition->type_expression = type_expression;
		definition->parent_lambda = parser.current_lambda;

		if (expression->kind == Ast_lambda) {
			auto lambda = (AstLambda *)expression;
			lambda->name.set(definition->name);
		}

		return definition;
	} else if (parser.token.kind == Token_return) {

		auto return_token = parser.token;

		if (!parser.next_not_end()) return 0;

		auto expression = parse_expression(parser);
		if (!expression)  return 0;

		if (!parser.expect(';'))  return 0;
		parser.next();

		auto ret = new_ast<AstReturn>();
		ret->location = return_token.string;
		ret->expression = expression;
		return ret;
	}


	report_error(parser.token.string, "Failed to parse statement. Unexpected token.");
	return 0;
}

bool parser_function() {
	Parser parser = {};
	if (!parser.next()) {
		return true;
	}
	while (!parser.reached_end) {
		auto statement = parse_statement(parser);
		if (!statement) {
			return false;
		}
		if (!can_be_global(statement)) {
			report_error(statement->location, "This statement can not be global.");
			return false;
		}
		auto found = global_statements.find(statement->location);
		if (found) {
			report_error(statement->location, (*found)->location, "Redefinition of '%'", statement->location);
			return false;
		}
		global_statements.get_or_insert(statement->location) = statement;
	}
	return true;
}

HashMap<Span<utf8>, AstStatement *> typechecked_globals;
RecursiveMutex typechecked_globals_mutex;

/*

struct TypeInfo {
};

TypeInfo type_void;
TypeInfo type_u8;
TypeInfo type_u16;
TypeInfo type_u32;
TypeInfo type_u64;
TypeInfo type_s8;
TypeInfo type_s16;
TypeInfo type_s32;
TypeInfo type_s64;
TypeInfo type_bool;
TypeInfo type_unsized_integer;

AstDefinition *definition_default_integer = &type_s64;

HashMap<Span<utf8>, AstDefinition *> built_in_types;
*/

umm append(StringBuilder &b, AstDefinition *definition) {
	if (definition == nullptr)  return append(b, "(null)");
	return append(b, definition->name);
}

struct TypecheckState {
	AstLambda *lambda = 0;
	AstDefinition *definition = 0;
	AstNode *waiting_for = 0;

	AstLambda *current_lambda = 0;
};

thread_local TypecheckState *typecheck_state;

struct DefinitionCallback {
	void (*function)(AstDefinition *, void *) = 0;
	void *state = 0;
	Allocator allocator = {};

	void operator()(AstDefinition *definition) {
		function(definition, state);
	}
};

DefinitionCallback create_definition_callback(void (*fn)(AstDefinition *, void *), void *state) {
	DefinitionCallback result;
	result.function = fn;
	result.state = state;
	return result;
}

DefinitionCallback create_definition_callback(void (*fn)(AstDefinition *)) {
	return create_definition_callback((void(*)(AstDefinition *, void *state))fn, 0);
}

template <class Fn>
DefinitionCallback create_definition_callback(Fn &fn) {
	if constexpr (std::is_convertible_v<Fn, void(*)(AstDefinition *)>) {
		return create_definition_callback((void(*)(AstDefinition *))fn);
	} else {
		DefinitionCallback result = {};
		result.function = [](AstDefinition *definition, void *state) {
			(*(Fn *)state)(definition);
		};
		result.allocator = current_allocator;
		result.state = result.allocator.allocate_uninitialized<Fn>();
		memcpy(result.state, &fn, sizeof(fn));
		return result;
	}
}

void free(DefinitionCallback &callback) {

}

template <class Result>
struct Waiter {
	Result volatile result = {};
};

HashMap<Span<utf8>, LinkedList<Waiter<AstDefinition *>>> definitions_waiters;
Mutex definitions_waiters_mutex;

u32 thread_count_waiting_for_definition = 0;
u32 thread_count_finished = 0;

AstDefinition *wait_for_definition(AstNode *node, Span<utf8> name) {
	if (typecheck_state->definition->name == name)
		return typecheck_state->definition;

	auto found_local = typecheck_state->current_lambda->local_definitions.find(name);
	if (found_local) {
		return *found_local;
	} else {
		lock(global_statements_mutex);
		auto found = global_statements.find(name);
		if (found) {
			auto statement = *found;
			if (statement->kind == Ast_definition) {
				unlock(global_statements_mutex);
				return (AstDefinition *)statement;
			}
			// Is this possible?
			invalid_code_path();
		} else {
			lock(definitions_waiters_mutex);
			auto &waiters = definitions_waiters.get_or_insert(name);
			auto &waiter = waiters.add();
			unlock(definitions_waiters_mutex);

			unlock(global_statements_mutex);

			typecheck_state->waiting_for = node;
			atomic_increment(&thread_count_waiting_for_definition);
			loop_while([&]{ return waiter.result == 0; });
			atomic_decrement(&thread_count_waiting_for_definition);
			typecheck_state->waiting_for = 0;

			auto result = waiter.result;

			lock(definitions_waiters_mutex);
			erase(waiters, &waiter);
			unlock(definitions_waiters_mutex);

			return result;
		}
	}
}

void harden_type(AstDefinition **type) {
	if (*type == &definition_unsized_integer) {
		*type = definition_default_integer;
	}
}

bool typecheck(AstExpression *expression) {
	switch (expression->kind) {
		case Ast_identifier: {
			auto identifier = (AstIdentifier *)expression;

			if (identifier->definition)
				return true;

			auto definition = wait_for_definition(identifier, identifier->name);
			identifier->definition = definition;
			identifier->type = definition->type;
			break;
		}
		case Ast_call: {
			auto call = (AstCall *)expression;
			auto definition = wait_for_definition(call, call->name);
			call->definition = definition;
			if (definition->expression->kind != Ast_lambda) {
				report_error(call->location, "Expression in not callable");
				return false;
			}
			auto lambda = (AstLambda *)definition->expression;
			call->lambda = lambda;

			if (lambda->return_type) {
				call->type = lambda->return_type;
			} else {
				call->type = &definition_void;
			}
			break;
		}
		case Ast_integer: {
			expression->type = &definition_unsized_integer;
			break;
		}
		case Ast_lambda: {
			auto lambda = (AstLambda *)expression;

			lambda->parent_lambda = typecheck_state->current_lambda;

			auto old_current_lamdda = typecheck_state->current_lambda;
			typecheck_state->current_lambda = lambda;
			defer { typecheck_state->current_lambda = old_current_lamdda; };

			if (lambda->return_type_expression) {
				typecheck(lambda->return_type_expression);
				lambda->return_type = lambda->return_type_expression->type;
			}

			for (auto statement : lambda->statements) {
				switch (statement->kind) {
					case Ast_return: {
						auto ret = (AstReturn *)statement;
						auto expression = ret->expression;
						if (!typecheck(expression))
							return false;
						lambda->return_types.add(expression->type);
						break;
					}
					case Ast_definition: {
						auto definition = (AstDefinition *)statement;

						auto where = lambda;
						while (where) {
							auto found_definition = where->local_definitions.find(definition->name);
							if (found_definition) {
								report_error(definition->location, (*found_definition)->location, "Redeclaration");
								return false;
							}
							where = where->parent_lambda;
						}

						lambda->local_definitions.get_or_insert(definition->name) = definition;
						if (definition->type_expression) {
							if (!typecheck(definition->type_expression))
								return false;
						}
						if (!typecheck(definition->expression))
							return false;

						if (definition->type_expression) {
							// Make sure that `type_expression` matches type of `expression`
						} else {
							definition->type = definition->expression->type;
							harden_type(&definition->type);
						}


						break;
					}
					default: {
						invalid_code_path();
					}
				}
			}

			if (lambda->return_types.count == 0) {
				lambda->return_type = &definition_void;
			} else if (lambda->return_types.count == 1) {
				lambda->return_type = lambda->return_types[0];
			} else {
				// Infer type from multiple return statements
				invalid_code_path("not implemented");
			}
			harden_type(&lambda->return_type);

			break;
		}
		case Ast_binary_operator: {
			auto bin = (AstBinaryOperator *)expression;
			if (!typecheck(bin->left))
				return false;
			if (!typecheck(bin->right))
				return false;

			assert(bin->operation == BinaryOperation::add);

			bin->type = bin->left->type;
			break;
		}
		default: {
			report_error(expression->location, "Internal error: unhandled case '%'", expression->kind);
			return false;
		}
	}
	return true;
}

bool typecheck_global_impl(AstStatement *statement, TypecheckState *state) {
	typecheck_state = state;

	switch (statement->kind) {
		case Ast_definition: {
			auto definition = (AstDefinition *)statement;
			state->definition = definition;

			if (definition->expression->kind == Ast_lambda) {
				typecheck_state->lambda = (AstLambda *)definition->expression;
			}

			if (!typecheck(definition->expression))
				return false;

			{
				scoped_lock(typechecked_globals_mutex);
				auto found_definition = typechecked_globals.find(definition->name);
				if (found_definition) {
					report_error(definition->location, (*found_definition)->location, "Redefinition");
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

			break;
		}
		default: {
			report_error(statement->location, "Internal error: unhandled case statement->kind");
			return false;
		}
	}

	atomic_increment(&thread_count_finished);
	return true;
}
bool typecheck_failed = false;
bool typecheck_global(AstStatement *statement, TypecheckState *state) {
	if (!typecheck_global_impl(statement, state)) {
		typecheck_failed = true;
		return false;
	}
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
void print_ast(AstInteger *node);
void print_ast(AstReturn *node);
void print_ast(AstCall *node);
void print_ast(AstNode *node) {
	switch (node->kind) {
		case Ast_definition: return print_ast((AstDefinition *)node);
		case Ast_lambda:     return print_ast((AstLambda     *)node);
		case Ast_identifier: return print_ast((AstIdentifier *)node);
		case Ast_integer:    return print_ast((AstInteger    *)node);
		case Ast_return:     return print_ast((AstReturn     *)node);
		case Ast_call:       return print_ast((AstCall       *)node);
		case Ast_binary_operator: return print_ast((AstBinaryOperator*)node);
		default:
			print_tabs();
			print("unknown - uid: %\n", node->uid);
			break;
	}
}
void print_ast(AstDefinition *node) {
	print_tabs();
	print("definition - name: %, type: %, uid: %\n", node->name, node->type, node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}
void print_ast(AstLambda *node) {
	print_tabs();
	print("lambda - return_type: %, uid: %\n", node->return_type, node->uid);
	tab_count += 1;
	for (auto statement : node->statements) {
		print_ast(statement);
	}
	tab_count -= 1;
}
void print_ast(AstIdentifier *node) {
	print_tabs();
	print("identifier - name: %, type: %, uid: %, definition.uid: %\n", node->name, node->type, node->uid, node->definition->uid);
}
void print_ast(AstCall *node) {
	print_tabs();
	print("call - name: %, type: %, uid: %, definition.uid: %\n", node->name, node->type, node->uid, node->definition->uid);
}
void print_ast(AstBinaryOperator *node) {
	print_tabs();
	print("binary - type: %, uid: %\n", node->type, node->uid);
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
void print_ast(AstInteger *node) {
	print_tabs();
	print("integer literal - value: %, uid: %\n", node->value, node->uid);
}
void print_ast(AstReturn *node) {
	print_tabs();
	print("return - uid: %\n", node->uid);
	tab_count += 1;
	print_ast(node->expression);
	tab_count -= 1;
}

struct Interpreter {
	struct Value {
		u64 u64;
	};

	struct Scope {
		HashMap<Span<utf8>, Value> identifiers;
	};

	bool should_return = false;
	List<Scope> scopes;
	Scope *current_scope;

	Value execute(AstNode *node) {
		switch (node->kind) {
			case Ast_definition: return execute((AstDefinition *)node);
			case Ast_return:	 return execute((AstReturn *)node);
			case Ast_lambda:	 return execute((AstLambda *)node);
			case Ast_identifier: return execute((AstIdentifier *)node);
			case Ast_integer:    return execute((AstInteger *)node);
		}
		invalid_code_path();
	}
	Value execute(AstDefinition *definition) {
		current_scope->identifiers.get_or_insert(definition->name) = execute(definition->expression);
		return {};
	}
	Value execute(AstReturn *ret) {
		should_return = true;
		return execute(ret->expression);
	}
	Value execute(AstIdentifier *identifier) {
		auto found = current_scope->identifiers.find(identifier->name);
		assert(found);
		return *found;
	}
	Value execute(AstInteger *integer) {
		return {integer->value};
	}
	Value execute(AstLambda *lambda) {
		return {};
	}
	Value execute_main(AstLambda *lambda) {
		current_scope = &scopes.add();
		for (auto statement : lambda->statements) {
			auto value = execute(statement);
			if (should_return) {
				should_return = false;
				return value;
			}
		}
		scopes.pop();
		current_scope = &scopes.back();
		return {};
	}
};

s32 tl_main(Span<Span<utf8>> arguments) {

	executable_path = arguments[0];
	auto parsed = parse_path(executable_path);
	executable_name = parsed.name;
	executable_directory = parsed.directory;

	if (arguments.count == 1) {
		print_help();
		return 1;
	}

	source_path = arguments[1];
	if (!is_absolute_path(source_path)) {
		source_path = make_absolute_path(source_path);
	}
	source_path_without_extension = parse_path(source_path).path_without_extension();

	construct(global_statements);
	construct(definitions_waiters);
	construct(typechecked_globals);
	construct(built_in_definitions);

	auto set_built_in_definition = [](u16 token, Span<utf8> name) {
		auto &definition = get_built_in_definition_from_token(token);
		definition.name = name;
		definition.is_type = true;
	};

	set_built_in_definition(Token_u8 , u8"u8"s);
	set_built_in_definition(Token_u16, u8"u16"s);
	set_built_in_definition(Token_u32, u8"u32"s);
	set_built_in_definition(Token_u64, u8"u64"s);
	set_built_in_definition(Token_s8 , u8"s8"s);
	set_built_in_definition(Token_s16, u8"s16"s);
	set_built_in_definition(Token_s32, u8"s32"s);
	set_built_in_definition(Token_s64, u8"s64"s);

	definition_unsized_integer.name = u8"(unsized_integer)"s;
	definition_unsized_integer.is_type = true;

	definition_void.name = u8"void"s;
	definition_void.is_type = true;

	definition_default_integer = find_built_in_definition_from_token(Token_s64);

	bool do_print_ast = find(arguments, u8"--print-ast"s);
	bool do_interpret = find(arguments, u8"--interpret"s);

	enum class Output {
		none,
		c,
		nasm,
	} output;

	auto found_output = find(arguments, u8"--output"s);
	if (found_output) {
		if (found_output >= &arguments.back()) {
			print(Print_error, "Missing output type argument\n");
		}
		auto output_string = found_output[1];

		using enum Output;
		     if (output_string == u8"none"s) output = none;
		else if (output_string == u8"c"s   ) output = c;
		else if (output_string == u8"nasm"s) output = nasm;
		else {
			print(Print_error, "Unknown output type '%'\n", output_string);
		}
	}

	source_buffer = read_entire_file(to_pathchars(source_path));
	source = as_utf8(source_buffer);


	//StringBuilder lexer_output_builder;
	//current_printer = {
	//	[](PrintKind kind, Span<utf8> string, void *context) {
	//		append(*(StringBuilder *)context, string);
	//	},
	//	&lexer_output_builder,
	//};
	auto lexer_thread  = create_thread(lexer_function);
	
	//StringBuilder parser_output_builder;
	//current_printer = {
	//	[](PrintKind kind, Span<utf8> string, void *context) {
	//		append(*(StringBuilder *)context, string);
	//	},
	//	&parser_output_builder,
	//};
	auto parser_thread = create_thread(parser_function);

	current_printer = console_printer;

	if (!join(lexer_thread)) {
		//print(to_string(lexer_output_builder));
		return 1;
	}

	if (!join(parser_thread)) {
		//print(to_string(parser_output_builder));
		return 1;
	}

	struct ThreadContext {
		ThreadRet<bool> thread;
		TypecheckState *typecheck_state;
	};

	List<ThreadContext> typer_threads;
	for_each(global_statements, [&](auto key, auto statement) {
		auto typecheck_state = default_allocator.allocate<TypecheckState>();
		typer_threads.add({create_thread(typecheck_global, statement, typecheck_state), typecheck_state});
	});

	while (1) {
		spin_iteration();

		if (typecheck_failed)
			return 1;

		if (thread_count_waiting_for_definition + thread_count_finished == typer_threads.count) {
			if (thread_count_waiting_for_definition == 0) {
				break;
			} else {
				for (auto thread : typer_threads) {
					if (thread.typecheck_state->waiting_for) {
						report_error(thread.typecheck_state->waiting_for->location, "Undeclared identifier '%'", thread.typecheck_state->waiting_for->location);
					}
				}
				return 1;
			}
		}
	}

	for (auto &typer_thread : typer_threads) {
		if (!join(typer_thread.thread)) {
			return 1;
		}
	}

	/*
	bool has_undeclared_identifier = false;
	for_each(definition_callbacks, [&](Span<utf8> &name, List<DefinitionCallback> &callbacks) {
		if (callbacks.count) {
			error_no_exit(name, "Undeclared identifier '%'", name);
			has_undeclared_identifier = true;
		}
	});
	if (has_undeclared_identifier) {
		return 1;
	}
	*/

	if (do_print_ast) {
		for_each(global_statements, [&](auto key, auto statement) {
			print_ast(statement);
		});
	}

	if (do_interpret) {
		//auto found_main_statement = global_statements.find_if([&](AstStatement *statement) {
		//	if (statement->kind == Ast_definition) {
		//		auto definition = (AstDefinition *)statement;
		//		if (definition->name == u8"main"s) {
		//			if (definition->expression->kind == Ast_lambda) {
		//				return true;
		//			}
		//		}
		//	}
		//	return false;
		//});
		//
		//if (found_main_statement) {
		//	auto main_statement = *found_main_statement;
		//	auto definition = (AstDefinition *)main_statement;
		//	auto lambda = (AstLambda *)definition->expression;
		//	Interpreter interpreter;
		//	auto ret = interpreter.execute_main(lambda);
		//	print("main returned %\n", ret.u64);
		//} else {
		//	error("Can't run the code: 'main' not found.");
		//}
	}

	switch (output) {
		using enum Output;
		case c:    output_c();    break;
		case nasm: output_nasm(); break;
	}

	return 0;
}
