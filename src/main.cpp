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
#define CORO_MALLOC(x) ::tl::page_allocator.allocate_uninitialized(x)
#define CORO_FREE(x)   ::tl::page_allocator.free(x)
#pragma push_macro("assert")
#include <coro.h>
#pragma pop_macro("assert")

#define YIELD_STATE state->coro
#define yield(x) (_ReadWriteBarrier(), ::coro_yield(YIELD_STATE, (size_t)x))

#define COUNT_ALLOCATIONS 0
#define USE_SLABS 0

struct Parser;
struct Reporter;
struct SourceFileContext;

static List<Span<utf8>> import_paths;

struct SourceFileInfo {
    Span<utf8> path;
    Span<utf8> source;
    List<Span<utf8>> lines;
};

SourceFileInfo &get_source_info(utf8 *location);
bool ensure_addressable(Reporter *reporter, AstExpression *expression);
AstExpression *make_pointer_type(AstExpression *type);
AstDefinition *parse_definition(Span<utf8> name, Parser *parser);
SourceFileContext *parse_file(Span<utf8> path);

void print_help() {
    print(R"(Usage:
    {} <path>
)", context.executable_name);
}

u32 get_line_number(utf8 *from) {
    auto lines = get_source_info(from).lines;

    // lines will be empty at lexing time.
    // So if an error occurs at lexing time,
    // slower algorithm is executed.
    if (lines.count) {
#if 1
        // binary search
        auto begin = lines.data;
        auto end = lines.data + lines.count;
        while (1) {
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

enum class ReportKind {
    info,
    warning,
    error,
};

PrintKind get_print_kind(ReportKind kind) {
    switch (kind) {
        using enum ReportKind;
        case info: return Print_info;
        case warning: return Print_warning;
        case error: return Print_error;
    }
    invalid_code_path();
}

void print_source_line(ReportKind kind, Span<utf8> location) {

    if (location.data == nullptr) {
        // print("(null location)\n\n");
        return;
    }


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
    auto error_line_number = get_line_number(error_line_begin);

    auto print_line = [&](auto line) {
        return print(Print_warning, "{}|", Format{line, align_right(4, ' ')});
    };

    // I don't know if this is really useful
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
    print_replacing_tabs_with_4_spaces(Print_info,  line_start);
    print_replacing_tabs_with_4_spaces(get_print_kind(kind), location);
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
    print("\n");
}

List<SourceFileInfo> sources;

SourceFileInfo &get_source_info(utf8 *location) {
    for (auto &source : sources) {
        if (source.source.begin() <= location && location < source.source.end()) {
            return source;
        }
    }
    invalid_code_path();
}

struct Report {
    Span<utf8> location;
    Span<utf8> message;
    ReportKind kind;
};

List<utf8> where(utf8 *location) {
    if (location) {
        return format(u8"{}:{}:{}", get_source_info(location).path, get_line_number(location), get_column_number(location));
    } else {
        return {};
    }
}

template <class ...Args>
Report make_report(ReportKind kind, Span<utf8> location, Span<utf8> severity, char const *format_string, Args const &...args) {
    Report r;
    r.location = location;
    r.kind = kind;
    if (location.data) {
        r.message = format(u8"{}: {}: {}", where(location.data), severity, format(format_string, args...));
    } else {
        r.message = format(u8"{}: {}", severity, format(format_string, args...));
    }
    return r;
}

template <class ...Args>
Report make_info_report(Span<utf8> location, char const *format_string, Args const &...args) {
    return make_report(ReportKind::info, location, u8"Info"s, format_string, args...);
}
template <class ...Args>
Report make_info_report(char const *format_string, Args const &...args) {
    return make_info_report(Span<utf8>{}, format_string, args...);
}
template <class ...Args>
Report make_error_report(Span<utf8> location, char const *format_string, Args const &...args) {
    return make_report(ReportKind::error, location, u8"Error"s, format_string, args...);
}

template <class ...Args>
Report make_error_report(char const *format_string, Args const &...args) {
    return make_error_report(Span<utf8>{}, format_string, args...);
}

void print_report(Report r) {
    print(r.message);
    print('\n');
    print_source_line(r.kind, r.location);
}

template <class ...Args>
void immediate_info(Span<utf8> location, char const *format_string, Args const &...args) {
    print_report(make_info_report(location, format_string, args...));
}
template <class ...Args>
void immediate_info(char const *format_string, Args const &...args) {
    immediate_info(Span<utf8>{}, format_string, args...);
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
    Span<utf8> source;
};

HashSet<Span<utf8>> double_char_tokens;
HashSet<Span<utf8>> triple_char_tokens;

f32 lexer_time;
bool lexer_function(Lexer *lexer) {
    timed_function(context.profiler);

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
        //if (ends_with(get_source_path(token.string.data), u8"std.tl"s))
        //	print("{}\n", token.string);
        //
        //if (token.string == u8"while"s)
        //	debug_break();
    };

    utf8 *line_start = current_p;
    List<Span<utf8>> lines;
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
                auto found = find(triple_char_tokens, token.string = Span(current_p, 3));
                if (found) {
                    int k = 3;
                } else {
                    found = find(double_char_tokens, token.string = Span(current_p, 2));
                    if (!found) {
                        token.string = Span(current_p, 1);
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
                            if (c == '\n') {
                                break;
                            }
                        }
                        continue;
                    } else if (c == '*') {
                        auto closed = [&] {
                            if (!next_char()) {
                                token.string.count = 2;
                                lexer->reporter->error(token.string, "Unclosed comment block (end of file)");
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
                                        lexer->reporter->error(token.string, "Unclosed comment block (end of file)");
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

                if (c == '.') {
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
    Span<utf8> extern_language;
    Span<utf8> extern_library;
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
            reporter->error(token->string, "Expected '{}', but got {}", token_kind_to_string(expected_kind), token_kind_to_string(token->kind));
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

AstLiteral *make_string(Span<utf8> value) {
    auto i = new_ast<AstLiteral>();
    i->literal_kind = LiteralKind::string;
    i->string = value;
    i->type = &type_string;
    return i;
}

AstLiteral *make_integer(BigInt value, AstExpression *type = &type_unsized_integer) {
    auto i = new_ast<AstLiteral>();
    i->literal_kind = LiteralKind::integer;
    i->integer = value;
    i->type = type;
    return i;
}

AstLiteral *make_integer(u64 value, AstExpression *type = &type_unsized_integer) {
    return make_integer(make_big_int(value), type);
}

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
    using enum BinaryOperation;
    switch (op) {
        case dot:
            return 100;

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

// TODO FIXME right now `!func()` parses as `(!func)()`

AstStatement *parse_statement(Parser *parser);
AstExpression *parse_expression(Parser *parser);
AstExpression *parse_sub_expression_and_call(Parser *parser);
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
        case Token_simd: {
            if (!parser->next_not_end())
                return 0;

            auto expr = parse_expression(parser);
            if (!expr)
                return 0;

            if (expr->kind != Ast_subscript) {
                parser->reporter->error(expr->location, "Expected an array type after simd keyword, but got {}", expr->kind);
                return 0;
            }

            auto array = (AstSubscript *)expr;

            // array->is_simd = true;

            return array;
        }
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
        case Token_float_literal: {
            auto result = new_ast<AstLiteral>();
            result->literal_kind = LiteralKind::Float;
            if (std::from_chars((char *)parser->token->string.begin(), (char *)parser->token->string.end(), result->Float).ec == std::errc::invalid_argument) {
                parser->reporter->error(parser->token->string, "Failed to parse floating point number");
                return 0;
            }
            result->type = &type_unsized_float;
            parser->next();
            return result;
        }
        case Token_integer_literal: {
            BigInt value = 0_ib;
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
                    value = value * 10_ib + make_big_int(digit);
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
            auto identifier = new_ast<AstIdentifier>();
            identifier->location = identifier->name = identifier_token->string;
            return identifier;
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
            } else if (parser->token->string == u8"#typeof"s) {
                auto typeof = new_ast<AstTypeof>();
                typeof->location = parser->token->string;

                if (!parser->next_not_end())
                    return 0;

                typeof->expression = parse_sub_expression(parser);
                if (!typeof->expression)
                    return 0;

                typeof->location = {typeof->location.begin(), typeof->expression->location.end()};
                return typeof;
            } else if (parser->token->string == u8"#file"s) {
                auto result = make_string(parser->lexer->source_info->path);
                result->location = parser->token->string;
                parser->next();
                return result;
            } else if (parser->token->string == u8"#line"s) {
                auto result = make_integer(get_line_number(parser->token->string.data));
                result->location = parser->token->string;
                parser->next();
                return result;
            } else if (parser->token->string == u8"#location"s) {
                auto result = make_string(where(parser->token->string.data));
                result->location = parser->token->string;
                parser->next();
                return result;
            } else {
                parser->reporter->error(parser->token->string, "Unexpected expression level directive");
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
                } else if (parser->token->string == u8"#intrinsic"s) {
                    lambda->is_intrinsic = true;
                } else {
                    parser->reporter->error(parser->token->string, "Unknown directive");
                    return 0;
                }
                if (!parser->next_not_end())
                    return 0;
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
                    lambda->return_parameter = make_retparam(parser, return_type);
                }
            } else {
                // TODO: this probably will be executed a lot, so maybe this should be cached ???
                lambda->return_parameter = make_retparam(parser, &type_void);
            }


            if (parser->token->kind != '{' && parser->token->kind != '=>' && parser->token->kind != ';') {
                parser->reporter->error(parser->token->string, "Expected '{' or '=>' or ';' or '->' instead of '{}'", parser->token->string);
                return 0;
            }

            bool is_short = false;

            if (is_parsing_type) {
                lambda->has_body = false;
                lambda->is_type = true;
                if (parser->token->kind == '{' || parser->token->kind == '=>') {
                    parser->reporter->error(lambda->location, "Body of a lambda can not be specified after a #type directive");
                    return 0;
                } else if (parser->token->kind != ';') {
                    parser->reporter->error(parser->token->string, "Expected ';' or return type instead of '{}'", parser->token->string);
                    return 0;
                }
            } else {

                if (parser->token->kind == '{') {
                } else if (parser->token->kind == '=>') {
                    is_short = true;
                } else if (parser->token->kind == ';') {
                    lambda->has_body = false;
                } else {
                    parser->reporter->error(parser->token->string, "Expected '{' or '=>' or ';' or return type instead of '{}'", parser->token->string);
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
                        parser->reporter->error(lambda->location, "Lambda has no body, but extern library was not provided");
                        print_example();
                        return 0;
                    }

                    /*
                    lambda->extern_language = extern_language_from_string(parser->extern_language);
                    if (lambda->extern_language == ExternLanguage::none) {
                        parser->reporter->error(lambda->location, "Unsupported language: {}", parser->extern_language);
                        print_example();
                        return 0;
                    }
                    */

                    lambda->extern_library = parser->extern_library;

                }
            }

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
        case Token_if: {
            auto If = new_ast<AstIfx>();
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
            auto import = new_ast<AstImport>();
            import->location = parser->token->string;
            if (!parser->next_not_end())
                return 0;

            import->path = unescape_string(parser->token->string);
            parser->next();

            for (auto import_path : import_paths) {
                auto child = parse_file((Span<utf8>)concatenate(import_path, '\\', import->path));
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
            auto end_token = parser->token->string;

            expression->location = {start_token.begin(), end_token.end()};

            parser->next();

            return expression;
        }
        case '?': {
            auto noinit = new_ast<AstLiteral>();
            noinit->location = parser->token->string;
            noinit->literal_kind = LiteralKind::noinit;
            parser->next();
            return noinit;
        }
#if 0
        case '[': {
            auto subscript = new_ast<AstSubscript>();
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
            subscript->expression = parse_sub_expression_no_cast(parser);
            if (!subscript->expression)
                return 0;

            return subscript;
        }
#endif
        default: {
            if (is_unary_operator(parser->token->kind)) {
                auto unop = new_ast<AstUnaryOperator>();
                unop->location = parser->token->string;
                unop->operation = parser->token->kind;
                if (!parser->next_not_end())
                    return 0;

                unop->expression = parse_sub_expression_and_call(parser);
                if (!unop->expression)
                    return 0;

                return unop;
            } else {
                parser->reporter->error(parser->token->string, "Unexpected token '{}'", parser->token->string);
                return 0;
            }
        }
    }


    invalid_code_path();
}

AstExpression *parse_call(Parser *parser, AstExpression *expression) {
    if (!expression)
        return 0;

    while (parser->token->kind == '(') {
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

        auto call = new_ast<AstCall>();
        call->location = Span(expression->location.data, parser->token->string.end());
        call->expression = expression;
        call->arguments = arguments;

        if (!parser->next_not_end())  return 0;
        expression = call;
    }
    return expression;
}

AstExpression *parse_sub_expression_and_call(Parser *parser) {
    auto expression = parse_sub_expression(parser);
    if (!expression)
        return 0;

    return parse_call(parser, expression);
}

AstExpression *parse_cast(Parser *parser, AstExpression *expression) {
    if (!expression)
        return 0;

    while (parser->token->kind == Token_as) {
        auto cast = new_ast<AstCast>();
        cast->expression = expression;

        if (!parser->next_not_end())
            return 0;

        cast->type = parse_sub_expression(parser);
        if (!cast->type)
            return 0;

        cast->location = {cast->expression->location.begin(), cast->type->location.end()};
        expression = cast;
    }

    return expression;
}

// foo() as int
AstExpression *parse_sub_expression_and_call_and_cast(Parser *parser) {
    auto expression = parse_sub_expression(parser);
    if (!expression)
        return 0;

    expression = parse_cast(parser, parse_call(parser, expression));
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
void simplify(AstExpression **_expression) {

    auto expression = *_expression;
    defer { *_expression = expression; };

    bool is_type = (expression->type == &type_type) || (expression->type && expression->type->kind == Ast_lambda);

    if (is_type) {
        //
        // Simplify type
        //
        auto &type = expression;
        switch (type->kind) {
            case Ast_identifier: {
                // auto identifier = (AstIdentifier *)type;
                // type = identifier->definition->expression;
                // simplify(&type);
                break;
            }
            case Ast_unary_operator: {
                auto unop = (AstUnaryOperator *)type;
                assert(unop->operation == '*');
                simplify(&unop->expression);
                break;
            }
            case Ast_struct: {
                break;
            }
            case Ast_subscript: {
                auto subscript = (AstSubscript *)type;
                simplify(&subscript->expression);
                simplify(&subscript->index_expression);
                break;
            }
        }
    } else {
        //
        // Simplify expression
        //
        switch (expression->kind) {
            case Ast_binary_operator: {
                auto binop = (AstBinaryOperator *)expression;

                simplify(&binop->left);
                simplify(&binop->right);

                using enum BinaryOperation;

                if (binop->operation == dot) {
                    //
                    // HACK HACK HACK TODO TODO TODO FIXME FIXME FIXME
                    //
                    if (binop->left->kind == Ast_literal) {
                        auto left = (AstLiteral *)binop->left;
                        if (left->literal_kind == LiteralKind::string) {
                            if (binop->right->kind == Ast_identifier) {
                                auto right = (AstIdentifier *)binop->right;
                                if (right->name == u8"count"s) {
                                    expression = make_integer(left->string.count, right->type);
                                }
                            }
                        }
                    }
                } else if (
                    binop->operation == add ||
                    binop->operation == sub ||
                    binop->operation == mul ||
                    binop->operation == div ||
                    binop->operation == mod ||
                    binop->operation == bxor ||
                    binop->operation == band||
                    binop->operation == bor ||
                    binop->operation == eq ||
                    binop->operation == ne ||
                    binop->operation == ge ||
                    binop->operation == le ||
                    binop->operation == bsl ||
                    binop->operation == bsr ||
                    binop->operation == gt ||
                    binop->operation == lt
                ) {
                    auto left_literal  = get_literal(binop->left);
                    auto right_literal = get_literal(binop->right);
                    if (!left_literal || !right_literal)
                        return;
                    if (left_literal->literal_kind == LiteralKind::integer && right_literal->literal_kind == LiteralKind::integer) {
                        auto left  = left_literal->integer;
                        auto right = right_literal->integer;

                        BigInt value;

                        switch (binop->operation) {
                            case add: expression = make_integer(left + right, binop->type); return;
                            case sub: expression = make_integer(left - right, binop->type); return;
                            case mul: expression = make_integer(left * right, binop->type); return;
                            case div: invalid_code_path("not implemented"); // expression = make_integer(left / right, binop->type); return;
                            case band: expression = make_integer(left & right, binop->type); return;
                            case bor: expression = make_integer(left | right, binop->type); return;
                            case bxor: expression = make_integer(left ^ right, binop->type); return;
                            case bsl: invalid_code_path("not implemented"); // expression = make_integer(left << right, binop->type); return;
                            case bsr: invalid_code_path("not implemented"); // expression = make_integer(left >> right, binop->type); return;
                            case lt:  expression = make_boolean(left < right); return;
                            case gt:  expression = make_boolean(left > right); return;
                            case le: expression = make_boolean(left <= right); return;
                            case ge: expression = make_boolean(left >= right); return;
                            case ne: expression = make_boolean(left != right); return;
                            case eq: expression = make_boolean(left == right); return;
                            default: invalid_code_path(); break;
                        }
                    }
                }
                break;
            }
            case Ast_unary_operator: {
                auto unop = (AstUnaryOperator *)expression;

                simplify(&unop->expression);

                if (unop->expression->kind == Ast_literal && (((AstLiteral *)unop->expression)->literal_kind == LiteralKind::integer) &&
                    (unop->operation == '+' ||
                     unop->operation == '-')
                ) {
                    auto left  = ((AstLiteral *)unop->expression)->integer;
                    switch (unop->operation) {
                        case '+': return;
                        case '-': expression = make_integer(-left); return;
                        default: invalid_code_path(); break;
                    }
                }

                break;
            }
            case Ast_identifier: {
                /*
                auto identifier = (AstIdentifier *)expression;
                if (identifier->definition) {
                    if (identifier->definition->is_constant) {
                        expression = identifier->definition->expression;
                        assert(expression->kind == Ast_literal);
                    }
                }
                */
                break;
            }
            case Ast_cast: {
                auto cast = (AstCast *)expression;
                simplify(&cast->expression);
                simplify(&cast->type);
                break;
            }
        }
    }
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
        default:
            invalid_code_path();
            break;
    }
}

AstExpression *parse_expression(Parser *parser) {
    timed_function(context.profiler);
    auto sub = parse_sub_expression_and_call_and_cast(parser);
    if (!sub) {
        return 0;
    }

    if (parser->reached_end) {
        if (sub) {
            combine_location(sub);
            return sub;
        }
        return 0;
    }

#define USE_POST_SUBSCRIPT 1
#if USE_POST_SUBSCRIPT
parse_subscript:
    while (parser->token->kind == '[') {
        auto subscript = new_ast<AstSubscript>();
        subscript->is_prefix = false;

        if (!parser->next_not_end())
            return 0;

        subscript->index_expression = parse_expression(parser);
        if (!subscript->index_expression)
            return 0;

        if (!parser->expect(']'))
            return 0;

        subscript->location = {sub->location.begin(), parser->token->string.end()};

        parser->next();
        subscript->expression = sub;

        sub = subscript;

        sub = parse_cast(parser, sub);
        if (!sub)
            return 0;
    }
#endif

    AstBinaryOperator *top_binop = 0;
    AstBinaryOperator *previous_binop = 0;
    s32 previous_precedence = 0;
    while (is_binary_operator(parser->token->kind)) {
        auto binop = new_ast<AstBinaryOperator>();
        binop->left = sub;

        binop->operation = binary_operation_from_token(parser->token->kind);

        binop->location = parser->token->string;

        auto precedence = get_precedence(binop->operation);

        if (!sub) {
            return 0;
        }

        if (!parser->next()) {
            parser->reporter->error("Unexpected end of file after binary operator");
            return 0;
        }

        using enum BinaryOperation;

        if (binop->operation == dot) {
            binop->right = parse_sub_expression(parser);
        } else {
            binop->right = parse_sub_expression_and_call_and_cast(parser);
        }
        if (!binop->right) {
            parser->reporter->info("While parsing binary operator '{}'", operator_string(binop->operation));
            return 0;
        }

        if (binop->operation == dot) {
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
        if (top_binop->operation == BinaryOperation::dot) {
            sub = parse_call(parser, sub);
            sub = parse_cast(parser, sub);
        }
    }

    if (sub) {
#if USE_POST_SUBSCRIPT
        if (parser->token->kind == '[') {
            sub = top_binop;
            goto parse_subscript;
        }
#endif
        combine_location(sub);
        simplify(&sub);
        return sub;
    }

    return 0;
}

// returns true for functions (overloading), otherwise false
bool is_redefinable(AstDefinition *definition) {
    return is_lambda(definition->expression);
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

#if 1
    // Redefinition checks now are impossible at parsing time because of function overloading
    if (definition->name != u8"_"s) {
        scoped_lock(parser->current_scope);
        parser->current_scope->definitions.get_or_insert(definition->name).add(definition);
    }
#else
    if (definition->name != u8"_"s) {
        bool check_redefinition_in_parent_scopes = true;
        if (definition->parent_scope->node && definition->parent_scope->node->kind == Ast_struct) {
            check_redefinition_in_parent_scopes = false;
        }

        {
            auto report_redefinition_error = [&] (AstDefinition *_new, AstDefinition *existing) {
                parser->reporter->error(_new->name, "Redefinition of '{}'", _new->name);
                parser->reporter->info(existing->name, "Top declaration is here");
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
    parser->reporter->error(parser->token->string, "Failed to parse definition");
    return 0;
}

bool is_statement(AstExpression *expression) {
    return expression->kind == Ast_call || expression->kind == Ast_import;
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
        }
        case Token_if: {
            auto If = new_ast<AstIf>();
            If->location = parser->token->string;
            if (!parser->next_not_end())
                return;

            If->condition = parse_expression(parser);
            if (!If->condition) {
                return;
            }

            if (parser->token->kind == Token_then && !parser->next_not_end()) {
                return;
            }

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
        }
        case Token_while: {
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
        }
        case Token_directive: {
            if (parser->token->string == u8"#test"s) {
                auto test = new_ast<AstTest>();
                test->location = parser->token->string;

                if (!parser->next_not_end())
                    return;

                if (parser->token->kind == Token_true) {
                    test->should_compile = true;
                } else if (parser->token->kind == Token_false) {
                    test->should_compile = false;
                } else {
                    parser->reporter->error(parser->token->string, "Unexpected token after #test directive, expected true or false");
                    return;
                }

                if (!parser->next_not_end())
                    return;

                if (!parse_block_or_single_statement(parser, &test->scope)) {
                    return;
                }

                result = test;
                return;
            } else if (parser->token->string == u8"#print"s) {
                if (!parser->next_not_end())
                    return;

                auto expression = parse_expression(parser);
                if (!expression) {
                    return;
                }

                auto print = new_ast<AstPrint>();
                print->expression = expression;
                result = print;
                return;
            } else if (parser->token->string == u8"#assert"s) {
                if (!parser->next_not_end())
                    return;

                auto expression = parse_expression(parser);
                if (!expression) {
                    return;
                }

                auto assert = new_ast<AstAssert>();
                assert->condition = expression;
                result = assert;
                return;
            } else {
                parser->reporter->error(parser->token->string, "Unknown statement level directive");
                return;
            }
            break;
        }
        case '{': {
            auto block = new_ast<AstBlock>();

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
        } else if (
            parser->token->kind == '=' ||
            parser->token->kind == '+=' ||
            parser->token->kind == '-=' ||
            parser->token->kind == '*=' ||
            parser->token->kind == '/=' ||
            parser->token->kind == '%=' ||
            parser->token->kind == '|=' ||
            parser->token->kind == '&=' ||
            parser->token->kind == '^=' ||
            parser->token->kind == '<<=' ||
            parser->token->kind == '>>='
        ) {
            auto ass = new_ast<AstBinaryOperator>();
            ass->location = parser->token->string;
            ass->left = expression;
            ass->operation = binary_operation_from_token(parser->token->kind);

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

HashMap<Span<utf8>, SourceFileContext *> parsed_files;

SourceFileContext *parse_file(Span<utf8> path) {
    timed_function(context.profiler);

    if (auto found = parsed_files.find(path)) {
        return *found;
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

    // context->scope.parent = &global_scope;

    context->parser.lexer = &context->lexer;
    context->parser.reporter = context->lexer.reporter = &context->reporter;
    // context->parser.current_scope = &context->scope;

    auto source_info = &sources.add({path, source});

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
            if (parser->token->string == u8"#extern_language"s) {
                if (!parser->next_expect(Token_string_literal)) {
                    parser->reporter->error("Expected language name. Currently only \"C\" is available.");
                    return ParseResult::syntax_error;
                }
                parser->extern_language = unescape_string(parser->token->string);
                if (parser->extern_language != u8"C"s) {
                    parser->reporter->error(parser->token->string, "Only \"C\" is supported.");
                    return ParseResult::syntax_error;
                }
                parser->next();
            } else if (parser->token->string == u8"#extern_library"s) {
                if (!parser->next_expect(Token_string_literal)) {
                    parser->reporter->error("Expected library name.");
                    return ParseResult::syntax_error;
                }
                parser->extern_library = unescape_string(parser->token->string);
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
                goto _parse_global_statement;
            }
        } else if (parser->token->kind == Token_import) {
            if (!parser->next_expect(Token_string_literal)) {
                parser->reporter->error("Expected library path.");
                return ParseResult::syntax_error;
            }
            auto libname = unescape_string(parser->token->string);
            auto child = parse_file((Span<utf8>)concatenate(context.executable_directory, "\\libs\\", libname));
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
    Span<utf8> waiting_for_name;

    AstLambda *current_lambda = 0;

    Scope *current_scope = 0;

    Reporter reporter;

    bool finished = false;

    coro_state *coro = 0;

    u32 no_progress_counter = 0;
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

List<AstDefinition *> get_definitions(TypecheckState *state, Span<utf8> name) {
    timed_function(context.profiler);
    List<AstDefinition *> result;
    auto scope = state->current_scope;
    while (scope) {
        auto found_local = scope->definitions.find(name);
        if (found_local)
            result.add(*found_local);
        scope = scope->parent;
    }
    return result;
}

struct IntegerInfo {
    AstStruct *type;
    BigInt min_value;
    BigInt max_value;
};

IntegerInfo integer_infos[8];

void report_not_convertible(Reporter *reporter, AstExpression *expression, AstExpression *type) {
    reporter->error(expression->location, "Expression of type {} is not implicitly convertible to {}", type_to_string(expression->type), type_to_string(type));
}

void harden_type(AstExpression *expression) {
    switch (expression->kind) {
        case Ast_literal: {
            auto literal = (AstLiteral *)expression;
            if (literal->type == &type_unsized_integer) { // Literals may already have their type set by, for example, cast expressions
                expression->type = &type_s64;
            }
            break;
        }
        case Ast_identifier: // TODO: unsized identifiers?
        case Ast_binary_operator:
        case Ast_call:
        case Ast_cast:
        case Ast_subscript:
        case Ast_unary_operator:
        case Ast_struct:
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
    result.type = &type_type;
    return result;
}

Box<AstLiteral> make_bool(bool val) {
    timed_function(context.profiler);
    AstLiteral result;
    result.literal_kind = LiteralKind::boolean;
    result.Bool = val;
    result.type = &type_bool;
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

            if (!ident->definition->is_constant) {
                state->reporter.error(expression->location, "Can't evaluate expression at compile time: definition is not constant");
                return nullptr;
            }

            if (ident->definition->evaluated)
                return ident->definition->evaluated;

            return ident->definition->evaluated = evaluate(state, ident->definition->expression);
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
                    assert(l->literal_kind == LiteralKind::integer);
                    assert(r->literal_kind == LiteralKind::integer);
                    return make_integer(l->integer + l->integer, bin->type);
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
        case Ast_typeof: {
            auto typeof = (AstTypeof *)expression;
            return make_type_literal(typeof->expression->type);
        }
        case Ast_struct: {
            return make_type_literal(expression);
        }
        case Ast_unary_operator: {
            auto unop = (AstUnaryOperator *)expression;
            if (types_match(expression->type, &type_type)) {
                auto child = evaluate(state, unop->expression);
                if (!child)
                    return nullptr;

                if (child->literal_kind == LiteralKind::type) {
                    return make_type_literal(make_pointer_type(child->type_value));
                }

                invalid_code_path();
            }
            break;
        }
        case Ast_call: {
            // TODO: this is very limited right now
            auto call = (AstCall *)expression;
            auto lambda = get_lambda(call->expression);
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
    timed_function(context.profiler);
    auto unop = new_ast<AstUnaryOperator>();
    unop->expression = type;
    unop->type = &type_type;
    unop->operation = '*';
    return unop;
}

void typecheck(TypecheckState *state, AstExpression *&expression);

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

bool ensure_fits(Reporter *reporter, AstExpression *expression, BigInt integer, IntegerInfo info) {
    if (integer >= info.min_value && integer <= info.max_value)
        return true;
    if (reporter) {
        auto type_string = type_name(info.type);
        reporter->error(expression->location, "{} does not fit into {}. You can write '{} as {}' to discard excess bits", integer, type_string, expression->location, type_string);
    }
    return false;
}

bool implicitly_cast(Reporter *reporter, AstExpression **_expression, AstExpression *type) {
    timed_function(context.profiler);
    auto expression = *_expression;
    defer { *_expression = expression; };

    if (types_match(expression->type, type)) {
        return true;
    }

    if (expression->kind == Ast_literal) {
        auto literal = (AstLiteral *)expression;
        if (literal->literal_kind == LiteralKind::integer) {
            if (types_match(literal->type, &type_pointer_to_void)) {
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

                if (types_match(type, &type_f32) || types_match(type, &type_f64)) {
                    expression->type = type;
                    return true;
                }
            }
        } else if (literal->literal_kind == LiteralKind::Float) {
            if (types_match(type, &type_f32) || types_match(type, &type_f64)) {
                return true;
            }
        } else if (literal->literal_kind == LiteralKind::noinit) {
            literal->type = type;
            return true;
        }
    } else if (expression->type->kind == Ast_unary_operator) {
        auto unop = (AstUnaryOperator *)expression->type;
        assert(unop->operation == '*');

        if (types_match(unop->expression, &type_void)) {
            if (is_pointer(type)) {
                return true;
            }
        }
    } else if (expression->type == &type_unsized_integer) {
        auto found_info = find_if(integer_infos, [&](auto &i) { return types_match(i.type, type); });
        if (found_info) {
            auto &info = *found_info;
            auto literal = get_literal(expression);
            assert(literal);
            if (!ensure_fits(reporter, expression, literal->integer, info)) {
                return false;
            }

            expression->type = type;
            return true;
        }

        if (types_match(type, &type_f32) || types_match(type, &type_f64)) {
            expression->type = type;
            return true;
        }
    } else {
        CastType request = {get_struct(expression->type), get_struct(type)};
        auto found_built_in = find(built_in_casts, request);

        if (found_built_in && found_built_in->implicit) {
            auto cast = new_ast<AstCast>();
            cast->expression = expression;
            cast->type = type;
            //cast->cast_kind = found_built_in->kind;
            expression = cast;
            return true;
        }
    }

    if (reporter) {
        report_not_convertible(reporter, expression, type);
    }
    return false;
}

void wait_iteration(TypecheckState *state, Span<utf8> name) {
    state->no_progress_counter++;
    if (state->no_progress_counter == 256) { /* TODO: This is not the best solution */
        state->reporter.error(name, "Undeclared identifier");
        yield(TypecheckResult::fail);
    }
    yield(TypecheckResult::wait);
}

List<AstDefinition *> wait_for_definitions(TypecheckState *state, Span<utf8> name) {
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

    reporter->error(expression->location, "Expression is not addressable");
    return false;
}
bool ensure_assignable(Reporter *reporter, AstExpression *expression) {
    timed_function(context.profiler);
    switch (expression->kind) {
        case Ast_identifier: {
            auto identifier = (AstIdentifier *)expression;
            if (identifier->definition->is_constant) {
                reporter->error(identifier->location, "Can't assign to '{}' because it is constant", identifier->location);
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
    }

    reporter->error(expression->location, "Expression is not assignable");
    return false;
}

bool ensure_subscriptable(TypecheckState *state, AstExpression *expression) {
    if (expression->type->kind == Ast_subscript || is_pointer(expression->type))
        return true;
    state->reporter.error(expression->location, "Expression is not subscriptable");
    return false;
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

                if (!implicitly_cast(&state->reporter, &expression, lambda->return_parameter->type))
                    yield(TypecheckResult::fail);
            }
            state->current_lambda->return_statements.add(ret);
            break;
        }
        case Ast_definition: {
            auto definition = (AstDefinition *)statement;
            if (definition->built_in) {
                break;
            }

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

                    auto evaluated_type = evaluate(state, definition->type);
                    assert(evaluated_type);
                    assert(evaluated_type->literal_kind == LiteralKind::type);
                    definition->type = evaluated_type->type_value;

                    if (definition->expression) {
                        if (!implicitly_cast(&state->reporter, &definition->expression, definition->type)) {
                            yield(TypecheckResult::fail);
                        }
                    }
                }
            } else {
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
            if (!implicitly_cast(&state->reporter, &If->condition, &type_bool)) {
                yield(TypecheckResult::fail);
            }

            typecheck_scope(&If->true_scope);
            typecheck_scope(&If->false_scope);

            break;
        }
        case Ast_while: {
            auto While = (AstWhile *)statement;

            typecheck(state, While->condition);
            if (!implicitly_cast(&state->reporter, &While->condition, &type_bool)) {
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

            auto typechecker = [](coro_state *, size_t param) -> size_t {
                auto test_params = (TestParams *)param;
                auto state = test_params->state;
                typecheck_scope(&test_params->test->scope);
                return (size_t)TypecheckResult::success;
            };

            coro_state *corostate;
            coro_init(&corostate, typechecker, 1024*1024);
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
                    state->reporter.info(print->expression->location, "{}", (s64)result->integer); // TODO: print the full number
                    break;
                }
                default:
                    invalid_code_path("not implemented");
            }

            break;
        }
        default: {
            invalid_code_path("invalid statement kind in typecheck");
        }
    }
}

// Array<Array<Array<bool, built_in_struct_count>, built_in_struct_count>, (u32)BinaryOperation::count> builtin_binary_operator_table;


#define REDECLARE(name, expr) auto _##name = expr; auto name = _##name;

bool signedness_matches(AstStruct *a, AstStruct *b) {
    assert(::is_integer(a));
    assert(::is_integer(b));
    if (a == &type_unsized_integer || b == &type_unsized_integer)
        return true;

    return ::is_signed(a) == ::is_signed(b);
}

void typecheck(TypecheckState *state, AstExpression *&expression) {
    assert(expression);

    // Skip typechecking builtin expressions
    if (expression->type && expression->type->type) {
        return;
    }
    defer {
        if (expression->kind == Ast_identifier && ((AstIdentifier *)expression)->possible_definitions.count) {
        } else {
            assert(expression->type);
            simplify(&expression);
            assert(expression->type);
        }
    };

    switch (expression->kind) {
        case Ast_identifier: {
            auto identifier = (AstIdentifier *)expression;

            if (identifier->definition)
                break;

            auto definitions = wait_for_definitions(state, identifier->name);

            if (definitions.count == 1) {
                auto definition = definitions[0];
                assert(definition->type);
                identifier->definition = definition;
                identifier->type = definition->type;
            } else {
                identifier->possible_definitions = definitions;
            }

            break;
        }
        case Ast_call: {
            auto call = (AstCall *)expression;

            typecheck(state, call->expression);
            for (u32 i = 0; i < call->arguments.count; ++i) {
                typecheck(state, call->arguments[i]);
            }

            struct Match {
                AstDefinition *definition = 0;
                AstLambda *lambda = 0;
            };

            List<Match> matches;
            matches.allocator = temporary_allocator;

            if (call->expression->kind == Ast_identifier) {
                auto identifier = (AstIdentifier *)call->expression;
                Match match;
                // TODO: These branches have similar code
                if (identifier->definition) {
                    match = {identifier->definition, get_lambda(identifier->definition->expression)};
                    if (!match.lambda) {
                        state->reporter.error(call->location, "No lambda with that name was found.");
                        state->reporter.info (match.definition->location, "Found a definition.");
                        yield(TypecheckResult::fail);
                    }

                    auto lambda = match.lambda;
                    while (!lambda->finished_typechecking_head)
                        yield(TypecheckResult::wait);

                    if (call->arguments.count != lambda->parameters.count) {
                        state->reporter.error(call->location, "Argument count does not match");
                        state->reporter.info(match.definition->location, "Definition is here:");
                        yield(TypecheckResult::fail);
                    }

                    for (u32 i = 0; i < call->arguments.count; ++i) {
                        auto &argument = call->arguments[i];
                        auto &parameter = lambda->parameters[i];
                        if (!implicitly_cast(&state->reporter, &argument, parameter->type)) {
                            yield(TypecheckResult::fail);
                        }
                    }
                } else {
                    for (auto definition : identifier->possible_definitions) {
                        auto e = direct(definition->expression);
                        if (e->kind != Ast_lambda)
                            continue;

                        auto lambda = (AstLambda *)e;
                        while (!lambda->finished_typechecking_head)
                            yield(TypecheckResult::wait);

                        if (call->arguments.count != lambda->parameters.count)
                            continue;

                        for (u32 i = 0; i < call->arguments.count; ++i) {
                            auto &argument = call->arguments[i];
                            auto &parameter = lambda->parameters[i];
                            if (!implicitly_cast(0, &argument, parameter->type)) { // no reports here, just checking
                                goto _continue_definition0;
                            }
                        }

                        matches.add({definition, lambda});
                    _continue_definition0:;
                    }
                    if (matches.count == 0) {
                        if (identifier->possible_definitions.count) {
                            state->reporter.error(call->location, "No matching lambda was found.");
                            state->reporter.info("Here is the list of definitions with that name:");
                            for (auto definition : identifier->possible_definitions) {
                                auto e = direct(definition->expression);
                                if (e->kind != Ast_lambda) {
                                    state->reporter.info(definition->location, "This is not a lambda");
                                    continue;
                                }

                                auto lambda = (AstLambda *)e;
                                while (!lambda->finished_typechecking_head)
                                    yield(TypecheckResult::wait);

                                if (call->arguments.count != lambda->parameters.count) {
                                    state->reporter.info(definition->location, "Argument count does not match");
                                    continue;
                                }

                                for (u32 i = 0; i < call->arguments.count; ++i) {
                                    auto &argument = call->arguments[i];
                                    auto &parameter = lambda->parameters[i];
                                    if (!implicitly_cast(&state->reporter, &argument, parameter->type)) { // implicitly_cast will add a report
                                        goto _continue_definition1;
                                    }
                                }
                            _continue_definition1:;
                            }
                        } else {
                            state->reporter.error(call->location, "Lambda with that name was not defined.");
                        }
                        yield(TypecheckResult::fail);
                    }
                    if (matches.count != 1) {
                        state->reporter.error(call->location, "Ambiguous overloads were found.");
                        for (auto match : matches) {
                            state->reporter.info(match.definition->location, "Here");
                        }
                        yield(TypecheckResult::fail);
                    }

                    match = matches[0];
                    identifier->definition = match.definition;
                    identifier->type = match.definition->type;
                    //identifier->possible_definitions = {}; // actually this is redundant
                }

                auto lambda = match.lambda;
                call->type = lambda->return_parameter->type;
            } else {
                invalid_code_path();
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
                case character:
                    expression->type = &type_u8;
                    break;
                case noinit:
                    expression->type = &type_noinit;
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
            typecheck(state, lambda->return_parameter);

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

                /*
                for (auto ret : lambda->return_statements) {
                    if (ret->expression) {
                        if (!harden_type(state, &ret->expression, lambda->return_parameter->type)) {
                            state->reporter.info(lambda->location, "When hardening return statement expression's type (lambda's return type is '{}')", type_to_string(lambda->return_parameter->type));
                            yield(TypecheckResult::fail);
                        }
                        if (!convertible(ret->expression, lambda->return_parameter->type)) {
                            report_not_convertible(&state->reporter, ret->expression, lambda->return_parameter->type);
                            yield(TypecheckResult::fail);
                        }
                    } else {
                        if (!types_match(lambda->return_parameter->type, &type_void)) {
                            if (!lambda->return_parameter->name.count) {
                                state->reporter.error(ret->location, "Attempt to return nothing when lambda's return type is '{}' and return parameter is unnamed", type_to_string(lambda->return_parameter->type));
                                yield(TypecheckResult::fail);
                            }
                        }
                    }
                }
                */

                // assert(lambda->return_parameter->type);

                //if (!do_all_paths_return(lambda)) {
                //	state->reporter.error(lambda->location, "Not all paths return a value");
                //	yield(TypecheckResult::fail);
                //}
            }

            break;
        }
        case Ast_binary_operator: {
            auto bin = (AstBinaryOperator *)expression;
            typecheck(state, bin->left);

            auto report_type_mismatch = [&] {
                state->reporter.error(bin->location, "Can't use binary {} on types {} and {}", operator_string(bin->operation), type_to_string(bin->left->type), type_to_string(bin->right->type));
                yield(TypecheckResult::fail);
            };

            using enum BinaryOperation;

            if (bin->operation == dot) {
                if (bin->left->type->kind == Ast_import) {
                    invalid_code_path("not implemented");
                    auto import = (AstImport *)get_definition_expression(bin->left);
                    assert(import->kind == Ast_import);
                    import->scope;
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

                        member_identifier->definition = *found_member;
                        member_identifier->type = member_identifier->definition->type;
                    } else {
                        state->reporter.error(bin->left->location, "Dot operator can not be applied to an expression of type {}", type_to_string(bin->left->type));
                        yield(TypecheckResult::fail);
                    }
                    bin->type = bin->right->type;
                }

            } else if (bin->operation == ass) {
                if (!ensure_assignable(&state->reporter, bin->left)) {
                    yield(TypecheckResult::fail);
                }

                typecheck(state, bin->right);
                if (!implicitly_cast(&state->reporter, &bin->right, bin->left->type)) {
                    yield(TypecheckResult::fail);
                }
                bin->type = &type_void;
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
                    REDECLARE(l, (AstStruct *)l);
                    REDECLARE(r, (AstStruct *)r);

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
                            if (l == &type_unsized_integer && r == &type_unsized_integer) {
                                bin->type = &type_unsized_integer;
                                return;
                            } else if (l == &type_unsized_integer && ri) {
                                bin->type = bin->left->type = r;
                                return;
                            } else if (r == &type_unsized_integer && li) {
                                bin->type = bin->right->type = l;
                                return;
                            }
                            break;
                        }
                        case ne:
                        case eq:
                        case lt:
                        case gt:
                        case le:
                        case ge: {
                            bin->type = &type_bool;
                            if (l == &type_unsized_integer && r == &type_unsized_integer) {
                                return;
                            } else if (l == &type_unsized_integer && ri) {
                                bin->left->type = r;
                                return;
                            } else if (r == &type_unsized_integer && li) {
                                bin->right->type = l;
                                return;
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
                                        return;
                                    }
                                    break;
                                } else {
                                    bin->type = l->size > r->size ? l : r;
                                    return;
                                }
                                break;
                            }

                            if (lf && rf) {
                                bin->type = l->size > r->size ? l : r;
                                return;
                            }
                            break;
                        }

                        case ne:
                        case eq: {
                            bin->type = &type_bool;

                            if (types_match(l, r)) {
                                return;
                            }

                            if ((li && r == &type_unsized_integer) || (ri && l == &type_unsized_integer)) {
                                return;
                            }

                            break;
                        }

                        case lt:
                        case gt:
                        case le:
                        case ge: {
                            bin->type = &type_bool;

                            if (li && ri) {
                                if (signedness_matches(l, r)) {
                                    return;
                                }
                                break;
                            }

                            if ((li && r == &type_unsized_integer) || (ri && l == &type_unsized_integer)) {
                                return;
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
                                    return;
                                }
                                break;
                            }
                            break;
                        }

                        case bsl:
                        case bsr:
                        case bslass:
                        case bsrass: {
                            if ((li && r == &type_unsized_integer) || (ri && l == &type_unsized_integer)) {
                                bin->type = r == &type_unsized_integer ? l : r;
                                return;
                            }

                            break;
                        }
                    }
                } else if (lp && rp) {
                    // Both are pointers
                    REDECLARE(l, (AstUnaryOperator *)l);
                    REDECLARE(r, (AstUnaryOperator *)r);
                    switch (bin->operation) {
                        case ne: {
                            bin->type = &type_bool;
                            if (types_match(l->expression, r->expression))
                                return;
                            break;
                        }
                    }
                } else if ((lp && ri) || (li && rp)) {
                    // One is pointer and other is integer
                    bin->type = lp ? l : r;
                    return;
                }

                report_type_mismatch();
                return;
            }

            break;
        }
        case Ast_unary_operator: {
            auto unop = (AstUnaryOperator *)expression;

            typecheck(state, unop->expression);

            if (is_type(unop->expression)) {
                if (unop->operation != '*') {
                    state->reporter.error(unop->location, "Unary operator '{}' can not be applied to a type expression", operator_string(unop->operation));
                    yield(TypecheckResult::fail);
                }
                unop->type = &type_type;
            } else {
                switch (unop->operation) {
                    case '-': {
                        if (::is_integer(unop->expression->type)) {
                            unop->type = unop->expression->type;
                        } else {
                            state->reporter.error(unop->location, "Unary minus can not be applied to expression of type {}", type_to_string(unop->expression->type));
                            yield(TypecheckResult::fail);
                        }
                        break;
                    }
                    case '&': {
                        if (!ensure_addressable(&state->reporter, unop->expression)) {
                            yield(TypecheckResult::fail);
                        }
                        unop->type = make_pointer_type(unop->expression->type);
                        break;
                    }
                    case '*': {
                        if (!is_pointer(unop->expression->type)) {
                            state->reporter.error(unop->location, "{} is not a pointer type, can't dereference it", type_to_string(unop->expression->type));
                            yield(TypecheckResult::fail);
                        }
                        unop->type = ((AstUnaryOperator *)unop->expression->type)->expression;
                        break;
                    }
                    case '!': {
                        // TODO: implicit cast from int to bool? ...
                        if (!types_match(unop->expression->type, &type_bool)/* && !::is_integer(unop->expression->type)*/) {
                            state->reporter.error(unop->location, "{} is not a boolean type, can't invert it", type_to_string(unop->expression->type));
                            yield(TypecheckResult::fail);
                        }
                        unop->type = &type_bool;
                        break;
                    }
                    default: {
                        invalid_code_path();
                        yield(TypecheckResult::fail);
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
                    typecheck(state, member);

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

            typecheck(state, subscript->index_expression);

            harden_type(subscript->index_expression);

            if (!::is_integer(subscript->index_expression->type)) {
                state->reporter.error(subscript->index_expression->location, "Expression must be of type integer but is {}", type_to_string(subscript->index_expression->type));
                yield(TypecheckResult::fail);
            }

            typecheck(state, subscript->expression);
            if (is_type(subscript->expression)) {
                subscript->type = &type_type;

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
            break;
        }
        case Ast_sizeof: {
            auto size_of = (AstSizeof *)expression;

            typecheck(state, size_of->expression);

            if (!is_type(size_of->expression)) {
                state->reporter.error(size_of->expression->location, "Expression must be a type");
                yield(TypecheckResult::fail);
            }

            expression = make_integer(get_size(size_of->expression));
            expression->type = &type_unsized_integer;

            break;
        }
        case Ast_cast: {
            auto cast = (AstCast *)expression;
            typecheck(state, cast->expression);
            typecheck(state, cast->type);

            auto &src_type = cast->expression->type;
            auto &dst_type = cast->type;

#define breakable_scope for(bool CONCAT(_bs_, __LINE__)=true;CONCAT(_bs_, __LINE__);CONCAT(_bs_, __LINE__)=false)

            breakable_scope {
                auto found_built_in = find(built_in_casts, {get_struct(src_type), get_struct(dst_type)});
                if (found_built_in) {
                    //cast->cast_kind = found_built_in->kind;
                    break;
                }

                if (::is_integer(src_type)) {
                    if (src_type == &type_unsized_integer) {
                        if (::is_pointer(dst_type) || ::is_integer(dst_type)) {
                            // Just replace cast with literal
                            expression = cast->expression;
                            expression->type = dst_type;
                            break;
                        }
                    } else {
                        if (::is_pointer(dst_type)) {
                            auto built_in_cast = find_if(built_in_casts, [&](auto c) { return c.from == src_type; });
                            assert(built_in_cast);
                            //cast->cast_kind = built_in_cast->kind;
                            break;
                        }
                    }
                } else if (::is_pointer(src_type)) {
                    if (::is_pointer(dst_type)) {
                        expression = cast->expression;
                        expression->type = cast->type;
                        break;
                    } else if (::is_integer(dst_type)) {
                        if (get_size(dst_type) == 8) {
                            //cast->cast_kind = CastKind::no_op;
                            break;
                        }

                        auto built_in_cast = find_if(built_in_casts, [&](auto c) {
                            return c.from == &type_u64 && types_match(c.to, dst_type);
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
        case Ast_ifx: {
            auto If = (AstIfx *)expression;

            typecheck(state, If->condition);
            if (!implicitly_cast(&state->reporter, &If->condition, &type_bool)) {
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

            break;
        }
        case Ast_typeof: {
            auto typeof = (AstTypeof *)expression;
            typecheck(state, typeof->expression);
            typeof->type = &type_type;
           //  print("typeof {} is {}\n", typeof->expression->location, type_to_string(typeof->expression->type));
            break;
        }
        case Ast_import: {
            auto import = (AstImport *)expression;
            import->type = import; // MYTYPEISME
            state->current_scope->append(*import->scope);
#if 0

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
                auto child = parse_file((Span<utf8>)concatenate(import_path, '\\', path_literal->string));
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

void *typecheck_global(coro_state *corostate, TypecheckState *state) {
    {
        push_scope(&global_scope);
        typecheck(state, state->statement);
    }
    yield(TypecheckResult::success);
    return 0;
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
    append(test, R"(
main :: fn () {
    main15();
}
)");
#if 1
    for (u32 i = 0; i < 256; ++i) {
        append_format(test, R"FOOBAR(/*
import "windows.tl"

standard_output_handle : HANDLE;
global_string : string;

AStruct :: struct {{
    data : *void;
    count : uint;
}}

main :: fn () -> s32 {{
    standard_output_handle = GetStdHandle(STD_OUTPUT_HANDLE);

    str : string = "Hello World!\n";

    WriteConsoleA(standard_output_handle, str.data, str.count, null, null);
    WriteConsoleA(standard_output_handle, "Hello World!\n".data, 13, null, null);
    print("Hello World!\n");
    print(str);

    if true {{
        dd : u64; // block-local variable
        print("true\n");
    }}

    if 0 == 0 {{ print("0 is 0\n"); }}
    else      {{ print("0 is not 0\n"); }}


    if 1 < 2 print("1 is less than 2\n");

    if true print("true\n");
    else  {{ print("false\n"); }}

    if false {{ print("true\n"); }}
    else       print("false\n");

    if 3 >= 0 print("3 is >= 0\n");
    else      print("3 is not >= 0\n");

    i := 10;

    while i > 0 {{
        j := i;
        while j > 0 {{
            print("*");
            j = j - 1;
        }}
        print("\n");
        i = i - 1;
    }}

    STR := "my file.txt\0";
    print(STR);
    file := CreateFileA(STR.data, GENERIC_WRITE, 0, null, CREATE_ALWAYS, /*FILE_ATTRIBUTE_NORMAL*/ 0, null);

    content := "Hello World!";
    WriteFile(file, content.data, content.count, null, null);

    global_string = "A global string";
    print(global_string);

    local_string : string;
    local_string = "a local string";
    print(local_string);

    a_struct : AStruct;
    a_struct.count = 7;
    a_struct.data = "AStruct".data;
    WriteConsoleA(standard_output_handle, a_struct.data, a_struct.count, null, null);

    a_string : string;
    a_string.count = 7;
    a_string.data = "A STRING".data;
    print(a_string);

    return x as s32;
}}

x :: 42;

print :: fn (str: string) {{
    WriteConsoleA(standard_output_handle, str.data, str.count, null, null);
}}
*/


import "std.tl"

main{} :: fn () {{
    class_name := "window_class\0".data;
    window_name := "hello window\0".data;

    hInstance := GetModuleHandleA(null);

    wc : WNDCLASSEXA;
    wc.hInstance = hInstance;
    wc.cbSize = #sizeof WNDCLASSEXA;
    wc.lpfnWndProc = fn #stdcall (hwnd : HWND, uMsg : UINT, wParam : WPARAM, lParam : LPARAM) -> LRESULT {{
        if uMsg == WM_DESTROY {{
            PostQuitMessage(0);
            return 0;
        }}
        return DefWindowProcA(hwnd, uMsg, wParam, lParam);
    }};
    //wc.lpfnWndProc = &DefWindowProcA;
    wc.lpszClassName = class_name;
    wc.hCursor = LoadCursorA(null, IDC_ARROW);
    if RegisterClassExA(&wc) != 0 {{
        print_string("Class created!\n");
    }} else {{
        print_string("Class Failed!\n");
    }}

    window := CreateWindowExA(
        0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE,
        CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
    );

    if window != null {{
        print_string("Window Success!\n");
    }} else {{
        print_string("Window Fail!\n");
    }}

    msg : MSG;

    while true {{
        while PeekMessageA(&msg, null, 0, 0, PM_REMOVE) != 0 {{
            if msg.message == WM_QUIT
                return;
            TranslateMessage(&msg);
            DispatchMessageA(&msg);
        }}
    }}

}}
/*
foo :: fn (x: int) -> int {{
    if x == 0 {{
        main();
        return 0;
    }}
    return x;
}}

main :: fn () -> int {{
    return foo(12);
}}
*/
/*
foo :: fn (x: u8) -> u8 {{
    return x;
}}

main :: fn () -> int {{
    a :: "HILLO";
    return foo(a.count as u8);
}}
*/

/*
import "std.tl"
main :: fn () {{
    print_string("hello");
}}
*/

/*

foo :: fn (data : *u8, size : uint) {{
}}

main :: fn () -> int {{
    hello :: "hello";
    foo(hello.data, hello.count);
}}

*/

/*
import "std.tl"

x :: u8;

main :: fn () {{
    a : x;
    print_hex(&a);
    print_char('\n');

    {{
        b : x;
        print_hex(&b);
        print_char('\n');
        c : x;
        print_hex(&c);
        print_char('\n');

        {{
            d : x;
            print_hex(&d);
            print_char('\n');
        }}
    }}

    e : x;
    print_hex(&e);
    print_char('\n');
}}

*/)FOOBAR", i);
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
    Span<utf8> output;

    List<Span<utf8>> source_files;

    bool print_ast = false;
    bool no_typecheck = false;

    bool success = false;
};

ParsedArguments parse_arguments(Span<Span<utf8>> arguments) {
    timed_function(context.profiler);

    ParsedArguments result = {};

    context.executable_path = arguments[0];

    if (!is_absolute_path(context.executable_path)) {
        context.executable_path = concatenate(context.current_directory, u8'\\', context.executable_path);
    }

    auto parsed = parse_path(context.executable_path);
    context.executable_name = parsed.name;
    context.executable_directory = parsed.directory;

    result.output = u8"nasm_x86_64_windows"s;

    //print("executable_path: {}\nexecutable_name: {}\nexecutable_directory: {}\n", executable_path, executable_name, executable_directory);

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
            result.output = arguments[i];
        } else {
            result.source_files.add(arguments[i]);
        }
    }

    result.success = true;
    return result;
}

#if COUNT_ALLOCATIONS
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
        return my_allocate(size, alignment);
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
        free(data, old_size, alignment, location);
        return result;
    }
    void free(void *data, umm size, umm alignment, std::source_location location) {
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
        return my_deallocate(data, size);
    }
};

SlabAllocator slab_allocator;

auto slab_allocator_func(AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> void * {
    switch (mode) {
        case Allocator_allocate: {
#if COUNT_ALLOCATIONS
            allocation_sizes.get_or_insert(location) += new_size;
#endif
            return slab_allocator.allocate(new_size, align, location);
        }
        case Allocator_reallocate: {
            return slab_allocator.reallocate(data, old_size, new_size, align, location);
        }
        case Allocator_free: {
            slab_allocator.free(data, new_size, align, location);
            break;
        }
    }
    return 0;
}

s32 tl_main(Span<Span<utf8>> arguments) {
    construct(context);

    auto global_timer = create_precise_timer();
    defer { print("Execution finished in {} ms\n", reset(global_timer) * 1000); };
    // defer { format("Execution finished in {} ms\n", reset(global_timer) * 1000); };     (print\(|format\()".*%.*"

    //CreateWindowExA(0, (char *)1, (char *)2, 3, 4, 5, 6, 7, (HWND)8, (HMENU)9, (HINSTANCE)10, (void *)11);

    set_console_encoding(Encoding_utf8);

    defer { print("Peak memory usage: {}\n", format_bytes(get_memory_info().peak_usage)); };

    write_test_source();

    init_ast_allocator();

#if COUNT_ALLOCATIONS
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
        [](AllocatorMode mode, void *data, umm old_size, umm new_size, umm align, std::source_location location, void *) -> void * {
            switch (mode) {
                case Allocator_allocate: {
#if COUNT_ALLOCATIONS
                    allocation_sizes.get_or_insert(location) += new_size;
#endif
                    return my_allocate(new_size, align);
                }
                case Allocator_reallocate: {
                    auto result = my_allocate(new_size, align);
                    memcpy(result, data, old_size);
                    return result;
                }
                case Allocator_free: {
                    my_deallocate(data, new_size);
                    return 0;
                }
            }
            return 0;
        },
        0
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
    defer { context.profiler.deinit(); };
#endif

    defer { write_entire_file("profile.tmd"s, context.profiler.output_for_timed()); };

    timed_function(context.profiler);

restart_main:

    timed_begin(context.profiler, "setup"s);

    context.current_directory = get_current_directory();

    auto args = parse_arguments(arguments);

    if (args.source_files.count == 0) {
        print("No source path received. Exiting.\n");
        return 1;
    }
    context.source_path = args.source_files[0];
    if (!is_absolute_path(context.source_path)) {
        context.source_path = make_absolute_path(context.source_path);
    }
    context.source_path_without_extension = parse_path(context.source_path).path_without_extension();

    construct(parsed_files);
    construct(sources);
    construct(global_scope);
    //construct(typechecked_globals);
    //construct(names_not_available_for_globals);
    construct(built_in_casts);

    construct(double_char_tokens);
    construct(triple_char_tokens);
    construct(import_paths);

    import_paths.add(context.current_directory);
    import_paths.add(concatenate(context.executable_directory, u8"\\libs"s));

    double_char_tokens.insert(u8"=="s);
    double_char_tokens.insert(u8"=>"s);
    double_char_tokens.insert(u8"!="s);
    double_char_tokens.insert(u8">="s);
    double_char_tokens.insert(u8"<="s);
    double_char_tokens.insert(u8"+="s);
    double_char_tokens.insert(u8"-="s);
    double_char_tokens.insert(u8"*="s);
    double_char_tokens.insert(u8"/="s);
    double_char_tokens.insert(u8"%="s);
    double_char_tokens.insert(u8"|="s);
    double_char_tokens.insert(u8"&="s);
    double_char_tokens.insert(u8"^="s);
    double_char_tokens.insert(u8"->"s);
    double_char_tokens.insert(u8">>"s);
    double_char_tokens.insert(u8"<<"s);

    triple_char_tokens.insert(u8">>="s);
    triple_char_tokens.insert(u8"<<="s);

    integer_infos[0] = {&type_u8,  -0xff_ib,               0xff_ib              };
    integer_infos[1] = {&type_u16, -0xffff_ib,             0xffff_ib            };
    integer_infos[2] = {&type_u32, -0xffffffff_ib,         0xffffffff_ib        };
    integer_infos[3] = {&type_u64, -0xffffffffffffffff_ib, 0xffffffffffffffff_ib};
    integer_infos[4] = {&type_s8,  -0xff_ib,               0xff_ib              };
    integer_infos[5] = {&type_s16, -0xffff_ib,             0xffff_ib            };
    integer_infos[6] = {&type_s32, -0xffffffff_ib,         0xffffffff_ib        };
    integer_infos[7] = {&type_s64, -0xffffffffffffffff_ib, 0xffffffffffffffff_ib};

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
        global_scope.definitions.get_or_insert(name).add(definition);
        //typechecked_globals.get_or_insert(name) = definition;
    };

    init_type(type_void,   u8"void"s, 0, 0);
    init_type(type_type,   u8"type"s, 8, 8);
    init_type(type_bool,   u8"bool"s, 1, 1);
    init_type(type_u8,     u8"u8"s,   1, 1);
    init_type(type_u16,    u8"u16"s,  2, 2);
    init_type(type_u32,    u8"u32"s,  4, 4);
    init_type(type_u64,    u8"u64"s,  8, 8);
    init_type(type_s8,     u8"s8"s,   1, 1);
    init_type(type_s16,    u8"s16"s,  2, 2);
    init_type(type_s32,    u8"s32"s,  4, 4);
    init_type(type_s64,    u8"s64"s,  8, 8);
    init_type(type_f32,    u8"f32"s,  4, 4);
    init_type(type_f64,    u8"f64"s,  8, 8);
    init_type(type_string, u8"string"s, 16, 8);
    init_type(type_unsized_integer,  u8"integer"s, 0, 0);
    init_type(type_unsized_float,  u8"float"s, 0, 0);
    init_type(type_noinit, u8"noinit"s, 0, 0);

    type_pointer_to_void.expression = &type_void;
    type_pointer_to_void.operation = '*';
    type_pointer_to_void.type = &type_type;

    type_default_integer = &type_s64;
    type_default_float = &type_f64;

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

    built_in_casts.insert({&type_u8 , &type_s8 , /*CastKind::u8_s8  , */false});
    built_in_casts.insert({&type_u8 , &type_s16, /*CastKind::u8_s16 , */true});
    built_in_casts.insert({&type_u8 , &type_s32, /*CastKind::u8_s32 , */true});
    built_in_casts.insert({&type_u8 , &type_s64, /*CastKind::u8_s64 , */true});
    built_in_casts.insert({&type_u8 , &type_u16, /*CastKind::u8_u16 , */true});
    built_in_casts.insert({&type_u8 , &type_u32, /*CastKind::u8_u32 , */true});
    built_in_casts.insert({&type_u8 , &type_u64, /*CastKind::u8_u64 , */true});
    built_in_casts.insert({&type_u16, &type_s8 , /*CastKind::u16_s8 , */false});
    built_in_casts.insert({&type_u16, &type_s16, /*CastKind::u16_s16, */false});
    built_in_casts.insert({&type_u16, &type_s32, /*CastKind::u16_s32, */true});
    built_in_casts.insert({&type_u16, &type_s64, /*CastKind::u16_s64, */true});
    built_in_casts.insert({&type_u16, &type_u8 , /*CastKind::u16_u8 , */false});
    built_in_casts.insert({&type_u16, &type_u32, /*CastKind::u16_u32, */true});
    built_in_casts.insert({&type_u16, &type_u64, /*CastKind::u16_u64, */true});
    built_in_casts.insert({&type_u32, &type_s8 , /*CastKind::u32_s8 , */false});
    built_in_casts.insert({&type_u32, &type_s16, /*CastKind::u32_s16, */false});
    built_in_casts.insert({&type_u32, &type_s32, /*CastKind::u32_s32, */false});
    built_in_casts.insert({&type_u32, &type_s64, /*CastKind::u32_s64, */true});
    built_in_casts.insert({&type_u32, &type_u8 , /*CastKind::u32_u8 , */false});
    built_in_casts.insert({&type_u32, &type_u16, /*CastKind::u32_u16, */false});
    built_in_casts.insert({&type_u32, &type_u64, /*CastKind::u32_u64, */true});
    built_in_casts.insert({&type_u64, &type_s8 , /*CastKind::u64_s8 , */false});
    built_in_casts.insert({&type_u64, &type_s16, /*CastKind::u64_s16, */false});
    built_in_casts.insert({&type_u64, &type_s32, /*CastKind::u64_s32, */false});
    built_in_casts.insert({&type_u64, &type_s64, /*CastKind::u64_s64, */false});
    built_in_casts.insert({&type_u64, &type_u8 , /*CastKind::u64_u8 , */false});
    built_in_casts.insert({&type_u64, &type_u16, /*CastKind::u64_u16, */false});
    built_in_casts.insert({&type_u64, &type_u32, /*CastKind::u64_u32, */false});
    built_in_casts.insert({&type_s8 , &type_s16, /*CastKind::s8_s16 , */true});
    built_in_casts.insert({&type_s8 , &type_s32, /*CastKind::s8_s32 , */true});
    built_in_casts.insert({&type_s8 , &type_s64, /*CastKind::s8_s64 , */true});
    built_in_casts.insert({&type_s8 , &type_u8 , /*CastKind::s8_u8  , */false});
    built_in_casts.insert({&type_s8 , &type_u16, /*CastKind::s8_u16 , */false});
    built_in_casts.insert({&type_s8 , &type_u32, /*CastKind::s8_u32 , */false});
    built_in_casts.insert({&type_s8 , &type_u64, /*CastKind::s8_u64 , */false});
    built_in_casts.insert({&type_s16, &type_s8 , /*CastKind::s16_s8 , */false});
    built_in_casts.insert({&type_s16, &type_s32, /*CastKind::s16_s32, */true});
    built_in_casts.insert({&type_s16, &type_s64, /*CastKind::s16_s64, */true});
    built_in_casts.insert({&type_s16, &type_u8 , /*CastKind::s16_u8 , */false});
    built_in_casts.insert({&type_s16, &type_u16, /*CastKind::s16_u16, */false});
    built_in_casts.insert({&type_s16, &type_u32, /*CastKind::s16_u32, */false});
    built_in_casts.insert({&type_s16, &type_u64, /*CastKind::s16_u64, */false});
    built_in_casts.insert({&type_s32, &type_s8 , /*CastKind::s32_s8 , */false});
    built_in_casts.insert({&type_s32, &type_s16, /*CastKind::s32_s16, */false});
    built_in_casts.insert({&type_s32, &type_s64, /*CastKind::s32_s64, */true});
    built_in_casts.insert({&type_s32, &type_u8 , /*CastKind::s32_u8 , */false});
    built_in_casts.insert({&type_s32, &type_u16, /*CastKind::s32_u16, */false});
    built_in_casts.insert({&type_s32, &type_u32, /*CastKind::s32_u32, */false});
    built_in_casts.insert({&type_s32, &type_u64, /*CastKind::s32_u64, */false});
    built_in_casts.insert({&type_s64, &type_s8 , /*CastKind::s64_s8 , */false});
    built_in_casts.insert({&type_s64, &type_s16, /*CastKind::s64_s16, */false});
    built_in_casts.insert({&type_s64, &type_s32, /*CastKind::s64_s32, */false});
    built_in_casts.insert({&type_s64, &type_u8 , /*CastKind::s64_u8 , */false});
    built_in_casts.insert({&type_s64, &type_u16, /*CastKind::s64_u16, */false});
    built_in_casts.insert({&type_s64, &type_u32, /*CastKind::s64_u32, */false});
    built_in_casts.insert({&type_s64, &type_u64, /*CastKind::s64_u64, */false});

    built_in_casts.insert({&type_f64, &type_s64, /*CastKind::f64_s64, */false});

    current_printer = standard_output_printer;

    defer {
        if (args.print_ast) {
            print_ast();
        }
    };

    timed_end("setup"s);



    {
        scoped_phase("Parsing");

        auto parsed = parse_file(concatenate(context.executable_directory, u8"\\libs\\preload.tl"s));
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
            print("Lexer failed. Exiting.\n");
            return 1;
        }
        if (failed_parser) {
            failed_parser->reporter->print_all();
            print("Parser failed. Exiting.\n");
            return 1;
        }
    }

    if (!args.no_typecheck) {
        scoped_phase("Typechecking");

        timed_block(context.profiler, "typecheck"s);

        Span<TypecheckState> typecheck_states;
        typecheck_states.count = count(global_scope.statements, [&](AstStatement *statement) { return !(statement->kind == Ast_definition && ((AstDefinition *)statement)->built_in); });
        if (typecheck_states.count) {
            typecheck_states.data = default_allocator.allocate<TypecheckState>(typecheck_states.count);

            u32 typechecks_finished = 0;
            bool fail = false;
            u32 state_index = 0;

            auto process_coroutine_result = [&](auto &state, auto result) {
                switch (result) {
                    case TypecheckResult::success:
                    case TypecheckResult::fail:
                        state.finished = true;
                        typechecks_finished++;
                        state.reporter.print_all();
                        if (result == TypecheckResult::fail) {
                            fail = true;
                        }
                        coro_free(&state.coro);
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
                coro_init(&state.coro, (coroutine_t)typecheck_global, 1024*1024);
                ++state_index;
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

#undef YIELD_STATE
#define YIELD_STATE state.coro
                    auto result = (TypecheckResult)(int)yield(&state);
                    switch (process_coroutine_result(state, result)) {
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

    auto found_definitions = global_scope.definitions.find(u8"main"s);
    if (found_definitions) {
        auto definitions = *found_definitions;
        if (definitions.count != 1) {
            immediate_error("Found multiple `main` definitions");
            for (auto definition : definitions) {
                immediate_info(definition->location, "Here");
            }
            return 1;
        }
        auto definition = definitions[0];
        if (!is_lambda(definition->expression)) {
            immediate_error(definition->location, "`main` is not a lambda");
            return 1;
        }
        context.main_lambda = get_lambda(definition->expression);

        if (!::is_integer(context.main_lambda->return_parameter->type) && !types_match(context.main_lambda->return_parameter->type, &type_void)) {
            immediate_error(context.main_lambda->return_parameter->type->location.data ? context.main_lambda->return_parameter->type->location : context.main_lambda->location, "Main function can return any type of integer or void, but not {}.", type_to_string(context.main_lambda->return_parameter->type));
            return 1;
        }
    } else {
        print("Main function was not defined. Exiting.\n");
        return 1;
    }

    Bytecode bytecode;
    {
        scoped_phase("Building bytecode");
        bytecode = build_bytecode();
    }

    if (args.output != u8"none"s) {
        scoped_phase("Generating executable");

        auto lib = LoadLibraryW((wchar *)to_utf16(concatenate(context.executable_directory, u8"\\outputs\\"s, args.output), true).data);
        if (!lib) {
            print("Failed to load output generator '{}'. Check 'tlang\\bin\\outputs' directory\n", args.output);
            return 1;
        }

        auto build = (OutputBuilder)GetProcAddress(lib, "tlang_build_output");
        if (!build) {
            print("Failed to load output generator '{}'. There is no build function\n", args.output);
            return 1;
        }

        build(context, bytecode);
    }

    return 0;
}

