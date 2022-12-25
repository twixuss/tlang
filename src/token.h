#pragma once
#include <common.h>

#define ENUMERATE_KEYWORDS(E) \
E(eof,      'eof') \
E(as,       'as') \
E(true,     'true') \
E(if,       'if') \
E(in,       'in') \
E(then,     'then') \
E(else,     'else') \
E(null,     'null') \
E(simd,     'simd') \
E(enum,     'enum') \
E(for,      'for') \
E(do,       'do') \
E(return,   0x1000000) \
E(false,    0x1000001) \
E(while,    0x1000002) \
E(struct,   0x1000003) \
E(import,   0x1000004) \
E(assert,   0x1000005) \
E(defer,    0x1000006) \
E(union,    0x1000007) \
E(operator, 0x1000008) \
E(implicit, 0x1000009) \
E(explicit, 0x100000a) \
E(continue, 0x100000b) \
E(break,    0x100000c) \
E(match,    0x100000d) \
E(empty,    0x100000e) \
E(using,    0x100000f) \
E(where,    0x1000010) \
E(yield,    0x1000011) \

#define ENUMERATE_TOKEN_KINDS(E) \
E(identifier,        0x2000000) \
E(integer_literal,   0x2000001) \
E(string_literal,    0x2000002) \
E(directive,         0x2000003) \
E(character_literal, 0x2000004) \
E(float_literal,     0x2000005) \
E(split_identifier,  0x2000006) \
ENUMERATE_KEYWORDS(E) \

using TokenKind = u32;
enum : TokenKind {
#define E(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KINDS(E)
#undef E
};

String token_kind_to_string(TokenKind kind);

struct Token {
	String string;
	TokenKind kind;
};
