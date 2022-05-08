#pragma once
#include <common.h>

#define ENUMERATE_KEYWORDS(E) \
E(as,       'as') \
E(fn,       'fn') \
E(true,     'true') \
E(if,       'if') \
E(then,     'then') \
E(else,     'else') \
E(null,     'null') \
E(this,     'this') \
E(simd,     'simd') \
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

#define ENUMERATE_TOKEN_KINDS(E) \
E(identifier,        0x2000000) \
E(integer_literal,   0x2000001) \
E(string_literal,    0x2000002) \
E(directive,         0x2000003) \
E(character_literal, 0x2000004) \
E(float_literal,     0x2000005) \
ENUMERATE_KEYWORDS(E) \

using TokenKind = u32;
enum : TokenKind {
#define E(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KINDS(E)
#undef E
};

Span<utf8> token_kind_to_string(TokenKind kind);

struct Token {
	String string;
	TokenKind kind;
};
