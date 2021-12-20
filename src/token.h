#pragma once
#include <common.h>

#define ENUMERATE_KEYWORDS(E) \
E(return,   0x2000000) \
E(fn,       0x2000001) \
E(true,     0x2000002) \
E(false,    0x2000003) \
E(if,       0x2000004) \
E(else,     0x2000005) \
E(while,    0x2000007) \
E(struct,   0x2000008) \
E(import,   0x2000009) \
E(null,     0x200000a) \
E(as,       0x200000b) \

#define ENUMERATE_TOKEN_KINDS(E) \
E(identifier,        0x1000000) \
E(integer_literal,   0x1000001) \
E(string_literal,    0x1000002) \
E(directive,         0x1000003) \
E(character_literal, 0x1000004) \
ENUMERATE_KEYWORDS(E) \

using TokenKind = u32;
enum : TokenKind {
#define E(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KINDS(E)
#undef E
};

Span<utf8> token_kind_to_string(TokenKind kind);

struct Token {
	Span<utf8> string;
	TokenKind kind;
};
