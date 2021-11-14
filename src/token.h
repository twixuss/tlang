#pragma once
#include <common.h>

#define ENUMERATE_KEYWORDS(E) \
E(return, 0x20000) \
E(fn,     0x20001) \
E(true,   0x20002) \
E(false,  0x20003) \
E(if,     0x20004) \
E(else,   0x20005) \
E(extern, 0x20006) \

#define ENUMERATE_TOKEN_KINDS(E) \
E(identifier, 0x10000) \
E(integer_literal, 0x10001) \
E(string_literal, 0x10002) \
ENUMERATE_KEYWORDS(E) \

using TokenKind = u32;
enum : TokenKind {
#define E(name, value) Token_##name = value,
	ENUMERATE_TOKEN_KINDS(E)
#undef E
};

Span<utf8> token_kind_to_string(TokenKind kind);

extern Span<utf8> source;

struct Token {
	u32 file;
	u32 start;
	u32 count;
	TokenKind kind;

	Span<utf8> string() {
		return {source.data + start, count};
	}
};
