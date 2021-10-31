#pragma once
#include <common.h>

u16 const keyword_built_in_type_flag = 0x400;
u16 const keyword_built_in_type_mask = keyword_built_in_type_flag - 1;
u16 const built_in_type_count = 8;

#define ENUMERATE_KEYWORDS(E) \
E(return, 0x200) \
E(u8,     0x400) \
E(u16,    0x401) \
E(u32,    0x402) \
E(u64,    0x403) \
E(s8,     0x404) \
E(s16,    0x405) \
E(s32,    0x406) \
E(s64,    0x407) \

#define ENUMERATE_TOKEN_KINDS(E) \
E(identifier, 0x100) \
E(integer_literal, 0x101) \
ENUMERATE_KEYWORDS(E) \

using TokenKind = u16;
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
