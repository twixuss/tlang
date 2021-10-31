#include "token.h"

Span<utf8> token_kind_to_string(TokenKind kind) {
	static constexpr Array<utf8, 256> single_char_tokens = []{
		Array<utf8, 256> result;
		for (u32 i = 0; i < 256; ++i) {
			result.data[i] = i;
		}
		return result;
	}();

	if (kind < 0x100) {
		return Span((utf8 *)&single_char_tokens.data[kind], 1);
	}

	switch (kind) {
#define E(name, value) case value: return u8#name##s;
	ENUMERATE_TOKEN_KINDS(E)
#undef E
	}

	return u8"unknown"s;
}
