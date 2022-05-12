#include "token.h"

String token_kind_to_string(TokenKind kind) {
	static constexpr Array<utf8, 256> single_char_tokens = []{
		Array<utf8, 256> result = {};
		for (u32 i = 0; i < 256; ++i) {
			result.data[i] = i;
		}
		return result;
	}();

	if (kind < 0x100) {
		return Span((utf8 *)&single_char_tokens.data[kind], 1);
	}

	static thread_local auto double_char_tokens = []() {
		Map<TokenKind, Span<utf8>> result;

#define X(x) result.get_or_insert(#x[1] | (#x[0] << 8)) = u8#x##s;

		X(==);
		X(!=);
		X(>=);
		X(<=);
		X(+=);
		X(-=);
		X(*=);
		X(/=);
		X(%=);
		X(|=);
		X(&=);
		X(^=);
		X(->);
		X(>>);
		X(<<);
		X(=>);
		X(->);

		return result;
	}();

	if (kind < 0x10000) {
		return double_char_tokens.find(kind)->value;
	}

	switch (kind) {
		case 'eof': return "end of file"str;
	}

	switch (kind) {
#define E(name, value) case value: return u8#name##s;
	ENUMERATE_TOKEN_KINDS(E)
#undef E
	}

	return "unknown"str;
}
