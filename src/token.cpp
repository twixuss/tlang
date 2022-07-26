#include "token.h"

// NOTE: this is used only in error reporting, so leak is not that important
String token_kind_to_string(TokenKind kind) {
	if (kind < 0x100) {
		switch (kind) {
			case '\n': return "\\n"str;
			case '\t': return "\\t"str;
			case '\r': return "\\r"str;
		}

		HeapString string;
		string.add(kind);
		return string;
	}

	if (kind < 0x10000) {
		HeapString string;
		string.add((kind >> 8) & 0xff);
		string.add((kind >> 0) & 0xff);
		return string;
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
