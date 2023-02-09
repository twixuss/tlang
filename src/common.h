#pragma once

// [ ---- Critical warnings ---- ]
#pragma warning(error: 4701)  // use of potentially uninitialized variable
#pragma warning(error: 4715)  // not all path return a value
#pragma warning(error: 26819) // implicit fallthrough in switch case

// [ ---- Insignificant warnings ---- ]

#pragma warning(disable: 4061) // not all cases are explicitly handled (even if default is used ...)
#pragma warning(disable: 4062)

#pragma warning(disable: 4201) // non standard extension: unnamed struct

#pragma warning(disable: 4514) // inline function was removed
#pragma warning(disable: 4702) // unreachable code
#pragma warning(disable: 4710) // function was not inlined
#pragma warning(disable: 5045) // spectre


// [ ---- Warnings that may be enabled in the future ---- ]
#pragma warning(disable: 4820) // struct was padded with n bytes


#if BUILD_CONFIG==0 // Debug
	#define TL_DEBUG 1
#elif BUILD_CONFIG==1 // Release
	#define TL_DEBUG 0
#elif BUILD_CONFIG==2 // UltraSpeed
	#define TL_DEBUG 0
	#define assert(...)
	#define bounds_check(...)
#endif
#define TL_ENABLE_PROFILER 0

#pragma warning(push, 0)
#include <source_location>
#include <cstring>
#pragma warning(pop)

inline bool operator==(std::source_location a, std::source_location b) {
	return a.column() == b.column() && a.line() == b.line() && strcmp(a.file_name(), b.file_name()) == 0;
}

#include <tl/forward.h>
using namespace tl;
using String = Span<utf8, u32>;

template <class ...Args>
void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, Args ...args);

template <>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function);

template <class ...Args, class Char>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, Char const *format_string, Args ...args);

template <>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, String location);

template <class ...Args, class Char>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, String location, Char const *format_string, Args ...args);

// used in ASSERTION_FAILURE so debugger points to the right place
inline void dummy() {}

#pragma warning(push, 0)
#define ASSERTION_FAILURE(cause_string, expression, ...) (tlang_assertion_failed(cause_string, __FILE__, __LINE__, expression, __FUNCSIG__, __VA_ARGS__), dummy())
#include <tl/console.h>
#include <tl/common.h>
#include <tl/file.h>
#include <tl/thread.h>
#include <tl/block_list.h>
#include <tl/array.h>
#include <tl/hash_map.h>
#include <tl/process.h>
#include <tl/profiler.h>
#include <tl/time.h>
#include <tl/ram.h>
#include <tl/pool32.h>
#include <tl/debug.h>
#include <tl/fly_string.h>
#pragma warning(pop)

struct MyAllocator : AllocatorBase<MyAllocator> {
	inline static MyAllocator current() { return {}; }

	AllocationResult allocate_impl(umm size, umm alignment, std::source_location location = std::source_location::current());
	AllocationResult reallocate_impl(void *data, umm old_size, umm new_size, umm alignment, std::source_location location = std::source_location::current());
	void deallocate_impl(void *data, umm size, umm alignment, std::source_location location = std::source_location::current());
};

void init_my_allocator();
void deinit_my_allocator();

template <class T>
using Ptr32 = typename Pool32<T>::template Ptr<T>;

// FIXME: use custom allocator
template <class T>
using SmallList = List<T, Allocator, u32>;

using HeapString = SmallList<utf8>;

// this string is used as key into hashmap
#if 0                        // parse time | type time | total
using KeyString = FlyString; // 8.3          3.5         11.8
#else
using KeyString = String;    // 6.4          5.9         12.3
#endif

// std::unordered_map is just a bit (10-15%) slower than tl::HashMap
#if 0
#include <xhash>
namespace std {
template <>
struct hash<String> {
	size_t operator()(String const &str) const {
		return get_hash(str);
	}
};
template <>
struct hash<Span<utf8>> {
	size_t operator()(Span<utf8> const &str) const {
		return get_hash(str);
	}
};
}

#include <unordered_map>
template <class Key, class Value, class Traits = DefaultHashTraits<Key>>
struct StdHashMap {
	using Hasher = typename Traits::Hasher;
	using CellState = ContiguousHashMapCellState;

	std::unordered_map<Key, Value> map;

    Value &get_or_insert(Key key) {
		return map[key];
	}

    Value *find(Key key) {
		auto found = map.find(key);
        return found == map.end() ? 0 : &found->second;
    }

	void clear() {
		map.clear();
	}
};

template <class Key, class Value, class Traits>
bool is_empty(StdHashMap<Key, Value, Traits> map) {
	return map.map.empty();
}


template <class Key, class Value, class Traits>
umm count_of(StdHashMap<Key, Value, Traits> map) {
	return map.map.size();
}


template <class Key, class Value, class Traits, class Fn>
void for_each(StdHashMap<Key, Value, Traits> map, Fn &&fn) {
    for (auto &[k, v] : map.map) {
		fn(k, v);
    }
}
#endif

#if 0
template <class K>
struct DebugHashTraits : DefaultHashTraits<K> {
	inline static constexpr void on_collision(K a, K b) {
		print("COLLISION: '{}' and '{}'\n", a, b);
	}
};

template <class K, class V>
using Map = ContiguousHashMap<K, V, DebugHashTraits<K>>;
#else
// FIXME: use custom allocator
template <class K, class V>
//using Map = HashMap<K, V>;
using Map = ContiguousHashMap<K, V>;
#endif

#pragma warning(disable: 4455)
inline constexpr String operator""str(char const *string, umm count) { return String((utf8 *)string, (String::Size)count); }
inline constexpr String operator""str(utf8 const *string, umm count) { return String((utf8 *)string, (String::Size)count); }

struct AstLambda;

struct SourceFileInfo {
	String path;
	String source;
	List<String> lines;
};

enum class SectionKind : u8 {
	data_readonly,
	data_readwrite,
	data_zero,
	code,
};

struct Relocation {
	SectionKind section;
	u64 offset;
	struct AstLambda *lambda;
};

struct Section {
	List<u8> buffer;
	List<Relocation> relocations;

	u32 w8(u64 v) {
		defer {
			buffer.add((v >>  0) & 0xff);
			buffer.add((v >>  8) & 0xff);
			buffer.add((v >> 16) & 0xff);
			buffer.add((v >> 24) & 0xff);
			buffer.add((v >> 32) & 0xff);
			buffer.add((v >> 40) & 0xff);
			buffer.add((v >> 48) & 0xff);
			buffer.add((v >> 56) & 0xff);
		};
		return buffer.count;
	}
	u32 w4(u32 v) {
		defer {
			buffer.add((v >>  0) & 0xff);
			buffer.add((v >>  8) & 0xff);
			buffer.add((v >> 16) & 0xff);
			buffer.add((v >> 24) & 0xff);
		};
		return buffer.count;
	}
	u32 w2(u16 v) {
		defer {
			buffer.add((v >>  0) & 0xff);
			buffer.add((v >>  8) & 0xff);
		};
		return buffer.count;
	}
	u32 w1(u8 v) {
		defer {
			buffer.add(v);
		};
		return buffer.count;
	}
	void align(u32 n) {
		while (buffer.count % n != 0)
			buffer.add(0);
	}
};

// first 4 registers are scratch and are used for expression evaluation
// rs is a stack pointer, and it must be aligned to 16 bytes before executing a call instruction
enum class Register : u8 {
	r0 = 0,
	r1 = 1,
	r2 = 2,
	rs = 255,
	rb = 254,
	parameters        = 253,
	return_parameters = 252,
	locals            = 251,
	temporary         = 250,
	constants         = 249,
	rwdata            = 248,
	zeros             = 247,
	instructions      = 246,
};

constexpr umm allocatable_register_first = 3;
constexpr umm allocatable_register_last  = 245;
constexpr umm register_count = 256;

using RegisterSet = BitSet<register_count>;

struct Strings {
	utf8 const *_start_marker = (utf8 *)-1;

	utf8 const *usage = 0;
	utf8 const *no_source_path_received = 0;
	utf8 const *error = 0;
	utf8 const *warning = 0;
	utf8 const *info = 0;

	utf8 const *_end_marker = (utf8 *)-1;
};

enum class Comparison : u8 {
	e,
	ne,
	l,
	le,
	g,
	ge,
};


enum class ReportKind {
	info,
	warning,
	error,
};

struct Report {
	String location;
	String message;
	ReportKind kind;
};

template <class Char>
concept CChar = is_char<Char>;

template <class ...Args, CChar Char>
inline Report make_report(ReportKind kind, String location, Char const *format_string, Args const &...args) {
	Report r;
	r.location = location;
	r.kind = kind;
	r.message = (String)format(format_string, args...);
	return r;
}

// Library name -> list of function names
using ExternLibraries = Map<String, LinearSet<String>>;

struct RelativeString {
	u32 offset;
	u32 count;
};

template <>
inline umm get_hash(std::source_location const &l) {
	return get_hash(l.column()) ^ get_hash(l.line());
}

inline bool operator==(String a, char const *b) {
	return as_chars(a) == as_span(b);
}

inline HeapString escape_string(String string) {
	if (!string.count)
		return {};

	HeapString new_string;
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

		switch (c) {
			case '"':  { new_string.add({'\\', '"'}); break; }
			case '\n': { new_string.add({'\\', 'n'}); break; }
			case '\r': { new_string.add({'\\', 'r'}); break; }
			case '\t': { new_string.add({'\\', 't'}); break; }
			case '\0': { new_string.add({'\\', '0'}); break; }
			case '\\': { new_string.add({'\\', '\\'}); break; }
			default: { new_string.add(c); break; }
		}
	}
	return new_string;
}

inline Optional<HeapString> unescape_string(String string) {

	if (!string.count)
		return HeapString{};

	if (string.front() == '"') {
		assert(string.back() == '"');
		string.data  += 1;
		string.count -= 2;
	} else if (string.front() == '\'') {
		assert(string.back() == '\'');
		string.data  += 1;
		string.count -= 2;
	}

	if (!string.count)
		return HeapString{};

	HeapString new_string;
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

template <class ...Args, class Char>
void immediate_error(String location, Char const *format_string, Args const &...args);

template <class ...Args, class Char>
void immediate_warning(String location, Char const *format_string, Args const &...args);

template <class ...Args, class Char>
void immediate_info(String location, Char const *format_string, Args const &...args);

template <>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function) {
	tlang_assertion_failed(cause, file, line, expression, function, String{}, "");
}
template <class ...Args, class Char>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, Char const *format_string, Args ...args) {
	tlang_assertion_failed(cause, file, line, expression, function, String{}, format_string, args...);
}

template <>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, String location) {
	tlang_assertion_failed(cause, file, line, expression, function, location, "");
}

inline thread_local struct AstNode *(*get_current_node)();

String dumb_get_location(struct AstNode *);

template <class ...Args, class Char>
inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function, String location, Char const *format_string, Args ...args) {
	withs(ConsoleColor::red) {
		::tl::println("================================");
		::tl::print("Assertion failed: ");
	};
	::tl::print("{}\n{}:{}: {} at {}\n", expression, file, line, cause, function);
	withs(ConsoleColor::red) {
		::tl::println("================================");
	};

	immediate_error(location, format_string, args...);

	AstNode *current_node = 0;

	if (get_current_node)
		current_node = get_current_node();

	if (current_node)
		immediate_info(dumb_get_location(current_node), "Current node:");

	if (debugger_attached())
		debug_break();
	else
		exit(-1);
}
