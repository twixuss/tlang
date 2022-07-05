#pragma once
#pragma warning(error: 4715) // not all path return a value

#define TRACK_ALLOCATIONS 0

#if BUILD_CONFIG==0 // Debug
	#define TL_DEBUG 1
#elif BUILD_CONFIG==1 // Release
	#define TL_DEBUG 0
#elif BUILD_CONFIG==2 // UltraSpeed
	#define TL_DEBUG 0
	#define assert(...)
	#define bounds_check(...)
#endif
#define TL_PARENT_SOURCE_LOCATION TRACK_ALLOCATIONS
#define TL_ENABLE_PROFILER 0
#include <source_location>
#include <cstring>
inline bool operator==(std::source_location a, std::source_location b) {
	return a.column() == b.column() && a.line() == b.line() && strcmp(a.file_name(), b.file_name()) == 0;
}

void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function);

template <class ...T>
inline void print(T ...args) {
	::tl::print(args...);
}
inline void print() {
}

#include <tl/console.h>
#undef ASSERTION_FAILURE
#if TL_DEBUG
#define ASSERTION_FAILURE(cause_string, expression, ...) (\
	::tl::set_console_color(::tl::ConsoleColor::red),\
	::tl::print("Assertion failed: "),\
	::tl::set_console_color(::tl::ConsoleColor::dark_gray),\
	::tl::print("{}\nMessage: ", expression),\
	::print(__VA_ARGS__),\
	::tl::print("\n{}:{}: {} at {}\n", __FILE__, __LINE__, cause_string, __FUNCSIG__),\
	debug_break()\
)
#else
#define ASSERTION_FAILURE(cause_string, expression, ...) tlang_assertion_failed(cause_string, __FILE__, __LINE__, expression, __FUNCSIG__)
#endif
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
using namespace tl;

#define REDECLARE_VAL(name, expr) auto _##name = expr; auto name = _##name;
#define REDECLARE_REF(name, expr) auto &_##name = expr; auto &name = _##name;

struct MyAllocator : AllocatorBase<MyAllocator> {
	inline static MyAllocator current() { return {}; }

	AllocationResult allocate_impl(umm size, umm alignment, std::source_location location = std::source_location::current());
	AllocationResult reallocate_impl(void *data, umm old_size, umm new_size, umm alignment, std::source_location location = std::source_location::current());
	void deallocate_impl(void *data, umm size, umm alignment, std::source_location location = std::source_location::current());
};

void init_my_allocator();

template <class T>
using Ptr32 = typename Pool32<T>::template Ptr<T>;

template <class T>
using SmallList = List<T, MyAllocator, u32>;

using String = Span<utf8, u32>;
using HeapString = SmallList<utf8>;

// this string is used as key into hashmap
#if 0                        // parse time | type time | total
using KeyString = FlyString; // 8.3          3.5         11.8
#else
using KeyString = String;    // 6.4          5.9         12.3
#endif

// std::unordered_map is just a bit (10-15%) slower than tl::HashMap
#if 1
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
template <class Key, class Value, class Traits = DefaultHashTraits<Key, Value>>
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
template <class K, class V>
struct DebugHashTraits : DefaultHashTraits<K, V> {
	inline static constexpr void on_collision(K a, K b) {
		print("COLLISION: '{}' and '{}'\n", a, b);
	}
};

template <class K, class V>
using Map = ContiguousHashMap<K, V, DebugHashTraits<K, V>>;
#else
template <class K, class V>
using Map = HashMap<K, V>;
//using Map = ContiguousHashMap<K, V>;
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

struct Section {
	BlockList<u8> buffer;
	List<u64> relocations;

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

struct CompilerContext {
	String source_path;
	String source_path_without_extension;
	String output_path;
	String compiler_path;
	String compiler_name;
	String compiler_directory;
	String current_directory;
	AstLambda *main_lambda;
	AstLambda *build_lambda;
	Profiler profiler;
    List<PreciseTimer> phase_timers;
	int tabs = 0;

	// Need pointer stability.
	// Maybe use different data structure.
	LinkedList<SourceFileInfo> sources;

	s64 stack_word_size = 0;
	s64 register_size = 0;
	s64 general_purpose_register_count = 0;
	bool do_profile = false;
	bool keep_temp = false;
	bool debug_poly = false;
	bool print_lowered = false;
	bool optimize = false;

	u8 optimization_pass_count = 4;

	List<AstLambda *> lambdas_with_body;
	List<AstLambda *> lambdas_without_body;

	Section constant_section;
	Section data_section;
	s64 zero_section_size = 0;

	HashMap<String, s64> string_set;
};
extern CompilerContext context;

#define scoped_phase(message) \
		/*sleep_milliseconds(1000);*/ \
		timed_block(context.profiler, as_utf8(as_span(message))); \
        context.phase_timers.add(create_precise_timer()); \
		++context.tabs; \
        defer { if(!context.do_profile) return; --context.tabs; for (int i = 0; i < context.tabs;++i) print("  "); print("{} done in {} ms.\n", message, get_time(context.phase_timers.pop()) * 1000); }

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
	HeapString message;
	ReportKind kind;
};

u32 get_line_number(Span<String> lines, utf8 *from);
u32 get_line_number(utf8 *from);

u32 get_column_number(utf8 *from);

HeapString where(SourceFileInfo *info, utf8 *location);
HeapString where(utf8 *location);

void print_report(Report r);

template <>
inline umm get_hash(std::source_location const &l) {
	return get_hash(l.column()) ^ get_hash(l.line());
}

inline bool operator==(String a, char const *b) {
	return as_chars(a) == as_span(b);
}

template <class Char>
concept CChar = is_char<Char>;

template <class ...Args, CChar Char>
Report make_report(ReportKind kind, String location, Char const *format_string, Args const &...args) {
	Report r;
	r.location = location;
	r.kind = kind;
	r.message = (HeapString)format<MyAllocator>(format_string, args...);
	return r;
}

template <class ...Args, class Char>
void immediate_info(String location, Char const *format_string, Args const &...args) {
	print_report(make_report(ReportKind::info, location, format_string, args...));
}
template <class ...Args, class Char>
void immediate_info(Char const *format_string, Args const &...args) {
	immediate_info(String{}, format_string, args...);
}

template <class ...Args, class Char>
void immediate_error(String location, Char const *format_string, Args const &...args) {
	print_report(make_report(ReportKind::error, location, format_string, args...));
}
template <class ...Args, class Char>
void immediate_error(Char const *format_string, Args const &...args) {
	immediate_error(String{}, format_string, args...);
}

HeapString escape_string(String string);
Optional<HeapString> unescape_string(String string);

struct Strings {
	utf8 const *_start_marker = (utf8 *)-1;

	utf8 const *usage = 0;
	utf8 const *no_source_path_received = 0;
	utf8 const *error = 0;
	utf8 const *warning = 0;
	utf8 const *info = 0;

	utf8 const *_end_marker = (utf8 *)-1;
};

extern Strings strings;
extern const Strings strings_en;
extern const Strings strings_ru;

inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function) {
	with(ConsoleColor::red, ::tl::print("Assertion failed: "));
	::tl::print("{}\n{}:{}: {} at {}\n", expression, file, line, cause, function);
	if (debugger_attached())
		debug_break();
	else
		exit(-1);
}
