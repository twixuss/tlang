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

constexpr umm allocatable_register_start = 3;
constexpr umm allocatable_register_end   = 247;
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

struct Compiler {
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

	Strings strings;

	SourceFileInfo *get_source_info(utf8 *location) {
		for (auto &source : sources) {
			if (source.source.begin() <= location && location < source.source.end()) {
				return &source;
			}
		}
		return 0;
	}

	u32 get_line_number(Span<String> lines, utf8 *from) {
		// lines will be empty at lexing time.
		// So if an error occurs at lexing time,
		// slower algorithm is executed.
		if (lines.count) {
	#if 1
			// binary search
			auto begin = lines.data;
			auto end = lines.data + lines.count;
			while (1) {
				auto line = begin + (end - begin) / 2;
				if (line->data <= from && from < line->data + line->count) {
					return line - lines.data + 1;
				}
				if (from < line->data) {
					end = line;
				} else {
					begin = line + 1;
				}
			}
			invalid_code_path();
	#else
			for (auto &line : lines) {
				if (line.begin() <= from && from < line.end()) {
					return &line - lines.data;
				}
			}
			invalid_code_path();
	#endif
		} else {
			u32 result = 1;
			while (*--from != 0)
				result += (*from == '\n');
			return result;
		}
	}
	u32 get_line_number(utf8 *from) {
		auto info = get_source_info(from);
		return info ? get_line_number(info->lines, from) : 0;
	}

	u32 get_column_number(utf8 *from) {
		u32 result = 0;
		while (1) {
			if (*from == '\n' || *from == '\0')
				break;

			if (*from == '\t')
				result += 4;
			else
				result += 1;

			from -= 1;
		}
		return result;
	}

	void print_replacing_tabs_with_4_spaces(Span<utf8> string) {
		for (auto c : string) {
			if (c == '\t') {
				print("    ");
			} else {
				print(c);
			}
		}
	}
	ConsoleColor get_console_color(ReportKind kind) {
		switch (kind) {
			using enum ReportKind;
			using enum ConsoleColor;
			case info: return dark_gray;
			case warning: return yellow;
			case error: return red;
		}
		invalid_code_path();
	}

	void print_source_line(SourceFileInfo *info, ReportKind kind, Span<utf8> location) {

		if (!location.data) {
			// print("(null location)\n\n");
			return;
		}
		if (!info) {
			return;
		}


		auto error_line_begin = location.begin();
		if (*error_line_begin != 0) {
			while (1) {
				error_line_begin--;
				if (*error_line_begin == 0 || *error_line_begin == '\n') {
					error_line_begin++;
					break;
				}
			}
		}

		auto error_line_end = location.end();
		while (1) {
			if (*error_line_end == 0 || *error_line_end == '\n') {
				break;
			}
			error_line_end++;
		}


		auto error_line = Span(error_line_begin, error_line_end);
		auto error_line_number = get_line_number(info->lines, error_line_begin);

		auto print_line = [&](auto line) {
			return print("{} | ", Format{line, align_right(5, ' ')});
		};

		// I don't know if previous line is really useful
	#if 0
		if (error_line.data[-1] != 0) {
			auto prev_line_end = error_line.data - 1;
			auto prev_line_begin = prev_line_end - 1;

			while (1) {
				if (*prev_line_begin == 0) {
					prev_line_begin += 1;
					break;
				}

				if (*prev_line_begin == '\n') {
					++prev_line_begin;
					break;
				}

				--prev_line_begin;
			}
			auto prev_line = Span(prev_line_begin, prev_line_end);
			auto prev_line_number = get_line_number(prev_line_begin);

			print_line(prev_line_number);
			print_replacing_tabs_with_4_spaces(Print_info, prev_line);
			print('\n');
		}
	#endif

		auto line_start = Span(error_line.begin(), location.begin());
		auto line_end   = Span(location.end(), error_line.end());
		auto offset = print_line(error_line_number);
		print_replacing_tabs_with_4_spaces(line_start);
		with(get_console_color(kind), print_replacing_tabs_with_4_spaces(location));
		print_replacing_tabs_with_4_spaces(line_end);
		print('\n');

		for (u32 i = 0; i < offset; ++i) {
			print(' ');
		}
		for (auto c : line_start) {
			if (c == '\t') {
				print("    ");
			} else {
				print(' ');
			}
		}
		for (auto c : location) {
			if (c == '\t') {
				print("^^^^");
			} else {
				print('^');
			}
		}
		print("\n");
	}

	List<utf8> where(SourceFileInfo *info, utf8 *location) {
		if (location) {
			if (info) {
				return format(u8"{}:{}:{}", parse_path(info->path).name_and_extension(), get_line_number(info->lines, location), get_column_number(location));
			}
		}
		return {};
	}
	List<utf8> where(utf8 *location) {
		return where(get_source_info(location), location);
	}

	void print_report(Report r) {
		auto source_info = r.location.data ? get_source_info(r.location.data) : 0;
		if (r.location.data) {
			if (source_info) {
				print("{}: ", where(source_info, r.location.data));
			}
		} else {
			print(" ================ ");
		}
		withs(get_console_color(r.kind),
			switch (r.kind) {
				case ReportKind::info:    print(strings.info   ); break;
				case ReportKind::warning: print(strings.warning); break;
				case ReportKind::error:	  print(strings.error  ); break;
				default: invalid_code_path();
			}
		);
		print(": {}\n", r.message);
		print_source_line(source_info, r.kind, r.location);
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

};
extern Compiler compiler;

#define scoped_phase(message) \
		/*sleep_milliseconds(1000);*/ \
		timed_block(compiler.profiler, as_utf8(as_span(message))); \
        compiler.phase_timers.add(create_precise_timer()); \
		++compiler.tabs; \
        defer { if(!compiler.do_profile) return; --compiler.tabs; for (int i = 0; i < compiler.tabs;++i) print("  "); print("{} done in {} ms.\n", message, get_time(compiler.phase_timers.pop().value()) * 1000); }

template <>
inline umm get_hash(std::source_location const &l) {
	return get_hash(l.column()) ^ get_hash(l.line());
}

inline bool operator==(String a, char const *b) {
	return as_chars(a) == as_span(b);
}

HeapString escape_string(String string);
Optional<HeapString> unescape_string(String string);

inline void tlang_assertion_failed(char const *cause, char const *file, int line, char const *expression, char const *function) {
	with(ConsoleColor::red, ::tl::print("Assertion failed: "));
	::tl::print("{}\n{}:{}: {} at {}\n", expression, file, line, cause, function);
	if (debugger_attached())
		debug_break();
	else
		exit(-1);
}

inline u32 get_line_number(utf8 *from) {
	return compiler.get_line_number(from);
}
inline u32 get_column_number(utf8 *from) {
	return compiler.get_column_number(from);
}
inline List<utf8> where(utf8 *location) { return compiler.where(location); }


template <class ...Args, class Char>
void immediate_info(String location, Char const *format_string, Args const &...args) {
	compiler.immediate_info(location, format_string, args...);
}
template <class ...Args, class Char>
void immediate_info(Char const *format_string, Args const &...args) {
	compiler.immediate_info(String{}, format_string, args...);
}

template <class ...Args, class Char>
void immediate_error(String location, Char const *format_string, Args const &...args) {
	compiler.immediate_error(location, format_string, args...);
}
template <class ...Args, class Char>
void immediate_error(Char const *format_string, Args const &...args) {
	compiler.immediate_error(String{}, format_string, args...);
}
