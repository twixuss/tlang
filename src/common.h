#pragma once
#define TL_PARENT_SOURCE_LOCATION 0
#define TL_ENABLE_PROFILER 1
#include <source_location>
#include <cstring>
inline bool operator==(std::source_location a, std::source_location b) {
	return a.column() == b.column() && a.line() == b.line() && strcmp(a.file_name(), b.file_name()) == 0;
}

#include <tl/common.h>
#include <tl/console.h>
#include <tl/file.h>
#include <tl/thread.h>
#include <tl/block_list.h>
#include <tl/array.h>
#include <tl/hash_map.h>
#include <tl/process.h>
#include <tl/profiler.h>
#include <tl/time.h>
using namespace tl;

struct CompilerContext {
	Span<utf8> source_path;
	Span<utf8> source_path_without_extension;
	Span<utf8> executable_path;
	Span<utf8> executable_name;
	Span<utf8> executable_directory;
	Span<utf8> current_directory;
	struct AstLambda *main_lambda;
	Profiler profiler;
    List<PreciseTimer> phase_timers;
	int tabs = 0;
};
extern CompilerContext context;

#define scoped_phase(message) \
		timed_block(context.profiler, as_utf8(as_span(message))); \
        context.phase_timers.add(create_precise_timer()); \
		++context.tabs; \
        defer { --context.tabs; for (int i = 0; i < context.tabs;++i) print("  "); print("{} done in {} s.\n", message, get_time(context.phase_timers.pop())); }

enum class Comparison : u8 {
	e,
	ne,
	l,
	le,
	g,
	ge,
};

List<utf8> where(utf8 *location);

template <>
inline umm get_hash(std::source_location l) {
	return get_hash(l.column()) ^ get_hash(l.line());
}

inline bool operator==(Span<utf8> a, char const *b) {
	return as_chars(a) == as_span(b);
}
