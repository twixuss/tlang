#pragma once
#ifdef NDEBUG
#define TL_DEBUG 0
#else
#define TL_DEBUG 1
#endif
#define TL_PARENT_SOURCE_LOCATION 0
#define TL_ENABLE_PROFILER 0
#include <source_location>
#include <cstring>
inline bool operator==(std::source_location a, std::source_location b) {
	return a.column() == b.column() && a.line() == b.line() && strcmp(a.file_name(), b.file_name()) == 0;
}

#include <tl/console.h>
#undef ASSERTION_FAILURE
#define ASSERTION_FAILURE(cause_string, expression, ...) (::tl::print("Assertion failed: {}\n{}:{}: {} at {}\n", cause_string, __FILE__, __LINE__, expression, __FUNCSIG__), debug_break())
#include <tl/common.h>
#include <tl/file.h>
#include <tl/thread.h>
#include <tl/block_list.h>
#include <tl/array.h>
#include <tl/hash_map.h>
#include <tl/process.h>
#include <tl/profiler.h>
#include <tl/time.h>
using namespace tl;

#define REDECLARE_VAL(name, expr) auto _##name = expr; auto name = _##name;
#define REDECLARE_REF(name, expr) auto &_##name = expr; auto &name = _##name;

struct AstLambda;

struct SourceFileInfo {
	Span<utf8> path;
	Span<utf8> source;
	List<Span<utf8>> lines;
};

struct CompilerContext {
	Span<utf8> source_path;
	Span<utf8> source_path_without_extension;
	Span<utf8> executable_path;
	Span<utf8> executable_name;
	Span<utf8> executable_directory;
	Span<utf8> current_directory;
	AstLambda *main_lambda;
	AstLambda *build_lambda;
	Profiler profiler;
    List<PreciseTimer> phase_timers;
	int tabs = 0;
	List<SourceFileInfo> sources;
	s64 stack_word_size = 0;
	s64 register_size = 0;
	s64 general_purpose_register_count = 0;
};
extern CompilerContext context;

#define scoped_phase(message) \
		timed_block(context.profiler, as_utf8(as_span(message))); \
        context.phase_timers.add(create_precise_timer()); \
		++context.tabs; \
        defer { --context.tabs; for (int i = 0; i < context.tabs;++i) print("  "); print("{} done in {} ms.\n", message, get_time(context.phase_timers.pop()) * 1000); }

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
	Span<utf8> location;
	List<utf8> where;
	List<utf8> message;
	ReportKind kind;
};

u32 get_line_number(utf8 *from);
u32 get_column_number(utf8 *from);
List<utf8> where(utf8 *location);

void print_report(Report r);

template <>
inline umm get_hash(std::source_location l) {
	return get_hash(l.column()) ^ get_hash(l.line());
}

inline bool operator==(Span<utf8> a, char const *b) {
	return as_chars(a) == as_span(b);
}

template <class ...Args>
Report make_report(ReportKind kind, Span<utf8> location, char const *format_string, Args const &...args) {
	Report r;
	r.location = location;
	r.kind = kind;
	if (location.data) {
		r.where = where(location.data);
	}
	r.message = (List<utf8>)format(format_string, args...);
	return r;
}

template <class ...Args>
void immediate_info(Span<utf8> location, char const *format_string, Args const &...args) {
	print_report(make_report(ReportKind::info, location, format_string, args...));
}
template <class ...Args>
void immediate_info(char const *format_string, Args const &...args) {
	immediate_info(Span<utf8>{}, format_string, args...);
}

template <class ...Args>
void immediate_error(Span<utf8> location, char const *format_string, Args const &...args) {
	print_report(make_report(ReportKind::error, location, format_string, args...));
}
template <class ...Args>
void immediate_error(char const *format_string, Args const &...args) {
	immediate_error(Span<utf8>{}, format_string, args...);
}
