#pragma once
#define TL_PARENT_SOURCE_LOCATION 0
#define TL_ENABLE_PROFILER 0
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
using namespace tl;

extern Span<utf8> source_path;
extern Span<utf8> source_path_without_extension;
extern Span<utf8> executable_path;
extern Span<utf8> executable_name;
extern Span<utf8> executable_directory;
extern Span<utf8> current_directory;

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
