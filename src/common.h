#pragma once
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
