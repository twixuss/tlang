#include "extern.h"
LinearSet<Span<utf8>> extern_libraries;
void extern_init() {
	extern_libraries.allocator = default_allocator;
}
