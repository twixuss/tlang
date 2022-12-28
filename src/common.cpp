#include "common.h"
#define NOMINMAX
#pragma warning(push, 0)
#include <Windows.h>
#include <algorithm>
#pragma warning(pop)

#define TRACK_ALLOCATIONS 0

#if TRACK_ALLOCATIONS

static HashMap<std::source_location, u32> allocation_sizes;

void print_allocation_count() {
	print("Allocations:\n");
	struct AllocationInfo {
		std::source_location location;
		u32 size;
	};

	List<AllocationInfo> allocations;
	allocations.allocator = temporary_allocator; // otherwise `allocation_sizes` will update inside `for_each`

	for_each(allocation_sizes, [&](std::source_location location, u32 size) {
		allocations.add({location, size});
	});

	std::sort(allocations.begin(), allocations.end(), [](auto a, auto b) {
		return a.size > b.size;
	});

	for (auto a : allocations) {
		print("{}: {}\n", a.location, format_bytes(a.size));
	}
}
#endif

SourceFileInfo *get_source_info(utf8 *location);


struct AllocationBlock {
	u8 *base   = 0;
	u8 *cursor = 0;
	umm size   = 0;
};

List<AllocationBlock> ast_allocation_blocks;
u32 last_allocation_block_index;
Mutex allocation_mutex;

umm ast_allocation_block_size;

u32 debug_allocation_blocks = 0;

void new_my_block() {
	AllocationBlock block;
	block.size = ast_allocation_block_size;
	block.cursor = block.base = page_allocator.allocate_uninitialized<u8>(ast_allocation_block_size);
	assert(block.cursor);
	ast_allocation_blocks.add(block);
	last_allocation_block_index += 1;
	atomic_increment(&debug_allocation_blocks);
}

void init_my_allocator() {
	ast_allocation_blocks.allocator = os_allocator;
	ast_allocation_block_size = 1024*1024;
	last_allocation_block_index = (u32)-1;
	new_my_block();
	ast_allocation_block_size *= 2;


#if TRACK_ALLOCATIONS
	debug_init();
	allocation_sizes.allocator = os_allocator;
	defer { print_allocation_count(); };
#endif
}
void deinit_my_allocator() {
#if TRACK_ALLOCATIONS
	print_allocation_count();
#endif
}

AllocationResult MyAllocator::allocate_impl(umm size, umm align, std::source_location location) {
	scoped_lock(allocation_mutex);

#if TRACK_ALLOCATIONS
	allocation_sizes.get_or_insert(location) += size;
#endif

retry:
	auto block = &ast_allocation_blocks[last_allocation_block_index];

	u8 *target = (u8 *)(((umm)block->cursor + align - 1) & ~(align - 1));
	if ((umm)((u8 *)block->base + block->size - target) < size) {
		ast_allocation_block_size *= 2;
		while (ast_allocation_block_size < size + align - 1) {
			ast_allocation_block_size *= 2;
		}
		new_my_block();
		goto retry;
	}

	block->cursor = target + size;

	return {
		.data = target,
		.count = size,
		.is_zeroed = false,
	};
}
AllocationResult MyAllocator::reallocate_impl(void *data, umm old_size, umm new_size, umm align, std::source_location location) {
	auto result = allocate_impl(new_size, align);
	memcpy(result.data, data, old_size);
	return result;
}

void MyAllocator::deallocate_impl(void *data, umm size, umm align, std::source_location location) {
	(void)data;
	(void)size;
}
