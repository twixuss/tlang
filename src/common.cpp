#include "common.h"
#define NOMINMAX
#pragma warning(push, 0)
#include <Windows.h>
#pragma warning(pop)

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
}

AllocationResult MyAllocator::allocate_impl(umm size, umm align, std::source_location location) {
	scoped_lock(allocation_mutex);

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
