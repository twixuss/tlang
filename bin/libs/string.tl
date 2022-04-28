import "std.tl"
import "allocator.tl"

STRING_BUILDER_INITIAL_BUFFER_CAPACITY :: 0x4000;

StringBuilder :: struct {
	Block :: struct {
		count: uint;
		capacity: uint;
		next: *Block;
	}
    
	// allocator: Allocator = current_allocator;
	first: Block;
	initial_buffer: u8[STRING_BUILDER_INITIAL_BUFFER_CAPACITY];
	last: *Block;
	alloc_last: *Block;
    count: uint;
    create :: fn (): result: StringBuilder {
        // result.allocator = current_allocator;
        result.first.capacity = STRING_BUILDER_INITIAL_BUFFER_CAPACITY;
        result.last = &result.first;
        result.alloc_last = &result.first;
    }
}

begin :: fn (block: *StringBuilder.Block): *u8 => @&block[1];
end   :: fn (block: *StringBuilder.Block): *u8 => begin(block) + block.count;
available_space :: fn (block: *StringBuilder.Block): uint => block.capacity - block.count;

allocate_block :: fn (b: *StringBuilder) {
    capacity := b.alloc_last.capacity*2;
    block := VirtualAlloc(null, #sizeof StringBuilder.Block, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE) as *StringBuilder.Block;
    // block := (Block *)allocator.allocate_uninitialized(sizeof(Block) + capacity, alignof(Block) TL_LA);
    block.count = 0;
    block.capacity = capacity;
    block.next = null;
    b.alloc_last = block;
    b.alloc_last.next = block;
    return block;
}

append_bytes :: fn (b: *StringBuilder, data: *u8, size: uint): uint {
    chars_to_write := size;
    d := data;
    while available_space(b.last) < chars_to_write {
        space_in_block := available_space(b.last);
        memcpy(end(b.last), d, space_in_block);
        chars_to_write -= space_in_block;
        b.last.count += space_in_block;
        d += space_in_block;
        if b.last.next == null {
            allocate_block(b);
            b.last = b.last.next;
        }
    }
    memcpy(end(b.last), d, chars_to_write);
    b.last.count += chars_to_write;
    b.count += size;
    return size;
}

append :: fn (b: *StringBuilder, str: string) => append_bytes(b, str.data, str.count);


fill :: fn (builder: *StringBuilder, dst_string: string): result: string {
    dst_char := dst_string.data;
    block := &builder.first;
    while block != null {
        memcpy(dst_char, begin(block), block.count);
        dst_char += block.count;
        block = block.next;
    }
    result.data = dst_string.data;
    result.count = builder.count;
}

to_string :: fn (builder: *StringBuilder): result: string {
    result.data = @ VirtualAlloc(null, builder.count, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	result.count = builder.count;
	fill(builder, result);
}
