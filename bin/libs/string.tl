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
append_char :: fn (b: *StringBuilder, ch: u8) => append_bytes(b, &ch, 1);

append :: fn (b: *StringBuilder, _val : int) {
    val := _val;
    if val == 0 {
        append_char(b, '0');
        return;
    }
	if val < 0 {
		append_char(b, '-');
		val = -val;
	}

    i := val;
    buffer : u8[64];
    dst_index := 64;

    while i != 0 {
        digit := i % 10;

        dst_index -= 1;
        buffer[dst_index] = @(digit + '0');

        i /= 10;
    }

    while dst_index != 64 {
        append_char(b, buffer[dst_index]);
        dst_index += 1;
    }
}
append :: fn (b: *StringBuilder, _val : uint) {
    val := _val;
    if val == 0 {
        append_char(b, '0');
        return;
    }

    i := val;
    buffer : u8[64];
    dst_index := 64;

    while i != 0 {
        digit := i % 10;

        dst_index -= 1;
        buffer[dst_index] = @(digit + '0');

        i /= 10;
    }

    while dst_index != 64 {
        append_char(b, buffer[dst_index]);
        dst_index += 1;
    }
}
append :: fn (b: *StringBuilder, v: s8 ) => append(b, v as s64);
append :: fn (b: *StringBuilder, v: s16) => append(b, v as s64);
append :: fn (b: *StringBuilder, v: s32) => append(b, v as s64);
append :: fn (b: *StringBuilder, v: u8 ) => append(b, v as u64);
append :: fn (b: *StringBuilder, v: u16) => append(b, v as u64);
append :: fn (b: *StringBuilder, v: u32) => append(b, v as u64);

append :: fn (b: *StringBuilder, value: any): void {
	info := value.type;
	match info.kind {
		.s8  => append(b, *(value.pointer as *s8));
		.s16 => append(b, *(value.pointer as *s16));
		.s32 => append(b, *(value.pointer as *s32));
		.s64 => append(b, *(value.pointer as *s64));
		.u8  => append(b, *(value.pointer as *u8));
		.u16 => append(b, *(value.pointer as *u16));
		.u32 => append(b, *(value.pointer as *u32));
		.u64 => append(b, *(value.pointer as *u64));
		.\struct => {
			if info.name == "string" {
				append(b, *(value.pointer as *string));
				return;
			}
			append(b, "{");
			i := 0;
			while i < info.members.count {
				member := &info.members[i];
				if i != 0
					append(b, ", ");
				append(b, member.name);
				append(b, "=");
				if member.type.name == "string" {
					append(b, "\"");
					append(b, *((value.pointer + member.offset) as *string));
					append(b, "\"");
				} else {
					append(b, any(value.pointer + member.offset, member.type));
				}
				i += 1;
			}
			append(b, "}");
		}
		.pointer => append(b, value.pointer as uint);
		.array => {
			i :u64 = 0;
			append(b, "[");
			
			if info.array_count > 16 {
				while i < 8 {
					if i != 0
						append(b, ", ");
						
					append(b, any(value.pointer + i * info.pointee.size, info.pointee));
					i += 1;
				}
				append(b, ", ...");
				
				i = info.array_count - 8;
				while i < info.array_count {
					append(b, ", ");
					append(b, any(value.pointer + i * info.pointee.size, info.pointee));
					i += 1;
				}
			} else {
				while i < info.array_count {
					if i != 0
						append(b, ", ");
						
					append(b, any(value.pointer + i * info.pointee.size, info.pointee));
					i += 1;
				}
			}
			append(b, "]");
		}
		else => append(b, "unknown");
	}
}


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

free :: fn (builder: *StringBuilder) {
	block := builder.first.next;
	while block != null {
		next := block.next;
		// builder.allocator.free_t(block);
		VirtualFree(block, 0, MEM_RELEASE);
		block = next;
	}
	// builder.allocator = {};
}