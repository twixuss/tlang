import "std.tl"
import "allocator.tl"

STRING_BUILDER_INITIAL_BUFFER_CAPACITY :: 0x4000;

StringBuilder :: struct {
	Block :: struct {
		count: UInt;
		capacity: UInt;
		next: *Block;
	}
    
	// allocator: Allocator = current_allocator;
	first: Block;
	initial_buffer: U8[STRING_BUILDER_INITIAL_BUFFER_CAPACITY];
	last: *Block;
	alloc_last: *Block;
    count: UInt;
}
create :: (this :: StringBuilder): result: StringBuilder {
	using result;
	// allocator = current_allocator;
	first.capacity = STRING_BUILDER_INITIAL_BUFFER_CAPACITY;
	last = &first;
	alloc_last = &first;
}

begin :: (block: *StringBuilder.Block): *U8 => @&block[1];
end   :: (block: *StringBuilder.Block): *U8 => begin(block) + block.count;
available_space :: (block: *StringBuilder.Block): UInt => block.capacity - block.count;

allocate_block :: (b: *StringBuilder) {
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

append_bytes :: (b: *StringBuilder, data: *U8, size: UInt): UInt {
    chars_to_write := size;
    d := data;

	xxxyyy := b;

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

append :: (b: *StringBuilder, str: String) => append_bytes(b, str.data, str.count);
append_char :: (b: *StringBuilder, ch: U8) => append_bytes(b, &ch, 1);

append :: (b: *StringBuilder, _val: Int) {
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
    buffer: U8[64];
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
append :: (b: *StringBuilder, _val: UInt) {
    val := _val;
    if val == 0 {
        append_char(b, '0');
        return;
    }

    i := val;
    buffer: U8[64];
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
append_hex :: (b: *StringBuilder, _val: UInt) {
    val := _val;
    if val == 0 {
        append(b, "0");
        return;
    }

	max_char_count: UInt: 16;

    i := val;
    buffer: U8[max_char_count];
    dst_index := max_char_count;

    while i != 0 {
        digit := i & 15;

        dst_index -= 1;
		
		if digit < 10
			buffer[dst_index] = @(digit + '0');
		else 
			buffer[dst_index] = @(digit + ('a' - 10));

        i >>= 4;
    }
    
    dst_index -= 1;
	
	while true {
		buffer[dst_index] = '0';
		
		if dst_index == 0
			break;
		dst_index -= 1;
	}
	
	append(b, String(&buffer[dst_index], max_char_count - dst_index));
}

append :: (b: *StringBuilder, v: S8 ) => append(b, v as S64);
append :: (b: *StringBuilder, v: S16) => append(b, v as S64);
append :: (b: *StringBuilder, v: S32) => append(b, v as S64);
append :: (b: *StringBuilder, v: U8 ) => append(b, v as U64);
append :: (b: *StringBuilder, v: U16) => append(b, v as U64);
append :: (b: *StringBuilder, v: U32) => append(b, v as U64);

append :: (b: *StringBuilder, value: Any): Void {
	info := value.type;
	
	append_span :: (b: *StringBuilder, data: *U8, count: UInt, info: *TypeInfo): Void {
		append(b, "[");
		i: UInt = 0;
		while i < count {
			if i != 0
				append(b, ", ");
			
			if i == 8 && i < count - 8 as UInt {
				i = count - 8;
				append(b, "..., ");
			}
			
			append(b, Any(data + i * info.size, info));
			i += 1;
		}
		append(b, "]");
	}
	
	match info.kind {
		.Bool => append(b, if *(value.pointer as *Bool) then "true" else "false");
		.S8  => append(b, *(value.pointer as *S8));
		.S16 => append(b, *(value.pointer as *S16));
		.S32 => append(b, *(value.pointer as *S32));
		.S64 => append(b, *(value.pointer as *S64));
		.U8  => append(b, *(value.pointer as *U8));
		.U16 => append(b, *(value.pointer as *U16));
		.U32 => append(b, *(value.pointer as *U32));
		.U64 => append(b, *(value.pointer as *U64));
		.\struct => {
			if info.name == "String" {
				append(b, *(value.pointer as *String));
				return;
			}
			append(b, "{");
			i: UInt = 0;
			while i < info.members.count {
				member := &info.members[i];
				if i != 0
					append(b, ", ");
				append(b, member.name);
				append(b, "=");
				if member.type.name == "String" {
					append(b, "\"");
					append(b, *((value.pointer + member.offset) as *String));
					append(b, "\"");
				} else {
					append(b, Any(value.pointer + member.offset, member.type));
				}
				i += 1;
			}
			append(b, "}");
		}
		.pointer => append_hex(b, *(value.pointer as *UInt));
		.array => append_span(b, value.pointer, info.array_count, info.pointee);
		.span => {
			span := *(value.pointer as *[]U8);
			append_span(b, span.data, span.count, info.pointee);
		}
		else => append(b, "unknown");
	}
}

find :: (where: String, what: String): ?String {
	if (where.count - what.count + 1) as Int <= 0
		return null;
	
	i: UInt = 0;
	same := true;
	j: UInt = 0;
	while i < where.count - what.count + 1 {
		defer i += 1;
		same = true;
		j = 0;
		while j < what.count {
			defer j += 1;
			if what[j] != where[i + j]
				same = false;
		}
		if same
			return String(&where[i], what.count);
	}
	return null;
}
append_format :: (b: *StringBuilder, format: String, args: ..Any): UInt {
	remaining := format;

	i: UInt = 0;
	while i != args.count {
		y := find(remaining, "{}");
		assert y; // invalid format String
		x := *y;
		
		append(b, String(remaining.data, @(x.data - remaining.data)));
		
		append(b, args[i]);
		
		remaining.count -= (x.data - remaining.data) as UInt + 2;
		remaining.data = x.data + 2;
		
		i += 1;
	}
	
	append(b, remaining);
	
	return 0;
}

fill :: (builder: *StringBuilder, dst_string: String): result: String {
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

to_string :: (builder: *StringBuilder): result: String {
    result.data = @ VirtualAlloc(null, builder.count, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
	result.count = builder.count;
	fill(builder, result);
}

free :: (builder: *StringBuilder) {
	block := builder.first.next;
	while block != null {
		next := block.next;
		// builder.allocator.free_t(block);
		VirtualFree(block, 0, MEM_RELEASE);
		block = next;
	}
	// builder.allocator = {};
}