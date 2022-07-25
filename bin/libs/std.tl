import "windows.tl"
import "allocator.tl"

write_to_stdout :: (str: String) {
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count as U32, null, null);
}
write_to_stdout :: (char: U8) {
    write_to_stdout(String(&char, 1));
}

StringizeCallback :: (state: *Void, str: String) #type;

stringize :: (_val: Int, state: *Void, callback: StringizeCallback) {
    val := _val;
    if val == 0 {
        callback(state, "0");
        return;
    }
    if val == 0x8000_0000_0000_0000 {
        callback(state, "-9223372036854775808");
        return;
    }
	if val < 0 {
		val = -val;
	}

    i := val;

    CAPACITY :: 64;
    buffer: U8[CAPACITY];
    dst_index := CAPACITY;

    while i != 0 {
        assert i > 0;

        digit := i % 10;

        assert digit >= 0;
        assert digit < 10;

        dst_index -= 1;
        buffer[dst_index] = @(digit + '0');

        i /= 10;
    }

    if _val < 0 {
        dst_index -= 1;
        buffer[dst_index] = '-';
    }

    callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)));
}
stringize :: (_val: UInt, state: *Void, callback: StringizeCallback) {
    val := _val;
    if val == 0 {
        callback(state, "0");
        return;
    }

    i := val;

    CAPACITY :: 64;
    buffer: U8[CAPACITY];
    dst_index := CAPACITY;

    while i != 0 {
        digit := i % 10;

        dst_index -= 1;
        buffer[dst_index] = @(digit + '0');

        i /= 10;
    }

    callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)));
}
stringize :: (v: S8 , state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback);
stringize :: (v: S16, state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback);
stringize :: (v: S32, state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback);
stringize :: (v: U8 , state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback);
stringize :: (v: U16, state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback);
stringize :: (v: U32, state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback);

stringize_hex :: (_val: UInt, state: *Void, callback: StringizeCallback) {
    val := _val;
    if val == 0 {
        callback(state, "0");
        return;
    }

	CAPACITY :: 16;

    i := val;
    buffer: U8[CAPACITY];
    dst_index := CAPACITY;

    while i != 0 {
        digit := i & 15;

        dst_index -= 1;
		
		if digit < 10
			buffer[dst_index] = @(digit + '0');
		else 
			buffer[dst_index] = @(digit + ('a' - 10));

        i >>= 4;
    }
    
	
	while true {
        dst_index -= 1;
        buffer[dst_index] = '0';
		
		if dst_index == 0
			break;
	}
	
    callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)));
}

is_nan :: (v: F32) {
    u := *(&v as *U32);
	return ((u & 0x7f800000) == 0x7f800000) && ((u & 0x007fffff) != 0);
}

is_nan :: (v: F64) {
    u := *(&v as *U64);
	return ((u & 0x7ff0000000000000) == 0x7ff0000000000000) && ((u & 0x000fffffffffffff) != 0);
}

stringize :: (_val: F64, state: *Void, callback: StringizeCallback) {
    val := _val;
    if val < 0 {
        val = -val;
        callback(state, "-");
    }
    
    // CAPACITY :: 256;
    // buffer: U8[CAPACITY];
    // begin := CAPACITY;
    // while val >= 1 {
    //     begin -= 1;
    //     buffer[begin] = @((fraction(val/10)*10) as Int - '0');
    //     val /= 10;
    // }
    //callback(state, String(@&buffer[begin], @(CAPACITY - begin)));

    /*
    // BINARY EXPONENT NOTATION
    u := *(&val as *U64);

    s := u >> 63;
    e := ((u >> 52) & 2047) - 1023;
    m := u & 4503599627370495;

    callback(state, "1.");

    precision :: 3;
    buffer: U8[precision+1];

    i := 0;
    while i < buffer.count {
        /*
        if (m & 0x8_0000_0000_0000) == 0 callback(state, "0"); 
        else                             callback(state, "1");

        m *= 2;
        
        */
             if m < 450359962737049  buffer[i] = 0; 
        else if m < 900719925474099  buffer[i] = 1;
        else if m < 1351079888211148 buffer[i] = 2;
        else if m < 1801439850948198 buffer[i] = 3;
        else if m < 2251799813685248 buffer[i] = 4;
        else if m < 2702159776422297 buffer[i] = 5;
        else if m < 3152519739159347 buffer[i] = 6;
        else if m < 3602879701896396 buffer[i] = 7;
        else if m < 4053239664633446 buffer[i] = 8;
        else                         buffer[i] = 9;

        m *= 10;
        m &= 0xF_FFFF_FFFF_FFFF;

        i += 1;
    }

    if buffer[buffer.count - 1] >= 5
        buffer[buffer.count - 2] += 1;

    i = buffer.count - 1;
    while i >= 0 {
        if buffer[i] >= 10 {
            buffer[i] -= 10;
            if i-1 >= 0
                buffer[i-1] += 1;
            else
                e += 1;
        }
        i -= 1;
    }

    i = 0;
    while i < buffer.count {
        buffer[i] += '0';
        i += 1;
    }

    callback(state, String(buffer.data, precision));
    callback(state, "*2^");
    stringize(e, state, callback);
    */

    // DECIMAL, NOT IDEAL PRECISION
    precision :: 3;
    i := 1;
    floored := 0;
    divisor := 1.0;

    while divisor * 10 < val {
        divisor *= 10;
        i += 1;
    }

    val /= divisor;

    while i > 0 {
        i -= 1;
        floored = val as S64;
        stringize(floored, state, callback);
        val -= floored as F64;
        val *= 10;
    }

    callback(state, ".");

    i = 0;
    while i < precision {
        i += 1;

        floored = val as S64;
        stringize(floored, state, callback);
        val -= floored as F64;
        val *= 10;
    }
}
stringize :: (_val: F32, state: *Void, callback: StringizeCallback) => stringize(_val as F64, state, callback);

stringize_quoted_if_string :: (value: Any, state: *Void, callback: StringizeCallback) {
	if value.type.name == "String" {
		callback(state, "\"");
		callback(state, *(value.pointer as *String));
		callback(state, "\"");
	} else {
		stringize(value, state, callback);
	}
}

stringize :: (value: Any, state: *Void, callback: StringizeCallback): Void {
	info := value.type;
	
	stringize_span :: (data: *U8, count: Int, info: *TypeInfo, state: *Void, callback: StringizeCallback): Void {
		callback(state, "[");
		for i in 0..count {
			if i != 0
                callback(state, ", ");
			
			if i == 8 && i < count - 8 {
				i = count - 8;
				callback(state, "..., ");
			}
			
			stringize_quoted_if_string(Any(data + i * info.size, info), state, callback);
		}
		callback(state, "]");
	}
	
	match info.kind {
		.Void => callback(state, "Void");
		.Bool => callback(state, if *(value.pointer as *Bool) then "true" else "false");
		.S8  => stringize(*(value.pointer as *S8), state, callback);
		.S16 => stringize(*(value.pointer as *S16), state, callback);
		.S32 => stringize(*(value.pointer as *S32), state, callback);
		.S64 => stringize(*(value.pointer as *S64), state, callback);
		.U8  => stringize(*(value.pointer as *U8), state, callback);
		.U16 => stringize(*(value.pointer as *U16), state, callback);
		.U32 => stringize(*(value.pointer as *U32), state, callback);
		.U64 => stringize(*(value.pointer as *U64), state, callback);
		.F32 => stringize(*(value.pointer as *F32), state, callback);
		.F64 => stringize(*(value.pointer as *F64), state, callback);
		.\enum => {
            enum_value := *(value.pointer as *Int);
            for i in 0..info.enum_members.count {
                if enum_value == info.enum_members[i].value {
                    callback(state, info.enum_members[i].name);
                    return;
                }
            }
        }
		.\struct => {
			if info.name == "String" {
				callback(state, *(value.pointer as *String));
			} else {
                callback(state, "{");
                for i in 0..info.members.count {
                    member := &info.members[i];
                    if i != 0
                        callback(state, ", ");
                    callback(state, member.name);
                    callback(state, "=");
                    stringize_quoted_if_string(Any(value.pointer + member.offset, member.type), state, callback);
                }
                callback(state, "}");
            }
        }
		.pointer => stringize_hex(*(value.pointer as *UInt), state, callback);
		.array => stringize_span(value.pointer, info.array_count, info.pointee, state, callback);
		.span => {
			span := *(value.pointer as *[]U8);
			stringize_span(span.data, span.count, info.pointee, state, callback);
		}
	}
}

dprint :: (value: Any) => 
    stringize(value, null, (state: *Void, str: String) {
        write_to_stdout(str);
    });
dprintln :: (value: Any) {
	dprint(value);
	write_to_stdout('\n');
}

print :: (value: Any) {
	b := StringBuilder.create();
	append(&b, value);
	write_to_stdout(to_string(&b));
	free(&b);
}

print :: (format: String, values: ..Any) {
	b := StringBuilder.create();
	append_format(&b, format, ..values);
	write_to_stdout(to_string(&b));
	free(&b);
}

println :: (value: Any) {
	print(value);
	write_to_stdout('\n');
}

println :: (value: String) {
	write_to_stdout(value);
	write_to_stdout('\n');
}

println :: (format: String, values: ..Any) {
	print(format, ..values);
	write_to_stdout('\n');
}

merge :: (a: U8, b: U8): U16 {
	return (a as U16 << 8) | b;
}

strlen :: (str: *U8): UInt {
	c := str;
	while *c != 0 c += 1;
	return c as UInt - str as UInt;
}

STRING_BUILDER_INITIAL_BUFFER_CAPACITY :: 0x1000;

StringBuilder :: struct {
	Block :: struct {
		count: Int;
		capacity: Int;
		next: *Block;
	}
    
	// allocator: Allocator = current_allocator;
	first: Block;
	initial_buffer: U8[STRING_BUILDER_INITIAL_BUFFER_CAPACITY];
	last: *Block;
	alloc_last: *Block;
    count: Int;
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
available_space :: (block: *StringBuilder.Block): Int => block.capacity - block.count;

allocate_block :: (b: *StringBuilder) {
    capacity := b.alloc_last.capacity*2;
    block := VirtualAlloc(null, (#sizeof StringBuilder.Block + capacity) as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE) as *StringBuilder.Block;
    // block := (Block *)allocator.allocate_uninitialized(sizeof(Block) + capacity, alignof(Block) TL_LA);
    block.count = 0;
    block.capacity = capacity;
    block.next = null;
    b.alloc_last = block;
    b.alloc_last.next = block;
    return block;
}

append :: (b: *StringBuilder, str: String): Int {
    remaining := str;
    while remaining.count > available_space(b.last) {
        space_in_block := available_space(b.last);
        memcpy(end(b.last), remaining.data, space_in_block as UInt);
        remaining.count -= space_in_block;
        remaining.data  += space_in_block;
        b.last.count += space_in_block;
		b.last = b.last.next;
        if b.last == null 
			b.last = allocate_block(b);
		dprintln(*b);
    }
	
    memcpy(end(b.last), remaining.data, remaining.count as UInt);
    b.last.count += remaining.count;
    b.count += str.count;
    return str.count;
}

append_char :: (b: *StringBuilder, ch: U8) => append(b, String(&ch, 1));

append :: (b: *StringBuilder, value: Any): Void {
	stringize(value, b, (state: *Void, str: String) {
        append(state as *StringBuilder, str);
    });
}

find :: (where: String, what: String): ?String {
	if (where.count - what.count + 1) as Int <= 0
		return null;
	
	same := true;
	for i in 0 .. where.count - what.count + 1 {
		same = true;
		for j in 0 .. what.count {
			if what[j] != where[i + j]
				same = false;
		}
		if same
			return String(&where[i], what.count);
	}
	return null;
}
append_format :: (b: *StringBuilder, format: String, args: ..Any): Int {
	remaining := format;

	for i in 0 .. args.count {
		y := find(remaining, "{}");
		assert y; // invalid format String
		x := *y;
		
		append(b, String(remaining.data, @(x.data - remaining.data)));
		
		append(b, args[i]);
		
		remaining.count -= x.data - remaining.data + 2;
		remaining.data = x.data + 2;
	}
	
	append(b, remaining);
	
	return 0;
}

fill :: (builder: *StringBuilder, dst_string: String): result: String {
    dst_char := dst_string.data;
    block := &builder.first;
    while block != null {
        memcpy(dst_char, begin(block), block.count as UInt);
        dst_char += block.count;
        block = block.next;
    }
    result.data = dst_string.data;
    result.count = builder.count;
}

to_string :: (builder: *StringBuilder): result: String {
    result.data = @ VirtualAlloc(null, builder.count as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE);
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