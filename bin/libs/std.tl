import "windows.tl"
import "allocator.tl"

write_to_stdout :: (str: String) {
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count as U32, null, null)
    return str.count
}

write_to_stdout :: (char: U8) =>
    write_to_stdout(String(&char, 1))

StringizeCallback :: (state: *Void, str: String): Int #type

stringize :: (_val: Int, state: *Void, callback: StringizeCallback) {
    val := _val
    if val == 0 {
        return callback(state, "0")
    }
    if val == 0x8000_0000_0000_0000 {
        return callback(state, "-9223372036854775808")
    }
	if val < 0 {
		val = -val
	}

    i := val

    CAPACITY :: 64
    buffer: U8[CAPACITY]
    dst_index := CAPACITY

    while i != 0 {
        assert i > 0

        digit := i % 10

        assert digit >= 0
        assert digit < 10

        dst_index -= 1
        buffer[dst_index] = @(digit + '0')

        i /= 10
    }

    if _val < 0 {
        dst_index -= 1
        buffer[dst_index] = '-'
    }

    return callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)))
}
stringize :: (_val: UInt, state: *Void, callback: StringizeCallback) {
    val := _val
    if val == 0 {
        return callback(state, "0")
    }

    i := val

    CAPACITY :: 64
    buffer: U8[CAPACITY]
    dst_index := CAPACITY

    while i != 0 {
        digit := i % 10

        dst_index -= 1
        buffer[dst_index] = @(digit + '0')

        i /= 10
    }

    return callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)))
}
stringize :: (v: S8 , state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback)
stringize :: (v: S16, state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback)
stringize :: (v: S32, state: *Void, callback: StringizeCallback) => stringize(v as S64, state, callback)
stringize :: (v: U8 , state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback)
stringize :: (v: U16, state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback)
stringize :: (v: U32, state: *Void, callback: StringizeCallback) => stringize(v as U64, state, callback)

stringize_hex :: (_val: UInt, state: *Void, callback: StringizeCallback) {
    val := _val
    if val == 0 {
        return callback(state, "0")
    }

	CAPACITY :: 16

    i := val
    buffer: U8[CAPACITY]
    dst_index := CAPACITY

    while i != 0 {
        digit := i & 15

        dst_index -= 1

		if digit < 10
			buffer[dst_index] = @(digit + '0')
		else
			buffer[dst_index] = @(digit + ('a' - 10))

        i >>= 4
    }


	while true {
        dst_index -= 1
        buffer[dst_index] = '0'

		if dst_index == 0
			break
	}

    return callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)))
}

is_nan :: (me: F32) {
    u := *(&me as *U32)
	return ((u & 0x7f800000) == 0x7f800000) && ((u & 0x007fffff) != 0)
}

is_nan :: (me: F64) {
    u := *(&me as *U64)
	return ((u & 0x7ff0000000000000) == 0x7ff0000000000000) && ((u & 0x000fffffffffffff) != 0)
}

stringize :: (_val: F64, state: *Void, callback: StringizeCallback) {
    bytes_written: Int

    val := _val

    if val.is_nan() {
        return callback(state, "NaN")
    }

    if val < 0 {
        val = -val
        bytes_written += callback(state, "-")
    }

    // CAPACITY :: 256
    // buffer: U8[CAPACITY]
    // begin := CAPACITY
    // while val >= 1 {
    //     begin -= 1
    //     buffer[begin] = @((fraction(val/10)*10) as Int - '0')
    //     val /= 10
    // }
    //callback(state, String(@&buffer[begin], @(CAPACITY - begin)))

    /*
    // BINARY EXPONENT NOTATION
    u := *(&val as *U64)

    s := u >> 63
    e := ((u >> 52) & 2047) - 1023
    m := u & 4503599627370495

    callback(state, "1.")

    precision :: 3
    buffer: U8[precision+1]

    i := 0
    while i < buffer.count {
        /*
        if (m & 0x8_0000_0000_0000) == 0 callback(state, "0")
        else                             callback(state, "1")

        m *= 2

        */
             if m < 450359962737049  buffer[i] = 0
        else if m < 900719925474099  buffer[i] = 1
        else if m < 1351079888211148 buffer[i] = 2
        else if m < 1801439850948198 buffer[i] = 3
        else if m < 2251799813685248 buffer[i] = 4
        else if m < 2702159776422297 buffer[i] = 5
        else if m < 3152519739159347 buffer[i] = 6
        else if m < 3602879701896396 buffer[i] = 7
        else if m < 4053239664633446 buffer[i] = 8
        else                         buffer[i] = 9

        m *= 10
        m &= 0xF_FFFF_FFFF_FFFF

        i += 1
    }

    if buffer[buffer.count - 1] >= 5
        buffer[buffer.count - 2] += 1

    i = buffer.count - 1
    while i >= 0 {
        if buffer[i] >= 10 {
            buffer[i] -= 10
            if i-1 >= 0
                buffer[i-1] += 1
            else
                e += 1
        }
        i -= 1
    }

    i = 0
    while i < buffer.count {
        buffer[i] += '0'
        i += 1
    }

    callback(state, String(buffer.data, precision))
    callback(state, "*2^")
    stringize(e, state, callback)
    */

    // DECIMAL, NOT IDEAL PRECISION
    precision :: 3
    i := 1
    floored := 0
    divisor := 1.0

    while divisor * 10 < val {
        divisor *= 10
        i += 1
    }

    val /= divisor

    while i > 0 {
        i -= 1
        floored = val as S64
        bytes_written += stringize(floored, state, callback)
        val -= floored as F64
        val *= 10
    }

    bytes_written += callback(state, ".")

    i = 0
    while i < precision {
        i += 1

        floored = val as S64
        bytes_written += stringize(floored, state, callback)
        val -= floored as F64
        val *= 10
    }

    return bytes_written
}
stringize :: (_val: F32, state: *Void, callback: StringizeCallback) => stringize(_val as F64, state, callback)

stringize_quoted_if_string :: (value: Any, state: *Void, callback: StringizeCallback) =>
	if value.type.name == "String" then
        callback(state, "\"") +
        callback(state, *(value.pointer as *String)) +
        callback(state, "\"")
	else
		stringize(value, state, callback)


stringize :: (value: Any, state: *Void, callback: StringizeCallback): Int {
	info := value.type

	stringize_span :: (data: *U8, count: Int, info: *TypeInfo, state: *Void, callback: StringizeCallback): Int {
        bytes_written: Int
		bytes_written += callback(state, "[")
		for i in 0..count {
			if i != 0
                bytes_written += callback(state, ", ")

			if i == 8 && i < count - 8 {
				i = count - 8
				bytes_written += callback(state, "..., ")
			}

			bytes_written += stringize_quoted_if_string(Any(data + i * info.size, info), state, callback)
		}
		bytes_written += callback(state, "]")
	}

	match info.kind {
		.Void => return callback(state, "Void")
		.Bool => return callback(state, if *(value.pointer as *Bool) then "true" else "false")
		.S8  => return stringize(*(value.pointer as *S8), state, callback)
		.S16 => return stringize(*(value.pointer as *S16), state, callback)
		.S32 => return stringize(*(value.pointer as *S32), state, callback)
		.S64 => return stringize(*(value.pointer as *S64), state, callback)
		.U8  => return stringize(*(value.pointer as *U8), state, callback)
		.U16 => return stringize(*(value.pointer as *U16), state, callback)
		.U32 => return stringize(*(value.pointer as *U32), state, callback)
		.U64 => return stringize(*(value.pointer as *U64), state, callback)
		.F32 => return stringize(*(value.pointer as *F32), state, callback)
		.F64 => return stringize(*(value.pointer as *F64), state, callback)
		.\enum => {
            enum_value := *(value.pointer as *Int)
            for i in 0..info.enum_members.count {
                if enum_value == info.enum_members[i].value {
                    return callback(state, info.enum_members[i].name)
                }
            }
        }
		.\struct => {
			if info.name == "String" {
				return callback(state, *(value.pointer as *String))
			} else {
                bytes_written: Int
                bytes_written += callback(state, "{")
                for i in 0..info.members.count {
                    member := &info.members[i]
                    if i != 0
                        bytes_written += callback(state, ", ")
                    bytes_written += callback(state, member.name)
                    bytes_written += callback(state, "=")
                    bytes_written += stringize_quoted_if_string(Any(value.pointer + member.offset, member.type), state, callback)
                }
                bytes_written += callback(state, "}")
                return bytes_written
            }
        }
		.pointer => return stringize_hex(*(value.pointer as *UInt), state, callback)
		.array => return stringize_span(value.pointer, info.array_count, info.pointee, state, callback)
		.span => {
			span := *(value.pointer as *[]U8)
			return stringize_span(span.data, span.count, info.pointee, state, callback)
		}
	}
}

dprint :: (value: Any) =>
    stringize(value, null, (state: *Void, str: String) =>
        write_to_stdout(str)
    )

dprintln :: (value: Any) {
	dprint(value)
	write_to_stdout('\n')
}

print :: (value: Any) {
	b := create_string_builder()
	b.append(value)
	write_to_stdout(b.to_string())
	b.free()
}

print :: (format: String, values: ..Any) {
	b := create_string_builder()
	b.append_format(format, ..values)
	write_to_stdout(b.to_string())
	b.free()
}

println :: (value: Any) {
	print(value)
	write_to_stdout('\n')
}

println :: (value: String) {
	write_to_stdout(value)
	write_to_stdout('\n')
}

println :: (format: String, values: ..Any) {
	print(format, ..values)
	write_to_stdout('\n')
}

merge :: (a: U8, b: U8): U16 =>
	(a as U16 << 8) | b

strlen :: (str: *U8) {
	c := str
	while *c != 0 c += 1
	return c as Int - str as Int
}

find :: (me: String, what: String): ?String {
	if (count - what.count + 1) as Int <= 0
		return null

	same := true
	for i in 0 .. count - what.count + 1 {
		same = true
		for j in 0 .. what.count {
			if what[j] != me[i + j]
				same = false
		}
		if same
			return String(&me[i], what.count)
	}
	return null
}

STRING_BUILDER_INITIAL_BUFFER_CAPACITY :: 0x1000

StringBuilder :: struct {
	Block :: struct {
		count: Int
		capacity: Int
		next: *Block
	}

	// allocator: Allocator = current_allocator
	first: Block
	initial_buffer: U8[STRING_BUILDER_INITIAL_BUFFER_CAPACITY]
	last: *Block
	alloc_last: *Block
    count: Int
}

// TODO: static extensions for structs
create_string_builder :: (): using result: StringBuilder {
	// allocator = current_allocator
	first.capacity = STRING_BUILDER_INITIAL_BUFFER_CAPACITY
	last = &first
	alloc_last = &first
}

begin :: (me: *StringBuilder.Block): *U8 => @&me[1]
end   :: (me: *StringBuilder.Block): *U8 => me.begin() + count
available_space :: (me: *StringBuilder.Block): Int => capacity - count

allocate_block :: (me: *StringBuilder) {
    capacity := alloc_last.capacity*2
    block := VirtualAlloc(null, (#sizeof StringBuilder.Block + capacity) as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE) as *StringBuilder.Block
    // block := (Block *)allocator.allocate_uninitialized(sizeof(Block) + capacity, alignof(Block) TL_LA)
    block.count = 0
    block.capacity = capacity
    block.next = null
    alloc_last = block
    alloc_last.next = block
    return block
}

append_char :: (me: *StringBuilder, ch: U8) => me.append(String(&ch, 1))

append :: (me: *StringBuilder, str: String) {
    remaining := str
    while remaining.count > last.available_space() {
        space_in_block := last.available_space()
        memcpy(last.end(), remaining.data, space_in_block as UInt)
        remaining.count -= space_in_block
        remaining.data  += space_in_block
        last.count += space_in_block
		last = last.next
        if last == null
			last = me.allocate_block()
    }

    memcpy(last.end(), remaining.data, remaining.count as UInt)
    last.count += remaining.count
    count += str.count
    return str.count
}

append :: (me: *StringBuilder, value: Any) =>
	stringize(value, me, (state: *Void, str: String) =>
        (state as *StringBuilder).append(str)
    )

append_format :: (me: *StringBuilder, format: String, args: ..Any) {
    bytes_written := 0
	remaining := format

	for i in 0 .. args.count {
		y := remaining.find("{}")
		assert y // invalid format String
		x := *y

		bytes_written += me.append(String(remaining.data, @(x.data - remaining.data)))

		bytes_written += me.append(args[i])

		remaining.count -= x.data - remaining.data + 2
		remaining.data = x.data + 2
	}

	bytes_written += me.append(remaining)
    return bytes_written
}

fill :: (me: *StringBuilder, dst_string: String): result: String {
    dst_char := dst_string.data
    block := &first
    while block != null {
        memcpy(dst_char, block.begin(), block.count as UInt)
        dst_char += block.count
        block = block.next
    }
    result.data = dst_string.data
    result.count = count
}

to_string :: (me: *StringBuilder): result: String {
    result.data = @ VirtualAlloc(null, count as SIZE_T, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
	result.count = count
	me.fill(result)
}

free :: (me: *StringBuilder) {
	block := first.next
	while block != null {
		next := block.next
		// allocator.free_t(block)
		VirtualFree(block, 0, MEM_RELEASE)
		block = next
	}
	// allocator = {}
}