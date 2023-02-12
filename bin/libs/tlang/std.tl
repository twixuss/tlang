import "tlang/windows.tl"

bitas :: (value: $T, type: %Type) {
    #assert #sizeof T == #sizeof type
    return *(&value as *type)
}

get_end :: (span: String) => span.data + span.count
set_end :: (span: *String, new_end: *U8) {
    span.count = new_end - span.data
}

get_begin :: (span: String) => span.data
set_begin :: (span: *String, new_begin: *U8) {
    new_count := span.end - new_begin
    span.data = new_begin
    span.count = new_count
}

CString :: *struct{}

operator as implicit :: (s: CString) *U8 => @s

merge :: (a, b: U8) U16 =>
	(a as U16 << 8) | b

strlen :: (str: *U8) {
	c := str
	while *c != 0 do c += 1
	return c as Int - str as Int
}

min :: (a: $T, b: T) => if a < b then a else b
max :: (a: $T, b: T) => if a < b then b else a
clamp :: (v, _min, _max: $T) => min(_max, max(_min, v))

lerp :: (a: $T, b: T, t: $U) => a + (b - a) * t
lerp :: (a: $T, b: T, c: T, t: $U) => a + t * (2 * (b - a) + t * (a - 2 * b + c))


find :: (haystack, needle: String) ?String {
	if (haystack.count - needle.count + 1) as Int <= 0
		return null

	same := true
	for i in 0 .. haystack.count - needle.count + 1 {
		same = true
		for j in 0 .. needle.count
			if needle[j] != haystack[i + j]
				same = false
		if same
			return String(&haystack[i], needle.count)
	}
	return null
}

// FIXME: implement following syntax for templates:
//
//     haystack, needle: []$T
//
find :: (haystack: []$T, needle: []T) ?[]T {
	if (haystack.count - needle.count + 1) as Int <= 0
		return null

	same := true
	for i in 0 .. haystack.count - needle.count + 1 {
		same = true
		for j in 0 .. needle.count
			if needle[j] != haystack[i + j]
				same = false
		if same
			return ([]T)(&haystack[i], needle.count)
	}
	return null
}

starts_with :: (haystack, needle: String) {
	if needle.count > haystack.count
        return false
    for i in 0..needle.count
        if haystack.data[i] != needle.data[i]
            return false
    return true
}

ends_with :: (haystack, needle: String) {
	if needle.count > haystack.count
        return false
    for i in 0..needle.count
        if haystack.data[i + haystack.count - needle.count] != needle.data[i]
            return false
    return true
}


is_digit :: (c: U8) => '0' <= c && c <= '9'
is_whitespace :: (c: U8) => c == ' ' || c == '\n' || c == '\t' || c =='\v'

get_first :: (s: String) => s[0]
set_first :: (s: String, c: U8) => s[0] = c

get_last :: (s: String) => s[s.count-1]
set_last :: (s: String, c: U8) => s[s.count-1] = c

trim :: (s: *String) {
	while s.count != 0 {
		if !s.first.is_whitespace()
			break
		s.begin += 1
	}

	while s.count != 0 {
		if !s.last.is_whitespace()
			break
		s.end -= 1
	}
}

chop_s64 :: (s: *String) ?S64 {
	if s.count == 0
		return null

	negative := false
	if s.data[0] == '-' {
		s.begin += 1
		negative = true
	}

	integer_string := String(s.data, 0)
	while s.count != 0 {
		if !is_digit(s.data)
			break
		s.begin += 1
		integer_string.count += 1
	}

	if integer_string.count == 0
		return null

	result := 0

	for c in integer_string {
		result *= 10
		result += c - '0'
	}

	if negative
		result = -result
	return result
}

chop_f64 :: (s: *String) ?F64 {
	if s.count == 0
		return null

	negative := false
	if s.data[0] == '-' {
		s.begin += 1
		negative = true
	}

	integer_part := s.chop_s64()
	if !integer_part
		return null

	result := *integer_part as F64

	if s.count != 0 && s.data[0] == '.' {
		s.begin += 1

		start_count := s.count
		fraction_part := s.chop_s64()
		if fraction_part {
			div := 1.0
			for i in s.count..start_count
				div *= 10


			result += *fraction_part as F64 / div
		}
	}
	if negative
		result = -result
	return result
}

chop_f32 :: (s: *String) ?F32 => chop_f64(s).map(F32)

map :: (x: ?$T, t: %Type) ?t {
	if x
		return *x as t
	else
		return null
}

write_to_stdout :: (str: String) {
    GetStdHandle(STD_OUTPUT_HANDLE).WriteFile(str.data, str.count as U32, null, null)
    return str.count
}

write_to_stdout :: (char: U8) =>
    write_to_stdout(String(&char, 1))

set_console_color :: (color: RGB_U8, background := false) {
    mask := 0x8
    if color.r >= 128 then mask |= 0x4
    if color.g >= 128 then mask |= 0x2
    if color.b >= 128 then mask |= 0x1
    if background then mask <<= 4
    GetStdHandle(STD_OUTPUT_HANDLE).SetConsoleTextAttribute(mask as DWORD)
}

StringizeCallback :: (state: *Void, str: String) Int #type

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
    buffer: [CAPACITY]U8
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
    buffer: [CAPACITY]U8
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
    buffer: [CAPACITY]U8
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


	while dst_index != 0 {
        dst_index -= 1
        buffer[dst_index] = '0'
	}

    return callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)))
}

is_nan :: (v: F32) {
    u := *(&v as *U32)
	return ((u & 0x7f800000) == 0x7f800000) && ((u & 0x007fffff) != 0)
}

is_nan :: (v: F64) {
    u := *(&v as *U64)
	return ((u & 0x7ff0000000000000) == 0x7ff0000000000000) && ((u & 0x000fffffffffffff) != 0)
}

is_infinity :: (v: F32) => v bitas U32 == 0x7f800000
is_infinity :: (v: F64) => v bitas U64 == 0x7ff0000000000000

absolute :: (v: F32) => (v bitas U32 & 0x7fff_ffff) bitas F32

get_sign :: (v: F32) F32 => if (v bitas U32)[31] then -1 else +1
set_sign :: (v: F32, s: F32) => ((v bitas U32 & 0x7fff_ffff) | (s bitas U32 & 0x8000_0000)) bitas F32

get_length :: (v: F32) => absolute(v)
set_length :: (v: F32, new_length: F32) => v.sign * new_length

modulo :: (a, b: F32) => set_sign(a - (a / b) as S32 as F32 * b, a)
frac :: (a: F32) => a - floor(a)
frac :: (a: F64) => a - floor(a)
frac :: (a, b: F32) => frac(a / b) * b
frac :: (a, b: F64) => frac(a / b) * b

pi :: 3.1415926535897932384626433832795
tau :: pi*2

sin :: (_v: F32) {
	v := frac(_v, tau)
	mask := v >= pi
	v = if mask then v - pi else v
	return (16 * v * (pi - v)) / (5 * pi * pi - 4 * v * (pi - v)) * if mask then -1 as F32 else 1 as F32
}
cos :: (v: F32) => sin(v + pi*0.5)
tan :: (v: F32) => sin(v) / cos(v)

stringize :: (_val: F64, state: *Void, callback: StringizeCallback) bytes_written: Int {
    val := _val
    if val < 0 {
        val = -val
        bytes_written += callback(state, "-")
    }

    if val.is_nan()
        return callback(state, "nan")

    if val.is_infinity()
        return callback(state, "inf")

    bytes_written += stringize(val as S64, state, callback)
    bytes_written += callback(state, ".")


    {
        ival := (frac(val) * 1000) as S64
        if ival == 0 {
            return callback(state, "000")
        }

        i := ival

        CAPACITY :: 3
        buffer: [CAPACITY]U8
        dst_index := CAPACITY

        while i != 0 {
            digit := i % 10

            dst_index -= 1
            buffer[dst_index] = @(digit + '0')

            i /= 10
        }

        while dst_index != 0 {
            dst_index -= 1
            buffer[dst_index] = '0'
        }

        bytes_written += callback(state, String(@&buffer[dst_index], @(CAPACITY - dst_index)))
    }
}
stringize :: (_val: F32, state: *Void, callback: StringizeCallback) => stringize(_val as F64, state, callback)

stringize_quoted_if_string :: (value: Any, state: *Void, callback: StringizeCallback) =>
	if value.type.name == "String" then
        callback(state, "\"") +
        callback(state, *(value.pointer as *String)) +
        callback(state, "\"")
	else
		stringize(value, state, callback)


// FIXME: use a HashMap(TypeInfo, Stringizer)
stringize :: (value: Any, state: *Void, callback: StringizeCallback) Int {
	info := value.type
    assert info != null

	stringize_span :: (data: *U8, count: Int, info: *TypeInfo, state: *Void, callback: StringizeCallback) bytes_written: Int {
		bytes_written += callback(state, "[")
        if count <= 16 {
            for i in 0..count {
                if i != 0
                    bytes_written += callback(state, ", ")
                bytes_written += stringize_quoted_if_string(Any(data + i * info.size, info), state, callback)
            }
        } else {
            for i in 0..8 {
                if i != 0
                    bytes_written += callback(state, ", ")
                bytes_written += stringize_quoted_if_string(Any(data + i * info.size, info), state, callback)
            }
            bytes_written += callback(state, ", ...")
            for i in count-8..count {
                bytes_written += callback(state, ", ")
                bytes_written += stringize_quoted_if_string(Any(data + i * info.size, info), state, callback)
            }
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
            return callback(state, info.name) +
                   callback(state, "(") +
                   stringize(enum_value, state, callback) +
                   callback(state, ")")
        }
		.\struct => {
			if info.name == "String" {
				return callback(state, *(value.pointer as *String))
            } else if info.name.starts_with("Span") {
                span := *(value.pointer as *[]U8)
                return stringize_span(span.data, span.count, info.parameters[0], state, callback)
            } else if info.name.starts_with("LinkedList") {
                bytes_written: Int

                list := (value.pointer as *LinkedList(U8))

                node := list.head

                bytes_written += callback(state, "[")
                while node != null {
                    if node != list.head {
                        bytes_written += callback(state, ", ")
                    }
                    bytes_written += stringize_quoted_if_string(
                        Any(&node.value, info.parameters[0]),
                        state,
                        callback
                    )

                    node = node.next
                }
                bytes_written += callback(state, "]")

				return bytes_written
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
        .option => {
            if *((value.pointer + info.pointee.size) as *Bool) {
                return stringize(Any(value.pointer, info.pointee), state, callback)
            } else {
                return callback(state, "{}")
            }
        }
	}
    return 0
}

stringize_format :: (state: *Void, callback: StringizeCallback, format: String, args: ..Any) Int {
    bytes_written := 0
	remaining := format

	for i in 0 .. args.count {
		y := remaining.find("{}")
		if !y break
		x := *y

		bytes_written += callback(state, String(remaining.data, @(x.data - remaining.data)))

		bytes_written += stringize(args[i], state, callback)

		remaining.count -= x.data - remaining.data + 2
		remaining.data = x.data + 2
	}

    bytes_written += callback(state, remaining)
    return bytes_written
}


dprint :: (value: String) => write_to_stdout(value)

dprint :: (value: Any) =>
    stringize(value, null, (state: *Void, str: String) =>
        write_to_stdout(str)
    )

dprint :: (format: String, values: ..Any) Int =>
    stringize_format(null, (state: *Void, string: String) {
        return dprint(string)
    }, format, ..values)


dprintln :: () => write_to_stdout('\n')

dprintln :: (string: String) {
    write_to_stdout(string)
    write_to_stdout('\n')
}

dprintln :: (value: Any) {
    dprint(value)
    write_to_stdout('\n')
}

dprintln :: (format: String, values: ..Any) {
	dprint(format, ..values)
	write_to_stdout('\n')
}


print :: (value: String) => write_to_stdout(value)

print :: (value: Any) {
	b := create_string_builder()
	b.append(value)
	b.to_string().write_to_stdout()
	b.free()
}

print :: (format: String, values: ..Any) {
	b := create_string_builder()
	b.append_format(format, ..values)
	b.to_string().write_to_stdout()
	b.free()
}

println :: () => write_to_stdout('\n')

println :: (value: String) {
	write_to_stdout(value)
	write_to_stdout('\n')
}

println :: (value: Any) {
	print(value)
	write_to_stdout('\n')
}

println :: (format: String, values: ..Any) {
	print(format, ..values)
	write_to_stdout('\n')
}

STRING_BUILDER_INITIAL_BUFFER_CAPACITY :: 0x1000

StringBuilder :: struct {
	Block :: struct {
		count: Int
		capacity: Int
		next: *Block
	}

	allocator: Allocator
	first: Block
	initial_buffer: [STRING_BUILDER_INITIAL_BUFFER_CAPACITY]U8
	last: *Block
	alloc_last: *Block
    count: Int
}

// TODO: static extensions for structs
create_string_builder :: () using result: StringBuilder {
	allocator = current_allocator
	first.capacity = STRING_BUILDER_INITIAL_BUFFER_CAPACITY
	last = &first
	alloc_last = &first
}

begin :: (using this: *StringBuilder.Block) *U8 => @&this[1]
end   :: (using this: *StringBuilder.Block) *U8 => this.begin() + count
available_space :: (using this: *StringBuilder.Block) Int => capacity - count

allocate_block :: (using this: *StringBuilder) {
    capacity := alloc_last.capacity*2
    block := allocator.allocate(#sizeof StringBuilder.Block + capacity) as *StringBuilder.Block
    block.count = 0
    block.capacity = capacity
    block.next = null
    alloc_last = block
    alloc_last.next = block
    return block
}

append_char :: (using this: *StringBuilder, ch: U8) => this.append(String(&ch, 1))

append :: (using this: *StringBuilder, str: String) {
    remaining := str
    while remaining.count > last.available_space() {
        space_in_block := last.available_space()
        memcpy(last.end(), remaining.data, space_in_block as UInt)
        remaining.count -= space_in_block
        remaining.data  += space_in_block
        last.count += space_in_block
		last = last.next
        if last == null
			last = this.allocate_block()
    }

    memcpy(last.end(), remaining.data, remaining.count as UInt)
    last.count += remaining.count
    count += str.count
    return str.count
}

append :: (using this: *StringBuilder, value: Any) =>
	stringize(value, this, (state: *Void, str: String) =>
        (state as *StringBuilder).append(str)
    )

append_format :: (using this: *StringBuilder, format: String, args: ..Any) =>
    stringize_format(this, (state: *Void, string: String) => (state as *StringBuilder).append(string), format, ..args)

fill :: (using this: *StringBuilder, dst_string: String) result: String {
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

to_string :: (using this: *StringBuilder) result: String {
    result.data = @ current_allocator.allocate(count)
	result.count = count
	this.fill(result)
}

free :: (using this: *StringBuilder) {
	block := first.next
	while block != null {
		next := block.next
		// allocator.free_t(block)
		VirtualFree(block, 0, MEM_RELEASE)
		block = next
	}
	// allocator = {}
}

read :: (file: HANDLE, span_: String) Int {
	max_bytes :: -1 as DWORD
	bytes_read: DWORD = 0
	total_bytes_read := 0
    span := span_

	while span.count > max_bytes as Int {
		if ReadFile(file, span.data, max_bytes, &bytes_read, null) == 0
			return total_bytes_read

		span.data += max_bytes
		span.count -= max_bytes

		total_bytes_read += bytes_read

		if bytes_read != max_bytes
			return total_bytes_read
	}
	if span.count != 0 {
		if ReadFile(file, span.data, span.count as DWORD, &bytes_read, null) == 0
			return total_bytes_read

		total_bytes_read += bytes_read

		if bytes_read as Int != span.count
			return total_bytes_read
	}
	return total_bytes_read
}

Buffer :: struct {
    data: String
    allocator: Allocator
}

read_entire_file :: (path: *U8) using result: Buffer {
    allocator = current_allocator

	file := CreateFileA(path, GENERIC_READ, 0, null, OPEN_EXISTING, 0, null)
	if file == INVALID_HANDLE_VALUE return


	SetFilePointerEx(file, 0, null, FILE_END)

	size: LARGE_INTEGER
	SetFilePointerEx(file, 0, &size, FILE_CURRENT)

	SetFilePointerEx(file, 0, null, FILE_BEGIN)


    data.data = @allocator.allocate(size)
    data.count = size

    read(file, data)

    // FIXME: redundant
    return
}

read_entire_file :: (path: String) Buffer {
    assert path.count < 256
    buf: [256]U8
    memcpy(buf.data, path.data, @path.count)
    buf[path.count] = 0
    return read_entire_file(buf.data)
}

Vector2 :: struct(T: Type) {
    x, y: T
}

Vector3 :: struct(T: Type) {
    x, y, z: T
}

Vector4 :: struct(T: Type) {
    x, y, z, w: T
}

vector2 :: (x, y: $T) => Vector2(T)(x, y)
vector3 :: (x, y, z: $T) => Vector3(T)(x, y, z)
vector4 :: (x, y, z, w: $T) => Vector4(T)(x, y, z, w)

vector2 :: (v: $T) => Vector2(T)(v, v)
vector3 :: (v: $T) => Vector3(T)(v, v, v)
vector4 :: (v: $T) => Vector4(T)(v, v, v, v)

vector4 :: (xyz: Vector3($T), w: T) => Vector4(T)(xyz.x, xyz.y, xyz.z, w)

get_xyz :: (v: Vector4($T)) => Vector3(T)(v.x, v.y, v.z)

operator - :: (v: Vector2($T)) => Vector2(T)(-v.x, -v.y)
operator - :: (v: Vector3($T)) => Vector3(T)(-v.x, -v.y, -v.z)
operator - :: (v: Vector4($T)) => Vector4(T)(-v.x, -v.y, -v.z, -v.w)

operator + :: (a, b: Vector2($T)) => Vector2(T)(a.x + b.x, a.y + b.y)
operator - :: (a, b: Vector2($T)) => Vector2(T)(a.x - b.x, a.y - b.y)
operator * :: (a, b: Vector2($T)) => Vector2(T)(a.x * b.x, a.y * b.y)
operator / :: (a, b: Vector2($T)) => Vector2(T)(a.x / b.x, a.y / b.y)
operator + :: (a: Vector2($T), b: T) => Vector2(T)(a.x + b, a.y + b)
operator - :: (a: Vector2($T), b: T) => Vector2(T)(a.x - b, a.y - b)
operator * :: (a: Vector2($T), b: T) => Vector2(T)(a.x * b, a.y * b)
operator / :: (a: Vector2($T), b: T) => Vector2(T)(a.x / b, a.y / b)
operator + :: (a: $T, b: Vector2(T)) => Vector2(T)(a + b.x, a + b.y)
operator - :: (a: $T, b: Vector2(T)) => Vector2(T)(a - b.x, a - b.y)
operator * :: (a: $T, b: Vector2(T)) => Vector2(T)(a * b.x, a * b.y)
operator / :: (a: $T, b: Vector2(T)) => Vector2(T)(a / b.x, a / b.y)

operator + :: (a, b: Vector3($T)) => Vector3(T)(a.x + b.x, a.y + b.y, a.z + b.z)
operator - :: (a, b: Vector3($T)) => Vector3(T)(a.x - b.x, a.y - b.y, a.z - b.z)
operator * :: (a, b: Vector3($T)) => Vector3(T)(a.x * b.x, a.y * b.y, a.z * b.z)
operator / :: (a, b: Vector3($T)) => Vector3(T)(a.x / b.x, a.y / b.y, a.z / b.z)
operator + :: (a: Vector3($T), b: T) => Vector3(T)(a.x + b, a.y + b, a.z + b)
operator - :: (a: Vector3($T), b: T) => Vector3(T)(a.x - b, a.y - b, a.z - b)
operator * :: (a: Vector3($T), b: T) => Vector3(T)(a.x * b, a.y * b, a.z * b)
operator / :: (a: Vector3($T), b: T) => Vector3(T)(a.x / b, a.y / b, a.z / b)
operator + :: (a: $T, b: Vector3(T)) => Vector3(T)(a + b.x, a + b.y, a + b.z)
operator - :: (a: $T, b: Vector3(T)) => Vector3(T)(a - b.x, a - b.y, a - b.z)
operator * :: (a: $T, b: Vector3(T)) => Vector3(T)(a * b.x, a * b.y, a * b.z)
operator / :: (a: $T, b: Vector3(T)) => Vector3(T)(a / b.x, a / b.y, a / b.z)

operator + :: (a, b: Vector4($T)) => Vector4(T)(a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
operator - :: (a, b: Vector4($T)) => Vector4(T)(a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
operator * :: (a, b: Vector4($T)) => Vector4(T)(a.x * b.x, a.y * b.y, a.z * b.z, a.w * b.w)
operator / :: (a, b: Vector4($T)) => Vector4(T)(a.x / b.x, a.y / b.y, a.z / b.z, a.w / b.w)
operator + :: (a: Vector4($T), b: T) => Vector4(T)(a.x + b, a.y + b, a.z + b, a.w + b)
operator - :: (a: Vector4($T), b: T) => Vector4(T)(a.x - b, a.y - b, a.z - b, a.w - b)
operator * :: (a: Vector4($T), b: T) => Vector4(T)(a.x * b, a.y * b, a.z * b, a.w * b)
operator / :: (a: Vector4($T), b: T) => Vector4(T)(a.x / b, a.y / b, a.z / b, a.w / b)
operator + :: (a: $T, b: Vector4(T)) => Vector4(T)(a + b.x, a + b.y, a + b.z, a + b.w)
operator - :: (a: $T, b: Vector4(T)) => Vector4(T)(a - b.x, a - b.y, a - b.z, a - b.w)
operator * :: (a: $T, b: Vector4(T)) => Vector4(T)(a * b.x, a * b.y, a * b.z, a * b.w)
operator / :: (a: $T, b: Vector4(T)) => Vector4(T)(a / b.x, a / b.y, a / b.z, a / b.w)

operator as explicit :: (using from: Vector2(S32)) => Vector2(F32)(@x, @y)
operator as explicit :: (using from: Vector2(F32)) => Vector2(S32)(@x, @y)

get_data :: (using v: *Vector2) => &x
get_data :: (using v: *Vector3) => &x
get_data :: (using v: *Vector4) => &x

get_length :: (using v: Vector2) => sqrt(x*x + y*y)
get_length :: (using v: Vector3) => sqrt(x*x + y*y + z*z)
get_length :: (using v: Vector4) => sqrt(x*x + y*y + z*z + w*w)

set_length :: (v: *Vector2, new_length: F32) => *v *= new_length / v.length
set_length :: (v: *Vector3, new_length: F32) => *v *= new_length / v.length
set_length :: (v: *Vector4, new_length: F32) => *v *= new_length / v.length

get_normalized :: (v: Vector2) => v / v.length
get_normalized :: (v: Vector3) => v / v.length
get_normalized :: (v: Vector4) => v / v.length

get_r :: (v: Vector2) => v.x
get_r :: (v: Vector3) => v.x
get_r :: (v: Vector4) => v.x

get_g :: (v: Vector2) => v.y
get_g :: (v: Vector3) => v.y
get_g :: (v: Vector4) => v.y

get_b :: (v: Vector2) => v.z
get_b :: (v: Vector3) => v.z
get_b :: (v: Vector4) => v.z

get_a :: (v: Vector2) => v.w
get_a :: (v: Vector3) => v.w
get_a :: (v: Vector4) => v.w

set_r :: (v: *Vector2($T), n: T) => v.x = n
set_r :: (v: *Vector3($T), n: T) => v.x = n
set_r :: (v: *Vector4($T), n: T) => v.x = n

set_g :: (v: *Vector2($T), n: T) => v.y = n
set_g :: (v: *Vector3($T), n: T) => v.y = n
set_g :: (v: *Vector4($T), n: T) => v.y = n

set_b :: (v: *Vector2($T), n: T) => v.z = n
set_b :: (v: *Vector3($T), n: T) => v.z = n
set_b :: (v: *Vector4($T), n: T) => v.z = n

set_a :: (v: *Vector2($T), n: T) => v.w = n
set_a :: (v: *Vector3($T), n: T) => v.w = n
set_a :: (v: *Vector4($T), n: T) => v.w = n

// I found an interesting thing:
// If you have an arbitrary quad (not necessarily flat),
// and connect midpoints of it's edges,
// you get another quad, that always lies flat on some plane.


RGB_U8 :: Vector3(U8)

Node :: struct(T: Type) {
    next: *Node(T)
    value: T
}
LinkedList :: struct(T: Type) {

    head, tail: *Node(T)
    allocator: Allocator
}

linked_list :: (T: %Type) using list: LinkedList(T) {
    allocator = current_allocator
}

add :: (using list: *LinkedList($T), value: T = T()) {
    if head == null {
        head = allocator.new(Node(T))
        tail = head
    } else {
        tail.next = allocator.new(Node(T))
        tail = tail.next
    }
    tail.value = value
}

free :: (using list: *LinkedList($T)) {
    node := head
    while node != null {
        next := node.next
        list.allocator.deallocate(node)
        node = next
    }
    list.head = null
    list.tail = null
}

HashMap :: struct(Key: Type, Value: Type) {
    KeyValue :: struct {
        key: Key
        value: Value
    }
    HashKeyValue :: struct {
        hash: U64
        key_value: KeyValue
    }

    buckets: []LinkedList(HashKeyValue)
}


// get_or_insert :: (using map: HashMap($Key, $Value), key: Key) {
// }

is_empty :: (span: []$T) => span.count == 0

// operator as implicit :: (span: []$T) => span.count != 0

List :: struct (T: Type) {
    data: []T
    capacity: Int
    allocator: Allocator
}

list :: (T: %Type) => List(T)(allocator = current_allocator)

reserve :: (using list: *List($T), new_capacity: Int) {
    if capacity >= new_capacity
        return

    new_data := allocator.new(T, new_capacity)

    if data.count != 0 {
        memcpy(new_data, data.data, @(data.count * #sizeof T))
        allocator.deallocate(data.data)
    }

    data.data = new_data
    capacity = new_capacity
}

add :: (using list: *List($T), value: T) {
    list.reserve(data.count + 1)
    data.data[data.count] = value
    data.count += 1
}

clear :: (using list: *List) {
    data.count = 0
}
