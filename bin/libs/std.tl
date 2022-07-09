import "windows.tl"
import "string.tl"

write_to_console :: (str: String) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count as U32, null, null);
}

dprint :: (str: String) {
    write_to_console(str);
}
dprint_char :: (char: U8) {
    write_to_console(String(&char, 1));
}

dprint :: (_val: Int) {
    val := _val;
    if val == 0 {
        dprint_char('0');
        return;
    }
	if val < 0 {
		dprint_char('-');
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
        dprint_char(buffer[dst_index]);
        dst_index += 1;
    }
}
dprint :: (_val: UInt) {
    val := _val;
    if val == 0 {
        dprint_char('0');
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
        dprint_char(buffer[dst_index]);
        dst_index += 1;
    }
}
dprint :: (v: S8 ) => dprint(v as S64);
dprint :: (v: S16) => dprint(v as S64);
dprint :: (v: S32) => dprint(v as S64);
dprint :: (v: U8 ) => dprint(v as U64);
dprint :: (v: U16) => dprint(v as U64);
dprint :: (v: U32) => dprint(v as U64);

dprint_hex :: (_val: UInt) {
    val := _val;
    if val == 0 {
        dprint_char('0');
        return;
    }

	max_char_count :: 16;

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
	
    while dst_index != max_char_count {
        dprint_char(buffer[dst_index]);
        dst_index += 1;
    }
}

dprintln :: (value: $T) {
	dprint(value);
	dprint_char('\n');
}

print :: (value: $T) {
	b := StringBuilder.create();
	append(&b, value);
	write_to_console(to_string(&b));
	free(&b);
}

//print :: (format: String, values: ..any) {
//	b := StringBuilder.create();
//	append_format(&b, format, values);
//	write_to_console(to_string(&b));
//	free(&b);
//}

println :: (value: $T) {
	print(value);
	dprint_char('\n');
}

println :: (value: String) {
	dprint(value);
	dprint_char('\n');
}
//println :: (format: String, values: ..any) {
//	print(format, values);
//	dprint_char('\n');
//}

merge :: (a: U8, b: U8): U16 {
	return (a as U16 << 8) | b;
}

strlen :: (str: *U8): UInt {
	c := str;
	while *c != 0 c += 1;
	return c as UInt - str as UInt;
}
