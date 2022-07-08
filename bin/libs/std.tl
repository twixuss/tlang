import "windows.tl"
import "string.tl"

write_to_console :: fn (str : string) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count as u32, null, null);
}

dprint :: fn (str: string) {
    write_to_console(str);
}
dprint_char :: fn (char : u8) {
    write_to_console(string(&char, 1));
}

dprint :: fn (_val : int) {
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
    buffer : u8[64];
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
dprint :: fn (_val : uint) {
    val := _val;
    if val == 0 {
        dprint_char('0');
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
        dprint_char(buffer[dst_index]);
        dst_index += 1;
    }
}
dprint :: fn (v: s8 ) => dprint(v as s64);
dprint :: fn (v: s16) => dprint(v as s64);
dprint :: fn (v: s32) => dprint(v as s64);
dprint :: fn (v: u8 ) => dprint(v as u64);
dprint :: fn (v: u16) => dprint(v as u64);
dprint :: fn (v: u32) => dprint(v as u64);

dprint_hex :: fn (_val : uint) {
    val := _val;
    if val == 0 {
        dprint_char('0');
        return;
    }

	max_char_count :: 16;

    i := val;
    buffer: u8[max_char_count];
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

dprintln :: fn (value) {
	dprint(value);
	dprint_char('\n');
}

print :: fn (value) {
	b := StringBuilder.create();
	append(&b, value);
	write_to_console(to_string(&b));
	free(&b);
}

println :: fn (value) {
	print(value);
	dprint_char('\n');
}

merge :: fn (a : u8, b : u8): u16 {
	return (a as u16 << 8) | b;
}

strlen :: fn (str: *u8): uint {
	c := str;
	while *c != 0 c += 1;
	return c as uint - str as uint;
}
