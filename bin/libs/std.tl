import "windows.tl"

print :: fn (str : string) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count as u32, null, null);
}

print_char :: fn (char : u8) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), &char as *void, 1, null, null);
}

print :: fn (_val : int) {
    val := _val;
    if val == 0 {
        print_char('0');
        return;
    }
	if val < 0 {
		print_char('-');
		val = -val;
	}

    i := val;
    buffer : int[64];
    dst_index := 64;

    while i != 0 {
        digit := i % 10;

        dst_index -= 1;
        buffer[dst_index] = digit + '0';

        i /= 10;
    }

    while dst_index != 64 {
        print_char(buffer[dst_index] as u8);
        dst_index += 1;
    }
}

print_hex :: fn (_val : int) {
    val := _val;
    if val == 0 {
        print_char('0');
        return;
    }

    i := val;
    buffer : int[64];
    dst_index := 64;

    while i != 0 {
        digit := i & 15;

        dst_index -= 1;
		
		if digit < 10
			buffer[dst_index] = digit + '0';
		else 
			buffer[dst_index] = digit - 10 + 'a';

        i >>= 4;
    }

    while dst_index != 64 {
        print_char(buffer[dst_index] as u8);
        dst_index += 1;
    }
}

merge :: fn (a : u8, b : u8): u16 {
	return (a as u16 << 8) | b;
}
