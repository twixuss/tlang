import "windows.tl"

print_string :: fn (str : string) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), str.data, str.count, null, null);
}

print_char :: fn (char : u8) {
    WriteConsoleA(GetStdHandle(STD_OUTPUT_HANDLE), &char, 1, null, null);
}

print_int :: fn (_val : int) {
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
    buffer : [64]int;
    dst_index := 64;

    while i != 0 {
        digit := i % 10;

        dst_index -= 1;
        [dst_index]buffer = digit + '0';

        i /= 10;
    }

    while dst_index != 64 {
        print_char([dst_index]buffer);
        dst_index += 1;
    }
}

merge :: fn (a : u8, b : u8) -> u16 {
	return (a << 8) | b;
}
