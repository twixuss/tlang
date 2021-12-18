import "std.tl"

main :: fn () {
    a : int = 0;
    b : int = 1;
    while a < 100 {
        print_int(a);
        print_string("\n");
        c := a + b;
        a = b;
        b = c;
	}
}