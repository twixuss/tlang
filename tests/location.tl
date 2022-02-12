import "std.tl"

main :: fn () {
    print_string(#file);     print_char('\n');
    print_int   (#line);     print_char('\n');
    print_string(#location); print_char('\n');
}