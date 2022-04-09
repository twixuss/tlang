import "std.tl"

main :: fn () {
    print(#file);     print_char('\n');
    print(#line);     print_char('\n');
    print(#location); print_char('\n');
}