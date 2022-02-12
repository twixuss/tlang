//import "std.tl"
foo :: fn (a: int) -> int => 1;
foo :: fn (a: bool) -> int => 2;
main :: fn () {
    foo(true);
}