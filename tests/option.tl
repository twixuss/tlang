import "std.tl"

main :: fn() {
    empty : ?string;
    print(unwrap(empty));   print("\n");
    print(unwrap("Hello")); print("\n");
}

unwrap :: fn (x: ?string) {
    return if x then *x else "empty";
}
