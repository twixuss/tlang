import "std.tl"

main ::() {
    empty : ?string;
    print(unwrap(empty));   print("\n");
    print(unwrap("Hello")); print("\n");
}

unwrap :: (x: ?string) {
    return if x then *x else "empty";
}
