import "std.tl"

test :: fn (a: int, b: int, c: int): int {
    return a * b + c;
}

main :: fn() {
   print(test(b=3, 2, 4)); // should print 2*3+4 = 10
}
