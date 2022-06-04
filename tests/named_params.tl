
test :: fn (a: int, b: int, c: int): int =>
    a * b + c;

main :: fn() {
   assert test(c=3, 2, 4) == 11;
}
