
test :: (a: int, b: int, c: int): int =>
    a * b + c;

main ::() {
   assert test(c=3, 2, 4) == 11;
}
