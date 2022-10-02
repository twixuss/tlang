
test :: (a: Int, b: Int, c: Int) Int =>
    a * b + c

main :: () => test(c=3, 2, 4) // 11
