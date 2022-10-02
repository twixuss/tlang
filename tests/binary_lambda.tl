import "tlang/std.tl"

Vector2 :: struct {
    x: Int
    y: Int
}

dot :: (a: Vector2, b: Vector2) => a.x * b.x + a.y * b.y

main :: () {
    a := Vector2(1, 2)
    b := Vector2(3, 4)

    println("dot(a, b) = {}", dot(a, b))

    // dot has two arguments, which means that it can be used as a binary operator:
    println("a dot b = {}", a dot b)

    sub :: (a: Int, b: Int) => a - b
    // These binary operations always have the same precedence and are left-associative.
    // The following expression is parsed as ((10 sub 2) sub 2).
    println(10 sub 2 sub 2)
}