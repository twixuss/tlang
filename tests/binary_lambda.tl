main :: () {
    Vector2 :: struct {
        x: Int
        y: Int
    }

    dot :: (a: Vector2, b: Vector2) => a.x * b.x + a.y * b.y

    a := Vector2(1, 2)
    b := Vector2(3, 4)

    assert dot(a, b) == 11

    // dot has two arguments, which means that it can be used as a binary operator:
    assert a dot b == 11

    sub :: (a: Int, b: Int) => a - b
    // These binary operations always have the same precedence and are left-associative.
    // The following expression is parsed as ((10 sub 2) sub 2).
    assert (10 sub 2 sub 2) == 6
}