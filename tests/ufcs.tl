import "tlang/std.tl"

Vector2 :: struct {
    x: Int
    y: Int
    add :: (a: Vector2, b: Vector2) => Vector2(a.x + b.x, a.y + b.y)
}

sub :: (a: Vector2, b: Vector2) => Vector2(a.x - b.x, a.y - b.y)

main :: () {
    a := Vector2(34, 390)
    b := Vector2(35, 30)

    println(a.sub(b));
    println(sub(a, b));

    // sub has two arguments, which means that it can be used as a binary operator:
    println(a sub b);

    // These binary operations always have the same precedence and are left-associative.
    // The following expression is parsed into sub(sub(a, b), b).
    println(a sub b sub b)

    println(Vector2.add(a, b));

    // Static members can be accessed from an instance.
    println(a.add(a, b))

    #assert !#compiles { a.add(b) }
}