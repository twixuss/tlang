import "tlang/std.tl"

Foo :: struct {
    x: Int
    y: Int
}

add :: (a: Foo, b: Foo) => Foo(a.x + b.x, a.y + b.y)

main :: () {
    a := Foo(34, 390)
    b := Foo(35, 30)

    println(a.add(b));
    println(add(a, b));

    // add has two arguments, which means that it can be used as a binary operator:
    println(a add b);

    // These binary operations always have the same precedence and are left-associative.
    // The following expression is parsed into add(add(a, b), b).
    println(a add b add b)
}