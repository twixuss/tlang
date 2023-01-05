Foo :: struct {
    a: Int
    b: Int
}

main :: () {
    a := Foo(1, 2)
    b := Foo(1, 3)

    assert a == a
    assert b == b
    assert a != b
}