import "tlang/std.tl"

Foo :: struct {
    x, y: U8
    r, g: U8 #at x
}

main :: () {
    v: Foo

    assert (&v.x as *U8 - &v as *U8) as Int == 0
    assert (&v.y as *U8 - &v as *U8) as Int == 1
    assert (&v.r as *U8 - &v as *U8) as Int == 0
    assert (&v.g as *U8 - &v as *U8) as Int == 1
}
