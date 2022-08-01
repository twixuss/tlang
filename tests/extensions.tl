import "std.tl"

Foo :: struct {
    a: Int
    b: Int
}

sum :: (me: *Foo) => me.a + me.b

main :: () {
    foo := Foo(34, 35)
    dprintln(foo.sum())
    dprintln(Foo(400, 20).sum())
}