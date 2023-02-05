Foo :: struct {
    a: U8
    b: U8
    c: U32
    d: U8
    e: U16
    f: U64
}

main :: () {
    members := (#typeinfo Foo).members
    assert members[0].offset == 0
    assert members[1].offset == 1
    assert members[2].offset == 4
    assert members[3].offset == 8
    assert members[4].offset == 10
    assert members[5].offset == 16
}