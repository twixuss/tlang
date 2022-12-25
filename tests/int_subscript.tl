main :: () {
    a: U8 = 0b00110101
    assert a[0] == true
    assert a[1] == false
    assert a[2] == true
    assert a[3] == false
    assert a[4] == true
    assert a[5] == true
    assert a[6] == false
    assert a[7] == false
}