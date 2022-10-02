import "tlang/std.tl"

test_float :: (T: %Type) {
    #assert T == Float32 || T == Float64

    a: T = 1.5
    b: T = 2.6

    assert a != b

    assert a == a
    assert b == b

    assert a < b
    assert b > a

    assert a <= b
    assert b >= a

    assert a <= a
    assert a >= a

    assert b >= b
    assert b <= b
}

main :: () {
    assert 999_999_999_999 + 999_999_999_999 == 1_999_999_999_998
    assert 999_999_999_999 - 1_999_999_999_998 == -999_999_999_999
    assert 999_999_999_999 * 999_999_999_999 == 999_999_999_998_000_000_000_001
    assert 999_999_999_998_000_000_000_001 / 999_999_999_999 == 999_999_999_999
    assert 999_999_999_998_000_000_000_001 % 999_999_999_999 == 0
    assert 0xFFFF << 6 == 0x3FFFC0
    assert 0x3FFFC0 >> 6 == 0xFFFF
    assert 0b10101010 | 0b01010101 == 0b11111111
    assert 0b10101010 & 0b01010101 == 0b00000000
    assert 0b11001100 ^ 0b01100110 == 0b10101010
    assert 123456789 < 987654321
    assert 123456789 <= 987654321
    assert 987654321 > 123456789
    assert 987654321 >= 123456789
    assert -99999999999999999999999999999999999999999 == -99999999999999999999999999999999999999999
    assert -99999999999999999999999999999999999999999 != -99999999999999999999999999999999999999998
    assert ~0b10101010 as U8 == 0b01010101 as U8

    // FIXME: implement missing conversions
    {
        a: S64 = 12
        //assert a as F32 == 12.0
        assert a as F64 == 12.0
    }
    {
        a :F64 = 12.0
        //assert a as S8 == 12
        //assert a as S16 == 12
        //assert a as S32 == 12
        assert a as S64 == 12
        //assert a as U8 == 12
        //assert a as U16 == 12
        //assert a as U32 == 12
        //assert a as U64 == 12
    }

    test_float(F32)
    test_float(F64)
}

# () => main()
