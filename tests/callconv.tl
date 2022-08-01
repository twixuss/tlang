main :: () {
    assert t0() == 59
    assert t1(1) == 1
    assert t2(1, 2) == 2
    assert t3(1, 2, 3) == 3
    assert t4(1, 2, 3, 4) == 4
    assert t5(1, 2, 3, 4, 5) == 5
    assert t6(1, 2, 3, 4, 5, 6) == 6
    assert s0() == 59
    assert s1(1) == 1
    assert s2(1, 2) == 2
    assert s3(1, 2, 3) == 3
    assert s4(1, 2, 3, 4) == 4
    assert s5(1, 2, 3, 4, 5) == 5
    assert s6(1, 2, 3, 4, 5, 6) == 6
}

# () => main()

t0 :: (): Int => 59
t1 :: (x: Int): Int => x
t2 :: (x: Int, y: Int): Int => y
t3 :: (x: Int, y: Int, z: Int): Int => z
t4 :: (x: Int, y: Int, z: Int, w: Int): Int => w
t5 :: (x: Int, y: Int, z: Int, w: Int, a: Int): Int => a
t6 :: (x: Int, y: Int, z: Int, w: Int, a: Int, b: Int): Int => b

s0 :: (): Int #stdcall => 59
s1 :: (x: Int): Int #stdcall => x
s2 :: (x: Int, y: Int): Int #stdcall => y
s3 :: (x: Int, y: Int, z: Int): Int #stdcall => z
s4 :: (x: Int, y: Int, z: Int, w: Int): Int #stdcall => w
s5 :: (x: Int, y: Int, z: Int, w: Int, a: Int): Int #stdcall => a
s6 :: (x: Int, y: Int, z: Int, w: Int, a: Int, b: Int): Int #stdcall => b
