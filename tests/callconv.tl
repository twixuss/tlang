main :: (): Int =>
    t0() +
    t1(1) -
    t2(1, 2) *
    t3(1, 2, 3) +
    t4(1, 2, 3, 4) -
    t5(1, 2, 3, 4, 5) *
    s0() +
    s1(1) -
    s2(1, 2) *
    s3(1, 2, 3) +
    s4(1, 2, 3, 4) -
    s5(1, 2, 3, 4, 5);

t0 :: (): Int => 59;
t1 :: (x: Int): Int => x;
t2 :: (x: Int, y: Int): Int => y;
t3 :: (x: Int, y: Int, z: Int): Int => z;
t4 :: (x: Int, y: Int, z: Int, w: Int): Int => w;
t5 :: (x: Int, y: Int, z: Int, w: Int, a: Int): Int => a;
t6 :: (x: Int, y: Int, z: Int, w: Int, a: Int, b: Int): Int => b;

s0 :: (): Int #stdcall => 59;
s1 :: (x: Int): Int #stdcall => x;
s2 :: (x: Int, y: Int): Int #stdcall => y;
s3 :: (x: Int, y: Int, z: Int): Int #stdcall => z;
s4 :: (x: Int, y: Int, z: Int, w: Int): Int #stdcall => w;
s5 :: (x: Int, y: Int, z: Int, w: Int, a: Int): Int #stdcall => a;
s6 :: (x: Int, y: Int, z: Int, w: Int, a: Int, b: Int): Int #stdcall => a;
