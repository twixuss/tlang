main :: fn (): int {
    t0();
    t1(1);
    t2(1, 2);
    t3(1, 2, 3);
    t4(1, 2, 3, 4);
    t5(1, 2, 3, 4, 5);
    s0();
    s1(1);
    s2(1, 2);
    s3(1, 2, 3);
    s4(1, 2, 3, 4);
    s5(1, 2, 3, 4, 5);
}

t0 :: fn (): int => 59;
t1 :: fn (x: int): int => x;
t2 :: fn (x: int, y: int): int => y;
t3 :: fn (x: int, y: int, z: int): int => z;
t4 :: fn (x: int, y: int, z: int, w: int): int => w;
t5 :: fn (x: int, y: int, z: int, w: int, a: int): int => a;
t6 :: fn (x: int, y: int, z: int, w: int, a: int, b: int): int => b;

s0 :: fn #stdcall (): int => 59;
s1 :: fn #stdcall (x: int): int => x;
s2 :: fn #stdcall (x: int, y: int): int => y;
s3 :: fn #stdcall (x: int, y: int, z: int): int => z;
s4 :: fn #stdcall (x: int, y: int, z: int, w: int): int => w;
s5 :: fn #stdcall (x: int, y: int, z: int, w: int, a: int): int => a;
s6 :: fn #stdcall (x: int, y: int, z: int, w: int, a: int, b: int): int => a;
