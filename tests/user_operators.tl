import "std.tl"

Vector3 :: struct {
    x: int;
    y: int;
    z: int;
}

operator + :: fn (a: Vector3, b: Vector3) {
    c: Vector3;
    c.x = a.x + b.x;
    c.y = a.y + b.y;
    c.z = a.z + b.z;
    return c;
}

operator - :: fn (a: Vector3, b: Vector3) {
    c: Vector3;
    c.x = a.x - b.x;
    c.y = a.y - b.y;
    c.z = a.z - b.z;
    return c;
}

operator * :: fn (a: Vector3, b: Vector3) {
    c: Vector3;
    c.x = a.x * b.x;
    c.y = a.y * b.y;
    c.z = a.z * b.z;
    return c;
}

main :: fn() {
    v: Vector3;
    v.x = 1;
    v.y = 2;
    v.z = 3;
    
    v = v * v;
    
    print(v.x);
    print(v.y);
    print(v.z);
}
