import "tlang/std.tl"

main :: () {
    Vec3 :: Vector3(Float)

    a := Vector3(Float)(1, 2, 3)
    b := Vector3(Float)(6, 5, 4)

    println(a + b)
    println(a - b)
    println(a * b)

    a += b; println(a)
    a -= b; println(a)
    a *= b; println(a)
}
