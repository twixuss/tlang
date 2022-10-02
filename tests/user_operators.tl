import "tlang/std.tl"

main :: () {
    a := Vector3(1, 2, 3)
    b := Vector3(6, 5, 4)

    println(a + b)
    println(a - b)
    println(a * b)

    a += b; println(a)
    a -= b; println(a)
    a *= b; println(a)
}
