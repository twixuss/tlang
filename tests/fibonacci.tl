import "tlang/std.tl"

main :: () {
    a := 0
    b := 1
    while a < 100 {
        println(a)
        c := a + b
        a = b
        b = c
	}
    return a
}