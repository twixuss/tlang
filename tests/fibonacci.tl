import "tlang/std.tl"

main :: () {
    a := 0
    b := 1
    while a < 100 {
        print(a)
        print("\n")
        c := a + b
        a = b
        b = c
	}
    return a
}