// PROGRAM CODE 144
// PROGRAM OUTPUT 0 1 1 2 3 5 8 13 21 34 55 89
import "tlang/std.tl"

main :: () {
    a := 0
    b := 1
    while a < 100 {
        print(a)
        print(" ")
        c := a + b
        a = b
        b = c
	}
    return a
}