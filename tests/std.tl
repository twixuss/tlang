// PROGRAM OUTPUT 1.000 0.000 0.923 0.383 0.705 0.705 0.383 0.923 0.000 1.000
import "tlang/std.tl"

main :: () {
    hpi :: pi / 2

    print("{} ", cos(hpi * 0.00))
    print("{} ", sin(hpi * 0.00))

    print("{} ", cos(hpi * 0.25))
    print("{} ", sin(hpi * 0.25))

    print("{} ", cos(hpi * 0.50))
    print("{} ", sin(hpi * 0.50))

    print("{} ", cos(hpi * 0.75))
    print("{} ", sin(hpi * 0.75))

    print("{} ", cos(hpi * 1.00))
    print("{} ", sin(hpi * 1.00))
}