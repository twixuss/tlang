import "tlang/std.tl"

main :: () Int {
    println(#file)
    println(#line)
    println(#location)
    println(#function)
    return 42
}