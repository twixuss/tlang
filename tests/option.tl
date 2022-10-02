import "tlang/std.tl"

main ::() {
    e: ?String
    println(unwrap(e))
    println(unwrap("Hello"))
}

unwrap :: (x: ?String) {
    return if x then *x else "empty"
}
