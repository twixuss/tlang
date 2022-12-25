// COMPILER ERROR
// COMPILER OUTPUT Branches of this if expression have different types, and there is no conversion from one to another

main :: () {
	x := "Hello"
    return if false then 1 else x
}
