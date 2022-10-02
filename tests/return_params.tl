
foo :: () result: Int {
	result = 64
	result = 32
	return
}


main :: () Int {
	a := foo()
	return a
}
