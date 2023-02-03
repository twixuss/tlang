// PROGRAM CODE 42
main :: () {
	foo :: (x: Int) => 42
	x := 1
	ptr := &x
	ptr.foo()
}