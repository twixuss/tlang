
Radians :: #distinct Int
Degrees :: #distinct Int
main :: () {
	x := Radians(1)
	y: Radians = 2
	z: Int = x

	assert x == 1
	assert y == Radians(2)
	#assert !#compiles { y == Degrees(2) }
	#assert !#compiles { y = Degrees(2) }
}
