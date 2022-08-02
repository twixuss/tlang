import "tlang/std.tl"
main :: () {
	Vector3 :: struct (T: Type) {
		x: T
		y: T
		z: T
	}

	vec := Vector3(Int)(x=1,y=4,z=9)
	println(vec)
}