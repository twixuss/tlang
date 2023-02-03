import "tlang/std.tl"
main :: () {
	Vector3 :: struct (T: Type) {
		x: T
		y: T
		z: T
	}

	dot :: (a, b: Vector3($T)) => a.x*b.x + a.y*b.y + a.z*b.z
	length :: (a: Vector3) => sqrt(dot(a, a) as F64)
	foo :: (a: Vector3, b: Vector3) => 42

	vec1 := Vector3(Int)(x=1,y=2,z=3)
	vec2 := Vector3(Int)(x=4,y=5,z=6)
	println(vec1)

	assert dot(vec1, vec2) == 32
	assert length(vec1) == sqrt(1+4+9 as F64)
	assert foo(vec1, Vector3(Float)()) == 42

	0
}