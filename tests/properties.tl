import "tlang/std.tl"

main :: () {
	Vector2 :: struct {
		x, y: F32
	}

	get_length :: (v: Vector2) => sqrt(v.x*v.x + v.y*v.y)
	set_length :: (v: *Vector2, new_length: F32) {
		l := new_length / v.get_length()
		v.x *= l
		v.y *= l
	}

	approx :: (a: F32, b: F32) => absolute(a - b) < 0.001

    v := Vector2(1, 1)

	assert v.x approx 1
	assert v.y approx 1
	assert v.length approx 1.414

	v.length = 10

	assert v.x approx 7.071
	assert v.y approx 7.071
	assert v.length approx 10

}
