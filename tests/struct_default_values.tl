import "tlang/std.tl"

Vector :: struct {
	x: Int = 1
	y: Int = 2
}

Nest :: struct {
	v: Vector
	w: Int = 3
}

main :: () {
	a: Nest
	println(a)

	b := Nest(v = Vector(4, 5))
	println(b)

	c := Nest(w = 6)
	println(c)
}
