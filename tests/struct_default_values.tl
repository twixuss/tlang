import "tlang/std.tl"

Vector :: struct {
	x: Int = 1
	y: Int = 2
}

Nest :: struct {
	v: Vector
	w: Int = 3
}

ag: Nest
bg := Nest(v = Vector(11, 22))
cg := Nest(w = 33)

main :: () {
	al: Nest
	bl := Nest(v = Vector(11, 22))
	cl := Nest(w = 33)

	println(ag)
	println(bg)
	println(cg)
	println(al)
	println(bl)
	println(cl)
}
