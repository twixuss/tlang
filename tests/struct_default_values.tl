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

	assert ag == Nest()
	assert bg == Nest(v = Vector(11, 22))
	assert cg == Nest(w = 33)
	assert al == Nest()
	assert bl == Nest(v = Vector(11, 22))
	assert cl == Nest(w = 33)
}
