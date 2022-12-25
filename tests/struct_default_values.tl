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

// FIXME: make == overloadable
// FIXME: generate default comparison operators for structs
eq :: (a: Nest, b: Nest) {
	return (
		a.v.x == b.v.x &&
		a.v.y == b.v.y &&
		a.w == b.w
	)
}

main :: () {
	al: Nest
	bl := Nest(v = Vector(11, 22))
	cl := Nest(w = 33)

	assert ag eq Nest()
	assert bg eq Nest(v = Vector(11, 22))
	assert cg eq Nest(w = 33)
	assert al eq Nest()
	assert bl eq Nest(v = Vector(11, 22))
	assert cl eq Nest(w = 33)
}
