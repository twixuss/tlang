Vector :: struct {
	x: Int = 1
	y: Int = 2
}

fffff := 413413

r1 := 5
r2 := 7

Nest :: struct {
	v: Vector
	w: *Int = &r1
}

ag: Nest
bg := Nest(v = Vector(11, 22))
cg := Nest(w = r2)

main :: () {
	al: Nest
	bl := Nest(v = Vector(11, 22))
	cl := Nest(w = r2)

	assert ag.v.x == 1
	assert ag.v.y == 2
	assert ag.w == &r1

	assert bg.v.x == 11
	assert bg.v.y == 22
	assert bg.w == &r1

	assert cg.v.x == 1
	assert cg.v.y == 2
	assert cg.w == &r2

	assert al.v.x == 1
	assert al.v.y == 2
	assert al.w == &r1

	assert bl.v.x == 11
	assert bl.v.y == 22
	assert bl.w == &r1

	assert cl.v.x == 1
	assert cl.v.y == 2
	assert cl.w == &r2
}