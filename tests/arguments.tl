// PROGRAM CODE 1234

Foo :: struct {
	x, y, z, w: Int
}

Bar :: struct {
	x, y: Int
}

test :: (a, b: Bar) {
	return Foo(a.x,a.y,b.x,b.y)
}

main :: () #print_bytecode {
	f := test(Bar(1,2),Bar(3,4))
	return f.x * 1000 + f.y * 100 + f.z * 10 + f.w
}