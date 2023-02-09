foo := () => 42
y := 2

main :: () {
	x := y
	assert x == y
	assert x == 2
	assert y == 2
	assert foo != null
	assert foo() == 42
}
