import "tlang/std.tl"

main :: () {
    Vec3 :: Vector3(Int)

    a := Vec3(1, 2, 3)
    b := Vec3(6, 5, 4)
	c: Vec3

	c = a + b
	assert c.x == 7
	assert c.y == 7
	assert c.z == 7

	c = a - b
	assert c.x == -5
	assert c.y == -3
	assert c.z == -1

	c = a * b
	assert c.x == 6
	assert c.y == 10
	assert c.z == 12

	c = a
	c += b
	assert c.x == 7
	assert c.y == 7
	assert c.z == 7

	c = a
	c -= b
	assert c.x == -5
	assert c.y == -3
	assert c.z == -1

	c = a
	c *= b
	assert c.x == 6
	assert c.y == 10
	assert c.z == 12
}
