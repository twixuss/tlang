main :: () {
    Vec2 :: struct {
		x, y: Int
	}

	operator + :: (a, b: Vec2) => Vec2(a.x + b.x, a.y + b.y)
	operator - :: (a, b: Vec2) => Vec2(a.x - b.x, a.y - b.y)
	operator * :: (a, b: Vec2) => Vec2(a.x * b.x, a.y * b.y)

    a := Vec2(1, 2)
    b := Vec2(6, 5)
	c: Vec2

	c = a + b
	assert c.x == 7
	assert c.y == 7

	c = a - b
	assert c.x == -5
	assert c.y == -3

	c = a * b
	assert c.x == 6
	assert c.y == 10

	c = a
	c += b
	assert c.x == 7
	assert c.y == 7

	c = a
	c -= b
	assert c.x == -5
	assert c.y == -3

	c = a
	c *= b
	assert c.x == 6
	assert c.y == 10
}
