main :: () {
	a := .[1, 4, 9]
	#assert #typeof a == [3]Int
	assert a[0] == 1
	assert a[1] == 4
	assert a[2] == 9

	b := .["hello", "world", "!"]
	#assert #typeof b == [3]String
	assert b[0] == "hello"
	assert b[1] == "world"
	assert b[2] == "!"
}
