foo :: () #macro {
	x = 10
}

bar :: (y: Int) #macro {
	x = y
}

main :: () {
	x := 5
	foo()
	assert x == 10
	bar(15)
	assert x == 15
}
