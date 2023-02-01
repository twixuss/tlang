
main :: () {
	{
		foo :: (a, b: $T) => a + b
		assert foo(20, 22) == 42
	}
}
