main :: () {
	{
		foo :: (a: String) => 0
		foo :: (a: $T) => 1
		foo :: (a: Any) => 2
		#assert foo("x") == 0
		#assert foo("x" as Any) == 2
	}
	{
		foo :: (a: $T) => 1
		foo :: (a: Any) => 2
		#assert foo("y") == 1
	}
	{
		foo :: (a: String) => 0
		foo :: (a: ..String) => 1
		#assert foo("z") == 0
		#assert foo() == 1
		#assert foo("x", "y") == 1
		#assert !#compiles { foo(1) }
	}
	{
		foo :: (a: String) => 0
		foo :: (b: String) => 1
		#assert foo(a="w") == 0
		#assert foo(b="w") == 1
	}
	{
		foo :: (a: String) => 0
		foo :: struct {
			b: String
		}
		#assert foo(a="n") == 0
		x := foo(b="n")
		// FIXME: make constexpr assert work
		assert x.b == "n"
	}
}
