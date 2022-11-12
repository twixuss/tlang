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
	{

		foo :: (a: Int, b: Int, c: Int) {
			return a * 100 + b * 10 + c
		}

		#assert !#compiles { foo() }
		#assert !#compiles { foo(1) }
		#assert !#compiles { foo(1, 2) }
		#assert !#compiles { foo(1, 2, 3, 4) }
		#assert !#compiles { 1.foo() }
		#assert !#compiles { 1.foo(2) }
		#assert !#compiles { 1.foo(2, 3, 4) }

		#assert foo(1, 2, 3) == 123
		#assert 1.foo(2, 3) == 123
		#assert 1.foo(c=2, 3) == 132
	}
}
