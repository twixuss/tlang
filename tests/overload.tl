main :: () {
	{
		foo :: (a: String) => 0
		foo :: (a: $T) => 1
		foo :: (a: Any) => 2
		assert foo("x") == 0
		assert foo("x" as Any) == 2
	}
	{
		foo :: (a: $T) => 1
		foo :: (a: Any) => 2
		assert foo("y") == 1
	}
	{
		foo :: (a: String) => 0
		foo :: (a: ..String) => 1
		assert foo("z") == 0
		assert foo() == 1
		assert foo("x", "y") == 1
		assert !#compiles { foo(1) }
	}
	{
		foo :: (a: String) => 0
		foo :: (b: String) => 1
		assert foo(a="w") == 0
		assert foo(b="w") == 1
	}
	{
		foo :: (a: String) => 0
		foo :: struct {
			b: String
		}
		assert foo(a="n") == 0
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

		assert foo(1, 2, 3) == 123
		assert 1.foo(2, 3) == 123
		assert 1.foo(c=2, 3) == 132
	}


	// ======================================= BASIC
	{
		foo :: () => 1
		assert foo() == 1
		#assert !#compiles { foo(0) }
		#assert !#compiles { foo("") }
		#assert !#compiles { foo(0, 0) }
		#assert !#compiles { foo("", 0) }
		#assert !#compiles { foo(0, "") }
		#assert !#compiles { foo("", "") }
	}
	{
		foo :: (a: Int) => 1
		foo :: (a: String) => 2
		assert foo(0) == 1
		assert foo("") == 2
		#assert !#compiles { foo() }
		#assert !#compiles { foo(0, 0) }
		#assert !#compiles { foo("", 0) }
		#assert !#compiles { foo(0, "") }
	}
	{
		foo :: (a: Int, b: String) => 1
		foo :: (a: String, b: String) => 2
		assert foo(0, "") == 1
		assert foo("", "") == 2
		#assert !#compiles { foo() }
		#assert !#compiles { foo(0, 0) }
		#assert !#compiles { foo("", 0) }
	}
	{
		foo :: (a: String, b: Int) => 1
		foo :: (a: String, b: String) => 2
		assert foo("", 0) == 1
		assert foo("", "") == 2
		#assert !#compiles { foo() }
		#assert !#compiles { foo(0, 0) }
		#assert !#compiles { foo(0, "") }
	}
	{
		foo :: (a: String) => 1
		foo :: () => 2
		assert foo("") == 1
		assert foo() == 2
		#assert !#compiles { foo(0) }
		#assert !#compiles { foo(0, "") }
		#assert !#compiles { foo("", 0) }
	}

	// ===================================== BASIC PACKS
	{
		foo :: (a: ..Int) => a.count
		foo :: (a: Int, b: Int, c: Int) => 42
		assert foo() == 0 // FIXME: these asserts can't be comptime rn
		assert foo(0) == 1
		assert foo(0,0) == 2
		assert foo(0,0,0) == 42
		#assert !#compiles { foo("") }
		#assert !#compiles { foo("", 0) }
		#assert !#compiles { foo(0, "") }
		#assert !#compiles { foo("", "") }
	}
	{
		foo :: (a: ..Int, b: ..String) => a.count * 10 + b.count
		assert foo() == 0 // FIXME: these asserts can't be comptime rn
		assert foo("") == 1
		assert foo(0) == 10
		assert foo("","") == 2
		assert foo(0,"") == 11
		assert foo(0,0) == 20
		assert foo(0,"","") == 12
		assert foo(0,0,"") == 21
		assert foo(0,0,"","") == 22
		#assert !#compiles { foo("",0) }
		#assert !#compiles { foo("",0,0) }
		#assert !#compiles { foo("","",0) }
		#assert !#compiles { foo("","",0,0) }
	}

	// ========================================= BASIC TEMPLATES
	{
		foo :: (a: $T) { return (#typeinfo T).name }
		assert foo(0) == "S64"
		assert foo("") == "String"
		#assert !#compiles { foo() }
		#assert !#compiles { foo(0,0) }
		#assert !#compiles { foo("","") }
	}
	{
		foo :: (a: $T, b: T) { return (#typeinfo T).name }
		assert foo(0,0) == "S64"
		assert foo("","") == "String"
		#assert !#compiles { foo() }
		#assert !#compiles { foo("",0) }
		#assert !#compiles { foo(0,"") }
	}
	{
		foo :: (a: $T, b: $U) { return (#typeinfo T).size * 100 + (#typeinfo U).size }
		assert foo(0,0) == 0808
		assert foo("","") == 1616
		assert foo("",0) == 1608
		assert foo(0,"") == 0816
		#assert !#compiles { foo() }
		#assert !#compiles { foo(0) }
		#assert !#compiles { foo("") }
	}

	// ====================== Any packs
	{
		foo :: (a: ..Any) { return a.count }
		foo :: (a: $T) { return 1337 }
		foo :: (a: Int, b: Int) { return 42 }
		assert foo() == 0
		assert foo(1) == 1337
		assert foo(1, "2") == 2
		assert foo(1, 2) == 42
	}
	{
		foo :: (a: ..Any) => a.count
		bar :: (a: ..Any) => foo(a)

		assert bar(1, 2, 3) == 3
		assert bar(.[1, 2, 3]) == 1
	}
	// ====================== Typed packs
	{
		foo :: (a: ..Int) => a.count
		bar :: (a: ..Int) => foo(a)

		assert bar(1, 2, 3) == 3
	}
	// ====================== Const
	{
		foo :: (a: %Int) {
			arr: U8[a]
			return a
		}

		assert foo(1) == 1
		assert foo(44) == 44
	}
}
