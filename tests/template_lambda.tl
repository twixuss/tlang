
main :: () {
	{
		foo :: (a, b: $T) => a + b
		assert foo(20, 22) == 42
	}
	{
		Array :: struct (A: Type, B: Int) {
			a: [B]A
		}

		foo :: (arr: *Array($T, 32)) {
			arr.a[0] = arr.B
		}

		x: Any = ""

		arr: Array(Int, 32)
		foo(&arr)
		assert arr.a[0] == 32
	}
}
