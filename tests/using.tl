import "tlang/std.tl"

Foo :: struct {
	a: Int
	b: String
}

get_foo :: (): using result: Foo {
	a = 53
	b = "haha"
	println(result)
}

write :: (using foo: Foo) {
	println(a)
	println(b)
}

main :: () {
	{
		using foo: Foo
		a = 12
		b = "hello"
		println(foo)
	}
	{
		foo := get_foo()
		using foo
		a = 14
		b = "world"
		write(foo)
	}
}