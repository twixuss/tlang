
Foo :: struct { value: Int }

operator as implicit :: (f: Foo) Int => f.value

foo :: () {
	if true {
		return Foo()
	} else {
		x := 42
		return x
	}
}

bar :: () {
	return true
	return true
}

main :: () {
	#assert #typeof foo() == Int
	#assert #typeof bar() == Bool
}
