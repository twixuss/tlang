
import "std.tl"

main :: () {
	array: [3]Int
	array[0] = 11
	array[1] = 22
	array[2] = 33

	println(array.count)

	println(array[0])
	println(array[1])
	println(array[2])

	println(array)

	span := array as []Int

	println(span.count)

	println(span[0])
	println(span[1])
	println(span[2])

	println(span)
}
