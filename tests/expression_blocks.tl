
import "tlang/std.tl"
main :: () {
	result: Int
	for it in 1..10 do result = if {println(it); it == 7} then break else it
    assert result == 6
}
