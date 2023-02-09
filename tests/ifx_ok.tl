main :: () {
	#assert #typeof(if true then 1 else 1) == Int
	#assert #typeof(if true then 1 as U8 else 1) == U8

	x := 1
	x + -if true then 1 else 2
}
