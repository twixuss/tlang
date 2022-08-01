import "std.tl"

MyStruct :: struct {
	kind: Int
	name: String
}
my :: MyStruct(name = "hello", kind = 24)
main :: () {
	info :: #typeinfo #typeof my
	println("name: {}", info.name)
	println("kind: {}", info.kind)
	println("size: {}", info.size)
	println("members: {}", info.members.count)
	for i in 0..info.members.count
		println("{}: {}  offset = {}", info.members[i].name, info.members[i].type.name, info.members[i].offset)
}
