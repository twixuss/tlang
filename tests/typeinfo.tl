import "std.tl"
TYPEINFO :: struct {
	kind: int;
	name: string;
}
typeinfo_string :: TYPEINFO(name = "hello");
main :: () {
	println(typeinfo_string.name);
	println((#typeinfo typeinfo).name);
	println((#typeinfo int).name);
	println((#typeinfo string).name);
	println((#typeinfo #typeof typeinfo_string).name);
}
