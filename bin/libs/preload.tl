/*
#builtin
string :: struct {
	data: *u8;
	count: int;
}

type_kind :: enum {
	void;
	bool;
	u8;
	u16;
	u32;
	u64;
	s8;
	s16;
	s32;
	s64;
	f32;
	f64;
	\struct;
	pointer;
}

#builtin
struct_member :: struct {
	name: string;
	type: *typeinfo;
}

#builtin
typeinfo :: struct {
	kind: type_kind;
	name: string;
	members: []struct_member;
	pointee: *typeinfo;
}

#builtin
any :: struct {
	pointer: *void;
	type: *typeinfo;
}
*/

debug_break :: fn #intrinsic ();
memcpy :: fn #intrinsic (dst: *void, src: *void, size: uint): void;
