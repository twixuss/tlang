/*
#builtin
string :: struct {
	data: *u8
	count: int
}

type_kind :: enum {
	void
	bool
	u8
	u16
	u32
	u64
	s8
	s16
	s32
	s64
	f32
	f64
	\struct
	pointer
}

#builtin
struct_member :: struct {
	name: string
	type: *typeinfo
}

#builtin
typeinfo :: struct {
	kind: type_kind
	name: string
	members: []struct_member
	pointee: *typeinfo
}

#builtin
any :: struct {
	pointer: *void
	type: *typeinfo
}
*/

debug_break :: () #intrinsic
memcpy :: (dst: *Void, src: *Void, size: UInt) #intrinsic
debug_print_int :: (value: Int) #intrinsic
mul128 :: (a: Int, b: Int) [2]Int #intrinsic
mul128 :: (a: UInt, b: UInt) [2]UInt #intrinsic
sqrt_F32 :: (a: F32) F32 #intrinsic
sqrt_F64 :: (a: F64) F64 #intrinsic

sqrt :: (a: F32) => sqrt_F32(a)
sqrt :: (a: F64) => sqrt_F64(a)

// RoundingMode :: enum {
// 	to_negative_infinity
// 	to_positive_infinity
// 	to_zero
// 	to_closest_integer
// }
//
// round :: (value: F64, mode: %RoundingMode) F64 #intrinsic
// fraction :: (value: F64, mode: %RoundingMode) => value - round(value, mode)

// FIXME: default arguments
// fraction :: (value: F64) => fraction(value, .to_negative_infinity)

init_runtime :: () {
	init_allocator()
}

Allocator :: struct {
    func:  (size: Int, state: *Void) *Void #type
    state: *Void
}

default_allocator: Allocator
current_allocator: Allocator

init_allocator :: () {
    default_allocator.func = (size: Int, state: *Void) *Void => VirtualAlloc(null, @size, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
    current_allocator = default_allocator
}

allocate :: (me: *Allocator, size: Int) => me.func(size, me.state)

new :: (T: %Type) *T => @current_allocator.allocate(#sizeof T)

import "tlang/windows.tl"
