// PROGRAM OUTPUT identifier

import "tlang/std.tl"

Kind :: enum {
	literal
	identifier
	lambda
	\struct
	\enum
}

main :: () {
	assert Kind.literal    as S64 == 0
	assert Kind.identifier as S64 == 1
	assert Kind.lambda     as S64 == 2
	assert Kind.\struct    as S64 == 3
	assert Kind.\enum      as S64 == 4

	kind := Kind.identifier

	println(match kind {
		else => "unknown"
		.literal => "literal"
		.identifier => "identifier"
		.\struct => "struct"
		.\enum => "enum"
	})

    return 0
}
