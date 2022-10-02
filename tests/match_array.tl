import "tlang/std.tl"

Kind :: enum {
	literal;
	identifier;
	lambda;
	\struct;
	\enum;
}

main :: () {
	kind := Kind.identifier

	match kind {
		.literal => println("literal")
		.[.identifier, .lambda] => println("identifier or lambda")
		.\struct => println("struct")
		.\enum => println("enum")
	}
}
