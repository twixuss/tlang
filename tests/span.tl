
import "std.tl"

main :: () {
	array: [3]int;
	array[0] = 11;
	array[1] = 22;
	array[2] = 33;
	
	println(array.data as int);
	println(array.count);
	
	println(array[0]);
	println(array[1]);
	println(array[2]);
	
	span := array as []int;
	
	println(span.data as int);
	println(span.count);
	
	println(span[0]);
	println(span[1]);
	println(span[2]);
}
