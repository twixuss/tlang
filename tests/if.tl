import "std.tl"
main :: fn () -> int {
	if true { print_string("true\n"); }
    else    { print_string("false\n"); }
    
	if true { a: int; print_string("true\n"); }
    else    {         print_string("false\n"); }
    
	if true {         print_string("true\n"); }
    else    { a: int; print_string("false\n"); }
    
	if true { a: int; print_string("true\n"); }
    else    { a: int; print_string("false\n"); }
    
	if false { print_string("true\n"); }
    else     { print_string("false\n"); }
    
	if false { a: int; print_string("true\n"); }
    else     {         print_string("false\n"); }
    
	if false {         print_string("true\n"); }
    else     { a: int; print_string("false\n"); }
    
	if false { a: int; print_string("true\n"); }
    else     { a: int; print_string("false\n"); }

    if true return 1;
    if true { return 2; }
    
    if true then return 3;
    if true then { return 3; }
    
    if true return 4; else return 5;
    if true return 4; else { return 5; }
    if true { return 4; } else return 5;
    if true { return 4; } else { return 5; }
    
    if true then return 4; else return 5;
    if true then return 4; else { return 5; }
    if true then { return 4; } else return 5;
    if true then { return 4; } else { return 5; }
    
    return if true 1 else 2;
    return if true then 1 else 2;
}