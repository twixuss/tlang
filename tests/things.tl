import "windows.tl"

standard_output_handle : HANDLE;
global_string : string;

AStruct :: struct {
	data : *void;
	count : uint;
}

main :: fn () -> s32 {
    standard_output_handle = GetStdHandle(STD_OUTPUT_HANDLE);

    str : string = "Hello World!\n";
    WriteConsoleA(standard_output_handle, str.data, str.count as u32, null, null);
    WriteConsoleA(standard_output_handle, "Hello World!\n".data, 13, null, null);
    print("Hello World!\n");
    print(str);

	if true {
		dd : u64; // block-local variable
		print("true\n");
	}
	
	if 0 == 0 { print("0 is 0\n"); }
	else      { print("0 is not 0\n"); }


	if 1 < 2 print("1 is less than 2\n");
	
	if true print("true\n");
	else  { print("false\n"); }
	
	if false { print("true\n"); }
	else       print("false\n");

	if 3 >= 0 print("3 is >= 0\n");
	else      print("3 is not >= 0\n");
		
	i := 10;
	
	while i > 0 {
		j := i;
		while j > 0 {
			print("*");
			j = j - 1;
		}
		print("\n");
		i = i - 1;
	}

	STR := "my file.txt\0";
	print(STR);
	file := CreateFileA(STR.data, GENERIC_WRITE, 0, null, CREATE_ALWAYS, /*FILE_ATTRIBUTE_NORMAL*/ 0, null);

	content := "Hello World!";
	WriteFile(file, content.data, content.count as u32, null, null);

	global_string = "A global string";
	print(global_string);

	local_string : string;
	local_string = "a local string";
	print(local_string);

	a_struct : AStruct;
	a_struct.count = 7;
	a_struct.data = "AStruct".data;
    WriteConsoleA(standard_output_handle, a_struct.data, a_struct.count as u32, null, null);
	
	a_string : string;
	a_string.count = 7;
	a_string.data = "A STRING".data;
	print(a_string);

    return x as s32;
}

x :: 42;

print :: fn (str: string) {
    WriteConsoleA(standard_output_handle, str.data, str.count as u32, null, null);
}
