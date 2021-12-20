/*
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

    WriteConsoleA(standard_output_handle, str.data, str.count, null, null);
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
	WriteFile(file, content.data, content.count, null, null);

	global_string = "A global string";
	print(global_string);

	local_string : string;
	local_string = "a local string";
	print(local_string);

	a_struct : AStruct;
	a_struct.count = 7;
	a_struct.data = "AStruct".data;
    WriteConsoleA(standard_output_handle, a_struct.data, a_struct.count, null, null);
	
	a_string : string;
	a_string.count = 7;
	a_string.data = "A STRING".data;
	print(a_string);

    return x as s32;
}

x :: 42;

print :: fn (str: string) {
    WriteConsoleA(standard_output_handle, str.data, str.count, null, null);
}
*/


import "std.tl"

wnd_proc :: fn #stdcall (hwnd : HWND, uMsg : UINT, wParam : WPARAM, lParam : LPARAM) -> LRESULT {
	if uMsg == WM_DESTROY {
		PostQuitMessage(0);
		return 0;
	}
	return DefWindowProcA(hwnd, uMsg, wParam, lParam);
}

main :: fn () {
	class_name := "window_class\0".data;
	window_name := "hello window\0".data;

	hInstance := GetModuleHandleA(null);

	wc : WNDCLASSEXA;
	wc.hInstance = hInstance;
	wc.cbSize = #sizeof WNDCLASSEXA;
	wc.lpfnWndProc = &wnd_proc;
	//wc.lpfnWndProc = &DefWindowProcA;
	wc.lpszClassName = class_name;
	if RegisterClassExA(&wc) != 0 {
		print_string("Class created!\n");
	} else {
		print_string("Class Failed!\n");
	}
	
	window := CreateWindowExA(
		0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE, 
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
	);
	
	if window != null {
		print_string("Window Success!\n");
	} else {
		print_string("Window Fail!\n");
	}
	
	msg : MSG;
	
	while true {
		while PeekMessageA(&msg, null, 0, 0, PM_REMOVE) != 0 {
			if msg.message == WM_QUIT
				return;
			TranslateMessage(&msg);
			DispatchMessageA(&msg);
		}
	}
	
}
/*
foo :: fn (x: int) -> int {
	if x == 0 {
		main();
		return 0;
	}
	return x;
}

main :: fn () -> int {
	return foo(12);
}
*/
/*
foo :: fn (x: u8) -> u8 {
	return x;
}

main :: fn () -> int {
	a :: "HILLO";
	return foo(a.count as u8);
}
*/

/*
import "std.tl"
main :: fn () {
	print_string("hello");
}
*/

/*

foo :: fn (data : *u8, size : uint) {
}

main :: fn () -> int {
	hello :: "hello";
	foo(hello.data, hello.count);
}

*/

/*
import "std.tl"

x :: u8;

main :: fn () {
	a : x;
	print_hex(&a);
	print_char('\n');

	{
		b : x;
		print_hex(&b);
		print_char('\n');
		c : x;
		print_hex(&c);
		print_char('\n');

		{
			d : x;
			print_hex(&d);
			print_char('\n');
		}
	}
	
	e : x;
	print_hex(&e);
	print_char('\n');
}

*/