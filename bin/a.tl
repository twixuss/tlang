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

foo :: fn #stdcall (x : int, y : int, z : int, w : int) -> int {
	return w;
}

main :: fn () -> int {
	return foo(1, 2, 3, 4);
}
*/


/*
X :: struct {
	a : int;
	b : int;
}

main :: fn () -> int {
	x : X;
	x.a = 1;
	x.b = 2;
	return x.b;
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