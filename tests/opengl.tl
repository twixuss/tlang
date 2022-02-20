import "std.tl"
import "opengl.tl"

main :: fn () -> int {
	class_name := "window_class\0".data;
	window_name := "hello window\0".data;

	hInstance := GetModuleHandleA(null);

	wc : WNDCLASSEXA;
	wc.hInstance = hInstance;
	wc.cbSize = #sizeof WNDCLASSEXA;
	wc.lpfnWndProc = fn #stdcall (hwnd : HWND, uMsg : UINT, wParam : WPARAM, lParam : LPARAM) -> LRESULT {
		if uMsg == WM_DESTROY {
			PostQuitMessage(0);
			return 0;
		}
		return DefWindowProcA(hwnd, uMsg, wParam, lParam);
	};
	wc.lpszClassName = class_name;
	wc.hCursor = LoadCursorA(null, IDC_ARROW);
    
    print_string(if RegisterClassExA(&wc) != 0 then "Class created!\n" else "Class Failed!\n");
	
	window := CreateWindowExA(
		0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE, 
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
	);
	
	print_string(if window != null then "Window Success!\n" else "Window Fail!\n");
	
	dp : PIXELFORMATDESCRIPTOR;
	dp.nSize = #sizeof PIXELFORMATDESCRIPTOR;
	dp.nVersion = 1;
	dp.dwFlags = PFD_SUPPORT_OPENGL | PFD_DRAW_TO_WINDOW | PFD_DOUBLEBUFFER;
	dp.cColorBits = 32;
	dp.cAlphaBits = 8;
	dp.cDepthBits = 24;
	dp.cStencilBits = 8;
	dp.iPixelType = PFD_TYPE_RGBA;
	dp.iLayerType = PFD_MAIN_PLANE;
	
    dc := GetDC(window);
    
    index := ChoosePixelFormat(dc, &dp);
	if index == 0 {
		error := GetLastError();
		print_string("ChoosePixelFormat failed with code ");
		print_int   (error);
		return 1;
	}
    
	sp : PIXELFORMATDESCRIPTOR;
	DescribePixelFormat(dc, index, #sizeof #typeof sp, &sp);

	SetPixelFormat(dc, index, &sp);

	context := wglCreateContext(dc);
	if wglMakeCurrent(dc, context) != 1 {
		print_string("wglMakeCurrent failed");
		return 1;
	}
    
	while true {
        msg : MSG;
		while PeekMessageA(&msg, null, 0, 0, PM_REMOVE) != 0 {
			if msg.message == WM_QUIT
				return;
			TranslateMessage(&msg);
			DispatchMessageA(&msg);
		}
        
        SwapBuffers(dc);
	}
}
