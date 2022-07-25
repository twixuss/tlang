import "std.tl"
import "opengl.tl"

dc: HDC;

draw :: () {
    glColor3f(1, 1, 1);
    glBegin(GL_TRIANGLES);

    glVertex2f(-0.5, -0.5);
    glVertex2f( 0.0,  0.5);
    glVertex2f( 0.5, -0.5);

    glEnd();

    SwapBuffers(dc);
}

main :: (): Int {
	class_name := "window_class\0".data;
	window_name := "hello window\0".data;

	hInstance := GetModuleHandleA(null);

	wc : WNDCLASSEXA;
	wc.hInstance = hInstance;
	wc.cbSize = #sizeof WNDCLASSEXA;
	wc.lpfnWndProc = (hwnd: HWND, uMsg: UINT, wParam: WPARAM, lParam: LPARAM): LRESULT {
		if uMsg == WM_DESTROY {
			PostQuitMessage(0);
		} else if uMsg == WM_SIZE {
            glViewport(0, 0, (lParam & 0xffff) as S32, ((lParam >> 16) & 0xffff) as S32);
            draw();
        } else {
            return DefWindowProcA(hwnd, uMsg, wParam, lParam);
        }
        return 0;
	};
	wc.lpszClassName = class_name;
	wc.hCursor = LoadCursorA(null, IDC_ARROW);
    
	println(wc);
	
    print(if RegisterClassExA(&wc) != 0 then "Class created!\n" else "Class Failed!\n");
	
	window := CreateWindowExA(
		0, class_name, window_name, WS_OVERLAPPEDWINDOW | WS_VISIBLE, 
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, null, null, hInstance, null
	);
	
	print(if window != null then "Window Success!\n" else "Window Fail!\n");
	
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
	
    dc = GetDC(window);
    
    index := ChoosePixelFormat(dc, &dp);
	if index == 0 {
		error := GetLastError();
		print("ChoosePixelFormat failed with code ");
		print(error);
		return 1;
	}
    
	sp : PIXELFORMATDESCRIPTOR;
	DescribePixelFormat(dc, index, #sizeof #typeof sp, &sp);

	SetPixelFormat(dc, index, &sp);

	context := wglCreateContext(dc);
	if wglMakeCurrent(dc, context) != 1 {
		print("wglMakeCurrent failed");
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
        
        draw();
	}
}
