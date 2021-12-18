bits 64
extern ExitProcess
extern WNDPROC
extern GetStdHandle
extern WriteConsoleA
extern CreateFileA
extern WriteFile
extern GetLastError
extern VirtualAlloc
extern GetModuleHandleA
extern RegisterClassExA
extern DefWindowProcA
extern CreateWindowExA
extern PeekMessageA
extern TranslateMessage
extern DispatchMessageA
extern PostQuitMessage
section .rodata
constants: db 2,0,0,0,0,0,0,0,119,105,110,100,111,119,95,99,108,97,115,115,0,104,101,108,108,111,32,119,105,110,100,111,119,0,67,108,97,115,115,32,99,114,101,97,116,101,100,33,10,67,108,97,115,115,32,70,97,105,108,101,100,33,10,0,0,0,128,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,16,0,0,0,0,87,105,110,100,111,119,32,83,117,99,99,101,115,115,33,10,87,105,110,100,111,119,32,70,97,105,108,33,10,1,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,245,255,255,255,255,255,255,255,246,255,255,255,255,255,255,255,244,255,255,255,255,255,255,255,0,0,0,128,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,136,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,
section .data
data: db 
section .bss
zeros: resb 0
section .text
global main
main:
push 0
call .2q
pop rcx
sub rsp, 16
call ExitProcess
ret
.0: push        rbp
.1: mov         rbp, rsp
; lambda wnd_proc;reserve space for return parameter
.2: sub         rsp, 8
.3: push        r9
.4: push        r8
.5: push        rdx
.6: push        rcx
.7: push 180288206
.8: push        rbp
.9: mov         rbp, rsp
; binary ==;load identifer uMsg
.a: sub         rsp, 8
.b: push        rsp
; push_address_of uMsg
.c: push        rbp
.d: add         qword [rsp], 24
; copy 4 bytes from uMsg into stack, reverse=false
.e: pop         rax
.f: pop         rbx
.g: mov         cl, byte [rax]
.h: mov         byte [rbx], cl
.i: add         rax, 1
.j: add         rbx, 1
.k: mov         cl, byte [rax]
.l: mov         byte [rbx], cl
.m: add         rax, 1
.n: add         rbx, 1
.o: mov         cl, byte [rax]
.p: mov         byte [rbx], cl
.q: add         rax, 1
.r: add         rbx, 1
.s: mov         cl, byte [rax]
.t: mov         byte [rbx], cl
; load identifer WM_DESTROY
.u: sub         rsp, 8
.v: push        rsp
; push_address_of WM_DESTROY
.w: mov rax, constants + 0
push rax
; copy 8 bytes from WM_DESTROY into stack, reverse=false
.x: pop         rax
.y: pop         rbx
.z: mov         rcx, qword [rax]
.A: mov         qword [rbx], rcx
.B: pop         rbx
.C: pop         rax
.D: push 1
mov rcx, 0
cmp rax, rbx
cmove rcx, qword [rsp]
add rsp, 8
.E: push        rcx
.F: pop         rax
.G: test rax, rax
jz .1a
; call PostQuitMessage
.H: mov         rax, rsp
.I: and         rsp, -16
.J: push        rax
.K: sub         rsp, 8
; literal 0
.L: push 0
.M: pop         rcx
.N: sub         rsp, 32
.O: call        PostQuitMessage
.P: add         rsp, 40
.Q: pop         rsp
.R: push        rax
; return;literal 0
.S: push 0
.T: push        rbp
.U: add         qword [rsp], 48
.V: push        rsp
.W: add         qword [rsp], 8
; copy 8 bytes from expression into parameter, reverse=false
.X: pop         rax
.Y: pop         rbx
.Z: mov         rcx, qword [rax]
.10: mov         qword [rbx], rcx
.11: mov         rsp, rbp
.12: pop         rbp
.13: add         rsp, 40
.14: pop         rax
.15: mov         rsp, rbp
.16: pop         rbp
.17: ret
.18: add         rsp, 0
.19: jmp         .1a
.1a: add         rsp, 0
; return;call DefWindowProcA
.1b: mov         rax, rsp
.1c: and         rsp, -16
.1d: push        rax
.1e: sub         rsp, 8
; load identifer lParam
.1f: sub         rsp, 8
.1g: push        rsp
; push_address_of lParam
.1h: push        rbp
.1i: add         qword [rsp], 40
; copy 8 bytes from lParam into stack, reverse=false
.1j: pop         rax
.1k: pop         rbx
.1l: mov         rcx, qword [rax]
.1m: mov         qword [rbx], rcx
; load identifer wParam
.1n: sub         rsp, 8
.1o: push        rsp
; push_address_of wParam
.1p: push        rbp
.1q: add         qword [rsp], 32
; copy 8 bytes from wParam into stack, reverse=false
.1r: pop         rax
.1s: pop         rbx
.1t: mov         rcx, qword [rax]
.1u: mov         qword [rbx], rcx
; load identifer uMsg
.1v: sub         rsp, 8
.1w: push        rsp
; push_address_of uMsg
.1x: push        rbp
.1y: add         qword [rsp], 24
; copy 4 bytes from uMsg into stack, reverse=false
.1z: pop         rax
.1A: pop         rbx
.1B: mov         cl, byte [rax]
.1C: mov         byte [rbx], cl
.1D: add         rax, 1
.1E: add         rbx, 1
.1F: mov         cl, byte [rax]
.1G: mov         byte [rbx], cl
.1H: add         rax, 1
.1I: add         rbx, 1
.1J: mov         cl, byte [rax]
.1K: mov         byte [rbx], cl
.1L: add         rax, 1
.1M: add         rbx, 1
.1N: mov         cl, byte [rax]
.1O: mov         byte [rbx], cl
; load identifer hwnd
.1P: sub         rsp, 8
.1Q: push        rsp
; push_address_of hwnd
.1R: push        rbp
.1S: add         qword [rsp], 16
; copy 8 bytes from hwnd into stack, reverse=false
.1T: pop         rax
.1U: pop         rbx
.1V: mov         rcx, qword [rax]
.1W: mov         qword [rbx], rcx
.1X: pop         rcx
.1Y: pop         rdx
.1Z: pop         r8
.20: pop         r9
.21: sub         rsp, 32
.22: call        DefWindowProcA
.23: add         rsp, 40
.24: pop         rsp
.25: push        rax
.26: push        rbp
.27: add         qword [rsp], 48
.28: push        rsp
.29: add         qword [rsp], 8
; copy 8 bytes from expression into parameter, reverse=false
.2a: pop         rax
.2b: pop         rbx
.2c: mov         rcx, qword [rax]
.2d: mov         qword [rbx], rcx
.2e: mov         rsp, rbp
.2f: pop         rbp
.2g: add         rsp, 40
.2h: pop         rax
.2i: mov         rsp, rbp
.2j: pop         rbp
.2k: ret
.2l: mov         rsp, rbp
.2m: pop         rbp
.2n: mov         rsp, rbp
.2o: pop         rbp
.2p: ret
.2q: push        rbp
.2r: mov         rbp, rsp
; lambda main;definition class_name;binary .
.2s: sub         rsp, 8
; literal "window_class\0"
.2t: push 13
.2u: mov rax, constants + 8
push rax
.2v: push        rsp
.2w: add         qword [rsp], 16
.2x: push        rsp
.2y: add         qword [rsp], 8
; copy 8 bytes from "window_class\0".data into stack, reverse=true
.2z: pop         rax
.2A: pop         rbx
.2B: mov         rcx, qword [rax]
.2C: mov         qword [rbx], rcx
.2D: add         rsp, 16
; definition window_name;binary .
.2E: sub         rsp, 8
; literal "hello window\0"
.2F: push 13
.2G: mov rax, constants + 21
push rax
.2H: push        rsp
.2I: add         qword [rsp], 16
.2J: push        rsp
.2K: add         qword [rsp], 8
; copy 8 bytes from "hello window\0".data into stack, reverse=true
.2L: pop         rax
.2M: pop         rbx
.2N: mov         rcx, qword [rax]
.2O: mov         qword [rbx], rcx
.2P: add         rsp, 16
; definition hInstance;call GetModuleHandleA
.2Q: mov         rax, rsp
.2R: and         rsp, -16
.2S: push        rax
.2T: sub         rsp, 8
; literal null
.2U: push 0
.2V: pop         rcx
.2W: sub         rsp, 32
.2X: call        GetModuleHandleA
.2Y: add         rsp, 40
.2Z: pop         rsp
.30: push        rax
; definition wc
.31: push 0
.32: push 0
.33: push 0
.34: push 0
.35: push 0
.36: push 0
.37: push 0
.38: push 0
.39: push 0
.3a: push 0
; binary =;load identifer hInstance
.3b: sub         rsp, 8
.3c: push        rsp
; push_address_of hInstance
.3d: push        rbp
.3e: add         qword [rsp], -24
; copy 8 bytes from hInstance into stack, reverse=false
.3f: pop         rax
.3g: pop         rbx
.3h: mov         rcx, qword [rax]
.3i: mov         qword [rbx], rcx
; push_address_of wc.hInstance;push_address_of wc
.3j: push        rbp
.3k: add         qword [rsp], -104
.3l: add         qword [rsp], 24
.3m: push        rsp
.3n: add         qword [rsp], 8
; copy 8 bytes from hInstance into wc.hInstance, reverse=false
.3o: pop         rax
.3p: pop         rbx
.3q: mov         rcx, qword [rax]
.3r: mov         qword [rbx], rcx
.3s: add         rsp, 8
; binary =;literal 
.3t: push 80
; push_address_of wc.cbSize;push_address_of wc
.3u: push        rbp
.3v: add         qword [rsp], -104
.3w: push        rsp
.3x: add         qword [rsp], 8
; copy 4 bytes from  into wc.cbSize, reverse=false
.3y: pop         rax
.3z: pop         rbx
.3A: mov         cl, byte [rax]
.3B: mov         byte [rbx], cl
.3C: add         rax, 1
.3D: add         rbx, 1
.3E: mov         cl, byte [rax]
.3F: mov         byte [rbx], cl
.3G: add         rax, 1
.3H: add         rbx, 1
.3I: mov         cl, byte [rax]
.3J: mov         byte [rbx], cl
.3K: add         rax, 1
.3L: add         rbx, 1
.3M: mov         cl, byte [rax]
.3N: mov         byte [rbx], cl
.3O: add         rsp, 8
; binary =;push_address_of wnd_proc
.3P: mov rax, .0
push rax
; push_address_of wc.lpfnWndProc;push_address_of wc
.3Q: push        rbp
.3R: add         qword [rsp], -104
.3S: add         qword [rsp], 8
.3T: push        rsp
.3U: add         qword [rsp], 8
; copy 8 bytes from &wnd_proc into wc.lpfnWndProc, reverse=false
.3V: pop         rax
.3W: pop         rbx
.3X: mov         rcx, qword [rax]
.3Y: mov         qword [rbx], rcx
.3Z: add         rsp, 8
; binary =;load identifer class_name
.40: sub         rsp, 8
.41: push        rsp
; push_address_of class_name
.42: push        rbp
.43: add         qword [rsp], -8
; copy 8 bytes from class_name into stack, reverse=false
.44: pop         rax
.45: pop         rbx
.46: mov         rcx, qword [rax]
.47: mov         qword [rbx], rcx
; push_address_of wc.lpszClassName;push_address_of wc
.48: push        rbp
.49: add         qword [rsp], -104
.4a: add         qword [rsp], 64
.4b: push        rsp
.4c: add         qword [rsp], 8
; copy 8 bytes from class_name into wc.lpszClassName, reverse=false
.4d: pop         rax
.4e: pop         rbx
.4f: mov         rcx, qword [rax]
.4g: mov         qword [rbx], rcx
.4h: add         rsp, 8
; binary !=;call RegisterClassExA
.4i: mov         rax, rsp
.4j: and         rsp, -16
.4k: push        rax
.4l: sub         rsp, 8
; push_address_of wc
.4m: push        rbp
.4n: add         qword [rsp], -104
.4o: pop         rcx
.4p: sub         rsp, 32
.4q: call        RegisterClassExA
.4r: add         rsp, 40
.4s: pop         rsp
.4t: push        rax
; literal 0
.4u: push 0
.4v: pop         rbx
.4w: pop         rax
.4x: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
.4y: push        rcx
.4z: pop         rax
.4A: test rax, rax
jz .4I
; call print_string
.4B: sub         rsp, 0
; literal "Class created!\n"
.4C: push 15
.4D: mov rax, constants + 34
push rax
.4E: call        .8V
.4F: add         rsp, 16
.4G: add         rsp, 0
.4H: jmp         .4N
; call print_string
.4I: sub         rsp, 0
; literal "Class Failed!\n"
.4J: push 14
.4K: mov rax, constants + 49
push rax
.4L: call        .8V
.4M: add         rsp, 16
.4N: add         rsp, 0
; definition window;call CreateWindowExA
.4O: mov         rax, rsp
.4P: and         rsp, -16
.4Q: push        rax
.4R: sub         rsp, 8
; literal null
.4S: push 0
; load identifer hInstance
.4T: sub         rsp, 8
.4U: push        rsp
; push_address_of hInstance
.4V: push        rbp
.4W: add         qword [rsp], -24
; copy 8 bytes from hInstance into stack, reverse=false
.4X: pop         rax
.4Y: pop         rbx
.4Z: mov         rcx, qword [rax]
.50: mov         qword [rbx], rcx
; literal null
.51: push 0
; literal null
.52: push 0
; load identifer CW_USEDEFAULT
.53: sub         rsp, 8
.54: push        rsp
; push_address_of CW_USEDEFAULT
.55: mov rax, constants + 63
push rax
; copy 8 bytes from CW_USEDEFAULT into stack, reverse=false
.56: pop         rax
.57: pop         rbx
.58: mov         rcx, qword [rax]
.59: mov         qword [rbx], rcx
; load identifer CW_USEDEFAULT
.5a: sub         rsp, 8
.5b: push        rsp
; push_address_of CW_USEDEFAULT
.5c: mov rax, constants + 63
push rax
; copy 8 bytes from CW_USEDEFAULT into stack, reverse=false
.5d: pop         rax
.5e: pop         rbx
.5f: mov         rcx, qword [rax]
.5g: mov         qword [rbx], rcx
; load identifer CW_USEDEFAULT
.5h: sub         rsp, 8
.5i: push        rsp
; push_address_of CW_USEDEFAULT
.5j: mov rax, constants + 63
push rax
; copy 8 bytes from CW_USEDEFAULT into stack, reverse=false
.5k: pop         rax
.5l: pop         rbx
.5m: mov         rcx, qword [rax]
.5n: mov         qword [rbx], rcx
; load identifer CW_USEDEFAULT
.5o: sub         rsp, 8
.5p: push        rsp
; push_address_of CW_USEDEFAULT
.5q: mov rax, constants + 63
push rax
; copy 8 bytes from CW_USEDEFAULT into stack, reverse=false
.5r: pop         rax
.5s: pop         rbx
.5t: mov         rcx, qword [rax]
.5u: mov         qword [rbx], rcx
; binary |;load identifer WS_OVERLAPPEDWINDOW
.5v: sub         rsp, 8
.5w: push        rsp
; push_address_of WS_OVERLAPPEDWINDOW
.5x: mov rax, constants + 71
push rax
; copy 8 bytes from WS_OVERLAPPEDWINDOW into stack, reverse=false
.5y: pop         rax
.5z: pop         rbx
.5A: mov         rcx, qword [rax]
.5B: mov         qword [rbx], rcx
; load identifer WS_VISIBLE
.5C: sub         rsp, 8
.5D: push        rsp
; push_address_of WS_VISIBLE
.5E: mov rax, constants + 79
push rax
; copy 8 bytes from WS_VISIBLE into stack, reverse=false
.5F: pop         rax
.5G: pop         rbx
.5H: mov         rcx, qword [rax]
.5I: mov         qword [rbx], rcx
.5J: pop         rax
.5K:  or         qword [rsp], rax
; load identifer window_name
.5L: sub         rsp, 8
.5M: push        rsp
; push_address_of window_name
.5N: push        rbp
.5O: add         qword [rsp], -16
; copy 8 bytes from window_name into stack, reverse=false
.5P: pop         rax
.5Q: pop         rbx
.5R: mov         rcx, qword [rax]
.5S: mov         qword [rbx], rcx
; load identifer class_name
.5T: sub         rsp, 8
.5U: push        rsp
; push_address_of class_name
.5V: push        rbp
.5W: add         qword [rsp], -8
; copy 8 bytes from class_name into stack, reverse=false
.5X: pop         rax
.5Y: pop         rbx
.5Z: mov         rcx, qword [rax]
.60: mov         qword [rbx], rcx
; literal 0
.61: push 0
.62: pop         rcx
.63: pop         rdx
.64: pop         r8
.65: pop         r9
.66: sub         rsp, 32
.67: call        CreateWindowExA
.68: add         rsp, 104
.69: pop         rsp
.6a: push        rax
; binary !=;load identifer window
.6b: sub         rsp, 8
.6c: push        rsp
; push_address_of window
.6d: push        rbp
.6e: add         qword [rsp], -112
; copy 8 bytes from window into stack, reverse=false
.6f: pop         rax
.6g: pop         rbx
.6h: mov         rcx, qword [rax]
.6i: mov         qword [rbx], rcx
; literal null
.6j: push 0
.6k: pop         rbx
.6l: pop         rax
.6m: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
.6n: push        rcx
.6o: pop         rax
.6p: test rax, rax
jz .6x
; call print_string
.6q: sub         rsp, 0
; literal "Window Success!\n"
.6r: push 16
.6s: mov rax, constants + 87
push rax
.6t: call        .8V
.6u: add         rsp, 16
.6v: add         rsp, 0
.6w: jmp         .6C
; call print_string
.6x: sub         rsp, 0
; literal "Window Fail!\n"
.6y: push 13
.6z: mov rax, constants + 103
push rax
.6A: call        .8V
.6B: add         rsp, 16
.6C: add         rsp, 0
; definition msg
.6D: push 0
.6E: push 0
.6F: push 0
.6G: push 0
.6H: push 0
.6I: push 0
; literal true
.6J: push 1
.6K: pop         rax
.6L: test rax, rax
jz .8S
; binary !=;call PeekMessageA
.6M: mov         rax, rsp
.6N: and         rsp, -16
.6O: push        rax
; load identifer PM_REMOVE
.6P: sub         rsp, 8
.6Q: push        rsp
; push_address_of PM_REMOVE
.6R: mov rax, constants + 116
push rax
; copy 8 bytes from PM_REMOVE into stack, reverse=false
.6S: pop         rax
.6T: pop         rbx
.6U: mov         rcx, qword [rax]
.6V: mov         qword [rbx], rcx
; literal 0
.6W: push 0
; literal 0
.6X: push 0
; literal null
.6Y: push 0
; push_address_of msg
.6Z: push        rbp
.70: add         qword [rsp], -160
.71: pop         rcx
.72: pop         rdx
.73: pop         r8
.74: pop         r9
.75: sub         rsp, 32
.76: call        PeekMessageA
.77: add         rsp, 40
.78: pop         rsp
.79: push        rax
; literal 0
.7a: push 0
.7b: pop         rbx
.7c: pop         rax
.7d: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
.7e: push        rcx
.7f: pop         rax
.7g: test rax, rax
jz .8Q
; binary ==;binary .
.7h: sub         rsp, 8
; load identifer msg
.7i: sub         rsp, 48
.7j: push        rsp
; push_address_of msg
.7k: push        rbp
.7l: add         qword [rsp], -160
; copy 48 bytes from msg into stack, reverse=false
.7m: pop         rax
.7n: pop         rbx
.7o: mov         rcx, qword [rax]
.7p: mov         qword [rbx], rcx
.7q: add         rax, 8
.7r: add         rbx, 8
.7s: mov         rcx, qword [rax]
.7t: mov         qword [rbx], rcx
.7u: add         rax, 8
.7v: add         rbx, 8
.7w: mov         rcx, qword [rax]
.7x: mov         qword [rbx], rcx
.7y: add         rax, 8
.7z: add         rbx, 8
.7A: mov         rcx, qword [rax]
.7B: mov         qword [rbx], rcx
.7C: add         rax, 8
.7D: add         rbx, 8
.7E: mov         rcx, qword [rax]
.7F: mov         qword [rbx], rcx
.7G: add         rax, 8
.7H: add         rbx, 8
.7I: mov         rcx, qword [rax]
.7J: mov         qword [rbx], rcx
.7K: push        rsp
.7L: add         qword [rsp], 48
.7M: push        rsp
.7N: add         qword [rsp], 16
; copy 4 bytes from msg.message into stack, reverse=true
.7O: pop         rax
.7P: pop         rbx
.7Q: add         rax, 3
.7R: add         rbx, 3
.7S: mov         cl, byte [rax]
.7T: mov         byte [rbx], cl
.7U: sub         rax, 1
.7V: sub         rbx, 1
.7W: mov         cl, byte [rax]
.7X: mov         byte [rbx], cl
.7Y: sub         rax, 1
.7Z: sub         rbx, 1
.80: mov         cl, byte [rax]
.81: mov         byte [rbx], cl
.82: sub         rax, 1
.83: sub         rbx, 1
.84: mov         cl, byte [rax]
.85: mov         byte [rbx], cl
.86: add         rsp, 48
; load identifer WM_QUIT
.87: sub         rsp, 8
.88: push        rsp
; push_address_of WM_QUIT
.89: mov rax, constants + 124
push rax
; copy 8 bytes from WM_QUIT into stack, reverse=false
.8a: pop         rax
.8b: pop         rbx
.8c: mov         rcx, qword [rax]
.8d: mov         qword [rbx], rcx
.8e: pop         rbx
.8f: pop         rax
.8g: push 1
mov rcx, 0
cmp rax, rbx
cmove rcx, qword [rsp]
add rsp, 8
.8h: push        rcx
.8i: pop         rax
.8j: test rax, rax
jz .8p
; return
.8k: mov         rsp, rbp
.8l: pop         rbp
.8m: ret
.8n: add         rsp, 0
.8o: jmp         .8p
.8p: add         rsp, 0
; call TranslateMessage
.8q: mov         rax, rsp
.8r: and         rsp, -16
.8s: push        rax
.8t: sub         rsp, 8
; push_address_of msg
.8u: push        rbp
.8v: add         qword [rsp], -160
.8w: pop         rcx
.8x: sub         rsp, 32
.8y: call        TranslateMessage
.8z: add         rsp, 40
.8A: pop         rsp
.8B: push        rax
; call DispatchMessageA
.8C: mov         rax, rsp
.8D: and         rsp, -16
.8E: push        rax
.8F: sub         rsp, 8
; push_address_of msg
.8G: push        rbp
.8H: add         qword [rsp], -160
.8I: pop         rcx
.8J: sub         rsp, 32
.8K: call        DispatchMessageA
.8L: add         rsp, 40
.8M: pop         rsp
.8N: push        rax
.8O: add         rsp, 0
.8P: jmp         .6M
.8Q: add         rsp, 0
.8R: jmp         .6J
.8S: mov         rsp, rbp
.8T: pop         rbp
.8U: ret
.8V: push        rbp
.8W: mov         rbp, rsp
; lambda print_string;call WriteConsoleA
.8X: mov         rax, rsp
.8Y: and         rsp, -16
.8Z: push        rax
; literal null
.90: push 0
; literal null
.91: push 0
; binary .
.92: sub         rsp, 8
; load identifer str
.93: sub         rsp, 16
.94: push        rsp
; push_address_of str
.95: push        rbp
.96: add         qword [rsp], 16
; copy 16 bytes from str into stack, reverse=false
.97: pop         rax
.98: pop         rbx
.99: mov         rcx, qword [rax]
.9a: mov         qword [rbx], rcx
.9b: add         rax, 8
.9c: add         rbx, 8
.9d: mov         rcx, qword [rax]
.9e: mov         qword [rbx], rcx
.9f: push        rsp
.9g: add         qword [rsp], 16
.9h: push        rsp
.9i: add         qword [rsp], 16
; copy 8 bytes from str.count into stack, reverse=true
.9j: pop         rax
.9k: pop         rbx
.9l: mov         rcx, qword [rax]
.9m: mov         qword [rbx], rcx
.9n: add         rsp, 16
; binary .
.9o: sub         rsp, 8
; load identifer str
.9p: sub         rsp, 16
.9q: push        rsp
; push_address_of str
.9r: push        rbp
.9s: add         qword [rsp], 16
; copy 16 bytes from str into stack, reverse=false
.9t: pop         rax
.9u: pop         rbx
.9v: mov         rcx, qword [rax]
.9w: mov         qword [rbx], rcx
.9x: add         rax, 8
.9y: add         rbx, 8
.9z: mov         rcx, qword [rax]
.9A: mov         qword [rbx], rcx
.9B: push        rsp
.9C: add         qword [rsp], 16
.9D: push        rsp
.9E: add         qword [rsp], 8
; copy 8 bytes from str.data into stack, reverse=true
.9F: pop         rax
.9G: pop         rbx
.9H: mov         rcx, qword [rax]
.9I: mov         qword [rbx], rcx
.9J: add         rsp, 16
; call GetStdHandle
.9K: mov         rax, rsp
.9L: and         rsp, -16
.9M: push        rax
.9N: sub         rsp, 8
; load identifer STD_OUTPUT_HANDLE
.9O: sub         rsp, 8
.9P: push        rsp
; push_address_of STD_OUTPUT_HANDLE
.9Q: mov rax, constants + 132
push rax
; copy 8 bytes from STD_OUTPUT_HANDLE into stack, reverse=false
.9R: pop         rax
.9S: pop         rbx
.9T: mov         rcx, qword [rax]
.9U: mov         qword [rbx], rcx
.9V: pop         rcx
.9W: sub         rsp, 32
.9X: call        GetStdHandle
.9Y: add         rsp, 40
.9Z: pop         rsp
.a0: push        rax
.a1: pop         rcx
.a2: pop         rdx
.a3: pop         r8
.a4: pop         r9
.a5: sub         rsp, 32
.a6: call        WriteConsoleA
.a7: add         rsp, 40
.a8: pop         rsp
.a9: push        rax
.aa: mov         rsp, rbp
.ab: pop         rbp
.ac: ret
.ad: push        rbp
.ae: mov         rbp, rsp
; lambda print_char;call WriteConsoleA
.af: mov         rax, rsp
.ag: and         rsp, -16
.ah: push        rax
; literal null
.ai: push 0
; literal null
.aj: push 0
; literal 1
.ak: push 1
; push_address_of char
.al: push        rbp
.am: add         qword [rsp], 16
; call GetStdHandle
.an: mov         rax, rsp
.ao: and         rsp, -16
.ap: push        rax
.aq: sub         rsp, 8
; load identifer STD_OUTPUT_HANDLE
.ar: sub         rsp, 8
.as: push        rsp
; push_address_of STD_OUTPUT_HANDLE
.at: mov rax, constants + 132
push rax
; copy 8 bytes from STD_OUTPUT_HANDLE into stack, reverse=false
.au: pop         rax
.av: pop         rbx
.aw: mov         rcx, qword [rax]
.ax: mov         qword [rbx], rcx
.ay: pop         rcx
.az: sub         rsp, 32
.aA: call        GetStdHandle
.aB: add         rsp, 40
.aC: pop         rsp
.aD: push        rax
.aE: pop         rcx
.aF: pop         rdx
.aG: pop         r8
.aH: pop         r9
.aI: sub         rsp, 32
.aJ: call        WriteConsoleA
.aK: add         rsp, 40
.aL: pop         rsp
.aM: push        rax
.aN: mov         rsp, rbp
.aO: pop         rbp
.aP: ret
.aQ: push        rbp
.aR: mov         rbp, rsp
; lambda print_int;definition val;load identifer _val
.aS: sub         rsp, 8
.aT: push        rsp
; push_address_of _val
.aU: push        rbp
.aV: add         qword [rsp], 16
; copy 8 bytes from _val into stack, reverse=false
.aW: pop         rax
.aX: pop         rbx
.aY: mov         rcx, qword [rax]
.aZ: mov         qword [rbx], rcx
; binary ==;load identifer val
.b0: sub         rsp, 8
.b1: push        rsp
; push_address_of val
.b2: push        rbp
.b3: add         qword [rsp], -8
; copy 8 bytes from val into stack, reverse=false
.b4: pop         rax
.b5: pop         rbx
.b6: mov         rcx, qword [rax]
.b7: mov         qword [rbx], rcx
; literal 0
.b8: push 0
.b9: pop         rbx
.ba: pop         rax
.bb: push 1
mov rcx, 0
cmp rax, rbx
cmove rcx, qword [rsp]
add rsp, 8
.bc: push        rcx
.bd: pop         rax
.be: test rax, rax
jz .bo
; call print_char
.bf: sub         rsp, 0
; literal '0'
.bg: push 48
.bh: call        .ad
.bi: add         rsp, 8
; return
.bj: mov         rsp, rbp
.bk: pop         rbp
.bl: ret
.bm: add         rsp, 0
.bn: jmp         .bo
.bo: add         rsp, 0
; binary <;load identifer val
.bp: sub         rsp, 8
.bq: push        rsp
; push_address_of val
.br: push        rbp
.bs: add         qword [rsp], -8
; copy 8 bytes from val into stack, reverse=false
.bt: pop         rax
.bu: pop         rbx
.bv: mov         rcx, qword [rax]
.bw: mov         qword [rbx], rcx
; literal 0
.bx: push 0
.by: pop         rbx
.bz: pop         rax
.bA: push 1
mov rcx, 0
cmp rax, rbx
cmovl rcx, qword [rsp]
add rsp, 8
.bB: push        rcx
.bC: pop         rax
.bD: test rax, rax
jz .c5
; call print_char
.bE: sub         rsp, 0
; literal '-'
.bF: push 45
.bG: call        .ad
.bH: add         rsp, 8
; binary =;load identifer val
.bI: sub         rsp, 8
.bJ: push        rsp
; push_address_of val
.bK: push        rbp
.bL: add         qword [rsp], -8
; copy 8 bytes from val into stack, reverse=false
.bM: pop         rax
.bN: pop         rbx
.bO: mov         rcx, qword [rax]
.bP: mov         qword [rbx], rcx
.bQ: pop         rbx
.bR: xor         rax, rax
.bS: sub         rax, rbx
.bT: push        rax
; push_address_of val
.bU: push        rbp
.bV: add         qword [rsp], -8
.bW: push        rsp
.bX: add         qword [rsp], 8
; copy 8 bytes from -val into val, reverse=false
.bY: pop         rax
.bZ: pop         rbx
.c0: mov         rcx, qword [rax]
.c1: mov         qword [rbx], rcx
.c2: add         rsp, 8
.c3: add         rsp, 0
.c4: jmp         .c5
.c5: add         rsp, 0
; definition i;load identifer val
.c6: sub         rsp, 8
.c7: push        rsp
; push_address_of val
.c8: push        rbp
.c9: add         qword [rsp], -8
; copy 8 bytes from val into stack, reverse=false
.ca: pop         rax
.cb: pop         rbx
.cc: mov         rcx, qword [rax]
.cd: mov         qword [rbx], rcx
; definition buffer
.ce: push 0
.cf: push 0
.cg: push 0
.ch: push 0
.ci: push 0
.cj: push 0
.ck: push 0
.cl: push 0
.cm: push 0
.cn: push 0
.co: push 0
.cp: push 0
.cq: push 0
.cr: push 0
.cs: push 0
.ct: push 0
.cu: push 0
.cv: push 0
.cw: push 0
.cx: push 0
.cy: push 0
.cz: push 0
.cA: push 0
.cB: push 0
.cC: push 0
.cD: push 0
.cE: push 0
.cF: push 0
.cG: push 0
.cH: push 0
.cI: push 0
.cJ: push 0
.cK: push 0
.cL: push 0
.cM: push 0
.cN: push 0
.cO: push 0
.cP: push 0
.cQ: push 0
.cR: push 0
.cS: push 0
.cT: push 0
.cU: push 0
.cV: push 0
.cW: push 0
.cX: push 0
.cY: push 0
.cZ: push 0
.d0: push 0
.d1: push 0
.d2: push 0
.d3: push 0
.d4: push 0
.d5: push 0
.d6: push 0
.d7: push 0
.d8: push 0
.d9: push 0
.da: push 0
.db: push 0
.dc: push 0
.dd: push 0
.de: push 0
.df: push 0
; definition dst_index;literal 64
.dg: push 64
; binary !=;load identifer i
.dh: sub         rsp, 8
.di: push        rsp
; push_address_of i
.dj: push        rbp
.dk: add         qword [rsp], -16
; copy 8 bytes from i into stack, reverse=false
.dl: pop         rax
.dm: pop         rbx
.dn: mov         rcx, qword [rax]
.do: mov         qword [rbx], rcx
; literal 0
.dp: push 0
.dq: pop         rbx
.dr: pop         rax
.ds: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
.dt: push        rcx
.du: pop         rax
.dv: test rax, rax
jz .eq
; definition digit;binary %;load identifer i
.dw: sub         rsp, 8
.dx: push        rsp
; push_address_of i
.dy: push        rbp
.dz: add         qword [rsp], -16
; copy 8 bytes from i into stack, reverse=false
.dA: pop         rax
.dB: pop         rbx
.dC: mov         rcx, qword [rax]
.dD: mov         qword [rbx], rcx
; literal 10
.dE: push 10
.dF: pop         rax
.dG: mov rcx, rax
mov rdx, 0
mov rax, qword[rsp]
div rcx
mov qword[rsp], rdx
; binary -=;literal 1
.dH: push 1
; push_address_of dst_index
.dI: push        rbp
.dJ: add         qword [rsp], -536
.dK: pop         rax
.dL: pop         rbx
.dM: sub         qword [rax], rbx
; binary =;binary +;load identifer digit
.dN: sub         rsp, 8
.dO: push        rsp
; push_address_of digit
.dP: push        rbp
.dQ: add         qword [rsp], -544
; copy 8 bytes from digit into stack, reverse=false
.dR: pop         rax
.dS: pop         rbx
.dT: mov         rcx, qword [rax]
.dU: mov         qword [rbx], rcx
; literal '0'
.dV: push 48
.dW: pop         rax
.dX: add         qword [rsp], rax
; push_address_of [dst_index]buffer;push_address_of buffer
.dY: push        rbp
.dZ: add         qword [rsp], -528
; load identifer dst_index
.e0: sub         rsp, 8
.e1: push        rsp
; push_address_of dst_index
.e2: push        rbp
.e3: add         qword [rsp], -536
; copy 8 bytes from dst_index into stack, reverse=false
.e4: pop         rax
.e5: pop         rbx
.e6: mov         rcx, qword [rax]
.e7: mov         qword [rbx], rcx
.e8: pop         rax
.e9: shl         rax, 3
.ea: add         qword [rsp], rax
.eb: push        rsp
.ec: add         qword [rsp], 8
; copy 8 bytes from digit + '0' into [dst_index]buffer, reverse=false
.ed: pop         rax
.ee: pop         rbx
.ef: mov         rcx, qword [rax]
.eg: mov         qword [rbx], rcx
.eh: add         rsp, 8
; binary /=;literal 10
.ei: push 10
; push_address_of i
.ej: push        rbp
.ek: add         qword [rsp], -16
.el: pop         rax
.em: pop         rbx
.en: mov r8, rax
mov rdx, 0
mov rax, qword[r8]
div rbx
mov qword[r8], rax
.eo: add         rsp, 8
.ep: jmp         .dh
; binary !=;load identifer dst_index
.eq: sub         rsp, 8
.er: push        rsp
; push_address_of dst_index
.es: push        rbp
.et: add         qword [rsp], -536
; copy 8 bytes from dst_index into stack, reverse=false
.eu: pop         rax
.ev: pop         rbx
.ew: mov         rcx, qword [rax]
.ex: mov         qword [rbx], rcx
; literal 64
.ey: push 64
.ez: pop         rbx
.eA: pop         rax
.eB: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
.eC: push        rcx
.eD: pop         rax
.eE: test rax, rax
jz .fb
; call print_char
.eF: sub         rsp, 0
; subscript;load identifer dst_index
.eG: sub         rsp, 8
.eH: push        rsp
; push_address_of dst_index
.eI: push        rbp
.eJ: add         qword [rsp], -536
; copy 8 bytes from dst_index into stack, reverse=false
.eK: pop         rax
.eL: pop         rbx
.eM: mov         rcx, qword [rax]
.eN: mov         qword [rbx], rcx
; push_address_of buffer
.eO: push        rbp
.eP: add         qword [rsp], -528
.eQ: pop         rax
.eR: pop         rbx
.eS: shl         rbx, 3
.eT: add         rax, rbx
.eU: sub         rsp, 8
.eV: push        rsp
.eW: push        rax
; copy 8 bytes from [dst_index]buffer into stack, reverse=false
.eX: pop         rax
.eY: pop         rbx
.eZ: mov         rcx, qword [rax]
.f0: mov         qword [rbx], rcx
.f1: call        .ad
.f2: add         rsp, 8
; binary +=;literal 1
.f3: push 1
; push_address_of dst_index
.f4: push        rbp
.f5: add         qword [rsp], -536
.f6: pop         rax
.f7: pop         rbx
.f8: add         qword [rax], rbx
.f9: add         rsp, 0
.fa: jmp         .eq
.fb: mov         rsp, rbp
.fc: pop         rbp
.fd: ret
.fe: push        rbp
.ff: mov         rbp, rsp
; lambda merge;return;binary |;binary <<;load identifer a
.fg: sub         rsp, 8
.fh: push        rsp
; push_address_of a
.fi: push        rbp
.fj: add         qword [rsp], 16
; copy 1 bytes from a into stack, reverse=false
.fk: pop         rax
.fl: pop         rbx
.fm: mov         cl, byte [rax]
.fn: mov         byte [rbx], cl
; literal 8
.fo: push 8
.fp: pop         rax
.fq: mov cl, al
shl qword[rsp], cl
; load identifer b
.fr: sub         rsp, 8
.fs: push        rsp
; push_address_of b
.ft: push        rbp
.fu: add         qword [rsp], 24
; copy 1 bytes from b into stack, reverse=false
.fv: pop         rax
.fw: pop         rbx
.fx: mov         cl, byte [rax]
.fy: mov         byte [rbx], cl
.fz: pop         rax
.fA:  or         qword [rsp], rax
.fB: push        rbp
.fC: add         qword [rsp], 32
.fD: push        rsp
.fE: add         qword [rsp], 8
; copy 2 bytes from expression into parameter, reverse=false
.fF: pop         rax
.fG: pop         rbx
.fH: mov         cl, byte [rax]
.fI: mov         byte [rbx], cl
.fJ: add         rax, 1
.fK: add         rbx, 1
.fL: mov         cl, byte [rax]
.fM: mov         byte [rbx], cl
.fN: mov         rsp, rbp
.fO: pop         rbp
.fP: ret
.fQ: mov         rsp, rbp
.fR: pop         rbp
.fS: ret
