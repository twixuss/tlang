bits 64
extern ExitProcess
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
constants: db 2,0,0,0,0,0,0,0,119,105,110,100,111,119,95,99,108,97,115,115,0,104,101,108,108,111,32,119,105,110,100,111,119,0,67,108,97,115,115,32,99,114,101,97,116,101,100,33,10,67,108,97,115,115,32,70,97,105,108,101,100,33,10,87,105,110,100,111,119,32,83,117,99,99,101,115,115,33,10,87,105,110,100,111,119,32,70,97,105,108,33,10,18,0,0,0,0,0,0,0,246,255,255,255,255,255,255,255,245,255,255,255,255,255,255,255,244,255,255,255,255,255,255,255,0,0,0,128,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,136,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,
section .data
data: db 
section .bss
zeros: resb 0
section .text
global main
main:
push 0
call .3f
pop rcx
sub rsp, 16
call ExitProcess
ret
.0: push        rbp
.1: mov         rbp, rsp
; lambda wnd_proc;reserve space for return parameter
.2: sub         rsp, 8
; 544
.3: push        r9
; 547
.4: push        r8
; 548
.5: push        rdx
; 549
.6: push        rcx
; 550
.7: push 180288206
.8: push        rbp
.9: mov         rbp, rsp
; binary ==;load identifer uMsg
.a: sub         rsp, 8
; 894
.b: push        rsp
; 895;push_address_of uMsg
.c: push        rbp
; 266
.d: add         qword [rsp], 24
; 267;copy 4 bytes from uMsg into stack, reverse=false
.e: pop         rax
; 344
.f: pop         rbx
; 345
.g: mov         ecx, dword [rax]
; 393
.h: mov         dword [rbx], ecx
; 394;load identifer WM_DESTROY
.i: sub         rsp, 8
; 894
.j: push        rsp
; 895;push_address_of WM_DESTROY
.k: mov rax, constants + 0
push rax
; 289;copy 4 bytes from WM_DESTROY into stack, reverse=false
.l: pop         rax
; 344
.m: pop         rbx
; 345
.n: mov         ecx, dword [rax]
; 393
.o: mov         dword [rbx], ecx
; 394
.p: pop         rbx
; 832
.q: pop         rax
; 833
.r: push 1
mov rcx, 0
cmp eax, ebx
cmove rcx, qword [rsp]
add rsp, 8
; 837
.s: push        rcx
; 841
.t: pop         rax
; 1031
.u: test rax, rax
jz .Y
; 1032;call PostQuitMessage
.v: mov         rax, rsp
; 943
.w: and         rsp, -16
; 944
.x: push        rax
; 945
.y: sub         rsp, 8
; 948;literal 0
.z: push 0
; 1016
.A: pop         rcx
; 956
.B: sub         rsp, 32
; 963
.C: call        PostQuitMessage
; 972
.D: add         rsp, 40
; 977
.E: pop         rsp
; 979
.F: push        rax
; 981;return;literal 0
.G: push 0
; 1020
.H: push        rbp
; 673
.I: add         qword [rsp], 48
; 674
.J: push        rsp
; 677
.K: add         qword [rsp], 8
; 678;copy 8 bytes from expression into parameter, reverse=false
.L: pop         rax
; 344
.M: pop         rbx
; 345
.N: mov         rcx, qword [rax]
; 366
.O: mov         qword [rbx], rcx
; 367
.P: mov         rsp, rbp
; 684
.Q: pop         rbp
; 685
.R: add         rsp, 40
; 686
.S: pop         rax
; 687
.T: mov         rsp, rbp
; 689
.U: pop         rbp
; 690
.V: ret
; 691
.W: add         rsp, 0
; 1044
.X: jmp         .Y
; 1045
.Y: add         rsp, 0
; 1059;return;call DefWindowProcA
.Z: mov         rax, rsp
; 943
.10: and         rsp, -16
; 944
.11: push        rax
; 945
.12: sub         rsp, 8
; 948;load identifer lParam
.13: sub         rsp, 8
; 894
.14: push        rsp
; 895;push_address_of lParam
.15: push        rbp
; 266
.16: add         qword [rsp], 40
; 267;copy 8 bytes from lParam into stack, reverse=false
.17: pop         rax
; 344
.18: pop         rbx
; 345
.19: mov         rcx, qword [rax]
; 366
.1a: mov         qword [rbx], rcx
; 367;load identifer wParam
.1b: sub         rsp, 8
; 894
.1c: push        rsp
; 895;push_address_of wParam
.1d: push        rbp
; 266
.1e: add         qword [rsp], 32
; 267;copy 8 bytes from wParam into stack, reverse=false
.1f: pop         rax
; 344
.1g: pop         rbx
; 345
.1h: mov         rcx, qword [rax]
; 366
.1i: mov         qword [rbx], rcx
; 367;load identifer uMsg
.1j: sub         rsp, 8
; 894
.1k: push        rsp
; 895;push_address_of uMsg
.1l: push        rbp
; 266
.1m: add         qword [rsp], 24
; 267;copy 4 bytes from uMsg into stack, reverse=false
.1n: pop         rax
; 344
.1o: pop         rbx
; 345
.1p: mov         ecx, dword [rax]
; 393
.1q: mov         dword [rbx], ecx
; 394;load identifer hwnd
.1r: sub         rsp, 8
; 894
.1s: push        rsp
; 895;push_address_of hwnd
.1t: push        rbp
; 266
.1u: add         qword [rsp], 16
; 267;copy 8 bytes from hwnd into stack, reverse=false
.1v: pop         rax
; 344
.1w: pop         rbx
; 345
.1x: mov         rcx, qword [rax]
; 366
.1y: mov         qword [rbx], rcx
; 367
.1z: pop         rcx
; 956
.1A: pop         rdx
; 957
.1B: pop         r8
; 958
.1C: pop         r9
; 959
.1D: sub         rsp, 32
; 963
.1E: call        DefWindowProcA
; 972
.1F: add         rsp, 40
; 977
.1G: pop         rsp
; 979
.1H: push        rax
; 981
.1I: push        rbp
; 673
.1J: add         qword [rsp], 48
; 674
.1K: push        rsp
; 677
.1L: add         qword [rsp], 8
; 678;copy 8 bytes from expression into parameter, reverse=false
.1M: pop         rax
; 344
.1N: pop         rbx
; 345
.1O: mov         rcx, qword [rax]
; 366
.1P: mov         qword [rbx], rcx
; 367
.1Q: mov         rsp, rbp
; 684
.1R: pop         rbp
; 685
.1S: add         rsp, 40
; 686
.1T: pop         rax
; 687
.1U: mov         rsp, rbp
; 689
.1V: pop         rbp
; 690
.1W: ret
; 691
.1X: mov         rsp, rbp
; 572
.1Y: pop         rbp
; 573
.1Z: mov         rsp, rbp
; 578
.20: pop         rbp
; 579
.21: ret
; 580
.22: push        rbp
.23: mov         rbp, rsp
; lambda print_string;call WriteConsoleA
.24: mov         rax, rsp
; 943
.25: and         rsp, -16
; 944
.26: push        rax
; 945;literal null
.27: push 0
; 1020;literal null
.28: push 0
; 1020;cast from 'u64' to 'u32';binary .
.29: sub         rsp, 8
; 723;load identifer str
.2a: sub         rsp, 16
; 894
.2b: push        rsp
; 895;push_address_of str
.2c: push        rbp
; 266
.2d: add         qword [rsp], 16
; 267;copy 16 bytes from str into stack, reverse=false
.2e: pop         rax
; 344
.2f: pop         rbx
; 345
.2g: mov         rcx, qword [rax]
; 366
.2h: mov         qword [rbx], rcx
; 367
.2i: add         rax, 8
; 370
.2j: add         rbx, 8
; 371
.2k: mov         rcx, qword [rax]
; 366
.2l: mov         qword [rbx], rcx
; 367
.2m: push        rsp
; 744
.2n: add         qword [rsp], 16
; 745
.2o: push        rsp
; 747
.2p: add         qword [rsp], 16
; 748;copy 8 bytes from . into stack, reverse=true
.2q: pop         rax
; 344
.2r: pop         rbx
; 345
.2s: mov         rcx, qword [rax]
; 356
.2t: mov         qword [rbx], rcx
; 357
.2u: add         rsp, 16
; 752;binary .
.2v: sub         rsp, 8
; 723;load identifer str
.2w: sub         rsp, 16
; 894
.2x: push        rsp
; 895;push_address_of str
.2y: push        rbp
; 266
.2z: add         qword [rsp], 16
; 267;copy 16 bytes from str into stack, reverse=false
.2A: pop         rax
; 344
.2B: pop         rbx
; 345
.2C: mov         rcx, qword [rax]
; 366
.2D: mov         qword [rbx], rcx
; 367
.2E: add         rax, 8
; 370
.2F: add         rbx, 8
; 371
.2G: mov         rcx, qword [rax]
; 366
.2H: mov         qword [rbx], rcx
; 367
.2I: push        rsp
; 744
.2J: add         qword [rsp], 16
; 745
.2K: push        rsp
; 747
.2L: add         qword [rsp], 8
; 748;copy 8 bytes from str.data into stack, reverse=true
.2M: pop         rax
; 344
.2N: pop         rbx
; 345
.2O: mov         rcx, qword [rax]
; 356
.2P: mov         qword [rbx], rcx
; 357
.2Q: add         rsp, 16
; 752;call GetStdHandle
.2R: mov         rax, rsp
; 943
.2S: and         rsp, -16
; 944
.2T: push        rax
; 945
.2U: sub         rsp, 8
; 948;literal 
.2V: push -11
; 1016
.2W: pop         rcx
; 956
.2X: sub         rsp, 32
; 963
.2Y: call        GetStdHandle
; 972
.2Z: add         rsp, 40
; 977
.30: pop         rsp
; 979
.31: push        rax
; 981
.32: pop         rcx
; 956
.33: pop         rdx
; 957
.34: pop         r8
; 958
.35: pop         r9
; 959
.36: sub         rsp, 32
; 963
.37: call        WriteConsoleA
; 972
.38: add         rsp, 40
; 977
.39: pop         rsp
; 979
.3a: push        rax
; 981
.3b: add         rsp, 8
; 1136
.3c: mov         rsp, rbp
; 487
.3d: pop         rbp
; 488
.3e: ret
; 489
.3f: push        rbp
.3g: mov         rbp, rsp
; lambda main;definition class_name;binary .
.3h: sub         rsp, 8
; 723;literal "window_class\0"
.3i: push 13
; 997
.3j: mov rax, constants + 8
push rax
; 1000
.3k: push        rsp
; 744
.3l: add         qword [rsp], 16
; 745
.3m: push        rsp
; 747
.3n: add         qword [rsp], 8
; 748;copy 8 bytes from "window_class\0".data into stack, reverse=true
.3o: pop         rax
; 344
.3p: pop         rbx
; 345
.3q: mov         rcx, qword [rax]
; 356
.3r: mov         qword [rbx], rcx
; 357
.3s: add         rsp, 16
; 752;definition window_name;binary .
.3t: sub         rsp, 8
; 723;literal "hello window\0"
.3u: push 13
; 997
.3v: mov rax, constants + 21
push rax
; 1000
.3w: push        rsp
; 744
.3x: add         qword [rsp], 16
; 745
.3y: push        rsp
; 747
.3z: add         qword [rsp], 8
; 748;copy 8 bytes from "hello window\0".data into stack, reverse=true
.3A: pop         rax
; 344
.3B: pop         rbx
; 345
.3C: mov         rcx, qword [rax]
; 356
.3D: mov         qword [rbx], rcx
; 357
.3E: add         rsp, 16
; 752;definition hInstance;call GetModuleHandleA
.3F: mov         rax, rsp
; 943
.3G: and         rsp, -16
; 944
.3H: push        rax
; 945
.3I: sub         rsp, 8
; 948;literal null
.3J: push 0
; 1020
.3K: pop         rcx
; 956
.3L: sub         rsp, 32
; 963
.3M: call        GetModuleHandleA
; 972
.3N: add         rsp, 40
; 977
.3O: pop         rsp
; 979
.3P: push        rax
; 981;definition wc
.3Q: push 0
; 621
.3R: push 0
; 621
.3S: push 0
; 621
.3T: push 0
; 621
.3U: push 0
; 621
.3V: push 0
; 621
.3W: push 0
; 621
.3X: push 0
; 621
.3Y: push 0
; 621
.3Z: push 0
; 621;binary =;load identifer hInstance
.40: sub         rsp, 8
; 894
.41: push        rsp
; 895;push_address_of hInstance
.42: push        rbp
; 266
.43: add         qword [rsp], -24
; 267;copy 8 bytes from hInstance into stack, reverse=false
.44: pop         rax
; 344
.45: pop         rbx
; 345
.46: mov         rcx, qword [rax]
; 366
.47: mov         qword [rbx], rcx
; 367;push_address_of wc.hInstance;push_address_of wc
.48: push        rbp
; 266
.49: add         qword [rsp], -104
; 267
.4a: add         qword [rsp], 24
; 309
.4b: push        rsp
; 801
.4c: add         qword [rsp], 8
; 802;copy 8 bytes from hInstance into wc.hInstance, reverse=false
.4d: pop         rax
; 344
.4e: pop         rbx
; 345
.4f: mov         rcx, qword [rax]
; 366
.4g: mov         qword [rbx], rcx
; 367
.4h: add         rsp, 8
; 808;binary =;literal 
.4i: push 80
; 1016;push_address_of wc.cbSize;push_address_of wc
.4j: push        rbp
; 266
.4k: add         qword [rsp], -104
; 267
.4l: push        rsp
; 801
.4m: add         qword [rsp], 8
; 802;copy 4 bytes from  into wc.cbSize, reverse=false
.4n: pop         rax
; 344
.4o: pop         rbx
; 345
.4p: mov         ecx, dword [rax]
; 393
.4q: mov         dword [rbx], ecx
; 394
.4r: add         rsp, 8
; 808;binary =;push_address_of wnd_proc
.4s: mov rax, .0
push rax
; 240;push_address_of wc.lpfnWndProc;push_address_of wc
.4t: push        rbp
; 266
.4u: add         qword [rsp], -104
; 267
.4v: add         qword [rsp], 8
; 309
.4w: push        rsp
; 801
.4x: add         qword [rsp], 8
; 802;copy 8 bytes from &wnd_proc into wc.lpfnWndProc, reverse=false
.4y: pop         rax
; 344
.4z: pop         rbx
; 345
.4A: mov         rcx, qword [rax]
; 366
.4B: mov         qword [rbx], rcx
; 367
.4C: add         rsp, 8
; 808;binary =;load identifer class_name
.4D: sub         rsp, 8
; 894
.4E: push        rsp
; 895;push_address_of class_name
.4F: push        rbp
; 266
.4G: add         qword [rsp], -8
; 267;copy 8 bytes from class_name into stack, reverse=false
.4H: pop         rax
; 344
.4I: pop         rbx
; 345
.4J: mov         rcx, qword [rax]
; 366
.4K: mov         qword [rbx], rcx
; 367;push_address_of wc.lpszClassName;push_address_of wc
.4L: push        rbp
; 266
.4M: add         qword [rsp], -104
; 267
.4N: add         qword [rsp], 64
; 309
.4O: push        rsp
; 801
.4P: add         qword [rsp], 8
; 802;copy 8 bytes from class_name into wc.lpszClassName, reverse=false
.4Q: pop         rax
; 344
.4R: pop         rbx
; 345
.4S: mov         rcx, qword [rax]
; 366
.4T: mov         qword [rbx], rcx
; 367
.4U: add         rsp, 8
; 808;binary !=;call RegisterClassExA
.4V: mov         rax, rsp
; 943
.4W: and         rsp, -16
; 944
.4X: push        rax
; 945
.4Y: sub         rsp, 8
; 948;push_address_of wc
.4Z: push        rbp
; 266
.50: add         qword [rsp], -104
; 267
.51: pop         rcx
; 956
.52: sub         rsp, 32
; 963
.53: call        RegisterClassExA
; 972
.54: add         rsp, 40
; 977
.55: pop         rsp
; 979
.56: push        rax
; 981;literal 0
.57: push 0
; 1013
.58: pop         rbx
; 832
.59: pop         rax
; 833
.5a: push 1
mov rcx, 0
cmp  al,  bl
cmovne rcx, qword [rsp]
add rsp, 8
; 836
.5b: push        rcx
; 841
.5c: pop         rax
; 1031
.5d: test rax, rax
jz .5l
; 1032;call print_string
.5e: sub         rsp, 0
; 909;literal "Class created!\n"
.5f: push 15
; 997
.5g: mov rax, constants + 34
push rax
; 1000
.5h: call        .22
; 920
.5i: add         rsp, 16
; 924
.5j: add         rsp, 0
; 1044
.5k: jmp         .5q
; 1045;call print_string
.5l: sub         rsp, 0
; 909;literal "Class Failed!\n"
.5m: push 14
; 997
.5n: mov rax, constants + 49
push rax
; 1000
.5o: call        .22
; 920
.5p: add         rsp, 16
; 924
.5q: add         rsp, 0
; 1059;definition window;call CreateWindowExA
.5r: mov         rax, rsp
; 943
.5s: and         rsp, -16
; 944
.5t: push        rax
; 945
.5u: sub         rsp, 8
; 948;literal null
.5v: push 0
; 1020;load identifer hInstance
.5w: sub         rsp, 8
; 894
.5x: push        rsp
; 895;push_address_of hInstance
.5y: push        rbp
; 266
.5z: add         qword [rsp], -24
; 267;copy 8 bytes from hInstance into stack, reverse=false
.5A: pop         rax
; 344
.5B: pop         rbx
; 345
.5C: mov         rcx, qword [rax]
; 366
.5D: mov         qword [rbx], rcx
; 367;literal null
.5E: push 0
; 1020;literal null
.5F: push 0
; 1020;literal 0x80000000
.5G: push -2147483648
; 1016;literal 0x80000000
.5H: push -2147483648
; 1016;literal 0x80000000
.5I: push -2147483648
; 1016;literal 0x80000000
.5J: push -2147483648
; 1016;literal 
.5K: push 282001408
; 1016;load identifer window_name
.5L: sub         rsp, 8
; 894
.5M: push        rsp
; 895;push_address_of window_name
.5N: push        rbp
; 266
.5O: add         qword [rsp], -16
; 267;copy 8 bytes from window_name into stack, reverse=false
.5P: pop         rax
; 344
.5Q: pop         rbx
; 345
.5R: mov         rcx, qword [rax]
; 366
.5S: mov         qword [rbx], rcx
; 367;load identifer class_name
.5T: sub         rsp, 8
; 894
.5U: push        rsp
; 895;push_address_of class_name
.5V: push        rbp
; 266
.5W: add         qword [rsp], -8
; 267;copy 8 bytes from class_name into stack, reverse=false
.5X: pop         rax
; 344
.5Y: pop         rbx
; 345
.5Z: mov         rcx, qword [rax]
; 366
.60: mov         qword [rbx], rcx
; 367;literal 0
.61: push 0
; 1016
.62: pop         rcx
; 956
.63: pop         rdx
; 957
.64: pop         r8
; 958
.65: pop         r9
; 959
.66: sub         rsp, 32
; 963
.67: call        CreateWindowExA
; 972
.68: add         rsp, 104
; 977
.69: pop         rsp
; 979
.6a: push        rax
; 981;binary !=;load identifer window
.6b: sub         rsp, 8
; 894
.6c: push        rsp
; 895;push_address_of window
.6d: push        rbp
; 266
.6e: add         qword [rsp], -112
; 267;copy 8 bytes from window into stack, reverse=false
.6f: pop         rax
; 344
.6g: pop         rbx
; 345
.6h: mov         rcx, qword [rax]
; 366
.6i: mov         qword [rbx], rcx
; 367;literal null
.6j: push 0
; 1020
.6k: pop         rbx
; 832
.6l: pop         rax
; 833
.6m: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
; 838
.6n: push        rcx
; 841
.6o: pop         rax
; 1031
.6p: test rax, rax
jz .6x
; 1032;call print_string
.6q: sub         rsp, 0
; 909;literal "Window Success!\n"
.6r: push 16
; 997
.6s: mov rax, constants + 63
push rax
; 1000
.6t: call        .22
; 920
.6u: add         rsp, 16
; 924
.6v: add         rsp, 0
; 1044
.6w: jmp         .6C
; 1045;call print_string
.6x: sub         rsp, 0
; 909;literal "Window Fail!\n"
.6y: push 13
; 997
.6z: mov rax, constants + 79
push rax
; 1000
.6A: call        .22
; 920
.6B: add         rsp, 16
; 924
.6C: add         rsp, 0
; 1059;definition msg
.6D: push 0
; 621
.6E: push 0
; 621
.6F: push 0
; 621
.6G: push 0
; 621
.6H: push 0
; 621
.6I: push 0
; 621;literal true
.6J: push 1
; 1006
.6K: pop         rax
; 1071
.6L: test rax, rax
jz .8A
; 1072;binary !=;call PeekMessageA
.6M: mov         rax, rsp
; 943
.6N: and         rsp, -16
; 944
.6O: push        rax
; 945;literal 0x0001
.6P: push 1
; 1016;literal 0
.6Q: push 0
; 1016;literal 0
.6R: push 0
; 1016;literal null
.6S: push 0
; 1020;push_address_of msg
.6T: push        rbp
; 266
.6U: add         qword [rsp], -160
; 267
.6V: pop         rcx
; 956
.6W: pop         rdx
; 957
.6X: pop         r8
; 958
.6Y: pop         r9
; 959
.6Z: sub         rsp, 32
; 963
.70: call        PeekMessageA
; 972
.71: add         rsp, 40
; 977
.72: pop         rsp
; 979
.73: push        rax
; 981;literal 0
.74: push 0
; 1016
.75: pop         rbx
; 832
.76: pop         rax
; 833
.77: push 1
mov rcx, 0
cmp eax, ebx
cmovne rcx, qword [rsp]
add rsp, 8
; 837
.78: push        rcx
; 841
.79: pop         rax
; 1071
.7a: test rax, rax
jz .8y
; 1072;binary ==;binary .
.7b: sub         rsp, 8
; 723;load identifer msg
.7c: sub         rsp, 48
; 894
.7d: push        rsp
; 895;push_address_of msg
.7e: push        rbp
; 266
.7f: add         qword [rsp], -160
; 267;copy 48 bytes from msg into stack, reverse=false
.7g: pop         rax
; 344
.7h: pop         rbx
; 345
.7i: mov         rcx, qword [rax]
; 366
.7j: mov         qword [rbx], rcx
; 367
.7k: add         rax, 8
; 370
.7l: add         rbx, 8
; 371
.7m: mov         rcx, qword [rax]
; 366
.7n: mov         qword [rbx], rcx
; 367
.7o: add         rax, 8
; 370
.7p: add         rbx, 8
; 371
.7q: mov         rcx, qword [rax]
; 366
.7r: mov         qword [rbx], rcx
; 367
.7s: add         rax, 8
; 370
.7t: add         rbx, 8
; 371
.7u: mov         rcx, qword [rax]
; 366
.7v: mov         qword [rbx], rcx
; 367
.7w: add         rax, 8
; 370
.7x: add         rbx, 8
; 371
.7y: mov         rcx, qword [rax]
; 366
.7z: mov         qword [rbx], rcx
; 367
.7A: add         rax, 8
; 370
.7B: add         rbx, 8
; 371
.7C: mov         rcx, qword [rax]
; 366
.7D: mov         qword [rbx], rcx
; 367
.7E: push        rsp
; 744
.7F: add         qword [rsp], 48
; 745
.7G: push        rsp
; 747
.7H: add         qword [rsp], 16
; 748;copy 4 bytes from msg.message into stack, reverse=true
.7I: pop         rax
; 344
.7J: pop         rbx
; 345
.7K: mov         ecx, dword [rax]
; 383
.7L: mov         dword [rbx], ecx
; 384
.7M: add         rsp, 48
; 752;load identifer WM_QUIT
.7N: sub         rsp, 8
; 894
.7O: push        rsp
; 895;push_address_of WM_QUIT
.7P: mov rax, constants + 92
push rax
; 289;copy 4 bytes from WM_QUIT into stack, reverse=false
.7Q: pop         rax
; 344
.7R: pop         rbx
; 345
.7S: mov         ecx, dword [rax]
; 393
.7T: mov         dword [rbx], ecx
; 394
.7U: pop         rbx
; 832
.7V: pop         rax
; 833
.7W: push 1
mov rcx, 0
cmp eax, ebx
cmove rcx, qword [rsp]
add rsp, 8
; 837
.7X: push        rcx
; 841
.7Y: pop         rax
; 1031
.7Z: test rax, rax
jz .85
; 1032;return
.80: mov         rsp, rbp
; 689
.81: pop         rbp
; 690
.82: ret
; 691
.83: add         rsp, 0
; 1044
.84: jmp         .85
; 1045
.85: add         rsp, 0
; 1059;call TranslateMessage
.86: mov         rax, rsp
; 943
.87: and         rsp, -16
; 944
.88: push        rax
; 945
.89: sub         rsp, 8
; 948;push_address_of msg
.8a: push        rbp
; 266
.8b: add         qword [rsp], -160
; 267
.8c: pop         rcx
; 956
.8d: sub         rsp, 32
; 963
.8e: call        TranslateMessage
; 972
.8f: add         rsp, 40
; 977
.8g: pop         rsp
; 979
.8h: push        rax
; 981
.8i: add         rsp, 8
; 1136;call DispatchMessageA
.8j: mov         rax, rsp
; 943
.8k: and         rsp, -16
; 944
.8l: push        rax
; 945
.8m: sub         rsp, 8
; 948;push_address_of msg
.8n: push        rbp
; 266
.8o: add         qword [rsp], -160
; 267
.8p: pop         rcx
; 956
.8q: sub         rsp, 32
; 963
.8r: call        DispatchMessageA
; 972
.8s: add         rsp, 40
; 977
.8t: pop         rsp
; 979
.8u: push        rax
; 981
.8v: add         rsp, 8
; 1136
.8w: add         rsp, 0
; 1084
.8x: jmp         .6M
; 1086
.8y: add         rsp, 0
; 1084
.8z: jmp         .6J
; 1086
.8A: mov         rsp, rbp
; 487
.8B: pop         rbp
; 488
.8C: ret
; 489
.8D: push        rbp
.8E: mov         rbp, rsp
; lambda print_char;call WriteConsoleA
.8F: mov         rax, rsp
; 943
.8G: and         rsp, -16
; 944
.8H: push        rax
; 945;literal null
.8I: push 0
; 1020;literal null
.8J: push 0
; 1020;literal 1
.8K: push 1
; 1016;cast from '*u8' to '*void';push_address_of char
.8L: push        rbp
; 266
.8M: add         qword [rsp], 16
; 267;call GetStdHandle
.8N: mov         rax, rsp
; 943
.8O: and         rsp, -16
; 944
.8P: push        rax
; 945
.8Q: sub         rsp, 8
; 948;literal 
.8R: push -11
; 1016
.8S: pop         rcx
; 956
.8T: sub         rsp, 32
; 963
.8U: call        GetStdHandle
; 972
.8V: add         rsp, 40
; 977
.8W: pop         rsp
; 979
.8X: push        rax
; 981
.8Y: pop         rcx
; 956
.8Z: pop         rdx
; 957
.90: pop         r8
; 958
.91: pop         r9
; 959
.92: sub         rsp, 32
; 963
.93: call        WriteConsoleA
; 972
.94: add         rsp, 40
; 977
.95: pop         rsp
; 979
.96: push        rax
; 981
.97: add         rsp, 8
; 1136
.98: mov         rsp, rbp
; 487
.99: pop         rbp
; 488
.9a: ret
; 489
.9b: push        rbp
.9c: mov         rbp, rsp
; lambda print_int;definition val;load identifer _val
.9d: sub         rsp, 8
; 894
.9e: push        rsp
; 895;push_address_of _val
.9f: push        rbp
; 266
.9g: add         qword [rsp], 16
; 267;copy 8 bytes from _val into stack, reverse=false
.9h: pop         rax
; 344
.9i: pop         rbx
; 345
.9j: mov         rcx, qword [rax]
; 366
.9k: mov         qword [rbx], rcx
; 367;binary ==;load identifer val
.9l: sub         rsp, 8
; 894
.9m: push        rsp
; 895;push_address_of val
.9n: push        rbp
; 266
.9o: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.9p: pop         rax
; 344
.9q: pop         rbx
; 345
.9r: mov         rcx, qword [rax]
; 366
.9s: mov         qword [rbx], rcx
; 367;literal 0
.9t: push 0
; 1020
.9u: pop         rbx
; 832
.9v: pop         rax
; 833
.9w: push 1
mov rcx, 0
cmp rax, rbx
cmove rcx, qword [rsp]
add rsp, 8
; 838
.9x: push        rcx
; 841
.9y: pop         rax
; 1031
.9z: test rax, rax
jz .9J
; 1032;call print_char
.9A: sub         rsp, 0
; 909;literal '0'
.9B: push 48
; 1004
.9C: call        .8D
; 920
.9D: add         rsp, 8
; 924;return
.9E: mov         rsp, rbp
; 689
.9F: pop         rbp
; 690
.9G: ret
; 691
.9H: add         rsp, 0
; 1044
.9I: jmp         .9J
; 1045
.9J: add         rsp, 0
; 1059;binary <;load identifer val
.9K: sub         rsp, 8
; 894
.9L: push        rsp
; 895;push_address_of val
.9M: push        rbp
; 266
.9N: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.9O: pop         rax
; 344
.9P: pop         rbx
; 345
.9Q: mov         rcx, qword [rax]
; 366
.9R: mov         qword [rbx], rcx
; 367;literal 0
.9S: push 0
; 1020
.9T: pop         rbx
; 832
.9U: pop         rax
; 833
.9V: push 1
mov rcx, 0
cmp rax, rbx
cmovl rcx, qword [rsp]
add rsp, 8
; 838
.9W: push        rcx
; 841
.9X: pop         rax
; 1031
.9Y: test rax, rax
jz .aq
; 1032;call print_char
.9Z: sub         rsp, 0
; 909;literal '-'
.a0: push 45
; 1004
.a1: call        .8D
; 920
.a2: add         rsp, 8
; 924;binary =;load identifer val
.a3: sub         rsp, 8
; 894
.a4: push        rsp
; 895;push_address_of val
.a5: push        rbp
; 266
.a6: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.a7: pop         rax
; 344
.a8: pop         rbx
; 345
.a9: mov         rcx, qword [rax]
; 366
.aa: mov         qword [rbx], rcx
; 367
.ab: pop         rbx
; 1156
.ac: xor         rax, rax
; 1157
.ad: sub         rax, rbx
; 1158
.ae: push        rax
; 1159;push_address_of val
.af: push        rbp
; 266
.ag: add         qword [rsp], -8
; 267
.ah: push        rsp
; 801
.ai: add         qword [rsp], 8
; 802;copy 8 bytes from -val into val, reverse=false
.aj: pop         rax
; 344
.ak: pop         rbx
; 345
.al: mov         rcx, qword [rax]
; 366
.am: mov         qword [rbx], rcx
; 367
.an: add         rsp, 8
; 808
.ao: add         rsp, 0
; 1044
.ap: jmp         .aq
; 1045
.aq: add         rsp, 0
; 1059;definition i;load identifer val
.ar: sub         rsp, 8
; 894
.as: push        rsp
; 895;push_address_of val
.at: push        rbp
; 266
.au: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.av: pop         rax
; 344
.aw: pop         rbx
; 345
.ax: mov         rcx, qword [rax]
; 366
.ay: mov         qword [rbx], rcx
; 367;definition buffer
.az: push 0
; 621
.aA: push 0
; 621
.aB: push 0
; 621
.aC: push 0
; 621
.aD: push 0
; 621
.aE: push 0
; 621
.aF: push 0
; 621
.aG: push 0
; 621
.aH: push 0
; 621
.aI: push 0
; 621
.aJ: push 0
; 621
.aK: push 0
; 621
.aL: push 0
; 621
.aM: push 0
; 621
.aN: push 0
; 621
.aO: push 0
; 621
.aP: push 0
; 621
.aQ: push 0
; 621
.aR: push 0
; 621
.aS: push 0
; 621
.aT: push 0
; 621
.aU: push 0
; 621
.aV: push 0
; 621
.aW: push 0
; 621
.aX: push 0
; 621
.aY: push 0
; 621
.aZ: push 0
; 621
.b0: push 0
; 621
.b1: push 0
; 621
.b2: push 0
; 621
.b3: push 0
; 621
.b4: push 0
; 621
.b5: push 0
; 621
.b6: push 0
; 621
.b7: push 0
; 621
.b8: push 0
; 621
.b9: push 0
; 621
.ba: push 0
; 621
.bb: push 0
; 621
.bc: push 0
; 621
.bd: push 0
; 621
.be: push 0
; 621
.bf: push 0
; 621
.bg: push 0
; 621
.bh: push 0
; 621
.bi: push 0
; 621
.bj: push 0
; 621
.bk: push 0
; 621
.bl: push 0
; 621
.bm: push 0
; 621
.bn: push 0
; 621
.bo: push 0
; 621
.bp: push 0
; 621
.bq: push 0
; 621
.br: push 0
; 621
.bs: push 0
; 621
.bt: push 0
; 621
.bu: push 0
; 621
.bv: push 0
; 621
.bw: push 0
; 621
.bx: push 0
; 621
.by: push 0
; 621
.bz: push 0
; 621
.bA: push 0
; 621;definition dst_index;literal 64
.bB: push 64
; 1020;binary !=;load identifer i
.bC: sub         rsp, 8
; 894
.bD: push        rsp
; 895;push_address_of i
.bE: push        rbp
; 266
.bF: add         qword [rsp], -16
; 267;copy 8 bytes from i into stack, reverse=false
.bG: pop         rax
; 344
.bH: pop         rbx
; 345
.bI: mov         rcx, qword [rax]
; 366
.bJ: mov         qword [rbx], rcx
; 367;literal 0
.bK: push 0
; 1020
.bL: pop         rbx
; 832
.bM: pop         rax
; 833
.bN: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
; 838
.bO: push        rcx
; 841
.bP: pop         rax
; 1071
.bQ: test rax, rax
jz .cL
; 1072;definition digit;binary %;load identifer i
.bR: sub         rsp, 8
; 894
.bS: push        rsp
; 895;push_address_of i
.bT: push        rbp
; 266
.bU: add         qword [rsp], -16
; 267;copy 8 bytes from i into stack, reverse=false
.bV: pop         rax
; 344
.bW: pop         rbx
; 345
.bX: mov         rcx, qword [rax]
; 366
.bY: mov         qword [rbx], rcx
; 367;literal 10
.bZ: push 10
; 1020
.c0: pop         rax
; 777
.c1: mov rcx, rax
mov rdx, 0
mov rax, qword[rsp]
div rcx
mov qword[rsp], rdx
; 783;binary -=;literal 1
.c2: push 1
; 1020;push_address_of dst_index
.c3: push        rbp
; 266
.c4: add         qword [rsp], -536
; 267
.c5: pop         rax
; 858
.c6: pop         rbx
; 859
.c7: sub         qword [rax], rbx
; 863;binary =;binary +;load identifer digit
.c8: sub         rsp, 8
; 894
.c9: push        rsp
; 895;push_address_of digit
.ca: push        rbp
; 266
.cb: add         qword [rsp], -544
; 267;copy 8 bytes from digit into stack, reverse=false
.cc: pop         rax
; 344
.cd: pop         rbx
; 345
.ce: mov         rcx, qword [rax]
; 366
.cf: mov         qword [rbx], rcx
; 367;literal '0'
.cg: push 48
; 1004
.ch: pop         rax
; 777
.ci: add         qword [rsp], rax
; 779;push_address_of [dst_index]buffer;push_address_of buffer
.cj: push        rbp
; 266
.ck: add         qword [rsp], -528
; 267;load identifer dst_index
.cl: sub         rsp, 8
; 894
.cm: push        rsp
; 895;push_address_of dst_index
.cn: push        rbp
; 266
.co: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.cp: pop         rax
; 344
.cq: pop         rbx
; 345
.cr: mov         rcx, qword [rax]
; 366
.cs: mov         qword [rbx], rcx
; 367
.ct: pop         rax
; 318
.cu: shl         rax, 3
; 146
.cv: add         qword [rsp], rax
; 324
.cw: push        rsp
; 801
.cx: add         qword [rsp], 8
; 802;copy 8 bytes from digit + '0' into [dst_index]buffer, reverse=false
.cy: pop         rax
; 344
.cz: pop         rbx
; 345
.cA: mov         rcx, qword [rax]
; 366
.cB: mov         qword [rbx], rcx
; 367
.cC: add         rsp, 8
; 808;binary /=;literal 10
.cD: push 10
; 1020;push_address_of i
.cE: push        rbp
; 266
.cF: add         qword [rsp], -16
; 267
.cG: pop         rax
; 858
.cH: pop         rbx
; 859
.cI: mov r8, rax
mov rdx, 0
mov rax, qword[r8]
div rbx
mov qword[r8], rax
; 865
.cJ: add         rsp, 8
; 1084
.cK: jmp         .bC
; 1086;binary !=;load identifer dst_index
.cL: sub         rsp, 8
; 894
.cM: push        rsp
; 895;push_address_of dst_index
.cN: push        rbp
; 266
.cO: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.cP: pop         rax
; 344
.cQ: pop         rbx
; 345
.cR: mov         rcx, qword [rax]
; 366
.cS: mov         qword [rbx], rcx
; 367;literal 64
.cT: push 64
; 1020
.cU: pop         rbx
; 832
.cV: pop         rax
; 833
.cW: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
; 838
.cX: push        rcx
; 841
.cY: pop         rax
; 1071
.cZ: test rax, rax
jz .dw
; 1072;call print_char
.d0: sub         rsp, 0
; 909;cast from 's64' to 'u8';subscript;load identifer dst_index
.d1: sub         rsp, 8
; 894
.d2: push        rsp
; 895;push_address_of dst_index
.d3: push        rbp
; 266
.d4: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.d5: pop         rax
; 344
.d6: pop         rbx
; 345
.d7: mov         rcx, qword [rax]
; 366
.d8: mov         qword [rbx], rcx
; 367;push_address_of buffer
.d9: push        rbp
; 266
.da: add         qword [rsp], -528
; 267
.db: pop         rax
; 1176
.dc: pop         rbx
; 1177
.dd: shl         rbx, 3
; 146
.de: add         rax, rbx
; 1184
.df: sub         rsp, 8
; 1188
.dg: push        rsp
; 1190
.dh: push        rax
; 1191;copy 8 bytes from [dst_index]buffer into stack, reverse=false
.di: pop         rax
; 344
.dj: pop         rbx
; 345
.dk: mov         rcx, qword [rax]
; 366
.dl: mov         qword [rbx], rcx
; 367
.dm: call        .8D
; 920
.dn: add         rsp, 8
; 924;binary +=;literal 1
.do: push 1
; 1020;push_address_of dst_index
.dp: push        rbp
; 266
.dq: add         qword [rsp], -536
; 267
.dr: pop         rax
; 858
.ds: pop         rbx
; 859
.dt: add         qword [rax], rbx
; 862
.du: add         rsp, 0
; 1084
.dv: jmp         .cL
; 1086
.dw: mov         rsp, rbp
; 487
.dx: pop         rbp
; 488
.dy: ret
; 489
.dz: push        rbp
.dA: mov         rbp, rsp
; lambda print_hex;definition val;load identifer _val
.dB: sub         rsp, 8
; 894
.dC: push        rsp
; 895;push_address_of _val
.dD: push        rbp
; 266
.dE: add         qword [rsp], 16
; 267;copy 8 bytes from _val into stack, reverse=false
.dF: pop         rax
; 344
.dG: pop         rbx
; 345
.dH: mov         rcx, qword [rax]
; 366
.dI: mov         qword [rbx], rcx
; 367;binary ==;load identifer val
.dJ: sub         rsp, 8
; 894
.dK: push        rsp
; 895;push_address_of val
.dL: push        rbp
; 266
.dM: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.dN: pop         rax
; 344
.dO: pop         rbx
; 345
.dP: mov         rcx, qword [rax]
; 366
.dQ: mov         qword [rbx], rcx
; 367;literal 0
.dR: push 0
; 1020
.dS: pop         rbx
; 832
.dT: pop         rax
; 833
.dU: push 1
mov rcx, 0
cmp rax, rbx
cmove rcx, qword [rsp]
add rsp, 8
; 838
.dV: push        rcx
; 841
.dW: pop         rax
; 1031
.dX: test rax, rax
jz .e7
; 1032;call print_char
.dY: sub         rsp, 0
; 909;literal '0'
.dZ: push 48
; 1004
.e0: call        .8D
; 920
.e1: add         rsp, 8
; 924;return
.e2: mov         rsp, rbp
; 689
.e3: pop         rbp
; 690
.e4: ret
; 691
.e5: add         rsp, 0
; 1044
.e6: jmp         .e7
; 1045
.e7: add         rsp, 0
; 1059;definition i;load identifer val
.e8: sub         rsp, 8
; 894
.e9: push        rsp
; 895;push_address_of val
.ea: push        rbp
; 266
.eb: add         qword [rsp], -8
; 267;copy 8 bytes from val into stack, reverse=false
.ec: pop         rax
; 344
.ed: pop         rbx
; 345
.ee: mov         rcx, qword [rax]
; 366
.ef: mov         qword [rbx], rcx
; 367;definition buffer
.eg: push 0
; 621
.eh: push 0
; 621
.ei: push 0
; 621
.ej: push 0
; 621
.ek: push 0
; 621
.el: push 0
; 621
.em: push 0
; 621
.en: push 0
; 621
.eo: push 0
; 621
.ep: push 0
; 621
.eq: push 0
; 621
.er: push 0
; 621
.es: push 0
; 621
.et: push 0
; 621
.eu: push 0
; 621
.ev: push 0
; 621
.ew: push 0
; 621
.ex: push 0
; 621
.ey: push 0
; 621
.ez: push 0
; 621
.eA: push 0
; 621
.eB: push 0
; 621
.eC: push 0
; 621
.eD: push 0
; 621
.eE: push 0
; 621
.eF: push 0
; 621
.eG: push 0
; 621
.eH: push 0
; 621
.eI: push 0
; 621
.eJ: push 0
; 621
.eK: push 0
; 621
.eL: push 0
; 621
.eM: push 0
; 621
.eN: push 0
; 621
.eO: push 0
; 621
.eP: push 0
; 621
.eQ: push 0
; 621
.eR: push 0
; 621
.eS: push 0
; 621
.eT: push 0
; 621
.eU: push 0
; 621
.eV: push 0
; 621
.eW: push 0
; 621
.eX: push 0
; 621
.eY: push 0
; 621
.eZ: push 0
; 621
.f0: push 0
; 621
.f1: push 0
; 621
.f2: push 0
; 621
.f3: push 0
; 621
.f4: push 0
; 621
.f5: push 0
; 621
.f6: push 0
; 621
.f7: push 0
; 621
.f8: push 0
; 621
.f9: push 0
; 621
.fa: push 0
; 621
.fb: push 0
; 621
.fc: push 0
; 621
.fd: push 0
; 621
.fe: push 0
; 621
.ff: push 0
; 621
.fg: push 0
; 621
.fh: push 0
; 621;definition dst_index;literal 64
.fi: push 64
; 1020;binary !=;load identifer i
.fj: sub         rsp, 8
; 894
.fk: push        rsp
; 895;push_address_of i
.fl: push        rbp
; 266
.fm: add         qword [rsp], -16
; 267;copy 8 bytes from i into stack, reverse=false
.fn: pop         rax
; 344
.fo: pop         rbx
; 345
.fp: mov         rcx, qword [rax]
; 366
.fq: mov         qword [rbx], rcx
; 367;literal 0
.fr: push 0
; 1020
.fs: pop         rbx
; 832
.ft: pop         rax
; 833
.fu: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
; 838
.fv: push        rcx
; 841
.fw: pop         rax
; 1071
.fx: test rax, rax
jz .hi
; 1072;definition digit;binary &;load identifer i
.fy: sub         rsp, 8
; 894
.fz: push        rsp
; 895;push_address_of i
.fA: push        rbp
; 266
.fB: add         qword [rsp], -16
; 267;copy 8 bytes from i into stack, reverse=false
.fC: pop         rax
; 344
.fD: pop         rbx
; 345
.fE: mov         rcx, qword [rax]
; 366
.fF: mov         qword [rbx], rcx
; 367;literal 15
.fG: push 15
; 1020
.fH: pop         rax
; 777
.fI: and         qword [rsp], rax
; 785;binary -=;literal 1
.fJ: push 1
; 1020;push_address_of dst_index
.fK: push        rbp
; 266
.fL: add         qword [rsp], -536
; 267
.fM: pop         rax
; 858
.fN: pop         rbx
; 859
.fO: sub         qword [rax], rbx
; 863;binary <;load identifer digit
.fP: sub         rsp, 8
; 894
.fQ: push        rsp
; 895;push_address_of digit
.fR: push        rbp
; 266
.fS: add         qword [rsp], -544
; 267;copy 8 bytes from digit into stack, reverse=false
.fT: pop         rax
; 344
.fU: pop         rbx
; 345
.fV: mov         rcx, qword [rax]
; 366
.fW: mov         qword [rbx], rcx
; 367;literal 10
.fX: push 10
; 1020
.fY: pop         rbx
; 832
.fZ: pop         rax
; 833
.g0: push 1
mov rcx, 0
cmp rax, rbx
cmovl rcx, qword [rsp]
add rsp, 8
; 838
.g1: push        rcx
; 841
.g2: pop         rax
; 1031
.g3: test rax, rax
jz .gB
; 1032;binary =;binary +;load identifer digit
.g4: sub         rsp, 8
; 894
.g5: push        rsp
; 895;push_address_of digit
.g6: push        rbp
; 266
.g7: add         qword [rsp], -544
; 267;copy 8 bytes from digit into stack, reverse=false
.g8: pop         rax
; 344
.g9: pop         rbx
; 345
.ga: mov         rcx, qword [rax]
; 366
.gb: mov         qword [rbx], rcx
; 367;literal '0'
.gc: push 48
; 1004
.gd: pop         rax
; 777
.ge: add         qword [rsp], rax
; 779;push_address_of [dst_index]buffer;push_address_of buffer
.gf: push        rbp
; 266
.gg: add         qword [rsp], -528
; 267;load identifer dst_index
.gh: sub         rsp, 8
; 894
.gi: push        rsp
; 895;push_address_of dst_index
.gj: push        rbp
; 266
.gk: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.gl: pop         rax
; 344
.gm: pop         rbx
; 345
.gn: mov         rcx, qword [rax]
; 366
.go: mov         qword [rbx], rcx
; 367
.gp: pop         rax
; 318
.gq: shl         rax, 3
; 146
.gr: add         qword [rsp], rax
; 324
.gs: push        rsp
; 801
.gt: add         qword [rsp], 8
; 802;copy 8 bytes from digit + '0' into [dst_index]buffer, reverse=false
.gu: pop         rax
; 344
.gv: pop         rbx
; 345
.gw: mov         rcx, qword [rax]
; 366
.gx: mov         qword [rbx], rcx
; 367
.gy: add         rsp, 8
; 808
.gz: add         rsp, 0
; 1044
.gA: jmp         .h9
; 1045;binary =;binary +;binary -;load identifer digit
.gB: sub         rsp, 8
; 894
.gC: push        rsp
; 895;push_address_of digit
.gD: push        rbp
; 266
.gE: add         qword [rsp], -544
; 267;copy 8 bytes from digit into stack, reverse=false
.gF: pop         rax
; 344
.gG: pop         rbx
; 345
.gH: mov         rcx, qword [rax]
; 366
.gI: mov         qword [rbx], rcx
; 367;literal 10
.gJ: push 10
; 1020
.gK: pop         rax
; 777
.gL: sub         qword [rsp], rax
; 780;literal 'a'
.gM: push 97
; 1004
.gN: pop         rax
; 777
.gO: add         qword [rsp], rax
; 779;push_address_of [dst_index]buffer;push_address_of buffer
.gP: push        rbp
; 266
.gQ: add         qword [rsp], -528
; 267;load identifer dst_index
.gR: sub         rsp, 8
; 894
.gS: push        rsp
; 895;push_address_of dst_index
.gT: push        rbp
; 266
.gU: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.gV: pop         rax
; 344
.gW: pop         rbx
; 345
.gX: mov         rcx, qword [rax]
; 366
.gY: mov         qword [rbx], rcx
; 367
.gZ: pop         rax
; 318
.h0: shl         rax, 3
; 146
.h1: add         qword [rsp], rax
; 324
.h2: push        rsp
; 801
.h3: add         qword [rsp], 8
; 802;copy 8 bytes from digit - 10 + 'a' into [dst_index]buffer, reverse=false
.h4: pop         rax
; 344
.h5: pop         rbx
; 345
.h6: mov         rcx, qword [rax]
; 366
.h7: mov         qword [rbx], rcx
; 367
.h8: add         rsp, 8
; 808
.h9: add         rsp, 0
; 1059;binary >>=;literal 4
.ha: push 4
; 1020;push_address_of i
.hb: push        rbp
; 266
.hc: add         qword [rsp], -16
; 267
.hd: pop         rax
; 858
.he: pop         rbx
; 859
.hf: mov cl, bl
shr qword[rax], cl
; 870
.hg: add         rsp, 8
; 1084
.hh: jmp         .fj
; 1086;binary !=;load identifer dst_index
.hi: sub         rsp, 8
; 894
.hj: push        rsp
; 895;push_address_of dst_index
.hk: push        rbp
; 266
.hl: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.hm: pop         rax
; 344
.hn: pop         rbx
; 345
.ho: mov         rcx, qword [rax]
; 366
.hp: mov         qword [rbx], rcx
; 367;literal 64
.hq: push 64
; 1020
.hr: pop         rbx
; 832
.hs: pop         rax
; 833
.ht: push 1
mov rcx, 0
cmp rax, rbx
cmovne rcx, qword [rsp]
add rsp, 8
; 838
.hu: push        rcx
; 841
.hv: pop         rax
; 1071
.hw: test rax, rax
jz .i3
; 1072;call print_char
.hx: sub         rsp, 0
; 909;cast from 's64' to 'u8';subscript;load identifer dst_index
.hy: sub         rsp, 8
; 894
.hz: push        rsp
; 895;push_address_of dst_index
.hA: push        rbp
; 266
.hB: add         qword [rsp], -536
; 267;copy 8 bytes from dst_index into stack, reverse=false
.hC: pop         rax
; 344
.hD: pop         rbx
; 345
.hE: mov         rcx, qword [rax]
; 366
.hF: mov         qword [rbx], rcx
; 367;push_address_of buffer
.hG: push        rbp
; 266
.hH: add         qword [rsp], -528
; 267
.hI: pop         rax
; 1176
.hJ: pop         rbx
; 1177
.hK: shl         rbx, 3
; 146
.hL: add         rax, rbx
; 1184
.hM: sub         rsp, 8
; 1188
.hN: push        rsp
; 1190
.hO: push        rax
; 1191;copy 8 bytes from [dst_index]buffer into stack, reverse=false
.hP: pop         rax
; 344
.hQ: pop         rbx
; 345
.hR: mov         rcx, qword [rax]
; 366
.hS: mov         qword [rbx], rcx
; 367
.hT: call        .8D
; 920
.hU: add         rsp, 8
; 924;binary +=;literal 1
.hV: push 1
; 1020;push_address_of dst_index
.hW: push        rbp
; 266
.hX: add         qword [rsp], -536
; 267
.hY: pop         rax
; 858
.hZ: pop         rbx
; 859
.i0: add         qword [rax], rbx
; 862
.i1: add         rsp, 0
; 1084
.i2: jmp         .hi
; 1086
.i3: mov         rsp, rbp
; 487
.i4: pop         rbp
; 488
.i5: ret
; 489
.i6: push        rbp
.i7: mov         rbp, rsp
; lambda merge;return;cast from 'u8' to 'u16';binary |;binary <<;load identifer a
.i8: sub         rsp, 8
; 894
.i9: push        rsp
; 895;push_address_of a
.ia: push        rbp
; 266
.ib: add         qword [rsp], 16
; 267;copy 1 bytes from a into stack, reverse=false
.ic: pop         rax
; 344
.id: pop         rbx
; 345
.ie: mov         cl, byte [rax]
; 420
.if: mov         byte [rbx], cl
; 421;literal 8
.ig: push 8
; 1010
.ih: pop         rax
; 777
.ii: mov cl, al
shl qword[rsp], cl
; 788;load identifer b
.ij: sub         rsp, 8
; 894
.ik: push        rsp
; 895;push_address_of b
.il: push        rbp
; 266
.im: add         qword [rsp], 24
; 267;copy 1 bytes from b into stack, reverse=false
.in: pop         rax
; 344
.io: pop         rbx
; 345
.ip: mov         cl, byte [rax]
; 420
.iq: mov         byte [rbx], cl
; 421
.ir: pop         rax
; 777
.is:  or         qword [rsp], rax
; 784
.it: and         qword [rsp], 255
; 1254
.iu: push        rbp
; 673
.iv: add         qword [rsp], 32
; 674
.iw: push        rsp
; 677
.ix: add         qword [rsp], 8
; 678;copy 2 bytes from expression into parameter, reverse=false
.iy: pop         rax
; 344
.iz: pop         rbx
; 345
.iA: mov         cl, byte [rax]
; 420
.iB: mov         byte [rbx], cl
; 421
.iC: add         rax, 1
; 424
.iD: add         rbx, 1
; 425
.iE: mov         cl, byte [rax]
; 420
.iF: mov         byte [rbx], cl
; 421
.iG: mov         rsp, rbp
; 689
.iH: pop         rbp
; 690
.iI: ret
; 691
.iJ: mov         rsp, rbp
; 487
.iK: pop         rbp
; 488
.iL: ret
; 489
