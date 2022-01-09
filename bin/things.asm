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
extern LoadCursorA
section .rodata
constants: db 246,255,255,255,255,255,255,255,245,255,255,255,255,255,255,255,244,255,255,255,255,255,255,255,0,0,0,128,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,136,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,138,127,0,0,0,0,0,0,0,127,0,0,0,0,0,0,3,127,0,0,0,0,0,0,137,127,0,0,0,0,0,0,139,127,0,0,0,0,0,0,1,127,0,0,0,0,0,0,129,127,0,0,0,0,0,0,136,127,0,0,0,0,0,0,128,127,0,0,0,0,0,0,134,127,0,0,0,0,0,0,131,127,0,0,0,0,0,0,133,127,0,0,0,0,0,0,130,127,0,0,0,0,0,0,132,127,0,0,0,0,0,0,4,127,0,0,0,0,0,0,2,127,0,0,0,0,0,0,72,101,108,108,111,32,87,111,114,108,100,33,10,72,101,108,108,111,32,87,111,114,108,100,33,10,72,101,108,108,111,32,87,111,114,108,100,33,10,116,114,117,101,10,48,32,105,115,32,48,10,48,32,105,115,32,110,111,116,32,48,10,49,32,105,115,32,108,101,115,115,32,116,104,97,110,32,50,10,116,114,117,101,10,102,97,108,115,101,10,116,114,117,101,10,102,97,108,115,101,10,51,32,105,115,32,62,61,32,48,10,51,32,105,115,32,110,111,116,32,62,61,32,48,10,42,10,109,121,32,102,105,108,101,46,116,120,116,0,72,101,108,108,111,32,87,111,114,108,100,33,65,32,103,108,111,98,97,108,32,115,116,114,105,110,103,97,32,108,111,99,97,108,32,115,116,114,105,110,103,65,83,116,114,117,99,116,65,32,83,84,82,73,78,71,42,0,0,0,0,0,0,0,
section .data
data: db 
section .bss
zeros: resb 24
section .text
global main
main:
push 0
call .0
pop rcx
and rsp, -16
sub rsp, 16
call ExitProcess
ret
.0: push rbp
.1: mov rbp, rsp
.2: mov r8, rsp
.3: and rsp, -16
.4: push r8
.5: sub rsp, 8
.6: push -11
.7: pop r8
.8: sub rsp, 32
.9: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call GetStdHandle
.a: add rsp, 40
.b: pop rsp
.c: push rax
.d: mov rax, zeros + 0
push rax
.e: push rsp
.f: add qword [rsp], 8
.g: pop r8
.h: pop r9
.i: mov r10, qword [r8]
.j: mov qword [r9], r10
.k: add rsp, 8
.l: push 13
.m: mov rax, constants + 736
push rax
.n: mov r8, rsp
.o: and rsp, -16
.p: push r8
.q: push 0
.r: push 0
.s: sub rsp, 8
.t: sub rsp, 16
.u: push rsp
.v: push rbp
.w: add qword [rsp], -16
.x: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.y: push rsp
.z: add qword [rsp], 16
.A: push rsp
.B: add qword [rsp], 16
.C: pop r8
.D: pop r9
.E: mov r10, qword [r8]
.F: mov qword [r9], r10
.G: add rsp, 16
.H: sub rsp, 8
.I: sub rsp, 16
.J: push rsp
.K: push rbp
.L: add qword [rsp], -16
.M: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.N: push rsp
.O: add qword [rsp], 16
.P: push rsp
.Q: add qword [rsp], 8
.R: pop r8
.S: pop r9
.T: mov r10, qword [r8]
.U: mov qword [r9], r10
.V: add rsp, 16
.W: sub rsp, 8
.X: push rsp
.Y: mov rax, zeros + 0
push rax
.Z: pop r8
.10: pop r9
.11: mov r10, qword [r8]
.12: mov qword [r9], r10
.13: pop r8
.14: pop r9
.15: pop r10
.16: pop r11
.17: sub rsp, 32
.18: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call WriteConsoleA
.19: add rsp, 40
.1a: pop rsp
.1b: push rax
.1c: add rsp, 8
.1d: mov r8, rsp
.1e: and rsp, -16
.1f: push r8
.1g: push 0
.1h: push 0
.1i: push 13
.1j: sub rsp, 8
.1k: push 13
.1l: mov rax, constants + 749
push rax
.1m: push rsp
.1n: add qword [rsp], 16
.1o: push rsp
.1p: add qword [rsp], 8
.1q: pop r8
.1r: pop r9
.1s: mov r10, qword [r8]
.1t: mov qword [r9], r10
.1u: add rsp, 16
.1v: sub rsp, 8
.1w: push rsp
.1x: mov rax, zeros + 0
push rax
.1y: pop r8
.1z: pop r9
.1A: mov r10, qword [r8]
.1B: mov qword [r9], r10
.1C: pop r8
.1D: pop r9
.1E: pop r10
.1F: pop r11
.1G: sub rsp, 32
.1H: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call WriteConsoleA
.1I: add rsp, 40
.1J: pop rsp
.1K: push rax
.1L: add rsp, 8
.1M: push 13
.1N: mov rax, constants + 762
push rax
.1O: call .8M
.1P: add rsp, 16
.1Q: sub rsp, 16
.1R: push rsp
.1S: push rbp
.1T: add qword [rsp], -16
.1U: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.1V: call .8M
.1W: add rsp, 16
.1X: push 1
.1Y: pop r8
.1Z: test r8, r8
jz .27
.20: push 0
.21: push 5
.22: mov rax, constants + 775
push rax
.23: call .8M
.24: add rsp, 16
.25: add rsp, 8
.26: jmp .27
.27: push 1
.28: pop r8
.29: test r8, r8
jz .2f
.2a: push 7
.2b: mov rax, constants + 780
push rax
.2c: call .8M
.2d: add rsp, 16
.2e: jmp .2j
.2f: push 11
.2g: mov rax, constants + 787
push rax
.2h: call .8M
.2i: add rsp, 16
.2j: push 1
.2k: pop r8
.2l: test r8, r8
jz .2r
.2m: push 17
.2n: mov rax, constants + 798
push rax
.2o: call .8M
.2p: add rsp, 16
.2q: jmp .2r
.2r: push 1
.2s: pop r8
.2t: test r8, r8
jz .2z
.2u: push 5
.2v: mov rax, constants + 815
push rax
.2w: call .8M
.2x: add rsp, 16
.2y: jmp .2D
.2z: push 6
.2A: mov rax, constants + 820
push rax
.2B: call .8M
.2C: add rsp, 16
.2D: push 0
.2E: pop r8
.2F: test r8, r8
jz .2L
.2G: push 5
.2H: mov rax, constants + 826
push rax
.2I: call .8M
.2J: add rsp, 16
.2K: jmp .2P
.2L: push 6
.2M: mov rax, constants + 831
push rax
.2N: call .8M
.2O: add rsp, 16
.2P: push 1
.2Q: pop r8
.2R: test r8, r8
jz .2X
.2S: push 10
.2T: mov rax, constants + 837
push rax
.2U: call .8M
.2V: add rsp, 16
.2W: jmp .31
.2X: push 14
.2Y: mov rax, constants + 847
push rax
.2Z: call .8M
.30: add rsp, 16
.31: push 10
.32: sub rsp, 8
.33: push rsp
.34: push rbp
.35: add qword [rsp], -24
.36: pop r8
.37: pop r9
.38: mov r10, qword [r8]
.39: mov qword [r9], r10
.3a: push 0
.3b: pop r9
.3c: pop r8
.3d: xor r10, r10
cmp r8, r9
setg r10b

.3e: push r10
.3f: pop r8
.3g: test r8, r8
jz .4t
.3h: sub rsp, 8
.3i: push rsp
.3j: push rbp
.3k: add qword [rsp], -24
.3l: pop r8
.3m: pop r9
.3n: mov r10, qword [r8]
.3o: mov qword [r9], r10
.3p: sub rsp, 8
.3q: push rsp
.3r: push rbp
.3s: add qword [rsp], -32
.3t: pop r8
.3u: pop r9
.3v: mov r10, qword [r8]
.3w: mov qword [r9], r10
.3x: push 0
.3y: pop r9
.3z: pop r8
.3A: xor r10, r10
cmp r8, r9
setg r10b

.3B: push r10
.3C: pop r8
.3D: test r8, r8
jz .43
.3E: push 1
.3F: mov rax, constants + 861
push rax
.3G: call .8M
.3H: add rsp, 16
.3I: sub rsp, 8
.3J: push rsp
.3K: push rbp
.3L: add qword [rsp], -32
.3M: pop r8
.3N: pop r9
.3O: mov r10, qword [r8]
.3P: mov qword [r9], r10
.3Q: push 1
.3R: pop r8
.3S: sub qword [rsp], r8
.3T: push rbp
.3U: add qword [rsp], -32
.3V: push rsp
.3W: add qword [rsp], 8
.3X: pop r8
.3Y: pop r9
.3Z: mov r10, qword [r8]
.40: mov qword [r9], r10
.41: add rsp, 8
.42: jmp .3p
.43: push 1
.44: mov rax, constants + 862
push rax
.45: call .8M
.46: add rsp, 16
.47: sub rsp, 8
.48: push rsp
.49: push rbp
.4a: add qword [rsp], -24
.4b: pop r8
.4c: pop r9
.4d: mov r10, qword [r8]
.4e: mov qword [r9], r10
.4f: push 1
.4g: pop r8
.4h: sub qword [rsp], r8
.4i: push rbp
.4j: add qword [rsp], -24
.4k: push rsp
.4l: add qword [rsp], 8
.4m: pop r8
.4n: pop r9
.4o: mov r10, qword [r8]
.4p: mov qword [r9], r10
.4q: add rsp, 8
.4r: add rsp, 8
.4s: jmp .32
.4t: push 12
.4u: mov rax, constants + 863
push rax
.4v: sub rsp, 16
.4w: push rsp
.4x: push rbp
.4y: add qword [rsp], -40
.4z: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.4A: call .8M
.4B: add rsp, 16
.4C: mov r8, rsp
.4D: and rsp, -16
.4E: push r8
.4F: push 0
.4G: push 0
.4H: push 2
.4I: push 0
.4J: push 0
.4K: push 1073741824
.4L: sub rsp, 8
.4M: sub rsp, 16
.4N: push rsp
.4O: push rbp
.4P: add qword [rsp], -40
.4Q: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.4R: push rsp
.4S: add qword [rsp], 16
.4T: push rsp
.4U: add qword [rsp], 8
.4V: pop r8
.4W: pop r9
.4X: mov r10, qword [r8]
.4Y: mov qword [r9], r10
.4Z: add rsp, 16
.50: pop r8
.51: pop r9
.52: pop r10
.53: pop r11
.54: sub rsp, 32
.55: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call CreateFileA
.56: add rsp, 56
.57: pop rsp
.58: push rax
.59: push 12
.5a: mov rax, constants + 875
push rax
.5b: mov r8, rsp
.5c: and rsp, -16
.5d: push r8
.5e: push 0
.5f: push 0
.5g: sub rsp, 8
.5h: sub rsp, 16
.5i: push rsp
.5j: push rbp
.5k: add qword [rsp], -64
.5l: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.5m: push rsp
.5n: add qword [rsp], 16
.5o: push rsp
.5p: add qword [rsp], 16
.5q: pop r8
.5r: pop r9
.5s: mov r10, qword [r8]
.5t: mov qword [r9], r10
.5u: add rsp, 16
.5v: sub rsp, 8
.5w: sub rsp, 16
.5x: push rsp
.5y: push rbp
.5z: add qword [rsp], -64
.5A: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.5B: push rsp
.5C: add qword [rsp], 16
.5D: push rsp
.5E: add qword [rsp], 8
.5F: pop r8
.5G: pop r9
.5H: mov r10, qword [r8]
.5I: mov qword [r9], r10
.5J: add rsp, 16
.5K: sub rsp, 8
.5L: push rsp
.5M: push rbp
.5N: add qword [rsp], -48
.5O: pop r8
.5P: pop r9
.5Q: mov r10, qword [r8]
.5R: mov qword [r9], r10
.5S: pop r8
.5T: pop r9
.5U: pop r10
.5V: pop r11
.5W: sub rsp, 32
.5X: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call WriteFile
.5Y: add rsp, 40
.5Z: pop rsp
.60: push rax
.61: add rsp, 8
.62: push 15
.63: mov rax, constants + 887
push rax
.64: mov rax, zeros + 8
push rax
.65: push rsp
.66: add qword [rsp], 8
.67: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.68: add rsp, 16
.69: sub rsp, 16
.6a: push rsp
.6b: mov rax, zeros + 8
push rax
.6c: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.6d: call .8M
.6e: add rsp, 16
.6f: push 0
.6g: push 0
.6h: push 14
.6i: mov rax, constants + 902
push rax
.6j: push rbp
.6k: add qword [rsp], -80
.6l: push rsp
.6m: add qword [rsp], 8
.6n: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.6o: add rsp, 16
.6p: sub rsp, 16
.6q: push rsp
.6r: push rbp
.6s: add qword [rsp], -80
.6t: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.6u: call .8M
.6v: add rsp, 16
.6w: push 0
.6x: push 0
.6y: push 7
.6z: push rbp
.6A: add qword [rsp], -96
.6B: add qword [rsp], 8
.6C: push rsp
.6D: add qword [rsp], 8
.6E: pop r8
.6F: pop r9
.6G: mov r10, qword [r8]
.6H: mov qword [r9], r10
.6I: add rsp, 8
.6J: sub rsp, 8
.6K: push 7
.6L: mov rax, constants + 916
push rax
.6M: push rsp
.6N: add qword [rsp], 16
.6O: push rsp
.6P: add qword [rsp], 8
.6Q: pop r8
.6R: pop r9
.6S: mov r10, qword [r8]
.6T: mov qword [r9], r10
.6U: add rsp, 16
.6V: push rbp
.6W: add qword [rsp], -96
.6X: push rsp
.6Y: add qword [rsp], 8
.6Z: pop r8
.70: pop r9
.71: mov r10, qword [r8]
.72: mov qword [r9], r10
.73: add rsp, 8
.74: mov r8, rsp
.75: and rsp, -16
.76: push r8
.77: push 0
.78: push 0
.79: sub rsp, 8
.7a: sub rsp, 16
.7b: push rsp
.7c: push rbp
.7d: add qword [rsp], -96
.7e: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.7f: push rsp
.7g: add qword [rsp], 16
.7h: push rsp
.7i: add qword [rsp], 16
.7j: pop r8
.7k: pop r9
.7l: mov r10, qword [r8]
.7m: mov qword [r9], r10
.7n: add rsp, 16
.7o: sub rsp, 8
.7p: sub rsp, 16
.7q: push rsp
.7r: push rbp
.7s: add qword [rsp], -96
.7t: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.7u: push rsp
.7v: add qword [rsp], 16
.7w: push rsp
.7x: add qword [rsp], 8
.7y: pop r8
.7z: pop r9
.7A: mov r10, qword [r8]
.7B: mov qword [r9], r10
.7C: add rsp, 16
.7D: sub rsp, 8
.7E: push rsp
.7F: mov rax, zeros + 0
push rax
.7G: pop r8
.7H: pop r9
.7I: mov r10, qword [r8]
.7J: mov qword [r9], r10
.7K: pop r8
.7L: pop r9
.7M: pop r10
.7N: pop r11
.7O: sub rsp, 32
.7P: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call WriteConsoleA
.7Q: add rsp, 40
.7R: pop rsp
.7S: push rax
.7T: add rsp, 8
.7U: push 0
.7V: push 0
.7W: push 7
.7X: push rbp
.7Y: add qword [rsp], -112
.7Z: add qword [rsp], 8
.80: push rsp
.81: add qword [rsp], 8
.82: pop r8
.83: pop r9
.84: mov r10, qword [r8]
.85: mov qword [r9], r10
.86: add rsp, 8
.87: sub rsp, 8
.88: push 8
.89: mov rax, constants + 923
push rax
.8a: push rsp
.8b: add qword [rsp], 16
.8c: push rsp
.8d: add qword [rsp], 8
.8e: pop r8
.8f: pop r9
.8g: mov r10, qword [r8]
.8h: mov qword [r9], r10
.8i: add rsp, 16
.8j: push rbp
.8k: add qword [rsp], -112
.8l: push rsp
.8m: add qword [rsp], 8
.8n: pop r8
.8o: pop r9
.8p: mov r10, qword [r8]
.8q: mov qword [r9], r10
.8r: add rsp, 8
.8s: sub rsp, 16
.8t: push rsp
.8u: push rbp
.8v: add qword [rsp], -112
.8w: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.8x: call .8M
.8y: add rsp, 16
.8z: push 42
.8A: push rbp
.8B: add qword [rsp], 16
.8C: push rsp
.8D: add qword [rsp], 8
.8E: pop r8
.8F: pop r9
.8G: mov r10d, dword [r8]
.8H: mov dword [r9], r10d
.8I: jmp .8J
.8J: mov rsp, rbp
.8K: pop rbp
.8L: ret
.8M: push rbp
.8N: mov rbp, rsp
.8O: mov r8, rsp
.8P: and rsp, -16
.8Q: push r8
.8R: push 0
.8S: push 0
.8T: sub rsp, 8
.8U: sub rsp, 16
.8V: push rsp
.8W: push rbp
.8X: add qword [rsp], 16
.8Y: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.8Z: push rsp
.90: add qword [rsp], 16
.91: push rsp
.92: add qword [rsp], 16
.93: pop r8
.94: pop r9
.95: mov r10, qword [r8]
.96: mov qword [r9], r10
.97: add rsp, 16
.98: sub rsp, 8
.99: sub rsp, 16
.9a: push rsp
.9b: push rbp
.9c: add qword [rsp], 16
.9d: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb
.9e: push rsp
.9f: add qword [rsp], 16
.9g: push rsp
.9h: add qword [rsp], 8
.9i: pop r8
.9j: pop r9
.9k: mov r10, qword [r8]
.9l: mov qword [r9], r10
.9m: add rsp, 16
.9n: sub rsp, 8
.9o: push rsp
.9p: mov rax, zeros + 0
push rax
.9q: pop r8
.9r: pop r9
.9s: mov r10, qword [r8]
.9t: mov qword [r9], r10
.9u: pop r8
.9v: pop r9
.9w: pop r10
.9x: pop r11
.9y: sub rsp, 32
.9z: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call WriteConsoleA
.9A: add rsp, 40
.9B: pop rsp
.9C: push rax
.9D: add rsp, 8
.9E: mov rsp, rbp
.9F: pop rbp
.9G: ret
