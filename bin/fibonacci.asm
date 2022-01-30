format PE64 console
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
push 0
call .aJ
pop rcx
and rsp, -16
sub rsp, 16
call [ExitProcess]
ret
.0: push rbp; bytecode.cpp:1142
.1: mov rbp, rsp; bytecode.cpp:1143
; lambda print_string;call WriteConsoleA
.2: mov r8, rsp; bytecode.cpp:800
.3: and rsp, -16; bytecode.cpp:801
.4: push r8; bytecode.cpp:802
; literal null
.5: push 0; bytecode.cpp:877
; literal null
.6: push 0; bytecode.cpp:877
; cast from 'u64' to 'u32';binary .
.7: sub rsp, 8; bytecode.cpp:563
; load identifer str
.8: sub rsp, 16; bytecode.cpp:747
.9: push rsp; bytecode.cpp:748
; push_address_of str
.a: push rbp; bytecode.cpp:299
.b: add qword [rsp], 16; bytecode.cpp:300
; copy 16 bytes from str into stack, reverse=false
.c: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb; bytecode.cpp:402
.d: push rsp; bytecode.cpp:584
.e: add qword [rsp], 16; bytecode.cpp:585
.f: push rsp; bytecode.cpp:587
.g: add qword [rsp], 16; bytecode.cpp:588
; copy 8 bytes from . into stack, reverse=true
.h: pop r8; bytecode.cpp:381
.i: pop r9; bytecode.cpp:382
.j: mov r10, qword [r8]; bytecode.cpp:384
.k: mov qword [r9], r10; bytecode.cpp:385
.l: add rsp, 16; bytecode.cpp:592
; binary .
.m: sub rsp, 8; bytecode.cpp:563
; load identifer str
.n: sub rsp, 16; bytecode.cpp:747
.o: push rsp; bytecode.cpp:748
; push_address_of str
.p: push rbp; bytecode.cpp:299
.q: add qword [rsp], 16; bytecode.cpp:300
; copy 16 bytes from str into stack, reverse=false
.r: mov rcx, 16
pop rsi
pop rdi
cld
rep movsb; bytecode.cpp:402
.s: push rsp; bytecode.cpp:584
.t: add qword [rsp], 16; bytecode.cpp:585
.u: push rsp; bytecode.cpp:587
.v: add qword [rsp], 8; bytecode.cpp:588
; copy 8 bytes from str.data into stack, reverse=true
.w: pop r8; bytecode.cpp:381
.x: pop r9; bytecode.cpp:382
.y: mov r10, qword [r8]; bytecode.cpp:384
.z: mov qword [r9], r10; bytecode.cpp:385
.A: add rsp, 16; bytecode.cpp:592
; call GetStdHandle
.B: mov r8, rsp; bytecode.cpp:800
.C: and rsp, -16; bytecode.cpp:801
.D: push r8; bytecode.cpp:802
.E: sub rsp, 8; bytecode.cpp:805
; literal 
.F: push -11; bytecode.cpp:873
.G: pop r8; bytecode.cpp:813
.H: sub rsp, 32; bytecode.cpp:820
.I: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [GetStdHandle]; bytecode.cpp:829
.J: add rsp, 40; bytecode.cpp:834
.K: pop rsp; bytecode.cpp:836
.L: push rax; bytecode.cpp:838
.M: pop r8; bytecode.cpp:813
.N: pop r9; bytecode.cpp:814
.O: pop r10; bytecode.cpp:815
.P: pop r11; bytecode.cpp:816
.Q: sub rsp, 32; bytecode.cpp:820
.R: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [WriteConsoleA]; bytecode.cpp:829
.S: add rsp, 40; bytecode.cpp:834
.T: pop rsp; bytecode.cpp:836
.U: push rax; bytecode.cpp:838
.V: add rsp, 8; bytecode.cpp:993
.W: mov rsp, rbp; bytecode.cpp:1205
.X: pop rbp; bytecode.cpp:1206
.Y: ret; bytecode.cpp:1229
.Z: push rbp; bytecode.cpp:1142
.10: mov rbp, rsp; bytecode.cpp:1143
; lambda print_char;call WriteConsoleA
.11: mov r8, rsp; bytecode.cpp:800
.12: and rsp, -16; bytecode.cpp:801
.13: push r8; bytecode.cpp:802
; literal null
.14: push 0; bytecode.cpp:877
; literal null
.15: push 0; bytecode.cpp:877
; literal 1
.16: push 1; bytecode.cpp:873
; push_address_of char
.17: push rbp; bytecode.cpp:299
.18: add qword [rsp], 16; bytecode.cpp:300
; call GetStdHandle
.19: mov r8, rsp; bytecode.cpp:800
.1a: and rsp, -16; bytecode.cpp:801
.1b: push r8; bytecode.cpp:802
.1c: sub rsp, 8; bytecode.cpp:805
; literal 
.1d: push -11; bytecode.cpp:873
.1e: pop r8; bytecode.cpp:813
.1f: sub rsp, 32; bytecode.cpp:820
.1g: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [GetStdHandle]; bytecode.cpp:829
.1h: add rsp, 40; bytecode.cpp:834
.1i: pop rsp; bytecode.cpp:836
.1j: push rax; bytecode.cpp:838
.1k: pop r8; bytecode.cpp:813
.1l: pop r9; bytecode.cpp:814
.1m: pop r10; bytecode.cpp:815
.1n: pop r11; bytecode.cpp:816
.1o: sub rsp, 32; bytecode.cpp:820
.1p: mov rcx, r8
mov rdx, r9
mov r8, r10
mov r9, r11
call [WriteConsoleA]; bytecode.cpp:829
.1q: add rsp, 40; bytecode.cpp:834
.1r: pop rsp; bytecode.cpp:836
.1s: push rax; bytecode.cpp:838
.1t: add rsp, 8; bytecode.cpp:993
.1u: mov rsp, rbp; bytecode.cpp:1205
.1v: pop rbp; bytecode.cpp:1206
.1w: ret; bytecode.cpp:1229
.1x: push rbp; bytecode.cpp:1142
.1y: mov rbp, rsp; bytecode.cpp:1143
; lambda print_int;definition val;load identifer _val
.1z: sub rsp, 8; bytecode.cpp:747
.1A: push rsp; bytecode.cpp:748
; push_address_of _val
.1B: push rbp; bytecode.cpp:299
.1C: add qword [rsp], 16; bytecode.cpp:300
; copy 8 bytes from _val into stack, reverse=false
.1D: pop r8; bytecode.cpp:381
.1E: pop r9; bytecode.cpp:382
.1F: mov r10, qword [r8]; bytecode.cpp:384
.1G: mov qword [r9], r10; bytecode.cpp:385
; binary ==;load identifer val
.1H: sub rsp, 8; bytecode.cpp:747
.1I: push rsp; bytecode.cpp:748
; push_address_of val
.1J: push rbp; bytecode.cpp:299
.1K: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.1L: pop r8; bytecode.cpp:381
.1M: pop r9; bytecode.cpp:382
.1N: mov r10, qword [r8]; bytecode.cpp:384
.1O: mov qword [r9], r10; bytecode.cpp:385
; literal 0
.1P: push 0; bytecode.cpp:877
.1Q: pop r9; bytecode.cpp:672
.1R: pop r8; bytecode.cpp:673
.1S: xor r10, r10
cmp r8, r9
sete r10b; bytecode.cpp:679
.1T: push r10; bytecode.cpp:691
.1U: pop r8; bytecode.cpp:890
.1V: test r8, r8
jz .21; bytecode.cpp:891
; call print_char;literal '0'
.1W: push 48; bytecode.cpp:861
.1X: call .Z; bytecode.cpp:774
.1Y: add rsp, 8; bytecode.cpp:781
; return
.1Z: jmp .5I; bytecode.cpp:517
.20: jmp .21; bytecode.cpp:903
; binary <;load identifer val
.21: sub rsp, 8; bytecode.cpp:747
.22: push rsp; bytecode.cpp:748
; push_address_of val
.23: push rbp; bytecode.cpp:299
.24: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.25: pop r8; bytecode.cpp:381
.26: pop r9; bytecode.cpp:382
.27: mov r10, qword [r8]; bytecode.cpp:384
.28: mov qword [r9], r10; bytecode.cpp:385
; literal 0
.29: push 0; bytecode.cpp:877
.2a: pop r9; bytecode.cpp:672
.2b: pop r8; bytecode.cpp:673
.2c: xor r10, r10
cmp r8, r9
setl r10b; bytecode.cpp:679
.2d: push r10; bytecode.cpp:691
.2e: pop r8; bytecode.cpp:890
.2f: test r8, r8
jz .2F; bytecode.cpp:891
; call print_char;literal '-'
.2g: push 45; bytecode.cpp:861
.2h: call .Z; bytecode.cpp:774
.2i: add rsp, 8; bytecode.cpp:781
; binary =;load identifer val
.2j: sub rsp, 8; bytecode.cpp:747
.2k: push rsp; bytecode.cpp:748
; push_address_of val
.2l: push rbp; bytecode.cpp:299
.2m: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.2n: pop r8; bytecode.cpp:381
.2o: pop r9; bytecode.cpp:382
.2p: mov r10, qword [r8]; bytecode.cpp:384
.2q: mov qword [r9], r10; bytecode.cpp:385
.2r: pop r9; bytecode.cpp:1013
.2s: xor r8, r8; bytecode.cpp:1014
.2t: sub r8, r9; bytecode.cpp:1015
.2u: push r8; bytecode.cpp:1016
; push_address_of val
.2v: push rbp; bytecode.cpp:299
.2w: add qword [rsp], -8; bytecode.cpp:300
.2x: push rsp; bytecode.cpp:641
.2y: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from -val into val, reverse=false
.2z: pop r8; bytecode.cpp:381
.2A: pop r9; bytecode.cpp:382
.2B: mov r10, qword [r8]; bytecode.cpp:384
.2C: mov qword [r9], r10; bytecode.cpp:385
.2D: add rsp, 8; bytecode.cpp:648
.2E: jmp .2F; bytecode.cpp:903
; definition i;load identifer val
.2F: sub rsp, 8; bytecode.cpp:747
.2G: push rsp; bytecode.cpp:748
; push_address_of val
.2H: push rbp; bytecode.cpp:299
.2I: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.2J: pop r8; bytecode.cpp:381
.2K: pop r9; bytecode.cpp:382
.2L: mov r10, qword [r8]; bytecode.cpp:384
.2M: mov qword [r9], r10; bytecode.cpp:385
; definition buffer
.2N: push 0; bytecode.cpp:454
.2O: push 0; bytecode.cpp:454
.2P: push 0; bytecode.cpp:454
.2Q: push 0; bytecode.cpp:454
.2R: push 0; bytecode.cpp:454
.2S: push 0; bytecode.cpp:454
.2T: push 0; bytecode.cpp:454
.2U: push 0; bytecode.cpp:454
.2V: push 0; bytecode.cpp:454
.2W: push 0; bytecode.cpp:454
.2X: push 0; bytecode.cpp:454
.2Y: push 0; bytecode.cpp:454
.2Z: push 0; bytecode.cpp:454
.30: push 0; bytecode.cpp:454
.31: push 0; bytecode.cpp:454
.32: push 0; bytecode.cpp:454
.33: push 0; bytecode.cpp:454
.34: push 0; bytecode.cpp:454
.35: push 0; bytecode.cpp:454
.36: push 0; bytecode.cpp:454
.37: push 0; bytecode.cpp:454
.38: push 0; bytecode.cpp:454
.39: push 0; bytecode.cpp:454
.3a: push 0; bytecode.cpp:454
.3b: push 0; bytecode.cpp:454
.3c: push 0; bytecode.cpp:454
.3d: push 0; bytecode.cpp:454
.3e: push 0; bytecode.cpp:454
.3f: push 0; bytecode.cpp:454
.3g: push 0; bytecode.cpp:454
.3h: push 0; bytecode.cpp:454
.3i: push 0; bytecode.cpp:454
.3j: push 0; bytecode.cpp:454
.3k: push 0; bytecode.cpp:454
.3l: push 0; bytecode.cpp:454
.3m: push 0; bytecode.cpp:454
.3n: push 0; bytecode.cpp:454
.3o: push 0; bytecode.cpp:454
.3p: push 0; bytecode.cpp:454
.3q: push 0; bytecode.cpp:454
.3r: push 0; bytecode.cpp:454
.3s: push 0; bytecode.cpp:454
.3t: push 0; bytecode.cpp:454
.3u: push 0; bytecode.cpp:454
.3v: push 0; bytecode.cpp:454
.3w: push 0; bytecode.cpp:454
.3x: push 0; bytecode.cpp:454
.3y: push 0; bytecode.cpp:454
.3z: push 0; bytecode.cpp:454
.3A: push 0; bytecode.cpp:454
.3B: push 0; bytecode.cpp:454
.3C: push 0; bytecode.cpp:454
.3D: push 0; bytecode.cpp:454
.3E: push 0; bytecode.cpp:454
.3F: push 0; bytecode.cpp:454
.3G: push 0; bytecode.cpp:454
.3H: push 0; bytecode.cpp:454
.3I: push 0; bytecode.cpp:454
.3J: push 0; bytecode.cpp:454
.3K: push 0; bytecode.cpp:454
.3L: push 0; bytecode.cpp:454
.3M: push 0; bytecode.cpp:454
.3N: push 0; bytecode.cpp:454
.3O: push 0; bytecode.cpp:454
; definition dst_index;literal 64
.3P: push 64; bytecode.cpp:877
; binary !=;load identifer i
.3Q: sub rsp, 8; bytecode.cpp:747
.3R: push rsp; bytecode.cpp:748
; push_address_of i
.3S: push rbp; bytecode.cpp:299
.3T: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from i into stack, reverse=false
.3U: pop r8; bytecode.cpp:381
.3V: pop r9; bytecode.cpp:382
.3W: mov r10, qword [r8]; bytecode.cpp:384
.3X: mov qword [r9], r10; bytecode.cpp:385
; literal 0
.3Y: push 0; bytecode.cpp:877
.3Z: pop r9; bytecode.cpp:672
.40: pop r8; bytecode.cpp:673
.41: xor r10, r10
cmp r8, r9
setne r10b; bytecode.cpp:679
.42: push r10; bytecode.cpp:691
.43: pop r8; bytecode.cpp:929
.44: test r8, r8
jz .4Z; bytecode.cpp:930
; definition digit;binary %;load identifer i
.45: sub rsp, 8; bytecode.cpp:747
.46: push rsp; bytecode.cpp:748
; push_address_of i
.47: push rbp; bytecode.cpp:299
.48: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from i into stack, reverse=false
.49: pop r8; bytecode.cpp:381
.4a: pop r9; bytecode.cpp:382
.4b: mov r10, qword [r8]; bytecode.cpp:384
.4c: mov qword [r9], r10; bytecode.cpp:385
; literal 10
.4d: push 10; bytecode.cpp:877
.4e: pop r8; bytecode.cpp:617
.4f: mov rdx, 0
mov rax, qword[rsp]
div r8
mov qword[rsp], rdx; bytecode.cpp:623
; binary -=;literal 1
.4g: push 1; bytecode.cpp:877
; push_address_of dst_index
.4h: push rbp; bytecode.cpp:299
.4i: add qword [rsp], -536; bytecode.cpp:300
.4j: pop r8; bytecode.cpp:708
.4k: pop r9; bytecode.cpp:709
.4l: sub qword [r8], r9; bytecode.cpp:713
; binary =;binary +;load identifer digit
.4m: sub rsp, 8; bytecode.cpp:747
.4n: push rsp; bytecode.cpp:748
; push_address_of digit
.4o: push rbp; bytecode.cpp:299
.4p: add qword [rsp], -544; bytecode.cpp:300
; copy 8 bytes from digit into stack, reverse=false
.4q: pop r8; bytecode.cpp:381
.4r: pop r9; bytecode.cpp:382
.4s: mov r10, qword [r8]; bytecode.cpp:384
.4t: mov qword [r9], r10; bytecode.cpp:385
; literal '0'
.4u: push 48; bytecode.cpp:861
.4v: pop r8; bytecode.cpp:617
.4w: add qword [rsp], r8; bytecode.cpp:619
; push_address_of buffer[dst_index];push_address_of buffer
.4x: push rbp; bytecode.cpp:299
.4y: add qword [rsp], -528; bytecode.cpp:300
; load identifer dst_index
.4z: sub rsp, 8; bytecode.cpp:747
.4A: push rsp; bytecode.cpp:748
; push_address_of dst_index
.4B: push rbp; bytecode.cpp:299
.4C: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.4D: pop r8; bytecode.cpp:381
.4E: pop r9; bytecode.cpp:382
.4F: mov r10, qword [r8]; bytecode.cpp:384
.4G: mov qword [r9], r10; bytecode.cpp:385
.4H: pop r8; bytecode.cpp:350
.4I: shl r8, 3; bytecode.cpp:157
.4J: add qword [rsp], r8; bytecode.cpp:356
.4K: push rsp; bytecode.cpp:641
.4L: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from digit + '0' into buffer[dst_index], reverse=false
.4M: pop r8; bytecode.cpp:381
.4N: pop r9; bytecode.cpp:382
.4O: mov r10, qword [r8]; bytecode.cpp:384
.4P: mov qword [r9], r10; bytecode.cpp:385
.4Q: add rsp, 8; bytecode.cpp:648
; binary /=;literal 10
.4R: push 10; bytecode.cpp:877
; push_address_of i
.4S: push rbp; bytecode.cpp:299
.4T: add qword [rsp], -16; bytecode.cpp:300
.4U: pop r8; bytecode.cpp:708
.4V: pop r9; bytecode.cpp:709
.4W: mov rdx, 0
mov rax, qword[r8]
div r9
mov qword[r8], rax; bytecode.cpp:715
.4X: add rsp, 8; bytecode.cpp:942
.4Y: jmp .3Q; bytecode.cpp:944
; binary !=;load identifer dst_index
.4Z: sub rsp, 8; bytecode.cpp:747
.50: push rsp; bytecode.cpp:748
; push_address_of dst_index
.51: push rbp; bytecode.cpp:299
.52: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.53: pop r8; bytecode.cpp:381
.54: pop r9; bytecode.cpp:382
.55: mov r10, qword [r8]; bytecode.cpp:384
.56: mov qword [r9], r10; bytecode.cpp:385
; literal 64
.57: push 64; bytecode.cpp:877
.58: pop r9; bytecode.cpp:672
.59: pop r8; bytecode.cpp:673
.5a: xor r10, r10
cmp r8, r9
setne r10b; bytecode.cpp:679
.5b: push r10; bytecode.cpp:691
.5c: pop r8; bytecode.cpp:929
.5d: test r8, r8
jz .5I; bytecode.cpp:930
; call print_char;cast from 's64' to 'u8';subscript;load identifer dst_index
.5e: sub rsp, 8; bytecode.cpp:747
.5f: push rsp; bytecode.cpp:748
; push_address_of dst_index
.5g: push rbp; bytecode.cpp:299
.5h: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.5i: pop r8; bytecode.cpp:381
.5j: pop r9; bytecode.cpp:382
.5k: mov r10, qword [r8]; bytecode.cpp:384
.5l: mov qword [r9], r10; bytecode.cpp:385
; push_address_of buffer
.5m: push rbp; bytecode.cpp:299
.5n: add qword [rsp], -528; bytecode.cpp:300
.5o: pop r8; bytecode.cpp:1033
.5p: pop r9; bytecode.cpp:1034
.5q: shl r9, 3; bytecode.cpp:157
.5r: add r8, r9; bytecode.cpp:1041
.5s: sub rsp, 8; bytecode.cpp:1045
.5t: push rsp; bytecode.cpp:1047
.5u: push r8; bytecode.cpp:1048
; copy 8 bytes from buffer[dst_index] into stack, reverse=false
.5v: pop r8; bytecode.cpp:381
.5w: pop r9; bytecode.cpp:382
.5x: mov r10, qword [r8]; bytecode.cpp:384
.5y: mov qword [r9], r10; bytecode.cpp:385
.5z: call .Z; bytecode.cpp:774
.5A: add rsp, 8; bytecode.cpp:781
; binary +=;literal 1
.5B: push 1; bytecode.cpp:877
; push_address_of dst_index
.5C: push rbp; bytecode.cpp:299
.5D: add qword [rsp], -536; bytecode.cpp:300
.5E: pop r8; bytecode.cpp:708
.5F: pop r9; bytecode.cpp:709
.5G: add qword [r8], r9; bytecode.cpp:712
.5H: jmp .4Z; bytecode.cpp:944
.5I: mov rsp, rbp; bytecode.cpp:1205
.5J: pop rbp; bytecode.cpp:1206
.5K: ret; bytecode.cpp:1229
.5L: push rbp; bytecode.cpp:1142
.5M: mov rbp, rsp; bytecode.cpp:1143
; lambda print_hex;definition val;load identifer _val
.5N: sub rsp, 8; bytecode.cpp:747
.5O: push rsp; bytecode.cpp:748
; push_address_of _val
.5P: push rbp; bytecode.cpp:299
.5Q: add qword [rsp], 16; bytecode.cpp:300
; copy 8 bytes from _val into stack, reverse=false
.5R: pop r8; bytecode.cpp:381
.5S: pop r9; bytecode.cpp:382
.5T: mov r10, qword [r8]; bytecode.cpp:384
.5U: mov qword [r9], r10; bytecode.cpp:385
; binary ==;load identifer val
.5V: sub rsp, 8; bytecode.cpp:747
.5W: push rsp; bytecode.cpp:748
; push_address_of val
.5X: push rbp; bytecode.cpp:299
.5Y: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.5Z: pop r8; bytecode.cpp:381
.60: pop r9; bytecode.cpp:382
.61: mov r10, qword [r8]; bytecode.cpp:384
.62: mov qword [r9], r10; bytecode.cpp:385
; literal 0
.63: push 0; bytecode.cpp:877
.64: pop r9; bytecode.cpp:672
.65: pop r8; bytecode.cpp:673
.66: xor r10, r10
cmp r8, r9
sete r10b; bytecode.cpp:679
.67: push r10; bytecode.cpp:691
.68: pop r8; bytecode.cpp:890
.69: test r8, r8
jz .6f; bytecode.cpp:891
; call print_char;literal '0'
.6a: push 48; bytecode.cpp:861
.6b: call .Z; bytecode.cpp:774
.6c: add rsp, 8; bytecode.cpp:781
; return
.6d: jmp .a6; bytecode.cpp:517
.6e: jmp .6f; bytecode.cpp:903
; definition i;load identifer val
.6f: sub rsp, 8; bytecode.cpp:747
.6g: push rsp; bytecode.cpp:748
; push_address_of val
.6h: push rbp; bytecode.cpp:299
.6i: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.6j: pop r8; bytecode.cpp:381
.6k: pop r9; bytecode.cpp:382
.6l: mov r10, qword [r8]; bytecode.cpp:384
.6m: mov qword [r9], r10; bytecode.cpp:385
; definition buffer
.6n: push 0; bytecode.cpp:454
.6o: push 0; bytecode.cpp:454
.6p: push 0; bytecode.cpp:454
.6q: push 0; bytecode.cpp:454
.6r: push 0; bytecode.cpp:454
.6s: push 0; bytecode.cpp:454
.6t: push 0; bytecode.cpp:454
.6u: push 0; bytecode.cpp:454
.6v: push 0; bytecode.cpp:454
.6w: push 0; bytecode.cpp:454
.6x: push 0; bytecode.cpp:454
.6y: push 0; bytecode.cpp:454
.6z: push 0; bytecode.cpp:454
.6A: push 0; bytecode.cpp:454
.6B: push 0; bytecode.cpp:454
.6C: push 0; bytecode.cpp:454
.6D: push 0; bytecode.cpp:454
.6E: push 0; bytecode.cpp:454
.6F: push 0; bytecode.cpp:454
.6G: push 0; bytecode.cpp:454
.6H: push 0; bytecode.cpp:454
.6I: push 0; bytecode.cpp:454
.6J: push 0; bytecode.cpp:454
.6K: push 0; bytecode.cpp:454
.6L: push 0; bytecode.cpp:454
.6M: push 0; bytecode.cpp:454
.6N: push 0; bytecode.cpp:454
.6O: push 0; bytecode.cpp:454
.6P: push 0; bytecode.cpp:454
.6Q: push 0; bytecode.cpp:454
.6R: push 0; bytecode.cpp:454
.6S: push 0; bytecode.cpp:454
.6T: push 0; bytecode.cpp:454
.6U: push 0; bytecode.cpp:454
.6V: push 0; bytecode.cpp:454
.6W: push 0; bytecode.cpp:454
.6X: push 0; bytecode.cpp:454
.6Y: push 0; bytecode.cpp:454
.6Z: push 0; bytecode.cpp:454
.70: push 0; bytecode.cpp:454
.71: push 0; bytecode.cpp:454
.72: push 0; bytecode.cpp:454
.73: push 0; bytecode.cpp:454
.74: push 0; bytecode.cpp:454
.75: push 0; bytecode.cpp:454
.76: push 0; bytecode.cpp:454
.77: push 0; bytecode.cpp:454
.78: push 0; bytecode.cpp:454
.79: push 0; bytecode.cpp:454
.7a: push 0; bytecode.cpp:454
.7b: push 0; bytecode.cpp:454
.7c: push 0; bytecode.cpp:454
.7d: push 0; bytecode.cpp:454
.7e: push 0; bytecode.cpp:454
.7f: push 0; bytecode.cpp:454
.7g: push 0; bytecode.cpp:454
.7h: push 0; bytecode.cpp:454
.7i: push 0; bytecode.cpp:454
.7j: push 0; bytecode.cpp:454
.7k: push 0; bytecode.cpp:454
.7l: push 0; bytecode.cpp:454
.7m: push 0; bytecode.cpp:454
.7n: push 0; bytecode.cpp:454
.7o: push 0; bytecode.cpp:454
; definition dst_index;literal 64
.7p: push 64; bytecode.cpp:877
; binary !=;load identifer i
.7q: sub rsp, 8; bytecode.cpp:747
.7r: push rsp; bytecode.cpp:748
; push_address_of i
.7s: push rbp; bytecode.cpp:299
.7t: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from i into stack, reverse=false
.7u: pop r8; bytecode.cpp:381
.7v: pop r9; bytecode.cpp:382
.7w: mov r10, qword [r8]; bytecode.cpp:384
.7x: mov qword [r9], r10; bytecode.cpp:385
; literal 0
.7y: push 0; bytecode.cpp:877
.7z: pop r9; bytecode.cpp:672
.7A: pop r8; bytecode.cpp:673
.7B: xor r10, r10
cmp r8, r9
setne r10b; bytecode.cpp:679
.7C: push r10; bytecode.cpp:691
.7D: pop r8; bytecode.cpp:929
.7E: test r8, r8
jz .9n; bytecode.cpp:930
; definition digit;binary &;load identifer i
.7F: sub rsp, 8; bytecode.cpp:747
.7G: push rsp; bytecode.cpp:748
; push_address_of i
.7H: push rbp; bytecode.cpp:299
.7I: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from i into stack, reverse=false
.7J: pop r8; bytecode.cpp:381
.7K: pop r9; bytecode.cpp:382
.7L: mov r10, qword [r8]; bytecode.cpp:384
.7M: mov qword [r9], r10; bytecode.cpp:385
; literal 15
.7N: push 15; bytecode.cpp:877
.7O: pop r8; bytecode.cpp:617
.7P: and qword [rsp], r8; bytecode.cpp:625
; binary -=;literal 1
.7Q: push 1; bytecode.cpp:877
; push_address_of dst_index
.7R: push rbp; bytecode.cpp:299
.7S: add qword [rsp], -536; bytecode.cpp:300
.7T: pop r8; bytecode.cpp:708
.7U: pop r9; bytecode.cpp:709
.7V: sub qword [r8], r9; bytecode.cpp:713
; binary <;load identifer digit
.7W: sub rsp, 8; bytecode.cpp:747
.7X: push rsp; bytecode.cpp:748
; push_address_of digit
.7Y: push rbp; bytecode.cpp:299
.7Z: add qword [rsp], -544; bytecode.cpp:300
; copy 8 bytes from digit into stack, reverse=false
.80: pop r8; bytecode.cpp:381
.81: pop r9; bytecode.cpp:382
.82: mov r10, qword [r8]; bytecode.cpp:384
.83: mov qword [r9], r10; bytecode.cpp:385
; literal 10
.84: push 10; bytecode.cpp:877
.85: pop r9; bytecode.cpp:672
.86: pop r8; bytecode.cpp:673
.87: xor r10, r10
cmp r8, r9
setl r10b; bytecode.cpp:679
.88: push r10; bytecode.cpp:691
.89: pop r8; bytecode.cpp:890
.8a: test r8, r8
jz .8H; bytecode.cpp:891
; binary =;binary +;load identifer digit
.8b: sub rsp, 8; bytecode.cpp:747
.8c: push rsp; bytecode.cpp:748
; push_address_of digit
.8d: push rbp; bytecode.cpp:299
.8e: add qword [rsp], -544; bytecode.cpp:300
; copy 8 bytes from digit into stack, reverse=false
.8f: pop r8; bytecode.cpp:381
.8g: pop r9; bytecode.cpp:382
.8h: mov r10, qword [r8]; bytecode.cpp:384
.8i: mov qword [r9], r10; bytecode.cpp:385
; literal '0'
.8j: push 48; bytecode.cpp:861
.8k: pop r8; bytecode.cpp:617
.8l: add qword [rsp], r8; bytecode.cpp:619
; push_address_of buffer[dst_index];push_address_of buffer
.8m: push rbp; bytecode.cpp:299
.8n: add qword [rsp], -528; bytecode.cpp:300
; load identifer dst_index
.8o: sub rsp, 8; bytecode.cpp:747
.8p: push rsp; bytecode.cpp:748
; push_address_of dst_index
.8q: push rbp; bytecode.cpp:299
.8r: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.8s: pop r8; bytecode.cpp:381
.8t: pop r9; bytecode.cpp:382
.8u: mov r10, qword [r8]; bytecode.cpp:384
.8v: mov qword [r9], r10; bytecode.cpp:385
.8w: pop r8; bytecode.cpp:350
.8x: shl r8, 3; bytecode.cpp:157
.8y: add qword [rsp], r8; bytecode.cpp:356
.8z: push rsp; bytecode.cpp:641
.8A: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from digit + '0' into buffer[dst_index], reverse=false
.8B: pop r8; bytecode.cpp:381
.8C: pop r9; bytecode.cpp:382
.8D: mov r10, qword [r8]; bytecode.cpp:384
.8E: mov qword [r9], r10; bytecode.cpp:385
.8F: add rsp, 8; bytecode.cpp:648
.8G: jmp .9f; bytecode.cpp:903
; binary =;binary +;binary -;load identifer digit
.8H: sub rsp, 8; bytecode.cpp:747
.8I: push rsp; bytecode.cpp:748
; push_address_of digit
.8J: push rbp; bytecode.cpp:299
.8K: add qword [rsp], -544; bytecode.cpp:300
; copy 8 bytes from digit into stack, reverse=false
.8L: pop r8; bytecode.cpp:381
.8M: pop r9; bytecode.cpp:382
.8N: mov r10, qword [r8]; bytecode.cpp:384
.8O: mov qword [r9], r10; bytecode.cpp:385
; literal 10
.8P: push 10; bytecode.cpp:877
.8Q: pop r8; bytecode.cpp:617
.8R: sub qword [rsp], r8; bytecode.cpp:620
; literal 'a'
.8S: push 97; bytecode.cpp:861
.8T: pop r8; bytecode.cpp:617
.8U: add qword [rsp], r8; bytecode.cpp:619
; push_address_of buffer[dst_index];push_address_of buffer
.8V: push rbp; bytecode.cpp:299
.8W: add qword [rsp], -528; bytecode.cpp:300
; load identifer dst_index
.8X: sub rsp, 8; bytecode.cpp:747
.8Y: push rsp; bytecode.cpp:748
; push_address_of dst_index
.8Z: push rbp; bytecode.cpp:299
.90: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.91: pop r8; bytecode.cpp:381
.92: pop r9; bytecode.cpp:382
.93: mov r10, qword [r8]; bytecode.cpp:384
.94: mov qword [r9], r10; bytecode.cpp:385
.95: pop r8; bytecode.cpp:350
.96: shl r8, 3; bytecode.cpp:157
.97: add qword [rsp], r8; bytecode.cpp:356
.98: push rsp; bytecode.cpp:641
.99: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from digit - 10 + 'a' into buffer[dst_index], reverse=false
.9a: pop r8; bytecode.cpp:381
.9b: pop r9; bytecode.cpp:382
.9c: mov r10, qword [r8]; bytecode.cpp:384
.9d: mov qword [r9], r10; bytecode.cpp:385
.9e: add rsp, 8; bytecode.cpp:648
; binary >>=;literal 4
.9f: push 4; bytecode.cpp:877
; push_address_of i
.9g: push rbp; bytecode.cpp:299
.9h: add qword [rsp], -16; bytecode.cpp:300
.9i: pop r8; bytecode.cpp:708
.9j: pop r9; bytecode.cpp:709
.9k: mov cl, r9b
shr qword[r8], cl; bytecode.cpp:720
.9l: add rsp, 8; bytecode.cpp:942
.9m: jmp .7q; bytecode.cpp:944
; binary !=;load identifer dst_index
.9n: sub rsp, 8; bytecode.cpp:747
.9o: push rsp; bytecode.cpp:748
; push_address_of dst_index
.9p: push rbp; bytecode.cpp:299
.9q: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.9r: pop r8; bytecode.cpp:381
.9s: pop r9; bytecode.cpp:382
.9t: mov r10, qword [r8]; bytecode.cpp:384
.9u: mov qword [r9], r10; bytecode.cpp:385
; literal 64
.9v: push 64; bytecode.cpp:877
.9w: pop r9; bytecode.cpp:672
.9x: pop r8; bytecode.cpp:673
.9y: xor r10, r10
cmp r8, r9
setne r10b; bytecode.cpp:679
.9z: push r10; bytecode.cpp:691
.9A: pop r8; bytecode.cpp:929
.9B: test r8, r8
jz .a6; bytecode.cpp:930
; call print_char;cast from 's64' to 'u8';subscript;load identifer dst_index
.9C: sub rsp, 8; bytecode.cpp:747
.9D: push rsp; bytecode.cpp:748
; push_address_of dst_index
.9E: push rbp; bytecode.cpp:299
.9F: add qword [rsp], -536; bytecode.cpp:300
; copy 8 bytes from dst_index into stack, reverse=false
.9G: pop r8; bytecode.cpp:381
.9H: pop r9; bytecode.cpp:382
.9I: mov r10, qword [r8]; bytecode.cpp:384
.9J: mov qword [r9], r10; bytecode.cpp:385
; push_address_of buffer
.9K: push rbp; bytecode.cpp:299
.9L: add qword [rsp], -528; bytecode.cpp:300
.9M: pop r8; bytecode.cpp:1033
.9N: pop r9; bytecode.cpp:1034
.9O: shl r9, 3; bytecode.cpp:157
.9P: add r8, r9; bytecode.cpp:1041
.9Q: sub rsp, 8; bytecode.cpp:1045
.9R: push rsp; bytecode.cpp:1047
.9S: push r8; bytecode.cpp:1048
; copy 8 bytes from buffer[dst_index] into stack, reverse=false
.9T: pop r8; bytecode.cpp:381
.9U: pop r9; bytecode.cpp:382
.9V: mov r10, qword [r8]; bytecode.cpp:384
.9W: mov qword [r9], r10; bytecode.cpp:385
.9X: call .Z; bytecode.cpp:774
.9Y: add rsp, 8; bytecode.cpp:781
; binary +=;literal 1
.9Z: push 1; bytecode.cpp:877
; push_address_of dst_index
.a0: push rbp; bytecode.cpp:299
.a1: add qword [rsp], -536; bytecode.cpp:300
.a2: pop r8; bytecode.cpp:708
.a3: pop r9; bytecode.cpp:709
.a4: add qword [r8], r9; bytecode.cpp:712
.a5: jmp .9n; bytecode.cpp:944
.a6: mov rsp, rbp; bytecode.cpp:1205
.a7: pop rbp; bytecode.cpp:1206
.a8: ret; bytecode.cpp:1229
.a9: push rbp; bytecode.cpp:1142
.aa: mov rbp, rsp; bytecode.cpp:1143
; lambda merge;return;cast from 'u8' to 'u16';binary |;binary <<;load identifer a
.ab: sub rsp, 8; bytecode.cpp:747
.ac: push rsp; bytecode.cpp:748
; push_address_of a
.ad: push rbp; bytecode.cpp:299
.ae: add qword [rsp], 16; bytecode.cpp:300
; copy 1 bytes from a into stack, reverse=false
.af: pop r8; bytecode.cpp:381
.ag: pop r9; bytecode.cpp:382
.ah: mov r10b, byte [r8]; bytecode.cpp:393
.ai: mov byte [r9], r10b; bytecode.cpp:394
; literal 8
.aj: push 8; bytecode.cpp:867
.ak: pop r8; bytecode.cpp:617
.al: mov cl, r8b
shl qword[rsp], cl; bytecode.cpp:628
; load identifer b
.am: sub rsp, 8; bytecode.cpp:747
.an: push rsp; bytecode.cpp:748
; push_address_of b
.ao: push rbp; bytecode.cpp:299
.ap: add qword [rsp], 24; bytecode.cpp:300
; copy 1 bytes from b into stack, reverse=false
.aq: pop r8; bytecode.cpp:381
.ar: pop r9; bytecode.cpp:382
.as: mov r10b, byte [r8]; bytecode.cpp:393
.at: mov byte [r9], r10b; bytecode.cpp:394
.au: pop r8; bytecode.cpp:617
.av: or qword [rsp], r8; bytecode.cpp:624
.aw: and qword [rsp], 255; bytecode.cpp:1110
.ax: push rbp; bytecode.cpp:506
.ay: add qword [rsp], 32; bytecode.cpp:507
.az: push rsp; bytecode.cpp:510
.aA: add qword [rsp], 8; bytecode.cpp:511
; copy 2 bytes from expression into parameter, reverse=false
.aB: pop r8; bytecode.cpp:381
.aC: pop r9; bytecode.cpp:382
.aD: mov r10w, word [r8]; bytecode.cpp:390
.aE: mov word [r9], r10w; bytecode.cpp:391
.aF: jmp .aG; bytecode.cpp:517
.aG: mov rsp, rbp; bytecode.cpp:1205
.aH: pop rbp; bytecode.cpp:1206
.aI: ret; bytecode.cpp:1229
.aJ: push rbp; bytecode.cpp:1142
.aK: mov rbp, rsp; bytecode.cpp:1143
; lambda main;definition a;literal 0
.aL: push 0; bytecode.cpp:877
; definition b;literal 1
.aM: push 1; bytecode.cpp:877
; binary <;load identifer a
.aN: sub rsp, 8; bytecode.cpp:747
.aO: push rsp; bytecode.cpp:748
; push_address_of a
.aP: push rbp; bytecode.cpp:299
.aQ: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from a into stack, reverse=false
.aR: pop r8; bytecode.cpp:381
.aS: pop r9; bytecode.cpp:382
.aT: mov r10, qword [r8]; bytecode.cpp:384
.aU: mov qword [r9], r10; bytecode.cpp:385
; literal 100
.aV: push 100; bytecode.cpp:877
.aW: pop r9; bytecode.cpp:672
.aX: pop r8; bytecode.cpp:673
.aY: xor r10, r10
cmp r8, r9
setl r10b; bytecode.cpp:679
.aZ: push r10; bytecode.cpp:691
.b0: pop r8; bytecode.cpp:929
.b1: test r8, r8
jz .c8; bytecode.cpp:930
; call print_int;load identifer a
.b2: sub rsp, 8; bytecode.cpp:747
.b3: push rsp; bytecode.cpp:748
; push_address_of a
.b4: push rbp; bytecode.cpp:299
.b5: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from a into stack, reverse=false
.b6: pop r8; bytecode.cpp:381
.b7: pop r9; bytecode.cpp:382
.b8: mov r10, qword [r8]; bytecode.cpp:384
.b9: mov qword [r9], r10; bytecode.cpp:385
.ba: call .1x; bytecode.cpp:774
.bb: add rsp, 8; bytecode.cpp:781
; call print_string;literal "\n"
.bc: push 1; bytecode.cpp:854
.bd: mov rax, constants + 736
push rax; bytecode.cpp:857
.be: call .0; bytecode.cpp:774
.bf: add rsp, 16; bytecode.cpp:781
; definition c;binary +;load identifer a
.bg: sub rsp, 8; bytecode.cpp:747
.bh: push rsp; bytecode.cpp:748
; push_address_of a
.bi: push rbp; bytecode.cpp:299
.bj: add qword [rsp], -8; bytecode.cpp:300
; copy 8 bytes from a into stack, reverse=false
.bk: pop r8; bytecode.cpp:381
.bl: pop r9; bytecode.cpp:382
.bm: mov r10, qword [r8]; bytecode.cpp:384
.bn: mov qword [r9], r10; bytecode.cpp:385
; load identifer b
.bo: sub rsp, 8; bytecode.cpp:747
.bp: push rsp; bytecode.cpp:748
; push_address_of b
.bq: push rbp; bytecode.cpp:299
.br: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from b into stack, reverse=false
.bs: pop r8; bytecode.cpp:381
.bt: pop r9; bytecode.cpp:382
.bu: mov r10, qword [r8]; bytecode.cpp:384
.bv: mov qword [r9], r10; bytecode.cpp:385
.bw: pop r8; bytecode.cpp:617
.bx: add qword [rsp], r8; bytecode.cpp:619
; binary =;load identifer b
.by: sub rsp, 8; bytecode.cpp:747
.bz: push rsp; bytecode.cpp:748
; push_address_of b
.bA: push rbp; bytecode.cpp:299
.bB: add qword [rsp], -16; bytecode.cpp:300
; copy 8 bytes from b into stack, reverse=false
.bC: pop r8; bytecode.cpp:381
.bD: pop r9; bytecode.cpp:382
.bE: mov r10, qword [r8]; bytecode.cpp:384
.bF: mov qword [r9], r10; bytecode.cpp:385
; push_address_of a
.bG: push rbp; bytecode.cpp:299
.bH: add qword [rsp], -8; bytecode.cpp:300
.bI: push rsp; bytecode.cpp:641
.bJ: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from b into a, reverse=false
.bK: pop r8; bytecode.cpp:381
.bL: pop r9; bytecode.cpp:382
.bM: mov r10, qword [r8]; bytecode.cpp:384
.bN: mov qword [r9], r10; bytecode.cpp:385
.bO: add rsp, 8; bytecode.cpp:648
; binary =;load identifer c
.bP: sub rsp, 8; bytecode.cpp:747
.bQ: push rsp; bytecode.cpp:748
; push_address_of c
.bR: push rbp; bytecode.cpp:299
.bS: add qword [rsp], -24; bytecode.cpp:300
; copy 8 bytes from c into stack, reverse=false
.bT: pop r8; bytecode.cpp:381
.bU: pop r9; bytecode.cpp:382
.bV: mov r10, qword [r8]; bytecode.cpp:384
.bW: mov qword [r9], r10; bytecode.cpp:385
; push_address_of b
.bX: push rbp; bytecode.cpp:299
.bY: add qword [rsp], -16; bytecode.cpp:300
.bZ: push rsp; bytecode.cpp:641
.c0: add qword [rsp], 8; bytecode.cpp:642
; copy 8 bytes from c into b, reverse=false
.c1: pop r8; bytecode.cpp:381
.c2: pop r9; bytecode.cpp:382
.c3: mov r10, qword [r8]; bytecode.cpp:384
.c4: mov qword [r9], r10; bytecode.cpp:385
.c5: add rsp, 8; bytecode.cpp:648
.c6: add rsp, 8; bytecode.cpp:942
.c7: jmp .aN; bytecode.cpp:944
.c8: mov rsp, rbp; bytecode.cpp:1205
.c9: pop rbp; bytecode.cpp:1206
.ca: ret; bytecode.cpp:1229
section '.rodata' data readable
constants db 246,255,255,255,255,255,255,255,245,255,255,255,255,255,255,255,244,255,255,255,255,255,255,255,0,0,0,128,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,16,0,0,0,0,4,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,3,0,0,0,0,0,0,0,5,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,16,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,128,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,32,0,0,0,0,0,0,64,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,192,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,64,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,136,128,0,0,0,0,0,0,4,0,0,0,0,0,0,0,8,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,207,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,138,127,0,0,0,0,0,0,0,127,0,0,0,0,0,0,3,127,0,0,0,0,0,0,137,127,0,0,0,0,0,0,139,127,0,0,0,0,0,0,1,127,0,0,0,0,0,0,129,127,0,0,0,0,0,0,136,127,0,0,0,0,0,0,128,127,0,0,0,0,0,0,134,127,0,0,0,0,0,0,131,127,0,0,0,0,0,0,133,127,0,0,0,0,0,0,130,127,0,0,0,0,0,0,132,127,0,0,0,0,0,0,4,127,0,0,0,0,0,0,2,127,0,0,0,0,0,0,10
section '.idata' import data readable writeable
library user32,'user32.dll',\
	kernel32,'kernel32.dll'
import user32,\
	RegisterClassExA,'RegisterClassExA',\
	DefWindowProcA,'DefWindowProcA',\
	CreateWindowExA,'CreateWindowExA',\
	PeekMessageA,'PeekMessageA',\
	TranslateMessage,'TranslateMessage',\
	DispatchMessageA,'DispatchMessageA',\
	PostQuitMessage,'PostQuitMessage',\
	LoadCursorA,'LoadCursorA'
import kernel32,\
	GetStdHandle,'GetStdHandle',\
	WriteConsoleA,'WriteConsoleA',\
	CreateFileA,'CreateFileA',\
	WriteFile,'WriteFile',\
	GetLastError,'GetLastError',\
	VirtualAlloc,'VirtualAlloc',\
	GetModuleHandleA,'GetModuleHandleA',\
	ExitProcess,'ExitProcess'
