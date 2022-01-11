format PE64 console
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
push 0
call .m
pop rcx
and rsp, -16
sub rsp, 16
call [ExitProcess]
ret
.0: push rbp; bytecode.cpp:1142
.1: mov rbp, rsp; bytecode.cpp:1143
; lambda foo;return;cast from 'u64' to 's64';load identifer val
.2: sub rsp, 8; bytecode.cpp:747
.3: push rsp; bytecode.cpp:748
; push_address_of val
.4: push rbp; bytecode.cpp:299
.5: add qword [rsp], 16; bytecode.cpp:300
; copy 8 bytes from val into stack, reverse=false
.6: pop r8; bytecode.cpp:381
.7: pop r9; bytecode.cpp:382
.8: mov r10, qword [r8]; bytecode.cpp:384
.9: mov qword [r9], r10; bytecode.cpp:385
.a: push rbp; bytecode.cpp:506
.b: add qword [rsp], 24; bytecode.cpp:507
.c: push rsp; bytecode.cpp:510
.d: add qword [rsp], 8; bytecode.cpp:511
; copy 8 bytes from expression into parameter, reverse=false
.e: pop r8; bytecode.cpp:381
.f: pop r9; bytecode.cpp:382
.g: mov r10, qword [r8]; bytecode.cpp:384
.h: mov qword [r9], r10; bytecode.cpp:385
.i: jmp .j; bytecode.cpp:517
.j: mov rsp, rbp; bytecode.cpp:1205
.k: pop rbp; bytecode.cpp:1206
.l: ret; bytecode.cpp:1229
.m: push rbp; bytecode.cpp:1142
.n: mov rbp, rsp; bytecode.cpp:1143
; lambda main;return;call foo
.o: sub rsp, 8; bytecode.cpp:762
; literal 
.p: push -1; bytecode.cpp:877
.q: call .0; bytecode.cpp:774
.r: add rsp, 8; bytecode.cpp:781
.s: push rbp; bytecode.cpp:506
.t: add qword [rsp], 16; bytecode.cpp:507
.u: push rsp; bytecode.cpp:510
.v: add qword [rsp], 8; bytecode.cpp:511
; copy 8 bytes from expression into parameter, reverse=false
.w: pop r8; bytecode.cpp:381
.x: pop r9; bytecode.cpp:382
.y: mov r10, qword [r8]; bytecode.cpp:384
.z: mov qword [r9], r10; bytecode.cpp:385
.A: jmp .B; bytecode.cpp:517
.B: mov rsp, rbp; bytecode.cpp:1205
.C: pop rbp; bytecode.cpp:1206
.D: ret; bytecode.cpp:1229
section '.idata' import data readable writeable
library kernel32,'kernel32.dll'
import kernel32,ExitProcess,'ExitProcess'
