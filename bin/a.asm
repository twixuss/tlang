format PE64 console
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
push 0
call .0
pop rcx
and rsp, -16
sub rsp, 16
call [ExitProcess]
ret
.0: push rbp; bytecode.cpp:1214
.1: mov rbp, rsp; bytecode.cpp:1215
; lambda main;definition a;literal ;float 41.9
.2: sub rsp, 8
mov dword [rsp], 858993459
mov dword [rsp+4], 1078260531; bytecode.cpp:913
; definition b;literal ;float 58.9
.3: sub rsp, 8
mov dword [rsp], 858993459
mov dword [rsp+4], 1078817587; bytecode.cpp:913
; return;cast from 'f64' to 's64';binary -;binary *;load identifer a
.4: sub rsp, 8; bytecode.cpp:789
.5: push rsp; bytecode.cpp:790
; push_address_of a
.6: lea r8, [rbp + -8]; bytecode.cpp:123
.7: pop r9; bytecode.cpp:396
.8: mov r10, qword [r8]; bytecode.cpp:398
.9: mov qword [r9], r10; bytecode.cpp:399
; load identifer b
.a: sub rsp, 8; bytecode.cpp:789
.b: push rsp; bytecode.cpp:790
; push_address_of b
.c: lea r8, [rbp + -16]; bytecode.cpp:123
.d: pop r9; bytecode.cpp:396
.e: mov r10, qword [r8]; bytecode.cpp:398
.f: mov qword [r9], r10; bytecode.cpp:399
.g: pop r9; bytecode.cpp:637
.h: pop r8; bytecode.cpp:638
.i: movq xmm0, r8; bytecode.cpp:639
.j: movq xmm1, r9; bytecode.cpp:640
.k: mulsd xmm0, xmm1; bytecode.cpp:644
.l: movq r8, xmm0; bytecode.cpp:654
.m: push r8; bytecode.cpp:655
; binary /;load identifer a
.n: sub rsp, 8; bytecode.cpp:789
.o: push rsp; bytecode.cpp:790
; push_address_of a
.p: lea r8, [rbp + -8]; bytecode.cpp:123
.q: pop r9; bytecode.cpp:396
.r: mov r10, qword [r8]; bytecode.cpp:398
.s: mov qword [r9], r10; bytecode.cpp:399
; load identifer b
.t: sub rsp, 8; bytecode.cpp:789
.u: push rsp; bytecode.cpp:790
; push_address_of b
.v: lea r8, [rbp + -16]; bytecode.cpp:123
.w: pop r9; bytecode.cpp:396
.x: mov r10, qword [r8]; bytecode.cpp:398
.y: mov qword [r9], r10; bytecode.cpp:399
.z: pop r9; bytecode.cpp:637
.A: pop r8; bytecode.cpp:638
.B: movq xmm0, r8; bytecode.cpp:639
.C: movq xmm1, r9; bytecode.cpp:640
.D: divsd xmm0, xmm1; bytecode.cpp:645
.E: movq r8, xmm0; bytecode.cpp:654
.F: mov r9, r8; bytecode.cpp:113
.G: pop r8; bytecode.cpp:638
.H: movq xmm0, r8; bytecode.cpp:639
.I: movq xmm1, r9; bytecode.cpp:640
.J: subsd xmm0, xmm1; bytecode.cpp:643
.K: movq r8, xmm0; bytecode.cpp:654
.L: push r8; bytecode.cpp:655
.M: cvtsd2si rax, [rsp]
mov [rsp], rax; bytecode.cpp:1195
.N: push rbp; bytecode.cpp:522
.O: add qword [rsp], 16; bytecode.cpp:523
.P: lea r8, [rsp + 8]; bytecode.cpp:123
.Q: pop r9; bytecode.cpp:396
.R: mov r10, qword [r8]; bytecode.cpp:398
.S: mov qword [r9], r10; bytecode.cpp:399
.T: jmp .U; bytecode.cpp:533
.U: mov rsp, rbp; bytecode.cpp:1246
.V: pop rbp; bytecode.cpp:1247
.W: ret; bytecode.cpp:1270
section '.idata' import data readable writeable
library kernel32,'kernel32.dll'
import kernel32,ExitProcess,'ExitProcess'
