bits 64
section .text
global main
main:jmp .0
.0: push        rbp           
.1: mov         rbp, rsp        
.2: push        1           
.3: sub         rsp, 8        
.4: push        rsp           
.5: push        rbp           
.6: mov         rax, rbp        
.7: add         rax, -8        
.8: pop         rbx           
.9: mov         rcx, qword [rax]
.a: mov         qword [rbx], rcx
.b: mov         rax, 3        
.c: add         qword [rsp], rax
.d: push        rbp           
.e: add         qword [rsp], -8
.f: push        rsp           
.g: mov         rax, rsp        
.h: add         rax, 8        
.i: pop         rbx           
.j: mov         rcx, qword [rax]
.k: mov         qword [rbx], rcx
.l: add         rsp, 8        
.m: sub         rsp, 8        
.n: push        rsp           
.o: push        rbp           
.p: mov         rax, rbp        
.q: add         rax, -8        
.r: pop         rbx           
.s: mov         rcx, qword [rax]
.t: mov         qword [rbx], rcx
.u: pop         rax           
.v: mov         rsp, rbp        
.w: pop         rbp           
.x: ret                     
