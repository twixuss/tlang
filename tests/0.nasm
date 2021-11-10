
bits 64

section .bss

section .rodata
c: dq 12

section .text

global main

square:;----------------
push rbp
mov rbp, rsp
push qword[rbp + 16]
push qword[rbp + 16]
pop rax
pop rbx
imul rax, rbx
push rax
pop rax
mov rsp, rbp
pop rbp
ret
main:;----------------
push rbp
mov rbp, rsp
push qword 9
push qword 6
push qword 9223372036854775807
push qword[rbp - 16]
mov rax, c
push qword [rax]
pop rax
pop rbx
imul rax, rbx
push rax
call square
add rsp, 8
push rax
pop rax
sub qword[rsp], rax
pop rax
mov rsp, rbp
pop rbp
ret
