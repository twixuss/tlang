
bits 64

section .bss
print_buffer: resb 64

section .rodata
string_literal_72:db"Hello World!"

section .text

global main

main:;----------------
push rbp
mov rbp, rsp
mov rax, qword string_literal_72
push rax
push qword 12
push qword[rbp - 16]
push qword[rbp - 24]
pop rax
mov rsp, rbp
pop rbp
ret
