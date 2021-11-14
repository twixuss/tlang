
bits 64
extern GetStdHandle
extern WriteConsoleA

section .bss

section .rodata
x: dq 42
STD_INPUT_HANDLE: dq -10
STD_OUTPUT_HANDLE: dq -11
STD_ERROR_HANDLE: dq -12
null: dq 0
string_literal_117:db"Hello World!"
string_literal_130:db"Hello World!"
string_literal_138:db"Hello World!"

section .text

global main

main:;----------------
push rbp
mov rbp, rsp
; - definition handle
; - call GetStdHandle
; - load identifier STD_OUTPUT_HANDLE
mov rax, STD_OUTPUT_HANDLE
push qword [rax]
pop rcx
call GetStdHandle
push rax
; - definition str
mov rax, qword string_literal_117
push rax
push qword 12
; - call WriteConsoleA
; - load identifier null
mov rax, null
push qword [rax]
; - load identifier null
mov rax, null
push qword [rax]
; - binary member_access
; - load identifier str
push qword[rbp - 16]
push qword[rbp - 24]
add rsp, 16
push qword[rsp - 16]
; - binary member_access
; - load identifier str
push qword[rbp - 16]
push qword[rbp - 24]
add rsp, 16
push qword[rsp - 8]
; - load identifier handle
push qword[rbp - 8]
pop rcx
pop rdx
pop r8
pop r9
call WriteConsoleA
add rsp, 8
push rax
add rsp,8
; - call WriteConsoleA
; - load identifier null
mov rax, null
push qword [rax]
; - load identifier null
mov rax, null
push qword [rax]
push dword 12
; - binary member_access
mov rax, qword string_literal_130
push rax
push qword 12
add rsp, 16
push qword[rsp - 8]
; - load identifier handle
push qword[rbp - 8]
pop rcx
pop rdx
pop r8
pop r9
call WriteConsoleA
add rsp, 8
push rax
add rsp,8
; - call print
mov rax, qword string_literal_138
push rax
push qword 12
call print
add rsp, 16
push rax
add rsp,8
; - return
; - load identifier x
mov rax, x
push qword [rax]
pop rax
mov rsp, rbp
pop rbp
ret
print:;----------------
push rbp
mov rbp, rsp
; - definition handle
; - call GetStdHandle
; - load identifier STD_OUTPUT_HANDLE
mov rax, STD_OUTPUT_HANDLE
push qword [rax]
pop rcx
call GetStdHandle
push rax
; - call WriteConsoleA
; - load identifier null
mov rax, null
push qword [rax]
; - load identifier null
mov rax, null
push qword [rax]
; - binary member_access
; - load identifier str
push qword[rbp + 24]
push qword[rbp + 16]
add rsp, 16
push qword[rsp - 16]
; - binary member_access
; - load identifier str
push qword[rbp + 24]
push qword[rbp + 16]
add rsp, 16
push qword[rsp - 8]
; - load identifier handle
push qword[rbp - 8]
pop rcx
pop rdx
pop r8
pop r9
call WriteConsoleA
add rsp, 8
push rax
add rsp,8
mov rsp, rbp
pop rbp
ret
