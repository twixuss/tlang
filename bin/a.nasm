
bits 64

section .text

global main

main$nested$nested2:
	push rbp
	mov rbp, rsp
	push qword 123
	push qword 543
	pop rax
	add qword[rsp], rax
	push qword 111
	pop rax
	add qword[rsp], rax
	push qword[rbp - 8]
	pop rax
	mov rsp, rbp
	pop rbp
	ret
main$nested:
	push rbp
	mov rbp, rsp
	call main$nested$nested2
	push rax
	pop rax
	mov rsp, rbp
	pop rbp
	ret
main:
	push rbp
	mov rbp, rsp
	call main$nested
	push rax
	pop rax
	mov rsp, rbp
	pop rbp
	ret
