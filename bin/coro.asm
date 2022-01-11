format PE64 console
entry main
include 'win64a.inc'
section '.text' code readable executable
main:
	mov rbx, 0; start coroutine
	call coro
	mov [res0], rax
	; rbx contains next address
	call coro
	mov [res1], rax

	and rsp, -16

	mov rcx, -11
	call [GetStdHandle]
	mov qword [handle], rax

	mov rcx, qword [handle]
	mov rdx, [res0]
	mov r8, 5
	mov r9, 0
	push 0
	sub rsp, 16
	call [WriteConsoleA]
	add rsp, 16 + 8
		
	mov rcx, qword [handle]
	mov rdx, [res1]
	mov r8, 5
	mov r9, 0
	push 0
	sub rsp, 16
	call [WriteConsoleA]
	add rsp, 16 + 8

	mov rcx, 0
	call [ExitProcess]

coro:
	push rbp
	mov rbp, rsp
	cmp rbx, 0
	je .first
	jmp rbx
.first:	

	; first yield
	mov rax, hello
	mov rbx, .after0
	jmp .ret
.after0:
	
	; second yield
	mov rax, world
	mov rbx, .after1
	jmp .ret
.after1:
	
.ret:
	mov rsp, rbp
	pop rbp
	ret

SECTION '.data' data readable writeable
hello db 'Hello'
world db 'World'
handle dq 0
xxx db '[XXX]'
res0 dq xxx
res1 dq xxx

section '.idata' import data readable writeable
library kernel32,'kernel32.dll'
import kernel32,\
	ExitProcess,'ExitProcess',\
	WriteConsoleA,'WriteConsoleA',\
	GetStdHandle,'GetStdHandle'
