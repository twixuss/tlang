
bits 64
extern GetStdHandle
extern GetLastError
extern WriteConsoleA
extern CreateFileA
extern WriteFile

section .bss
standard_output_handle: resb 8
global_string: resb 16

section .rodata
x: dq 0x2A
GENERIC_WRITE: dq 0x40000000
FILE_SHARE_DELETE: dq 0x4
FILE_SHARE_WRITE: dq 0x2
FILE_FLAG_WRITE_THROUGH: dq 0x80000000
CREATE_NEW: dq 0x1
FILE_ATTRIBUTE_ENCRYPTED: dq 0x4000
OPEN_EXISTING: dq 0x3
TRUNCATE_EXISTING: dq 0x5
FILE_FLAG_BACKUP_SEMANTICS: dq 0x2000000
FILE_FLAG_POSIX_SEMANTICS: dq 0x1000000
GENERIC_READ: dq 0x80000000
FILE_SHARE_READ: dq 0x1
FILE_FLAG_OPEN_NO_RECALL: dq 0x100000
FILE_ATTRIBUTE_TEMPORARY: dq 0x100
FILE_ATTRIBUTE_NORMAL: dq 0x80
FILE_FLAG_OVERLAPPED: dq 0x40000000
FILE_ATTRIBUTE_READONLY: dq 0x1
FILE_ATTRIBUTE_HIDDEN: dq 0x2
FILE_FLAG_SEQUENTIAL_SCAN: dq 0x8000000
FILE_FLAG_RANDOM_ACCESS: dq 0x10000000
STD_INPUT_HANDLE: dq 0xFFFFFFFFFFFFFFF6
STD_OUTPUT_HANDLE: dq 0xFFFFFFFFFFFFFFF5
STD_ERROR_HANDLE: dq 0xFFFFFFFFFFFFFFF4
FILE_FLAG_SESSION_AWARE: dq 0x800000
FILE_FLAG_DELETE_ON_CLOSE: dq 0x4000000
null: dq 0x0
string_literal_118:db 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 10, 
string_literal_140:db 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 10, 
string_literal_158:db 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 10, 
string_literal_187:db 116, 114, 117, 101, 10, 
string_literal_199:db 48, 32, 105, 115, 32, 48, 10, 
string_literal_202:db 48, 32, 105, 115, 32, 110, 111, 116, 32, 48, 10, 
string_literal_210:db 49, 32, 105, 115, 32, 108, 101, 115, 115, 32, 116, 104, 97, 110, 32, 50, 10, 
string_literal_215:db 116, 114, 117, 101, 10, 
string_literal_218:db 102, 97, 108, 115, 101, 10, 
string_literal_223:db 116, 114, 117, 101, 10, 
string_literal_226:db 102, 97, 108, 115, 101, 10, 
string_literal_234:db 51, 32, 105, 115, 32, 62, 61, 32, 48, 10, 
string_literal_237:db 51, 32, 105, 115, 32, 110, 111, 116, 32, 62, 61, 32, 48, 10, 
string_literal_254:db 42, 
string_literal_263:db 10, 
string_literal_274:db 109, 121, 32, 102, 105, 108, 101, 46, 116, 120, 116, 0, 
string_literal_292:db 72, 101, 108, 108, 111, 32, 87, 111, 114, 108, 100, 33, 
string_literal_306:db 65, 32, 103, 108, 111, 98, 97, 108, 32, 115, 116, 114, 105, 110, 103, 
string_literal_316:db 97, 32, 108, 111, 99, 97, 108, 32, 115, 116, 114, 105, 110, 103, 
string_literal_334:db 65, 83, 116, 114, 117, 99, 116, 
string_literal_362:db 65, 32, 83, 84, 82, 73, 78, 71, 
GENERIC_EXECUTE: dq 0x20000000
FILE_FLAG_OPEN_REPARSE_POINT: dq 0x200000
GENERIC_ALL: dq 0x10000000
CREATE_ALWAYS: dq 0x2
OPEN_ALWAYS: dq 0x4
FILE_ATTRIBUTE_ARCHIVE: dq 0x20
FILE_ATTRIBUTE_OFFLINE: dq 0x1000
FILE_ATTRIBUTE_SYSTEM: dq 0x4
FILE_FLAG_NO_BUFFERING: dq 0x20000000

section .text

global main

l376:;---------------- print
push rbp
mov rbp, rsp
; - call WriteConsoleA
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
; - binary .
; - load identifier str
push qword[rbp + 24]
push qword[rbp + 16]
add rsp, 16
push qword[rsp + -8]
; - binary .
; - load identifier str
push qword[rbp + 24]
push qword[rbp + 16]
add rsp, 16
push qword[rsp + -16]
; - load identifier standard_output_handle
mov rax, standard_output_handle
push qword [rax + 0]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call WriteConsoleA
add rsp, 40
pop rsp

push rax
add rsp,8
mov rsp, rbp
pop rbp
ret
main:
l100:;---------------- main
push rbp
mov rbp, rsp
; - binary =
; - call GetStdHandle
mov rax, rsp
and rsp, ~15
push rax
sub rsp, 8
; - load identifier STD_OUTPUT_HANDLE
mov rax, STD_OUTPUT_HANDLE
push qword [rax + 0]
pop rcx
sub rsp, 32
call GetStdHandle
add rsp, 32
add rsp, 8
pop rsp

push rax
; - load address of standard_output_handle
mov rbx, standard_output_handle
push rbx
pop rax
pop rbx
mov qword[rax + 0], rbx
; - definition str
push qword 13
mov rax, qword string_literal_118
push rax
; - call WriteConsoleA
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
; - binary .
; - load identifier str
push qword[rbp + -8]
push qword[rbp + -16]
add rsp, 16
push qword[rsp + -8]
; - binary .
; - load identifier str
push qword[rbp + -8]
push qword[rbp + -16]
add rsp, 16
push qword[rsp + -16]
; - load identifier standard_output_handle
mov rax, standard_output_handle
push qword [rax + 0]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call WriteConsoleA
add rsp, 40
pop rsp

push rax
add rsp,8
; - call WriteConsoleA
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
push dword 13
; - binary .
push qword 13
mov rax, qword string_literal_140
push rax
add rsp, 16
push qword[rsp + -16]
; - load identifier standard_output_handle
mov rax, standard_output_handle
push qword [rax + 0]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call WriteConsoleA
add rsp, 40
pop rsp

push rax
add rsp,8
; - call print
push qword 13
mov rax, qword string_literal_158
push rax
call l376
add rsp, 16
push rax
add rsp,8
; - call print
; - load identifier str
push qword[rbp + -8]
push qword[rbp + -16]
call l376
add rsp, 16
push rax
add rsp,8
; - if
push byte 0
pop rax
test rax, rax
jz .f178
; - definition x
push 0
; - call print
push qword 5
mov rax, qword string_literal_187
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 8
jmp .e178
.f178:
add rsp, 0
.e178:
; - if
push byte 0
pop rax
test rax, rax
jz .f191
; - call print
push qword 7
mov rax, qword string_literal_199
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
jmp .e191
.f191:
; - call print
push qword 11
mov rax, qword string_literal_202
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
.e191:
; - if
push byte 0
pop rax
test rax, rax
jz .f205
; - call print
push qword 17
mov rax, qword string_literal_210
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
jmp .e205
.f205:
add rsp, 0
.e205:
; - if
push byte 0
pop rax
test rax, rax
jz .f213
; - call print
push qword 5
mov rax, qword string_literal_215
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
jmp .e213
.f213:
; - call print
push qword 6
mov rax, qword string_literal_218
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
.e213:
; - if
push byte 0
pop rax
test rax, rax
jz .f221
; - call print
push qword 5
mov rax, qword string_literal_223
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
jmp .e221
.f221:
; - call print
push qword 6
mov rax, qword string_literal_226
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
.e221:
; - if
push byte 0
pop rax
test rax, rax
jz .f229
; - call print
push qword 10
mov rax, qword string_literal_234
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
jmp .e229
.f229:
; - call print
push qword 14
mov rax, qword string_literal_237
push rax
call l376
add rsp, 16
push rax
add rsp,8
add rsp, 0
.e229:
; - definition i
push dword 10
; - while
.n243:
; - binary >
; - load identifier i
push qword[rbp + -24]
push dword 0
pop rax
pop rbx
mov rcx, 0
mov rdx, 1
cmp rbx, rax
cmovg rcx, rdx
push rcx
pop rax
test rax, rax
jz .e243
; - definition j
; - load identifier i
push qword[rbp + -24]
; - while
.n250:
; - binary >
; - load identifier j
push qword[rbp + -32]
push dword 0
pop rax
pop rbx
mov rcx, 0
mov rdx, 1
cmp rbx, rax
cmovg rcx, rdx
push rcx
pop rax
test rax, rax
jz .e250
; - call print
push qword 1
mov rax, qword string_literal_254
push rax
call l376
add rsp, 16
push rax
add rsp,8
; - binary =
; - binary -
; - load identifier j
push qword[rbp + -32]
push dword 1
pop rax
sub qword[rsp], rax
; - load address of j
lea rax, qword [rbp + -32]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
add rsp, 0
jmp .n250
.e250:
; - call print
push qword 1
mov rax, qword string_literal_263
push rax
call l376
add rsp, 16
push rax
add rsp,8
; - binary =
; - binary -
; - load identifier i
push qword[rbp + -24]
push dword 1
pop rax
sub qword[rsp], rax
; - load address of i
lea rax, qword [rbp + -24]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
add rsp, 8
jmp .n243
.e243:
; - definition STR
push qword 12
mov rax, qword string_literal_274
push rax
; - call print
; - load identifier STR
push qword[rbp + -32]
push qword[rbp + -40]
call l376
add rsp, 16
push rax
add rsp,8
; - definition file
; - call CreateFileA
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
push dword 0
; - load identifier CREATE_ALWAYS
mov rax, CREATE_ALWAYS
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
push dword 0
; - load identifier GENERIC_WRITE
mov rax, GENERIC_WRITE
push qword [rax + 0]
; - binary .
; - load identifier STR
push qword[rbp + -32]
push qword[rbp + -40]
add rsp, 16
push qword[rsp + -16]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call CreateFileA
add rsp, 56
pop rsp

push rax
; - definition content
push qword 12
mov rax, qword string_literal_292
push rax
; - call WriteFile
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
; - binary .
; - load identifier content
push qword[rbp + -56]
push qword[rbp + -64]
add rsp, 16
push qword[rsp + -8]
; - binary .
; - load identifier content
push qword[rbp + -56]
push qword[rbp + -64]
add rsp, 16
push qword[rsp + -16]
; - load identifier file
push qword[rbp + -48]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call WriteFile
add rsp, 40
pop rsp

push rax
add rsp,8
; - binary =
push qword 15
mov rax, qword string_literal_306
push rax
; - load address of global_string
mov rbx, global_string
push rbx
pop rax
pop rbx
mov qword[rax + 8], rbx
pop rbx
mov qword[rax + 0], rbx
; - call print
; - load identifier global_string
mov rax, global_string
push qword [rax + 0]
push qword [rax + 8]
call l376
add rsp, 16
push rax
add rsp,8
; - definition local_string
push 0
push 0
; - binary =
push qword 14
mov rax, qword string_literal_316
push rax
; - load address of local_string
lea rax, qword [rbp + -72]
push rax
pop rax
pop rbx
mov qword[rax + 8], rbx
pop rbx
mov qword[rax + 0], rbx
; - call print
; - load identifier local_string
push qword[rbp + -72]
push qword[rbp + -80]
call l376
add rsp, 16
push rax
add rsp,8
; - definition a_struct
push 0
push 0
; - binary =
push dword 7
; - load address of a_struct.count
lea rax, qword [rbp + -88]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
; - binary =
; - binary .
push qword 7
mov rax, qword string_literal_334
push rax
add rsp, 16
push qword[rsp + -16]
; - load address of a_struct.data
lea rax, qword [rbp + -96]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
; - call WriteConsoleA
mov rax, rsp
and rsp, ~15
push rax
; - load identifier null
mov rax, null
push qword [rax + 0]
; - load identifier null
mov rax, null
push qword [rax + 0]
; - binary .
; - load identifier a_struct
push qword[rbp + -88]
push qword[rbp + -96]
add rsp, 16
push qword[rsp + -8]
; - binary .
; - load identifier a_struct
push qword[rbp + -88]
push qword[rbp + -96]
add rsp, 16
push qword[rsp + -16]
; - load identifier standard_output_handle
mov rax, standard_output_handle
push qword [rax + 0]
pop rcx
pop rdx
pop r8
pop r9
sub rsp, 32
call WriteConsoleA
add rsp, 40
pop rsp

push rax
add rsp,8
; - definition a_string
push 0
push 0
; - binary =
push dword 7
; - load address of a_string.count
lea rax, qword [rbp + -104]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
; - binary =
; - binary .
push qword 8
mov rax, qword string_literal_362
push rax
add rsp, 16
push qword[rsp + -16]
; - load address of a_string.data
lea rax, qword [rbp + -112]
push rax
pop rax
pop rbx
mov qword[rax + 0], rbx
; - call print
; - load identifier a_string
push qword[rbp + -104]
push qword[rbp + -112]
call l376
add rsp, 16
push rax
add rsp,8
; - return
; - load identifier x
push qword[rbp + -24]
pop rax
mov rsp, rbp
pop rbp
ret
