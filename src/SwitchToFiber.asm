global t_SwitchToFiber
; extern BasepFiberCookie
; extern GetImmersiveColorTypeFromName
segment .text

t_SwitchToFiber:
    ; mov         rax,qword [rcx+10h]
    ; xor         rax,qword [BasepFiberCookie]
    ; xor         rax,rcx
    ; cmp         qword [rcx+520h],rax
    ; jne         GetImmersiveColorTypeFromName+1A880h
    mov         rdx,qword gs:[30h]
    mov         rax,qword [rdx+20h]
    mov         r8,qword [rcx+20h]
    mov         qword [rdx+1478h],r8
    mov         qword [rdx+20h],rcx
    mov         r8,qword [rdx+10h]
    mov         qword [rax+18h],r8
    mov         r8d,dword [rdx+1748h]
    mov         dword [rax+518h],r8d
    mov         r8,qword [rdx+17C8h]
    mov         qword [rax+510h],r8
    mov         r8,qword [rdx+2C8h]
    mov         qword [rax+508h],r8
    lea         r8,[rax+30h]
    mov         qword [r8+90h],rbx
    mov         qword [r8+0A0h],rbp
    mov         qword [r8+0A8h],rsi
    mov         qword [r8+0B0h],rdi
    mov         qword [r8+0D8h],r12
    mov         qword [r8+0E0h],r13
    mov         qword [r8+0E8h],r14
    mov         qword [r8+0F0h],r15
    ;movaps      [r8+200h],xmm6
    ;movaps      [r8+210h],xmm7
    ;movaps      [r8+220h],xmm8
    ;movaps      [r8+230h],xmm9
    ;movaps      [r8+240h],xmm10
    ;movaps      [r8+250h],xmm11
    ;movaps      [r8+260h],xmm12
    ;movaps      [r8+270h],xmm13
    ;movaps      [r8+280h],xmm14
    ;movaps      [r8+290h],xmm15
    ;stmxcsr     dword [r8+34h]
    ;fnclex
    ;wait
    ;fnstcw      word [r8+100h]
    mov         r9,qword [rsp]
    mov         qword [r8+0F8h],r9
    mov         qword [r8+98h],rsp
    mov         r8,qword [rcx+10h]
    mov         qword [rdx+8],r8
    mov         r8,qword [rcx+18h]
    mov         qword [rdx+10h],r8
    mov         r8d,dword [rcx+518h]
    mov         dword [rdx+1748h],r8d
    mov         r8,qword [rcx+510h]
    mov         qword [rdx+17C8h],r8
    mov         r8,qword [rcx+508h]
    mov         qword [rdx+2C8h],r8
    lea         r8,[rcx+30h]
    mov         rbx,qword [r8+90h]
    mov         rbp,qword [r8+0A0h]
    mov         rsi,qword [r8+0A8h]
    mov         rdi,qword [r8+0B0h]
    mov         r12,qword [r8+0D8h]
    mov         r13,qword [r8+0E0h]
    mov         r14,qword [r8+0E8h]
    mov         r15,qword [r8+0F0h]
    ;movaps      xmm6, [r8+200h]
    ;movaps      xmm7, [r8+210h]
    ;movaps      xmm8, [r8+220h]
    ;movaps      xmm9, [r8+230h]
    ;movaps      xmm10, [r8+240h]
    ;movaps      xmm11, [r8+250h]
    ;movaps      xmm12, [r8+260h]
    ;movaps      xmm13, [r8+270h]
    ;movaps      xmm14, [r8+280h]
    ;movaps      xmm15, [r8+290h]
    ;ldmxcsr     dword [r8+34h]
    ;fldcw       word [r8+100h]
    mov         rsp,qword [r8+98h]
    ret
