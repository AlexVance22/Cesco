    global strlen
    global strcmp
    global strncmp
    
    section .text

strlen:
    push    rbp
    mov     rbp, rsp
    xor     rax, rax
.strlen_loop:
    cmp     BYTE [rcx+rax], 0
    je      .strlen_end
    inc     rax
    jmp     .strlen_loop
.strlen_end:
    leave
    ret

strcmp:
    push    rbp
    mov     rbp, rsp
    xor     rax, rax
.strcmp_loop:
    movzx   r8, BYTE [rcx+rax]
    movzx   r9, BYTE [rdx+rax]
    cmp     r8, r9
    jne     .strcmp_neq
    cmp     r8, 0
    je      .strcmp_eq
    inc     rax
    jmp     .strcmp_loop
.strcmp_eq:
    xor     rax, rax
    leave
    ret
.strcmp_neq:
    mov     rax, 1
    leave
    ret

strncmp:
    push    rbp
    mov     rbp, rsp
    mov     rax, r8
.strncmp_loop:
    movzx   r8, BYTE [rcx+rax]
    movzx   r9, BYTE [rdx+rax]
    cmp     r8, r9
    jne     .strncmp_neq
    cmp     r8, 0
    je      .strncmp_eq
    dec     rax
    cmp     rax, 0
    je      .strncmp_eq
    jmp     .strncmp_loop
.strncmp_eq:
    xor     rax, rax
    leave
    ret
.strncmp_neq:
    mov     rax, 1
    leave
    ret