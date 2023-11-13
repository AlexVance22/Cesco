    global init_stdio
    global puts
    global dump
    global to_str
    global fopen
    global fwrite
    global fputs
    global fclose
    global exit

    global stdout
    global stdin
    global stderr

    section .text
    extern GetStdHandle
    extern CreateFileA
    extern CloseHandle
    extern WriteFile
    extern ExitProcess

    extern strlen
    extern new

init_stdio:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32
    mov     rcx, -10
    call    GetStdHandle
    mov     rcx, stdin
    mov     [rcx], rax
    mov     rcx, -11
    call    GetStdHandle
    mov     rcx, stdout
    mov     [rcx], rax
    mov     rcx, -12
    call    GetStdHandle
    mov     rcx, stderr
    mov     [rcx], rax
    add     rsp, 32
    leave
    ret

dump:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    mov QWORD [rbp-32], 0
    mov QWORD [rbp-24], 0
    mov QWORD [rbp-16], 0
    mov QWORD [rbp-8], 0
    xor r8, r8
.dump_loop:
    cmp rcx, 0
    jle .dump_end
    inc r8
    mov rax, rcx
    xor rdx, rdx
    mov r9, 10
    idiv r9
    add rdx, 48
    mov r9, 31
    sub r9, r8
    mov BYTE [rbp-32+r9], dl
    mov rcx, rax
    jmp .dump_loop
.dump_end:
    mov r9, 31
    sub r9, r8
    lea rcx, [rbp-32+r9]
    sub rsp, 32
    call puts
    add rsp, 32
    leave
    ret

to_str:
    push rbp
    mov rbp, rsp
    push rcx
    mov rcx, 32
    sub rsp, 32
    call new
    add rsp, 32
    pop rcx
    push rbx
    mov rbx, rax
    mov QWORD [rbx], 0
    mov QWORD [rbx+8], 0
    mov QWORD [rbx+16], 0
    mov QWORD [rbx+24], 0
    xor r8, r8
.to_str_loop:
    cmp rcx, 0
    jle .to_str_end
    inc r8
    mov rax, rcx
    xor rdx, rdx
    mov r9, 10
    idiv r9
    add rdx, 48
    mov r9, 31
    sub r9, r8
    mov BYTE [rbx+r9], dl
    mov rcx, rax
    jmp .to_str_loop
.to_str_end:
    mov r9, 31
    sub r9, r8
    lea rcx, [rbx+r9]
    pop rbx
    sub rsp, 32
    call puts
    add rsp, 32
    leave
    ret

fopen:
    push    rbp
    mov     rbp, rsp
    push    0                   ; Template file handlle
    push    0x00000080          ; FILE_ATTRIBUTE_NORMAL, Flags and attributes
    mov     r9, rdx
    cmp     BYTE [r9], 'r'
    je      .fopen_read
    cmp     BYTE [r9], 'a'
    je      .fopen_append
    cmp     BYTE [r9], 'w'
    je      .fopen_write
    jmp     .fopen_fail
.fopen_read:
    mov     rdx, 0x80000000     ; GENERIC_READ, Desired access
    push    3                   ; OPEN_EXISTING
    jmp     .fopen_l2
.fopen_append:
    mov     rdx, 0x40000000     ; GENERIC_WRITE, Desired access
    push    4                   ; OPEN_ALWAYS
    jmp     .fopen_l2
.fopen_write:
    mov     rdx, 0x40000000     ; GENERIC_WRITE, Desired access
    push    2                   ; CREATE_ALWAYS
    jmp     .fopen_l2
.fopen_l2:
    cmp     BYTE [r9+1], 0
    je      .fopen_end
    cmp     BYTE [r9+1], '+'
    mov     r8, 0xc0000000
    cmove   rdx, r8             ; GENERIC_READ | GENERIC_WRITE
.fopen_end:
    mov     r8, 0x00000001,     ; FILE_SHARE_READ, Share mode
    mov     r9, 0               ; Security attributes
    sub     rsp, 32
    call    CreateFileA
    add     rsp, 32
    leave
    ret
.fopen_fail:
    mov     rax, -1
    leave
    ret

fclose:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    call CloseHandle
    add rsp, 32
    leave
    ret

fwrite:     ; winapi: stream, string, len
    push    rbp
    mov     rbp, rsp
    sub     rsp, 4
    lea     r9, [rbp-4] ; bytes written
    push    0
    sub     rsp, 32
    call    WriteFile
    add     rsp, 32
    leave
    ret

fputs:
    push    rbp
    mov     rbp, rsp
    push    rcx
    push    rdx
    mov     rcx, rdx
    sub     rsp, 32
    call    strlen
    add     rsp, 32
    pop     rdx
    pop     rcx
    mov     r8, rax     ; bytes to write
    sub     rsp, 4
    lea     r9, [rbp-4] ; bytes written
    push    0
    sub     rsp, 32
    call    WriteFile
    add     rsp, 32
    leave
    ret

puts:
    push    rbp
    mov     rbp, rsp
    mov     rdx, rcx
    mov     rcx, stdout
    mov     rcx, [rcx]  ; stream
    sub     rsp, 32
    call    fputs
    add     rsp, 32
    leave
    ret

exit:
    push    rbp
    mov     rbp, rsp
    mov     rax, rcx
    call    ExitProcess

    section .bss
stdin:  resq    1
stdout: resq    1
stderr: resq    1
