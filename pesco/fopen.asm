    global main

    section .text
    extern init_stdio
    extern fopen
    extern fwrite
    extern fputs
    extern fclose
    extern dump
    extern puts
    extern exit

main:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32
    call    init_stdio
    add     rsp, 32

    mov     rcx, .filename
    mov     rdx, .mode
    sub     rsp, 32
    call    fopen
    add     rsp, 32
    push    rax

    mov     rcx, rax
    mov     rdx, .content
    sub     rsp, 32
    call    fputs
    add     rsp, 32

    mov     rcx, .msg
    sub     rsp, 32
    call    puts
    add     rsp, 32

    sub     rsp, 16
    mov     DWORD [rbp-16], 1
    mov     DWORD [rbp-12], 2
    mov     DWORD [rbp-8], 3
    mov     DWORD [rbp-4], 4
    pop     rcx
    push    rcx
    lea     rdx, [rbp-16]
    mov     r8, 16
    sub     rsp, 32
    call    fwrite
    add     rsp, 32

    pop     rcx
    sub     rsp, 32
    call    fclose
    add     rsp, 32

    mov     rcx, 206
    sub     rsp, 32
    call    dump
    add     rsp, 32

    xor     rcx, rcx
    call    exit


    section .data
.filename: db "testfile.txt", 0

.content: db "Random String", 0

.mode: db "w", 0

.msg: db "Printed to file", 10, 0