    global main
    section .text

    extern ExitProcess

__N_test_func_0:
    mov     QWORD [rsp+16], rcx
    mov     QWORD [rsp+8], rdx
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32
    lea     rax, [rbp+8]
    mov     QWORD [rbp-8], rax
    mov     QWORD [rbp-16], 0
    mov     DWORD [rbp-20], 69
    mov     rax, 0
    leave
    ret

main:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 16
    mov     QWORD [rbp-8], 10
    mov     QWORD [rbp-16], 20
    mov     rcx, QWORD [rbp-8]
    mov     rdx, QWORD [rbp-16]
    sub     rsp, 32
    call    __N_test_func_0
    add     rsp, 32
    mov     rcx, rax
    call    ExitProcess
    leave
    ret
