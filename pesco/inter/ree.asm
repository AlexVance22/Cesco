    global main
    section .text
    extern ExitProcess
    extern init_stdio
    extern puts
    extern dump
main:
    call init_stdio
    push 0
addr_1:
    pop rax
    push rax
    push rax
    mov rbx, 100
    pop rax
    cmp rax, rbx
    jnl cond_1
    mov rax, lit_0
    mov rcx, rax
    sub rsp, 32
    call puts
    add rsp, 32
    pop rax
    inc rax
    push rax
    mov rcx, rax
    sub rsp, 32
    call dump
    add rsp, 32
    mov rax, lit_1
    mov rcx, rax
    sub rsp, 32
    call puts
    add rsp, 32
    jmp addr_1
cond_1:
    xor rax, rax
    call ExitProcess

    section .data
lit_0: db "ABC", 0
lit_1: db 10, 0
