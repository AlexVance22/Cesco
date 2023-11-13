    global init_heap
    global new
    global del

    section .text
    extern GetProcessHeap
    extern HeapAlloc
    extern HeapFree

init_heap:
    push    rbp
    mov     rsp, rbp
    sub     rsp, 32
    call    GetProcessHeap
    add     rsp, 32
    mov     rcx, .heap
    mov     [rcx], rax
    leave
    ret

new:
    push    rbp
    mov     rsp, rbp
    mov     r8, rcx
    mov     rcx, .heap
    mov     rcx, [rcx]
    xor     rdx, rdx
    sub     rsp, 32
    call    HeapAlloc
    add     rsp, 32
    leave
    ret

del:
    push    rbp
    mov     rsp, rbp
    mov     r8, rcx
    mov     rcx, .heap
    mov     rcx, [rcx]
    xor     rdx, rdx
    sub     rsp, 32
    call    HeapFree
    add     rsp, 32
    leave
    ret


    section .bss
.heap:  resq    1