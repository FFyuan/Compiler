.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    movq $1, %rbx
    xorq $1, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11949
    movq $1, 0(%rbp)
    jmp _end11950
_then11949:
    movq $0, 0(%rbp)
_end11950:
    movq $1, %rax
    cmpq 0(%rbp), %rax
    je _then11951
    movq $777, %rbx
    jmp _end11952
_then11951:
    movq $42, %rbx
_end11952:
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq

