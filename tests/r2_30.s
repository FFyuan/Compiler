.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    subq $16, %rsp
    callq _read_int
    movq %rax, %rbx
    callq _read_int
    movq %rax, 0(%rbp)
    cmpq 0(%rbp), %rbx
    sete %al
    movzbq %al, %rbx
    movq $1, %rax
    cmpq %rbx, %rax
    je _then11851
    movq $777, 0(%rbp)
    jmp _end11852
_then11851:
    movq $42, 0(%rbp)
_end11852:
    movq 0(%rbp), %rdi
    callq _print_int
    movq $0, %rax
    addq $16, %rsp
    popq %rbp
    retq
