.global _main
_main:
    pushq %rbp
    movq %rsp, %rbp
    movq $42, %rbx
    movq %rbx, %rdi
    callq _print_int
    movq $0, %rax
    popq %rbp
    retq
