#lang racket
(require "../utilities.rkt")
(provide print-x86)

(define print-x86-arg
  (lambda (arg)
    (match arg
           [`(int ,value) (string-append "$" (number->string value))]
           [`(reg ,reg) (string-append "%" (symbol->string reg))]
           [`(byte-reg ,reg) (string-append "%" (symbol->string reg))]
           [`(deref ,reg ,value) (string-append (number->string value) "(%" (symbol->string reg) ")")]
           [else (symbol->string arg)])))

(define print-x86
  (lambda (p)
    (define mac? (equal? 'macosx (system-type 'os)))
    (match p
           [`(program (,size ,rspills) ,type ,stms) 
    (string-append
      (string-append* 
        ".global " (if mac? "_" "") "main" "\n" 
        (if mac? "_" "") "main:" "\n" 
        "    pushq %rbp" "\n"
        "    movq %rsp, %rbp" "\n" 
        (if (< 0 size) (string-append "    subq $" (number->string size) ", %rsp" "\n") "")
        "    movq $16384, %rdi" "\n"
        "    movq $16, %rsi" "\n"
        "    callq _initialize" "\n"
        "    movq _rootstack_begin(%rip), %r15" "\n"
        "    movq $0, (%r15)" "\n"
        "    addq $" (number->string rspills) ", %r15" "\n"
        (map (lambda (stm) 
               (match stm
                      [`(label ,label)
                        (string-append (if mac? "_" "") (symbol->string label) ":\n")]
                      [`(jmp-if ,cc ,label)
                        (string-append "    " "j" (symbol->string cc) " " (if mac? "_" "") (symbol->string label) "\n")] 
                      [`(jmp ,label)
                        (string-append "    " "jmp " (if mac? "_" "") (symbol->string label) "\n")]
                      [`(set ,cc ,reg)
                        (string-append "    " "set" (symbol->string cc) " " (print-x86-arg reg) "\n")]
                      [`(,op ,arg1 ,arg2)
                        (string-append "    " (symbol->string op) " " (print-x86-arg arg1) ", " (print-x86-arg arg2) "\n")]
                      [`(,op ,arg1)
                        (string-append "    " (symbol->string op) " " (print-x86-arg arg1) "\n")]
                        )) stms)) 
      (match type
        [`(type Integer) (string-append "    callq " (if mac? "_" "") "print_int" "\n")]
        [`(type Boolean) (string-append "    callq " (if mac? "_" "") "print_bool" "\n")])
      "    movq $0, %rax" "\n"
      "    subq $" (number->string rspills) ", %r15" "\n"
      (if (< 0 size) (string-append  "    addq $" (number->string size) ", %rsp" "\n") "")
      "    popq %rbp" "\n"
      "    retq" "\n")])))


