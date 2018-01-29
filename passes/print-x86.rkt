#lang racket
(require "../utilities.rkt")
(require "../runtime-config.rkt")
(provide print-x86)

(define print-x86-arg
  (lambda (arg)
    (match arg
           [`(int ,value) (string-append "$" (number->string value))]
           [`(reg ,reg) (string-append "%" (symbol->string reg))]
           [`(byte-reg ,reg) (string-append "%" (symbol->string reg))]
           [`(deref ,reg ,value) (string-append (number->string value) "(%" (symbol->string reg) ")")]
           [`(global-value ,val) (string-append "_" (symbol->string val) "(%rip)")]
           [else (symbol->string arg)])))

(define print-save-caller-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "saving_caller")) ":\n")
      (let loop ([regs (set->list caller-save)])
        (if (null? regs)
            ""
            (string-append "    pushq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_saving_caller")) ":\n"))))

(define print-restore-caller-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "restoring_caller")) ":\n")
      (let loop ([regs (reverse (set->list caller-save))])
        (if (null? regs)
            ""
            (string-append "    popq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_restoring_caller")) ":\n"))))

(define print-save-callee-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "saving_callee")) ":\n")
      (let loop ([regs (set->list callee-save)])
        (if (null? regs)
            ""
            (string-append "    pushq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_saving_callee")) ":\n"))))

(define print-restore-callee-save
  (lambda ()
    (string-append
      (string-append (if mac? "_" "") (symbol->string (gensym "restoring_callee")) ":\n")
      (let loop ([regs (reverse (set->list callee-save))])
        (if (null? regs)
            ""
            (string-append "    popq " (print-x86-arg `(reg ,(car regs))) "\n" (loop (cdr regs)))))
      (string-append (if mac? "_" "") (symbol->string (gensym "done_restoring_callee")) ":\n"))))

(define mac? (equal? 'macosx (system-type 'os)))

(define print-x86
  (lambda (p)
    (match p
           [`(program (,size ,rspills) (type ,type) ,stms)
    (string-append
      (string-append* 
        ".global " (if mac? "_" "") "main" "\n" 
        (if mac? "_" "") "main:" "\n" 
        (print-save-callee-save)
        "    movq %rsp, %rbp" "\n" 
        (if (< 0 size) (string-append "    subq $" (number->string size) ", %rsp" "\n") "")
        "    movq $" (number->string (rootstack-size)) ", %rdi" "\n"
        "    movq $" (number->string (heap-size)) ", %rsi" "\n"
        (print-save-caller-save)
        "    callq _initialize" "\n"
        (print-restore-caller-save)
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
                      [`(callq ,func)
                        (string-append
                          (print-save-caller-save)
                          "    callq " (print-x86-arg func) "\n"
                          (print-restore-caller-save))]
                      [`(,op ,arg1 ,arg2)
                        (string-append "    " (symbol->string op) " " (print-x86-arg arg1) ", " (print-x86-arg arg2) "\n")]
                      [`(,op ,arg1)
                        (string-append "    " (symbol->string op) " " (print-x86-arg arg1) "\n")]
                        )) stms))
      "    movq %rdi, %rax" "\n"
      (print-by-type type)
      "    movq $0, %rax" "\n"
      "    subq $" (number->string rspills) ", %r15" "\n"
      (if (< 0 size) (string-append  "    addq $" (number->string size) ", %rsp" "\n") "")
      (print-restore-callee-save)
      "    retq" "\n")])))
