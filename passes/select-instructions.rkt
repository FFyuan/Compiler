#lang racket
(require "../utilities.rkt")
(provide select-instructions)

(define select-intr-arg
  (lambda (arg)
    (match arg
      [(? integer?) (list 'int arg)]
      [(? boolean?) (list 'int (if arg 1 0))]
      [else (list 'var arg)]
      )))

(define select-intr-stms
  (lambda (stms) 
    (if (null? stms) (list)
      (match (car stms)
             [`(assign ,var (+ ,arg1 ,arg2)) 
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var)) 
                     (cons (list `addq (select-intr-arg arg2) (select-intr-arg var)) (select-intr-stms (cdr stms))))
               ]
             [`(assign ,var (- ,arg1))
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var))
                     (cons (list `negq (select-intr-arg var)) (select-intr-stms (cdr stms))))
               ]
             [`(assign ,var (read)) 
               (begin
               (define mac? (equal? 'macosx (system-type 'os)))
               (cons `(callq ,(if mac? '_read_int '_read_int)) (cons `(movq (reg rax) ,(select-intr-arg var)) (select-intr-stms (cdr stms)))))
               ]
             [`(assign ,var (not ,arg1))
               (cons (list `movq (select-intr-arg arg1) (select-intr-arg var))
                     (cons (list `xorq `(int 1) (select-intr-arg var)) (select-intr-stms (cdr stms))))]
             [`(assign ,var (eq? ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `e `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) (select-intr-stms (cdr stms)))))]
             [`(assign ,var (> ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `g `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) (select-intr-stms (cdr stms)))))]
             [`(assign ,var (>= ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `ge `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) (select-intr-stms (cdr stms)))))]
             [`(assign ,var (< ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `l `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) (select-intr-stms (cdr stms)))))]
             [`(assign ,var (<= ,arg1 ,arg2))
               (cons (list `cmpq (select-intr-arg arg2) (select-intr-arg arg1))
               (cons (list `set `le `(byte-reg al))
               (cons (list `movzbq `(byte-reg al) (select-intr-arg var)) (select-intr-stms (cdr stms)))))]
             [`(assign ,var ,arg)
               (cons (list `movq (select-intr-arg arg) (select-intr-arg var)) (select-intr-stms (cdr stms)))
               ]
             [`(return ,arg)
               (cons (list `movq (select-intr-arg arg) `(reg rdi)) (select-intr-stms (cdr stms)))
               ]
             [`(if (eq? ,arg1 ,arg2) ,thn ,els)
               (cons (list `if `(eq? ,(select-intr-arg arg1) ,(select-intr-arg arg2)) (select-intr-stms thn) (select-intr-stms els)) (select-intr-stms (cdr stms)))]
             ))))

(define select-instructions
  (lambda (p)
    (match p
           [`(program ,vars ,type ,stms) `(program ,vars ,type ,(select-intr-stms stms))] )))

