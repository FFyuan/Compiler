#lang racket
(require "../utilities.rkt")
(provide patch-instructions)

(define patch-instructions-stms
  (lambda (stms)
    (if (null? stms) null
      (match (car stms) 
             [`(movq ,arg1 ,arg2)
               (if (equal? arg1 arg2)
                 (patch-instructions-stms (cdr stms))
                 (if (and (equal? 'deref (car arg2)) (or (equal? 'deref (car arg1)) (equal? 'global-value (car arg1)))) 
                   (cons `(movq ,arg1 (reg rax)) 
                         (cons `(movq (reg rax) ,arg2) (patch-instructions-stms (cdr stms))))
                   (cons (car stms) (patch-instructions-stms (cdr stms)))))]
             [`(movzbq ,arg1 (deref ,n ,reg))
               (cons `(movzbq ,arg1 (reg rax))
                     (cons `(movq (reg rax) (deref ,n ,reg)) (patch-instructions-stms (cdr stms))))]
             [`(cmpq ,arg1 ,arg2)
               (if (or (equal? 'int (car arg2)) (and (equal? 'deref (car arg1)) (equal? 'deref (car arg2))))
                 (cons `(movq ,arg2 (reg rax))
                         (cons `(cmpq ,arg1 (reg rax)) (patch-instructions-stms (cdr stms))))
                 (cons (car stms) (patch-instructions-stms (cdr stms))))]
             [else (cons (car stms) (patch-instructions-stms (cdr stms)))])))) 

(define patch-instructions
  (lambda (p)
    (match p
           [`(program ,maxn ,type ,stms) `(program ,maxn ,type ,(patch-instructions-stms stms))])))


