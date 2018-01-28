#lang racket
(require "../utilities.rkt")
(provide lower-conditionals)

(define lower-conditionals
  (lambda (e)
    (match e
           [`(program ,maxn ,type ,stms)
             `(program ,maxn ,type ,(lower-conditionals-stms stms))])))

(define lower-conditionals-stms
  (lambda (stms)
    (if (null? stms) '()
      (match (car stms)
             [`(if (eq? ,arg1 ,arg2) ,thn ,els)
               (let ([then-label (gensym "then")]
                     [end-label (gensym "end")])

                 (append 
                   `((cmpq ,arg2 ,arg1) 
                     (jmp-if e ,then-label))
                   (lower-conditionals-stms els)
                   `((jmp ,end-label)
                     (label ,then-label))
                   (lower-conditionals-stms thn)
                   `((label ,end-label))
                   (lower-conditionals-stms (cdr stms))))]
             [else (cons (car stms) (lower-conditionals-stms (cdr stms)))]))))
