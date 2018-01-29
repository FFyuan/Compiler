#lang racket
(require "../utilities.rkt")
(provide assign-homes)

(define assign-homes-insts
  (lambda (insts alist) 
    (if (null? insts) null
      (match (car insts)
             [`(,op ,es ...)
               (cons `(,op ,@(map (lambda (var) (if (pair? var) (match var 
                                                                       [`(var ,var) `(deref rbp ,(car (lookup var alist)))] 
                                                                       [else var]
                                                                       ) var)) es)) (assign-homes-insts (cdr insts) alist))]))))

(define assign-homes
  (lambda (p)
    (match p
           [`(program ,vars ,stms) 
             (let ([alist (letrec ([helper (lambda (vars n) (if (null? vars) (list) (cons (list (car vars) (- n 16)) (helper (cdr vars) (- n 16)))))]) (helper vars 0))] [maxn (* 16 (length vars))])
               `(program ,maxn ,(assign-homes-insts stms alist)))])))


