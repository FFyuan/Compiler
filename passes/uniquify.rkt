#lang racket
(require "../utilities.rkt")
(provide uniquify)

(define (uniquify-helper alist)
  (lambda (rawExp)
    (match rawExp
           [`(has-type ,e ,t)
             (match e
                    [(? symbol?) `(has-type ,(car (lookup e alist)) ,t)]
                    [(? integer?) rawExp]
                    [(? boolean?) rawExp]
                    [`(let ([,x ,e]) ,body) 
                      (let ([newsym (gensym x)])
                        `(has-type (let ([,newsym ,((uniquify-helper alist) e)]) ,((uniquify-helper (cons (list x newsym) alist)) body)) ,t))]
                    [`(,op ,es ...)
                      `(has-type (,op ,@(map (uniquify-helper alist) es)) ,t)]
                    [`(read)
                      `(has-type (read) ,t)])]
           [`(program ,type ,e)
             `(program ,type ,((uniquify-helper alist) e))]
           )))

(define uniquify
  (lambda (e)
    ((uniquify-helper '()) e)))
