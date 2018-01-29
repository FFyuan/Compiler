#lang racket
(require "../utilities.rkt")
(provide flatten)

(define flatten-helper
  (lambda (rawExp)
    (match rawExp
           [`(has-type ,exp ,t)
             (match exp
                    [(? symbol?) (values exp null null)]
                    [(? integer?) (values exp null null)]
                    [(? boolean?) (values exp null null)]
                    [`(void) (values exp null null)]
                    [`(allocate ,count ,type)
                      (values exp null null)]
                    [`(collect ,(app flatten-helper newExp stms var))
                      (values `(collect ,newExp) null null)]
                    [`(global-value free_ptr) 
                      (let ([newExp (gensym "tmp")])
                        (values newExp 
                                (list (list `assign newExp exp))
                                (list (cons newExp 'Integer))))]
                    [`(global-value fromspace_end) 
                      (let ([newExp (gensym "tmp")])
                        (values newExp 
                                (list (list `assign newExp exp))
                                (list (cons newExp 'Integer))))]
                    [`(let ([,x (has-type ,exp ,type)]) ,body)
                      (define-values (prevExp stms vars) (flatten-helper `(has-type ,exp ,type)))
                      (define-values (bodyExp bodystms bodyvars) (flatten-helper body))
                      (values bodyExp 
                              (append stms (list (list `assign x prevExp)) bodystms) 
                              (append (cons (cons x type) vars) bodyvars))
                      ]
                    [`(read) (let ([newExp (gensym "tmp")]) 
                               (values newExp 
                                       (list (list `assign newExp `(read))) 
                                       (list (cons newExp 'Integer))))]
                    [`(if ,cnd ,thn ,els)
                      (let ([newExp (gensym "if")])
                        (define-values (prev-exp prev-stms prev-vars) (flatten-helper cnd))
                        (define-values (thn-exp thn-stms thn-vars) (flatten-helper thn))
                        (define-values (els-exp els-stms els-vars) (flatten-helper els))
                        (values newExp
                                (append prev-stms
                                        (list (list `if `(eq? #t ,prev-exp)
                                                    (append thn-stms (list (list `assign newExp thn-exp)))
                                                    (append els-stms (list (list `assign newExp els-exp))))))
                                (cons (cons newExp t) (append prev-vars thn-vars els-vars))))]
                    [`(and ,exp1 ,exp2)
                      (flatten-helper `(has-type (if ,exp1 ,exp2 (has-type #f Boolean)) Boolean))]
                    [`(,op ,exp)
                      (let ([newExp (gensym "tmp")])
                        (define-values (prevExp stms vars) (flatten-helper exp))
                        (values newExp 
                                (append stms (list (list `assign newExp `(,op ,prevExp)))) 
                                (cons (cons newExp t) vars)))
                      ]
                    [`(,op ,exp1 ,exp2) 
                      (let ([newExp (gensym "tmp")])
                        (define-values (prevExp1 stms1 vars1) (flatten-helper exp1))
                        (define-values (prevExp2 stms2 vars2) (flatten-helper exp2))
                        (values newExp 
                                (append stms1 stms2 (list (list `assign newExp `(,op ,prevExp1 ,prevExp2)))) 
                                (cons (cons newExp t) (append vars1 vars2))))
                      ]
                    [`(,op ,exp1 ,exp2 ,exp3)
                      (let ([newExp (gensym "tmp")])
                        (define-values (prevExp1 stms1 vars1) (flatten-helper exp1))
                        (define-values (prevExp2 stms2 vars2) (flatten-helper exp2))
                        (define-values (prevExp3 stms3 vars3) (flatten-helper exp3))
                        (values newExp 
                                (append stms1 stms2 stms3 (list (list `assign newExp `(,op ,prevExp1 ,prevExp2 ,prevExp3)))) 
                                (cons (cons newExp t) (append vars1 vars2 vars3))))]
                    )]
           [`(program ,type ,e)
             (define-values (newExp stms vars) (flatten-helper e))
             (values (list) `(program ,(remove-duplicates vars) ,type ,(append stms (list (list `return newExp)))) (list))
             ]
           )))

(define flatten 
  (lambda (exp)
    (define-values (tmp stms vars) (flatten-helper exp))
    stms))
