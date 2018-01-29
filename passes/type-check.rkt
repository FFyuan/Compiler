#lang racket
(require "../utilities.rkt")
(provide type-check)

(define (type-check-R3 env)
  (lambda (e)
    (match e
           [(? fixnum?) (values `(has-type ,e Integer) 'Integer)]
           [(? boolean?) (values `(has-type ,e Boolean) 'Boolean)]
           [(? symbol?) (values `(has-type ,e ,(lookup e env)) (lookup e env))]
           [`(let ([,x ,(app (type-check-R3 env) e T)]) ,body)
             (define new-env (cons (cons x T) env))
             (define-values (e-body T-body) ((type-check-R3 new-env) body))
             (values `(has-type (let ([,x ,e]) ,e-body) ,T-body) T-body)]
           [`(not ,(app (type-check-R3 env) e t))
             (match t
                    [`Boolean (values `(has-type (not ,e) Boolean) 'Boolean)]
                    [else (error `type-check-r3 "`not` expects a Boolean: ~a" e)])]
           [`(and ,(app (type-check-R3 env) e1 t1) ,(app (type-check-R3 env) e2 t2))
             (match t1
                    [`Boolean (values `(has-type (and ,e1 ,e2) `Boolean) 'Boolean)]
                    [else (error `type-check-r3 "`and` expects two Booleans: ~a" e)])
             (match t2
                    [`Boolean (values `(has-type (and ,e1 ,e2) `Boolean) 'Boolean)]
                    [else (error `type-check-r3 "`and` expects a Booleans: ~a" e)])]

           [`(if ,(app (type-check-R3 env) e-cnd t-cnd) ,(app (type-check-R3 env) e-thn t-thn) ,(app (type-check-R3 env) e-els t-els))
             (if (eq? 'Boolean t-cnd)
               (if (eq?  t-thn t-els)
                 (values `(has-type (if ,e-cnd ,e-thn ,e-els) ,t-thn) t-thn)
                 (error `type-check-r2 "'if' expects 'then' and 'else' to be the same type: ~a" e))
               (error `type-check-r2 "'if' expects condition to be Boolean: ~a" e))]
           [`(- ,(app (type-check-R3 env) e t))
             (if (eq? 'Integer t)
               (values `(has-type (- ,e) Integer) 'Integer)
               (error 'type-check-r2 "`-` expects an Integers: ~a" e))]
           [`(+ ,(app (type-check-R3 env) e1 t1) ,(app (type-check-R3 env) e2 t2))
             (if (and (eq? 'Integer t1) (eq? 'Integer t2))
               (values `(has-type (+ ,e1 ,e2) Integer) 'Integer)
               (error 'type-check-R3 "'+' expects two Integers: ~a" e))]
           [`(read)
               (values `(has-type (read) 'Integer) 'Integer)]
           [`(void) (values `(has-type (void) Void) `Void)]
           [`(vector ,(app (type-check-R3 env) e* t*) ...)
             (let ([t `(Vector ,@t*)])
               (values `(has-type (vector ,@e*) ,t) t))]
           [`(vector-ref ,(app (type-check-R3 env) e t) ,i)
             (match t
                    [`(Vector ,ts ...)
                      (unless (and (exact-nonnegative-integer? i)
                                   (i . < . (length ts)))
                        (error `type-check "invalid index ~a" i))
                      (let ([t (list-ref ts i)])
                        (values `(has-type (vector-ref ,e (has-type ,i Integer)) ,t)
                                t))]
                    [else (error "expected a vector in vector-ref, not" t)])]
           [`(vector-set! ,(app (type-check-R3 env) e-vec^ t-vec) ,i
                          ,(app (type-check-R3 env) e-arg^ t-arg))
             (match t-vec
                    [`(Vector ,ts ...)
                      (unless (and (exact-nonnegative-integer? i)
                                   (i . < . (length ts)))
                        (error `type-check "invalid index ~a" i))
                      (unless (equal? (list-ref ts i) t-arg)
                        (error `type-check "type mismatch in vector-set! ~a ~a"
                               (list-ref ts i) t-arg))
                      (values `(has-type (vector-set! ,e-vec^
                                                      (has-type ,i Integer)
                                                      ,e-arg^) Void) `Void)]
                    [else (error `type-check
                                 "expected a vector in vector-set!, not ~a" t-vec)])]
           [`(eq? ,(app (type-check-R3 env) e1 t1)
                  ,(app (type-check-R3 env) e2 t2))
             (match* (t1 t2)
                     [(`(Vector ,ts1 ...) `(Vector ,ts2 ...))
                      (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)]
                     [(other wise) 
                       (if (eq? t1 t2) 
                         (values `(has-type (eq? ,e1 ,e2) Boolean) 'Boolean)
                         (error `type-check "`eq?` expects two Integers or two Booleans: ~a" e))])]
             [`(,op ,(app (type-check-R3 env) e1 t1) ,(app (type-check-R3 env) e2 t2))
               (if (and (eq? 'Integer t1) (eq? 'Integer t2))
                 (values `(has-type (,op ,e1 ,e2) `Boolean) 'Boolean)
                 (error `type-check-r2 (string-append (symbol->string op) " expects two Integers: ~a ~a ~a") e t1 t2))]
           [`(program ,(app (type-check-R3 env) e-body t-body))
             `(program (type ,t-body) ,e-body)]
           )))

(define type-check
  (lambda (e)
  ((type-check-R3 '()) e)))
