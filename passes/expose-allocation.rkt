#lang racket (require "../utilities.rkt")
(provide expose-allocation)

(define expose-allocation
  (lambda (e)
    (match e
           [`(program ,type ,body)
             `(program ,type ,(expose-allocation body))]
           [`(has-type (vector ,es ...) ,types) 
             (expose-allocation-helper es '() types 0 (gensym "alloc"))]
           [`(has-type (,op ,e1 ,e2) ,ts)
             `(has-type (,op ,(expose-allocation e1) ,(expose-allocation e2)) ,ts)]
           [`(has-type (,op ,e1) ,ts)
             `(has-type (,op ,(expose-allocation e1)) ,ts)]
           [`(has-type (,op ,e1 ,e2 ,e3) ,ts)
             `(has-type (,op ,(expose-allocation e1) ,(expose-allocation e2) ,(expose-allocation e3)) ,ts)]
           [else e]
           )))

(define expose-allocation-helper
  (lambda (es esr types count alloc-sym)
    (define bytes (+ 16 (* 16 count)))
    (if (null? es)
      `(has-type (let ([,(gensym "collectret") (has-type (if 
                                      (has-type (< (has-type (+ (has-type (global-value free_ptr)Integer) (has-type ,bytes Integer)) Integer) (has-type (global-value fromspace_end) Integer)) Boolean)
                                      (has-type (void) Void)
                                      (has-type (collect 
                                                  (has-type ,bytes Integer)) Void))
                                    Void)])
                   (has-type (let ([,alloc-sym (has-type (allocate ,count ,types) ,types)])
                               ,(expose-allocation-helper1 esr alloc-sym 0 types (cdr types))) ,types)) ,types)
      (let ([x (gensym "vecinit")])
        `(has-type (let ([,x ,(expose-allocation (car es))])
                     ,(expose-allocation-helper (cdr es) (append esr (list x)) types (+ 1 count) alloc-sym)) ,types)))))

(define expose-allocation-helper1 
  (lambda (es alloc-sym count types cur-types)
    (if (null? es)
      `(has-type ,alloc-sym ,types)
      `(has-type (let ([,(gensym "initret") (has-type (vector-set! 
                                                        (has-type ,alloc-sym ,types)
                                                        (has-type ,count Integer)
                                                        (has-type ,(car es) ,(car cur-types)) 
                                                        ) Void)])
                   ,(expose-allocation-helper1 (cdr es) alloc-sym (+ 1 count) types (cdr cur-types))) ,types))))

 (define test
   (lambda (e)
     (view-has-type (expose-allocation e))))

