#lang racket
(require "utilities.rkt")
(require "interp.rkt")
(provide r3-passes)

(require "passes/type-check.rkt")
(require "passes/uniquify.rkt")
(require "passes/expose-allocation.rkt")
(require "passes/flatten.rkt")
(require "passes/select-instructions.rkt")
;(require "passes/assign-homes.rkt")
(require "passes/uncover-live.rkt")
(require "passes/build-interference.rkt")
(require "passes/allocate-registers.rkt")
(require "passes/lower-conditionals.rkt")
(require "passes/patch-instructions.rkt") 
(require "passes/print-x86.rkt")


(define print-list
  (lambda (ls)
    (if (null? ls) null
      (begin
        (display (car ls))
        (newline)
        (print-list (cdr ls))))))

(define test-compile
  (lambda (e)
     (patch-instructions (lower-conditionals (allocate-registers (build-interference (uncover-live (select-instructions (flatten (expose-allocation (uniquify (type-check e))))))))))))

(define tt
  (lambda (e)
    (build-interference (uncover-live (select-instructions (flatten (expose-allocation (uniquify (type-check e)))))))))

(define r3-passes
  `(("uniquify" ,uniquify ,null)
    ("expose-allocation" ,expose-allocation ,null)
    ("flatten" ,flatten ,null)
    ("select-instructions" ,select-instructions ,null)
    ("uncover-live" ,uncover-live ,null)
    ("build-interference" ,build-interference ,null)
    ("allocate-registers" ,allocate-registers ,null)
    ("lower-conditionals" ,lower-conditionals ,null)
    ("patch-instructions" ,patch-instructions ,null)
    ("print-x86" ,print-x86 ,interp-x86)
    ))


