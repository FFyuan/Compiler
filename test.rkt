#lang racket
(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

;(compiler-tests "compiler.rkt" type-check r3-passes "r0" (make-list 4))
;(compiler-tests "compiler.rkt" type-check r3-passes "r1" (make-list 48))
;(compiler-tests "compiler.rkt" type-check r3-passes "r2" (make-list 59))
(compiler-tests "compiler.rkt" type-check r3-passes "r3" (make-list 35))
