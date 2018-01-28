#lang racket
(require "utilities.rkt")
(require "compiler.rkt")
(require "passes/type-check.rkt")

(compiler-tests "compiler.rkt" type-check r3-passes "r3" (list 29 30 31 32 33 34 35))
