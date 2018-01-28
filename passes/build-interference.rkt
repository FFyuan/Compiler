#lang racket
(require "../utilities.rkt")
(require racket/trace)
(provide build-interference)

(define add-edges-helper
  (lambda (d vs ignores graph var-types)
    (let ([ign (remove null 
                       (map (lambda (x) 
                              (match x 
                                     [`(var ,var) var] 
                                     [else null])) ignores))])
      (match d
             [`(var ,var)
               (begin
                 (let ([type (lookup var var-types)])
                    (if (equal? 'Vector type)
                        (map (lambda (r)
                                (add-edge graph var r)) caller-save)
                        (void)))
                 (map (lambda (v) (add-edge graph var v)) (filter (lambda (x) (not (member x ign))) vs))
                 graph)]
             [else graph]))))

(define helper
  (lambda (live-after stms graph var-types)
    (if (null? stms) graph
      (helper (cdr live-after) (cdr stms) 
              (match (car stms)
                     [`(movq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph var-types)]
                     [`(movzbq ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) graph var-types)]
                     [`(negq ,arg1)
                       (add-edges-helper arg1 (car live-after) (list arg1) graph var-types)]
                     [`(if (eq? ,arg1 ,arg2) ,thn ,thn-lives ,els ,els-lives)
                       (add-edges-helper arg2 (car live-after) (list arg1 arg2) (helper els-lives els (helper thn-lives thn graph var-types) var-types) var-types)]
                     [`(,op ,arg1 ,arg2)
                       (add-edges-helper arg2 (car live-after) (list arg2) graph var-types)]
                     [else graph]
                     )
              var-types))))

(define build-interference
  (lambda (program)
    (match program
           [`(program (,var-types ,live-afters ,type) ,stms)
             (let ([vars (set-union (map car var-types) (set->list caller-save))])
                  `(program ,(list var-types (helper live-afters stms (make-graph vars) var-types) type ) ,stms))])))
