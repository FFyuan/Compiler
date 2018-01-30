#lang racket
(require "../utilities.rkt")
(provide allocate-registers)

;input: interference graph, variables
;output: a graph of variables to colors.
(define (color-graph g vars)
  (let ([rg (make-graph vars)])
    (while (not (null? vars))
      (set! vars (sort vars (lambda (a b)
                              (let ([neighborsA (set->list (adjacent g a))]
                                    [neighborsB (set->list (adjacent g b))])
                                    (let ([countColored (lambda (n)
                                                          (if (null? (adjacent rg n))
                                                              0
                                                              1))])
                                          (let ([countA (apply + (map countColored neighborsA))]
                                                [countB (apply + (map countColored neighborsB))])
                                                (>= countA countB)))))))
      (let ([mostSaturated (car vars)])
        (let ([neighbors (set->list (adjacent g mostSaturated))])
          (let ([lowestColor (let ([neighborColors (map (lambda (n) (if (set-empty? (adjacent rg n))
                                                                          #f
                                                                          (car (set->list (adjacent rg n))))) neighbors)])
            (let loop ([lowestColor 0])
              (if (member lowestColor neighborColors)
                  (loop (+ 1 lowestColor))
                  lowestColor)))])
            (add-edge rg mostSaturated lowestColor)
            (set! vars (remove mostSaturated vars))))))
    rg))

(define (color-graph-convert g)
  (let* ([colors (filter number? (vertices g))]
        [color-vars (map (lambda (color) (set->list (adjacent g color))) colors)])
    (let loop ([colors colors]
               [color-vars color-vars]
               [ls '()])
         (if (null? colors)
             ls
             (begin
               (map (lambda (var)
                       (set! ls (cons `(,var ,(car colors)) ls))) (car color-vars))
               (loop (cdr colors) (cdr color-vars) ls))))))

(define regs '(rbx rcx rdx rsi rdi r8 r9 r10 r12 r13 r14))

(define vec?
  (lambda (v vtypes)
    (let ([vtype (lookup v vtypes)])
      (and (list? vtype) (equal? `Vector (car vtype))))))

(define allocate-registers-insts
  (lambda (insts alist var-types rspillslst)
    (if (null? insts) null
      (match (car insts)
             [`(,if ,cnd ,thn ,thn-live ,els ,els-live)
               (cons (list 'if (car (allocate-registers-insts (list cnd) alist var-types rspillslst))
                                    (allocate-registers-insts thn alist var-types rspillslst)
                                    (allocate-registers-insts els alist var-types rspillslst))
                                            (allocate-registers-insts (cdr insts) alist var-types rspillslst))]
             [`(,op ,es ...)
               (cons `(,op ,@(map (lambda (var) 
                                    (if (pair? var) 
                                      (match var
                                             [`(var ,var)
                                               (if (vec? var var-types)
                                                 (let ([spillloc (lookup var rspillslst)])
                                                    `(deref r15 ,(- spillloc)))
                                                 (let ([value (car (lookup var alist))])
                                                          (cond
                                                            [(< value (length regs)) `(reg ,(list-ref regs value))]
                                                            [else `(deref rbp ,(* 16 (- value (length regs))))])))]
                                             [else var])
                                      var)) es)) 
                     (allocate-registers-insts (cdr insts) alist var-types rspillslst))]))))


(define allocate-registers 
  (lambda (program)
    (match program
           [`(program (,vars ,graph ,type) ,stms)
             (let* ([alist (color-graph-convert (color-graph graph (map car vars)))]
                    [max (apply max (append '(0) (map cadr alist)))]
                    [maxn (* 16 max)]
                    [var-types vars]
                    [vec-colors (filter (lambda (v) (vec? (car v) var-types)) alist)]
                    [colors-for-vec (sort (set->list (list->set (map cadr vec-colors))) <)]
                    [color-to-spillloc (let loop ([cs (reverse colors-for-vec)]
                                                  [loc 8]
                                                  [csmap '()])
                                            (if (null? cs)
                                                csmap
                                                (cons `(,(car cs) . ,loc) (loop (cdr cs) (+ 8 loc) csmap))))]
                    [rspills (* 8 (length colors-for-vec))]
                    [rspillslst (let loop ([vecs (map car vec-colors)]
                                           [rspillslst '()])
                                    (if (null? vecs)
                                        rspillslst
                                        (cons `(,(car vecs) . ,(lookup (car (lookup (car vecs) alist)) color-to-spillloc))
                                          (loop (cdr vecs) rspillslst))))])
                    `(program (,maxn ,rspills) ,type ,(allocate-registers-insts stms alist var-types rspillslst)))]
           )))
