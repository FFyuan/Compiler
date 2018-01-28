#lang racket
(require "../utilities.rkt")
(provide allocate-registers)

;input: interference graph, variables
;output: a mapping of varibles to colors. ex: (('x 0) ('y 1))
(define color-graph
  (lambda (graph vars)
    (let recur ([colors '()] [sats (map (lambda (x) (list x '())) vars)]) 
      (if (null? sats) colors
        (let* ([u (foldl (lambda (n result)
                           (if (> (length (cadr n)) (length (cadr result)))
                             n
                             result
                             )) (car sats) sats)]
               [new-color (find-color (car u) graph colors)]
               [new-sats (fill-saturation graph (list (car u) new-color) (remove u sats))])
          ;(display (car u)) (display new-color) (newline) (display new-sats) (newline)
          (recur (cons (list (car u) new-color) colors) new-sats))))))

;input: Node u, graph, existing colors
;output: lowest color is not in colors[adjacent[u]]
(define find-color
  (lambda (u graph colors)
    (let ([ls (sort (map cadr (filter (lambda (x) (not (null? x))) (map (lambda (v) ((lambda (p) (if (null? p) null (car p))) (filter (lambda (pair) (equal? v (car pair))) colors))) (set->list (adjacent graph u))))) <)])
      ;(display ls) (newline)
      (letrec ([helper (lambda (ls index)
                         (if (null? ls) index
                           (if (equal? (car ls) index) 
                             (helper (cdr ls) (+ 1 index))
                             index)))])
        (helper ls 0)))))

;output: new filled saturations
(define fill-saturation
  (lambda (graph new-color sats) 
    (map (lambda (pair) 
           (if (member (car pair) (set->list (adjacent graph (car new-color))))
             (list (car pair) (set-add (cadr pair) (cadr new-color)))
             pair)
           ) sats)
    ))

(define regs '(rbx))

(define vec?
  (lambda (v vtypes)
    (equal? `Vector (lookup v vtypes))))

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
                                                    `(deref (global-value rootstack_begin) (- spillloc)))
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
             (let* ([alist (color-graph graph (map car vars))]
                    [maxn (* 16 (apply max (append '(0) (map cadr alist))))]
                    [var-types vars]
                    [rspills (* 16 (length (filter (lambda (e)
                                                        (vec? e var-types)) (map car var-types))))]
                    [rspillslst (let loop ([a 0]
                                           [vars (map car var-types)]
                                           [rspillslst '()])
                                    (cond
                                      [(null? vars) rspillslst]
                                      [(not (vec? (car vars) var-types)) (loop a (cdr vars) rspillslst)]
                                      [else (cons `(,(car vars) . ,a) (loop (+ 1 a) (cdr vars) rspillslst))]))])
                    `(program (,maxn ,rspills) ,type ,(allocate-registers-insts stms alist var-types rspillslst)))]
           )))
