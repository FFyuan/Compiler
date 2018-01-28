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
                                                    `(deref r15 ,spillloc))
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
                    [max (apply max (append '(0) (map cadr alist)))]
                    [maxn (* 16 max)]
                    [var-types vars]
                    [vec-colors (filter (lambda (v) (vec? (car v) var-types)) alist)]
                    [colors-for-vec (sort (set->list (list->set (map cadr vec-colors))) <)]
                    [color-to-spillloc (let loop ([cs colors-for-vec]
                                                  [loc 0]
                                                  [csmap '()])
                                            (if (null? cs)
                                                csmap
                                                (cons `(,(car cs) . ,loc) (loop (cdr cs) (+ 16 loc) csmap))))]
                    [rspills (* 16 (length colors-for-vec))]
                    [rspillslst (let loop ([vecs (map car vec-colors)]
                                           [rspillslst '()])
                                    (if (null? vecs)
                                        rspillslst
                                        (cons `(,(car vecs) . ,(lookup (car (lookup (car vecs) alist)) color-to-spillloc))
                                          (loop (cdr vecs) rspillslst))))])
                    `(program (,maxn ,rspills) ,type ,(allocate-registers-insts stms alist var-types rspillslst)))]
           )))
