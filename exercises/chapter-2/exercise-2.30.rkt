#lang sicp

(define (square-tree-rec tree)
  (cond
    [(null? tree) nil]
    [(not (pair? tree)) (* tree tree)]
    [else (cons (square-tree-rec (car tree)) (square-tree-rec (cdr tree)))]))

(define (square-tree-map tree)
  (map (lambda (sub-tree) 
        (if (pair? sub-tree) 
            (square-tree-map sub-tree) 
            (* sub-tree sub-tree)))
        tree))

; Is this actually iterative though?
; Need to think about it? :thonk:
(define (square-tree-iter tree)
    (define (iter tree mem)
        (cond
            [(null? tree) mem]
            [(pair? (car tree)) (iter (cdr tree) (append mem (list (square-tree-iter (car tree)))))]
            [else (iter (cdr tree) (append mem (list (* (car tree) (car tree)))))]))
    (iter tree nil))