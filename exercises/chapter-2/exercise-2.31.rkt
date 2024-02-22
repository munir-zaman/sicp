#lang sicp

(define (tree-map proc tree)
  (cond
    [(null? tree) nil]
    [(not (pair? (car tree))) (cons (proc (car tree)) (tree-map proc (cdr tree)))]
    [else (cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))]))

(define (square-tree tree)
  (tree-map (lambda (x) (* x x)) tree))
