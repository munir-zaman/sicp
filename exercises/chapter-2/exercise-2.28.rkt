#lang sicp

(define t '((1 (2)) (3 4) (5 6 (7 (8 9) 10 11))))

(define (fringe tree)
    (cond
        ; handle empty lists
        [(null? tree) (display "")]
        ; if the first value is a leaf then
        ; print and recurse on the rest
        [(not (list? (car tree))) (display (car tree)) (display " ") (fringe (cdr tree))]
        ; if not
        ; then recurse on both
        [else (fringe (car tree)) (fringe (cdr tree))]))