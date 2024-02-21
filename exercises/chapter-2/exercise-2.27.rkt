#lang sicp

(define test '(1 (2 3) 4 (5 6 7) 8 9 (10)))

(define (deep-reverse items)
    (cond
        [(null? items) '()]
        [(not (list? (car items))) (append (deep-reverse (cdr items)) (list (car items)))]
        [(list? (car items)) (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))]))