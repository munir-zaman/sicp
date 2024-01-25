#lang sicp

(define (parity? x) (remainder x 2))
(define (same-parity-iter parity nums)
    (cond
        [(null? nums) '()] 
        [(= (parity? (car nums)) parity) (cons (car nums) (same-parity-iter parity (cdr nums)))]
        [else (same-parity-iter parity (cdr nums))]))

(define (same-parity x . n)
    (same-parity-iter (parity? x) (cons x n)))