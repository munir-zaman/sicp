#lang sicp

(define square (lambda (x) (* x x)))

(define (square-list-1 items)
  (if (null? items) nil (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))
