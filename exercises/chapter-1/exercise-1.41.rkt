#lang racket

(define inc add1)
(define (double f) (lambda (x) (f (f x))))

;; (((double (double double)) inc) 5) -> 21