#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ([next-guess (f guess)]) (if (close-enough? next-guess guess) next-guess (try next-guess))))
  (try first-guess))

;; y^2 = x -> y = x/y
(define (sqrt x)
  (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))
