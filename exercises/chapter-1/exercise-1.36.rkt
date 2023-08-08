#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (print guess)
    (newline)
    (let ([next-guess (f guess)]) (if (close-enough? next-guess guess) next-guess (try next-guess))))
  (try first-guess))

;; solve x^x = a
(define (solve a)
  (fixed-point (lambda (x) (/ (log a) (log x))) 2.0))

;; solve with average damping x^x = a
(define (average x y)
  (/ (+ x y) 2))

(define (solve-damp a)
  (fixed-point (lambda (x) (average x (/ (log a) (log x)))) 2.0))
