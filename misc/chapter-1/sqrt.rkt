#lang racket

(define epsilon 0.00001)
(define (good-enough? guess x)
  (<= (abs (- x (* guess guess))) epsilon))

(define (new-good-enough? guess x)
  (<= (abs (- guess (improve-guess guess x))) epsilon))

(define (improve-guess guess x)
  (/ (+ guess (/ x guess)) 2))

(define (old-sqrt-iter guess x)
  (if (good-enough? guess x) guess (sqrt-iter (improve-guess guess x) x)))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess x) guess (sqrt-iter (improve-guess guess x) x)))

(define init-guess 1)
(define (sqrt x)
  (sqrt-iter init-guess x))
