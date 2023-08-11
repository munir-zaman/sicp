#lang sicp

(define (iterative-improve good-enough? improve-guess)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve-guess) (improve-guess guess)))))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define epsilon 0.00000001)
  (define (good-enough? guess)
    (< (abs (- x (square guess))) epsilon))
  (define (improve-guess guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) 1.0))

(define (fixed-point f)
  (define epsilon 0.000000001)
  (define (good-enough? guess)
    (< (abs (- (f guess) guess)) epsilon))
  (define (improve-guess guess)
    (average (f guess) guess))
  ((iterative-improve good-enough? improve-guess) 1.0))
