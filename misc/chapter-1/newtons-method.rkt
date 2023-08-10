#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? x y)
    (< (abs (- x y)) tolerance))
  (define (try guess)
    (let ([next-guess (f guess)]) (if (close-enough? next-guess guess) next-guess (try next-guess))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;; g(y) = y^2 - x
(define (sqrt-newton x)
  (newtons-method (lambda (y) (- (* y y) x)) 1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; fixed-point-of-transform (FPT) versions of sqrt

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))
(define (sqrt-fpt x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (square x)
  (* x x))
(define (sqrt-newton-fpt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x)) newton-transform 1.0))
