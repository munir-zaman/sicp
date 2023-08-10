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

;; (cubic a b c) -> (x |-> x^3 + ax^2 + bx + c)

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubic-solve a b c)
  (newtons-method (cubic a b c) 1.0))
