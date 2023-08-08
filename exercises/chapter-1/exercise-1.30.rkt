#lang racket

;; iterative `sum`
(define (sum term a next b)
  (define (sum-iter a result)
    (if (> a b) result (sum-iter (next a) (+ result (term a)))))
  (sum-iter a 0))

(define (id x) x)
(define (inc x) (add1 x))
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (inverse x) (/ 1 x))

(define (sum-squares a b) (sum square a inc b))
(define (sum-cubes a b) (sum cube a inc b))
(define (sum-harmonic a b) (sum inverse a inc b))

(define (integral f a b)
  (define dx 0.00001)
  (define (add-dx x)
    (+ x dx)) ; x -> x+dx next
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))


