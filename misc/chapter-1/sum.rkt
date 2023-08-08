#lang racket

; term a -> computes f(a)
; next a -> gets the next index in the sequence e.g. a -> a+1 -> ... -> b

(define (sum term a next b)
  (if (> a b) 0 (+ (term a) (sum term (next a) next b))))


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


