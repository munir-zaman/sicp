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


; wiki -> https://en.wikipedia.org/wiki/Simpson%27s_rule

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k)
    (f (+ a (* k h)))) ; y_k = f(a + k*h)
  (define (term k)
    (cond
      [(= k 0) (y k)]
      [(= k n) (y k)]
      [(even? k) (* 2 (y k))]
      [else (* 4 (y k))]))
  (* (/ h 3) (sum term 0 inc n)))
