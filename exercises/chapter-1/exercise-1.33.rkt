#lang racket

(define (filtered-accumulate-recur filter? combiner null-value term a next b)
  (cond
    [(> a b) null-value]
    [(filter? a)
     (combiner (term a) (filtered-accumulate-recur filter? combiner null-value term (next a) next b))]
    [else (filtered-accumulate-recur filter? combiner null-value term (next a) next b)]))

(define (filtered-accumulate-iter filter? combiner null-value term a next b)
  (define (iter a result)
    (cond
      [(> a b) result]
      [(filter? a) (iter (next a) (combiner result (term a)))]
      [else (iter (next a) result)]))
  (iter a null-value))

(define filtered-accumulate filtered-accumulate-iter)

;; helper funcs

(define (id x)
  x)
(define inc add1)
(define (square x)
  (* x x))

(define (divisible? b a)
  (= (remainder b a) 0))

(define (prime? n)
  (define (iter p n)
    (if (>= p n) #t (and (not (divisible? n p)) (iter (inc p) n))))
  (if (= n 1) #f (iter 2 n)))

;; a ->
(define (sum-prime-squares a b)
  (filtered-accumulate prime? + 0 square a inc b))

;; b ->
(define (prod-coprimes n)
  (define (coprime? p)
    (= (gcd p n) 1))
  (filtered-accumulate coprime? * 1 id 1 inc n))
