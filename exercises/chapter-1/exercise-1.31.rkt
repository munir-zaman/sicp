#lang racket

;; recursive `prod`
(define (prod-recur term a next b)
  (if (> a b) 1 (* (term a) (prod term (next a) next b))))

;; iterative `prod`
(define (prod-iter term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (* result (term a)))))
  (iter a 1))

;; define which `prod` to use
(define prod prod-iter)

;; helpers
(define (inc x) (+ x 1))
(define (id x) x)
(define (square x) (* x x))

;; factorial
(define (fact n) (prod id 1 inc n))

;; n-th even and odd number
(define (even-n n) (* 2 n)) 
(define (odd-n n) (- (* 2 n) 1))

;; f(n) = 2n * 2(n+1) / (2n + 1)^2
(define (pi-prod-term n)
  (/ (* (even-n n) (even-n (inc n))) (square (odd-n (inc n)))))

;; pi / 4 = f(1) * f(2) * ...
(define (pi-prod n) (prod pi-prod-term 1 inc n))

(define pi-accuracy 1000)
(define pi (exact->inexact (* 4 (pi-prod pi-accuracy))))