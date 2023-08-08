#lang racket

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b) null-value (combiner (term a) (accumulate-recur combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define accumulate accumulate-iter)

(define (sum term a next b) (accumulate + 0 term a next b))
(define (prod term a next b) (accumulate * 1 term a next b))

