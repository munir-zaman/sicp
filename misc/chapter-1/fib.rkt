#lang racket

(define (fib n)
  (define (fib-iter counter f1 f2)
    (if (= counter n) f1 (fib-iter (add1 counter) f2 (+ f1 f2))))
  (fib-iter 0 0 1))
