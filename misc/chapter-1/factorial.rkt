#lang racket

(define (factorial n)
  (define (factorial-iter counter product)
    (if (= counter n) product (factorial-iter (add1 counter) (* product (add1 counter)))))
  (factorial-iter 1 1))
