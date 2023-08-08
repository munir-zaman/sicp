#lang racket

(define (pascal n k)
  (cond
    [(= k 0) 1]
    [(= k n) 1]
    [(or (> k n) (< k 0)) 0]
    [else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))]))
