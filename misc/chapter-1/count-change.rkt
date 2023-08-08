#lang racket

(define (count-change amount)
  (cc amount 5))
(define (cc amount kind-of-coins)
  (cond
    ;; base cases
    [(= amount 0) 1] ;; cc( 0, k) = 1
    [(< amount 0) 0] ;; cc(<0, k) = 1
    [(= kind-of-coins 0) 0] ;; cc(a, 0) = 0
    [else ;; cc(a, k) = cc(a, k-1) + cc(a - d, k)
     (+ (cc amount (- kind-of-coins 1))
        (cc (- amount (first-denomination kind-of-coins)) kind-of-coins))]))

#|
    basically a list that gives us the first kind of denomination
|#
(define (first-denomination kind-of-coins)
  (cond
    [(= kind-of-coins 1) 1]
    [(= kind-of-coins 2) 5]
    [(= kind-of-coins 3) 10]
    [(= kind-of-coins 4) 25]
    [(= kind-of-coins 5) 50]))


;; Challange: Turn this into an O(n) iterative procedure