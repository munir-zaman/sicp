#lang racket

(define (linear-expt a n)
  (if (or (= a 1) (= n 0)) 1 (* a (linear-expt a (sub1 n)))))

(define (square a) (* a a))
(define (half n) (/ n 2))

(define (bin-expt a n)
  (cond
    [(or (= a 1) (= n 0)) 1]
    [(= (remainder n 2) 0) (square (bin-expt a (half n)))]
    [else (* a (square (bin-expt a (half (sub1 n)))))]))


;; TODO: FIX THIS >:(
(define (bin-expt-iter s a n)
  (cond
    [(= n 0) s]
    [(= (remainder n 2) 0) (bin-expt-iter (bin-expt-iter s a (half n)) a (half n))]
    [else (bin-expt-iter (* s a) a (sub1 n))]))
