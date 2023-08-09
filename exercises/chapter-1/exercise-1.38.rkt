#lang racket

;; iterative cont-frac
(define (cont-frac n d k)
  (define (frac-iter i result)
    (cond
      [(= i 0) result]
      [else (frac-iter (- i 1) (/ (n i) (+ (d i) result)))]))
  (frac-iter k (/ (n k) (d k))))

(define mod remainder)

(define (euler-N i)
  1)
(define (euler-D i)
  (cond
    [(= (mod i 3) 2) (* 2 (ceiling (/ i 3)))]
    [else 1]))

(define (e-cont-frac k)
  (+ 2 (cont-frac euler-N euler-D k)))
