#lang racket

;; iterative cont-frac
(define (cont-frac n d k)
  (define (frac-iter i result)
    (cond
      [(= i 0) result]
      [else (frac-iter (- i 1) (/ (n i) (+ (d i) result)))]))
  (frac-iter k (/ (n k) (d k))))

(define (tan-cf x k)

  (define (tan-N i)
    (if (= i 1) x (- (* x x))))

  (define (tan-D i)
    (- (* 2 i) 1))

  (cont-frac tan-N tan-D k))
