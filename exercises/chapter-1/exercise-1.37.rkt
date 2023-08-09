#lang racket

;; a ->
(define (psi-n i)
  1)
(define (psi-d i)
  1)

;; recursive cont-frac
(define (cont-frac-rec n d k)
  (define (frac-rec i)
    (if (= i k) (/ (n i) (d i)) (/ (n i) (+ (d i) (frac-rec (+ i 1))))))
  (frac-rec 1))

;; iterative cont-frac
(define (cont-frac-iter n d k)
  (define (frac-iter i result)
    (cond
      [(= i 0) result]
      [else (frac-iter (- i 1) (/ (n i) (+ (d i) result)))]))
  (frac-iter k (/ (n k) (d k))))
