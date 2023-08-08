#lang racket

(define threshhold 0.0001)
(define (average x y)
  (/ (+ x y) 2))
(define (close-enough? neg pos)
  (<= (abs (- neg pos)) threshhold))

(define (search f neg-point pos-point)
  (let ([mid-point (average neg-point pos-point)])
    (let ([f-mid (f mid-point)])
      (cond
        [(close-enough? neg-point pos-point) mid-point]
        [(= f-mid 0) mid-point]
        [(< f-mid 0) (search f mid-point pos-point)]
        [(> f-mid 0) (search f neg-point mid-point)]))))

(define (half-interval-method f a b)
  (let ([a-val (f a)] [b-val (f b)])
    (cond
      [(and (positive? a-val) (negative? b-val)) (search f b a)]
      [(and (negative? a-val) (positive? b-val)) (search f a b)]
      [else (error "values are not of opposite sign!" a b)])))
