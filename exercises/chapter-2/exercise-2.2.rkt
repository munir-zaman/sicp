#lang sicp

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-point s) (car s))
(define (end-point s) (cdr s))
(define (print-segment s)
  (print-point (start-point s))
  (display " <--> ")
  (print-point (end-point s)))

(define (midpoint-segment s)
  (make-point
   (/ (+ (x-point (start-point s)) (x-point (end-point s))) 2)
   (/ (+ (y-point (start-point s)) (y-point (end-point s))) 2)))
