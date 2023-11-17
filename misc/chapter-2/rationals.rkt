#lang sicp

;; selectors and constructor
(define (make-rat n d) (cons n d))
(define (numer r) (car r))
(define (denom r) (cdr r))

;; x1/y1 + x2/y2 = (x1*y2 + y1*x2)/y1*y2
(define (add-rat x y)
  (make-rat
   (+
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

;; x1/y1 - x2/y2 = (x1*y2 - y1*x2)/y1*y2
(define (sub-rat x y)
  (make-rat
   (-
    (* (numer x) (denom y))
    (* (numer y) (denom x)))
   (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat
   (* (numer x) (numer y))
   (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat
   (* (numer x) (denom y))
   (* (denom x) (numer y))))

(define (equal-rat? x y)
  (=
   (* (numer x) (denom y))
   (* (denom x) (numer y))))
