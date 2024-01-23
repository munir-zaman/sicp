#lang sicp

(define (last-pair x)
  (if (null? x) (error "LIST IS NULL"))
  (if (= (length x) 1) (car x) (last-pair (cdr x))))
