#lang sicp

(list 'a 'b 'c)
(define (memq item x)
  (cond
    [(null? x) false]
    [(eq? (car x) item) x]
    [else (memq item (cdr x))]))