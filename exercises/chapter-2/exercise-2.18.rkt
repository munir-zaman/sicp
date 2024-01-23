#lang sicp

(define (myreverse x)
    (cond [(null? x) x]
        [else (append (myreverse (cdr x)) (car x))]))