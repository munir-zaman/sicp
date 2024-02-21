#lang sicp

(define (for-each proc items)
  (cond
    [(null? items) (error "Cant apply on an empty list.")]
    [(null? (cdr items)) (proc (car items))]
    [else
     (proc (car items))
     (for-each proc (cdr items))]))
