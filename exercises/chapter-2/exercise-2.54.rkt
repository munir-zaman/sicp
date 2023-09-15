#lang sicp

(define (equal? p q)
  (cond
    [(and (null? p) (null? q)) #t] ;; both are null
    [(or  (null? p) (null? q)) #f] ;; one of them is null
    [(and (list? (car p)) (list? (car q))) ;; (car p) and (car q) are lists
     (and
      (equal? (car p) (car q))
      (equal? (cdr p) (cdr q)))]
    [else
     (and
      (eq? (car p) (car q))
      (equal? (cdr p) (cdr q)))])) ;; (car p) and (car q) are symbols