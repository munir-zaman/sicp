#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

; count-leaves of tree -> count-leaves of subtree -> accumulate
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (cond
                       [(not (pair? x)) 1]
                       [else (count-leaves x)]))
                   t)))
