#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence) initial (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (enumerate-interval n m)
  (cond
    [(= n m) (list n)]
    [(> m n) (cons n (enumerate-interval (+ n 1) m))]
    [else (error "The first arg should be larger than the second arg!")]))

(define (generate-pairs n)
  (accumulate append
              nil
              (map (lambda (j i) (map (lambda (x) (cons x i)) j))
                   (map (lambda (i) (enumerate-interval 1 (- i 1))) (enumerate-interval 2 n))
                   (enumerate-interval 1 n))))
