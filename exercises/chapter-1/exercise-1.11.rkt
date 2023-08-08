#lang racket

#|
    Exercise 1.11: A function f is deﬁned by the rule that

    f(n) =  {
                n if n < 3

                else,
                    f(n-1) + 2f(n-2) + 3f(n-3)
            }

    Write a procedure that computes f by means of a recursive
    process. Write a procedure that computes f by means of an
    iterative process.
|#

(define (recursive-f n)
  (cond
    [(< n 3) n]
    [else (+ (recursive-f (- n 1)) (* 2 (recursive-f (- n 2))) (* 3 (recursive-f (- n 3))))]))

(define (iter-f counter f0 f1 f2 n)
  (define f3 (+ f2 (* 2 f1) (* 3 f0)))
  (if (= counter n) f0 (iter-f (add1 counter) f1 f2 f3 n)))
