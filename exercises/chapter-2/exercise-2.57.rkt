#lang sicp

#|
    rules:
        (f + g)' = f' + g'
        (f * g)' = f g' + f' g
        (f / g)' = (g f' - f g') / g^2
        (f ** n)' = n * f**(n-1) f'
|#

#|
    Some wishful thinking

        (variable? x) -> is x a variable
        (same-variable? x y) -> are x and y the same
        (sum? x) -> is the expression a sum
        (addend e) -> if e = l + r then l
        (augend e) -> if e = l + r then r
        (make-sum l r) -> l + r
        (product? e) -> is the expression a product
        (multiplier e) -> e = l * r -> l
        (multiplicand e) -> e = l * r -> r
        (make-product l r) -> l * r
        (exponentiation? e) -> is the expression x^n
        (make-exponentiation? base exponent) -> (** base exponent)

|#

#| Date representation |#

(define (variable? x) (symbol? x))
(define (same-variable? x y) (eq? x y))

(define (sum? expr) (and (pair? expr) (eq? (car expr) '+)))
(define (addend expr) (cadr expr))
(define (augend expr) (caddr expr))

(define (make-sum . exprs)
    (cond [(pair? exprs) 
        (list '+ 
            (car exprs) 
            (apply make-sum (cdr exprs)))]
        [else exprs]))

;; TODO: HAVE TO FIX make-sum

(define (product? expr) (and (pair? expr) (eq? (car expr) '*)))
(define (multiplier expr) (cadr expr))
(define (multiplicand expr) (caddr expr))
(define (make-product expr1 expr2) (list '* expr1 expr2))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**) (number? (caddr e))))
(define (base e) (cadr e))
(define (exponent e) (caddr e))
(define (make-exponentiation base exponent)
  (list '** base exponent))

(define (derv expr var)
  (cond
    [(number? expr) 0]
    [(variable? expr)
     (if (same-variable? var expr) 1 0)]
    [(sum? expr)
     (make-sum
      (derv (addend expr) var)
      (derv (augend expr) var))]
    [(product? expr)
     (make-sum
      (make-product (multiplier expr) (derv (multiplicand expr) var))
      (make-product (multiplicand expr) (derv (multiplier expr) var)))]
    [(exponentiation? expr)
        (make-product 
            (exponent expr) 
            (make-product 
                (make-exponentiation (base expr) (- (exponent expr) 1))
                (derv (base expr) var)))]
    [else (error "EXPRESSION TYPE UNKNOWN. CANNOT FIND THE DERIVATIVE")]))

