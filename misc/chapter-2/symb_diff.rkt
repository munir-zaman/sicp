#lang sicp

#|
    rules:
        (f + g)' = f' + g'
        (f * g)' = f g' + f' g
        (f / g)' = (g f' - f g') / g^2
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

|#

#| Date representation |#

(define (variable? x) (symbol? x))
(define (same-variable? x y) (eq? x y))

(define (sum? expr) (and (pair? expr) (eq? (car expr) '+)))
(define (addend expr) (cadr expr))
(define (augend expr) (caddr expr))
(define (make-sum expr1 expr2) (list '+ expr1 expr2))

(define (product? expr) (and (pair? expr) (eq? (car expr) '*)))
(define (multiplier expr) (cadr expr))
(define (multiplicand expr) (caddr expr))
(define (make-product expr1 expr2) (list '* expr1 expr2))

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
        [else (error "EXPRESSION TYPE UNKNOWN. CANNOT FIND THE DERIVATIVE")]))
