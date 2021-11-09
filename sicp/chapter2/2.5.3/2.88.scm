#lang sicp

;;Exercise 2.88: Extend the polynomial system to include
;;subtraction of polynomials. (Hint: You may find it helpful
;;to define a generic negation operation.)

(#%require "./poly.scm")

;----------------------------------------------------

;; it is assumed that neg operation is defined for every type that can act as a coefficient

;; generic negation
(define (neg x)
  (apply-generic 'neg x)
  )

;; negate each term's coeff
(define (neg-term term)
  (make-term (order term) (neg (coeff term)))
  )

(define (neg-poly p)
  (let (
        (terms (term-list p))
        (var (variable p))
        )
    (make-poly var (map neg-term terms))
    )
  )

;; p1 - p2 = p1 + (-p2)
(define (sub-poly p1 p2)
  (add-poly p1 (neg-poly p2))
  )

(define (sub-terms L1 L2)
  (add-terms L1 (neg-terms
                 )

             ;; addition to the polynomial package
             (put 'sub '(polynomial polynomial)
                  (lambda (p1 p2) (tag (sub-poly p1 p2))))

             