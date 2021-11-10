#lang sicp

;;Exercise 2.94: Using div-terms, implement the procedure
;;remainder-terms and use this to define gcd-terms as above.
;;Now write a procedure gcd-poly that computes the poly-
;;nomial GCD of two polys. (The procedure should signal an
;;error if the two polys are not in the same variable.) Install in
;;the system a generic operation greatest-common-divisor
;;that reduces to gcd-poly for polynomials and to ordinary
;;gcd for ordinary numbers.

(#%require "./poly.scm")
(#%require "./2.91.scm") ; div-poly & div-terms

;-------------------------------------------------

(define (remainder-terms a b)
  (cadr (div-terms a b))
  )

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (gcd-poly p1 p2)
  (if (not (same-variable? p1 p2))
      (error "Polynomials are not in the same variable: GCD-POLY" p1 p2)
      (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
      )
  )

;; generic operation greatest-common-divisor

;; integers (ordinary numbers)
(put 'gcd '(scheme-number scheme-number)
     (lambda (a b) (tag (gcd a b))))

;; polynomials
(put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))

(define (greatest-common-divisor a b)
  (apply-generic 'gcd a b)
  )

