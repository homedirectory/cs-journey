#lang sicp

;;Exercise 2.87: Install =zero? for polynomials in the generic
;;arithmetic package. This will allow adjoin-term to work
;;for polynomials with coefficients that are themselves poly-
;;nomials.

;; get the degree of a polynomial
;; since we assume that the term-list is an ordered set by the order of a term
; the degree of a polynomial is the order of the first term

(#%require "./poly.scm")

;----------------------------------------------------

(define (degree p)
  (order (first-term (term-list p)))
  )

(define (=zero? p)
  (and (eq? 0 (degree p)))
  )

;; addition to the polynomial package
(put '=zero? 'polynomial
       (lambda (p) (tag (=zero? p))))