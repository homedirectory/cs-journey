#lang sicp

;;Exercise 2.87: Install =zero? for polynomials in the generic
;;arithmetic package. This will allow adjoin-term to work
;;for polynomials with coefficients that are themselves poly-
;;nomials.

(#%require "./poly.scm")

;----------------------------------------------------


(define (=zero? poly) 
  (if (empty-termlist? poly) 
      (the-empty-termlist) 
      (if (=zero? (coeff (first-term poly))) 
          (=zero? (rest-terms poly)) 
          #f))) 

;; addition to the polynomial package
(put '=zero? 'polynomial
     (lambda (p) (tag (=zero? p))))