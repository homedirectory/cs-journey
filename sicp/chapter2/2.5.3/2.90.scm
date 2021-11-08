#lang sicp

;;Exercise 2.90: Suppose we want to have a polynomial sys-
;;tem that is efficient for both sparse and dense polynomials.
;;One way to do this is to allow both kinds of term-list repre-
;;sentations in our system. The situation is analogous to the
;;complex-number example of Section 2.4, where we allowed
;;both rectangular and polar representations. To do this we
;;must distinguish different types of term lists and make the
;;operations on term lists generic. Redesign the polynomial
;;system to implement this generalization. This is a major effort,
;;not a local change.

(#%require "./poly.scm")

;----------------------------------------------------

;; the idea is to make 2 packages:
;; example: x^4 + 2x^2 + x + 5
;; 1) sparse term-list (1 0 2 1 5)
;; 2) dense term-list ((4,1), (2,2), (1,1) (0,5))
;;
;; don't make 2 separate packages for polynomials!!!
;; only for term-lists
;; and DON'T duplicate methods that repeat

(define (install-sparse-term-list-package)
  ;; internal procedures
  (define (adjoin-term term term-list)

    (define (iter result counter)
      (if (> counter (length result))
          (iter (cons (make-term (length result) 0) result) (- counter 1))
          (cons term result)
          )
      )
  
    (if (=zero? (coeff term))
        term-list
        (iter term-list (order term))
        )
    )
  (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))

  ;; dont forget !!!
  
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;⟨procedures same-variable? and variable? from section 2.3.2 ⟩
  ;; representation of terms and term lists
  ;⟨procedures adjoin-term . . . coeff from text below ⟩
  ;⟨procedures used by add-poly⟩  
  ;⟨procedures used by mul-poly⟩
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  'done)
