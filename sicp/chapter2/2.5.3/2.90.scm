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
;;
;; there are 2 methods that differ across 2 representations:
;; 1) adjoin-term    2) first-term
;; it is sufficient to make these generic

;; SPARSE PACKAGE
(define (install-sparse-terms-package)
  (define (tag x) (attach-tag 'sparse x))
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
  (define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))
  (define (make-term-list term-list) term-list)

  ;; install procedures
  ;; 1) we need to have a representation of a "term" for it to have a type
  (put 'adjoin-term '(term sparse)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term 'sparse
       (lambda (term-list) (tag (first-term term-list))))

  ;; 2) "term" has no representation in the table
  (put 'adjoin-term1 'sparse
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  
  (put 'make 'sparse
       (lambda (term-list) (tag (make-term-list term-list))))
  
  'done)


;; DENSE PACKAGE
(define (install-dense-terms-package)
  ;; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (first-term term-list) (car term-list))
  (define (make-term-list) term-list)

  ;; install procedures
  ;; 1) we need to have a representation of a "term" for it to have a type
  (put 'adjoin-term '(term dense)
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  (put 'first-term 'dense
       (lambda (term-list) (tag (first-term term-list))))

  ;; 2) "term" has no representation in the table
  (put 'adjoin-term1 'dense
       (lambda (term term-list) (tag (adjoin-term term term-list))))
  
  (put 'make 'dense
       (lambda (term-list) (tag (make-term-list term-list))))
  
  'done)


(define (make-sparse-term-list term-list)
  ((get 'make 'sparse) term-list))

(define (make-dense-term-list term-list)
  ((get 'make 'dense) term-list))


;; polynomial package that utilizes both term lists packages
(define (install-polynomial-package)
  ;; install both representations
  (install-sparse-terms-package)
  (install-dense-terms-package)
  ;; internal procedures
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  ;; THIS PIECE HERE IS IMPORTANT
  ;; 1) doing it this way requires some representation of "term" in the table
  (define (adjoin-term term term-list)
    (apply-generic 'adjoin-term term term-list)
    )
  (define (first-term term-list)
    (apply-generic 'first-term term-list)
    )

  ;; 2) this way we can work around "term" having no representation in the table
  (define (adjoin-term1 term term-list)
    ((get 'adjoin-term1 (type-tag term-list)) term term-list)
    )
  
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
