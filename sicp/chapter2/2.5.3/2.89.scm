#lang sicp

;;Exercise 2.89: Define procedures that implement the term-
;;list representation described above as appropriate for dense
;;polynomials.

(#%require "./poly.scm")

;----------------------------------------------------

;; modified
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

(define (the-empty-termlist) '())

;; modified
(define (first-term term-list) (make-term (- (length term-list) 1) (car term-list)))

(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))