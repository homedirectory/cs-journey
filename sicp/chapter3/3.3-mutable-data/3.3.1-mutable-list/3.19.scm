#lang sicp

;;Exercise 3.19: Redo Exercise 3.18 using an algorithm that
;;takes only a constant amount of space. (This requires a very
;;clever idea.)

(#%require "./3.12.scm") ;; last-pair
(#%require "./3.13.scm") ;; make-cycle

;---------------------------------------------------

(define x (list 1 2 3))
(define xc (make-cycle x))
(define a (list 1 2 3 4))

;; constant amount of space
;; At each step we change the cdr of an element to its previous element.
;; This way if we encounter a cycle we are guaranteed to come back
;; to the beginning of the list.
;; The only disadvantage of this method is that it modifies the original list.
(define (cycle? lst)
  (if (null? lst)
      #f
      (let ((first (car lst)))
        
        (define (main x prev)
          (let ((real-cdr (cdr x)))
            (set-cdr! x prev)
            (cond
              ((null? real-cdr) #f)
              ((eq? (car real-cdr) first) #t)
              (else (main real-cdr x))
              )
            )
          )

        (main lst nil)
        )
      )
  )

(define b (list 1 2 3 4 5 6 7))
(set-cdr! (cdr (cddddr b)) (cdr b))
(cycle? b)