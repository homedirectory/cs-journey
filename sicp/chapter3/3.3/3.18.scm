#lang sicp

;;Exercise 3.18: Write a procedure that examines a list and
;;determines whether it contains a cycle, that is, whether a
;;program that tried to find the end of the list by taking suc-
;;cessive cdrs would go into an infinite loop. Exercise 3.13
;;constructed such lists.

(#%require "./3.12.scm") ;; last-pair
(#%require "./3.13.scm") ;; make-cycle

;---------------------------------------------------

(define x (list 1 2 3))
(define xc (make-cycle x))
(define a (list 1 2 3 4))

;; non-constant amount of space
;; store each element of a list and check whether cdr is in storage
(define (cycle? lst)
  (let ((storage (list)))

    (define (store x)
      (if (null? x)
          nil
          (set! storage (append storage (list x)))
          )
      )

    (define (exists? x)
      (define (iter strg)
        (cond
          ((null? strg) #f)
          ((eq? x (car strg)) #t)
          (else (iter (cdr strg)))
          )
        )
      (iter storage)
      )

    (define (main x)
;      (display x)
;      (newline)
      (if (null? x)
          #f
          (if (exists? (cdr x))
              #t
              (begin
                (store x)
                (main (cdr x))
                )
              )
          )
      )

    (main lst)
    )
  )
