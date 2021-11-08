#lang sicp

;;Exercise 2.83: Suppose you are designing a generic arith-
;;metic system for dealing with the tower of types shown in
;;Figure 2.25: integer, rational, real, complex. For each type
;;(except complex), design a procedure that raises objects of
;;that type one level in the tower. Show how to install a
;;generic raise operation that will work for each type (ex-
;;cept complex).

;; integer -> rational -> real -> complex

;; real number is just a complex number with imaginary part = 0

;---------------------------------------------

;; integer -> rational
(define (raise-int integer)
  (make-rational integer 1)
  )

;; rational -> real (complex, img part = 0)
(define (raise-rat rational)
  (make-complex-from-real-imag rational 0)
  )

;; real -> complex
;; ?

  

;;; generic raise operation

;; scheme-number package
(put 'raise 'scheme-number (lambda (x) (tag raise-int x)))

;; rational package
(put 'raise 'rational (lambda (x) (tag (raise-rat rational))))

;; real package?


(define (raise1 x) (apply-generic 'raise x))

;; another way
(define tower '(scheme-number rational complex))

(define (get-upper-type t)

  (define (iter twr)
    (if (null? twr)
        (error "Type not in tower" t)
        (if (equal? (type-tag (car twr)) t)
            (if (null? (cdr twr))
                t
                (cadr twr))
            (iter (cdr twr))
            )
        )
    )

  (iter tower)

  )

(define (raise x)
  (let ((type-to-raise-to (get-upper-type (type-tag x))))
    (if (equal? (type-tag x) type-to-raise-to)
        x
        ((get-coercion (type-tag x) type-to-raise-to) x)
        )
      )
  )

(#%provide raise)