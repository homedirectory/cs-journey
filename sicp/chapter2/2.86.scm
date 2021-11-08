#lang sicp

;;Exercise 2.86: Suppose we want to handle complex num-
;;bers whose real parts, imaginary parts, magnitudes, and an-
;;gles can be either ordinary numbers, rational numbers, or
;;other numbers we might wish to add to the system. De-
;;scribe and implement the changes to the system needed to
;;accommodate this. You will have to define operations such
;;as sine and cosine that are generic over ordinary numbers
;;and rational numbers.

;------------------------------------------------------------

;; as the book says it is necessary to define generic sin and cos

;; int (scheme-number) package
(define (sin-int x) (sin x))
(define (cos-int x) (cos x))

(put 'sin 'scheme-number (lambda (x) (tag (sin-int x))))
(put 'cos 'scheme-number (lambda (x) (tag (cos-int x))))

;; rational package
(define (sin-rat x) (sin (/ (numer x) (denom x))))
(define (cos-rat x) (cos (/ (numer x) (denom x))))

(put 'sin 'rational (lambda (x) (tag (sin-rat x))))
(put 'cos 'rational (lambda (x) (tag (cos-rat x))))

;; real package
;; same as integer
