#lang sicp

;;Exercise 2.93: Modify the rational-arithmetic package to
;;use generic operations, but change make-rat so that it does
;;not attempt to reduce fractions to lowest terms. Test your
;;system by calling make-rational on two polynomials to
;;produce a rational function

(#%require "./poly.scm")

;-------------------------------------------------

;; rational numbers package
;; only modified procedures are included
(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d) (cons n d)) ;; no gcd here
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                 (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (eq?-rat x y)
    (=zero? (sub (mul (numer x) (denom y))
              (mul (numer y) (denom x))))
    )
  (define (=zero?-rat x) (=zero? (numer x)))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y) (tag (eq?-rat x y))))
  (put '=zero? 'rational
       (lambda (x) (tag (=zero?-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)