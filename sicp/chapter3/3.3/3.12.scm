#lang sicp

;; Exercise 3.12

;--------------------------

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

;;Consider the interaction
(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
;z
;;OUTPUT: (a b c d)
;(cdr x)
;;OUTPUT: (b)
(define w (append! x y))
;w
;;OUTPUT: (a b c d)
;(cdr x)
;;OUTPUT: (b c d)

(#%provide last-pair append!)