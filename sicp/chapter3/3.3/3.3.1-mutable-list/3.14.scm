#lang sicp

;;Exercise 3.14: The following procedure is quite useful, al-
;;though obscure:
(define (mystery x)
  (define (loop x y)
    (display x)
    (display " ")
    (display y)
    (newline)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; 1: x = (a b c d), y = ()
;; temp = (b c d)
;; x = (a)
;; 2: x = (b c d), y =(a)
;; temp = (c d)
;; x = (b a)
;; 3: x = (c d), y  = (b a)
;; temp = (d)
;; x = (c b a)
;; 4: x = (d), y = (c b a)
;; temp = ()
;; x = (d c b a)
;; 5: return x
