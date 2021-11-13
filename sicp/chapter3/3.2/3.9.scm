#lang sicp

;;Exercise 3.9
;;Show the environment structures created by evaluating
;;(factorial 6) using each version of the factorial procedure.

;---------------------------------------------------

;; recursive
(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
      )
  )

;; global env (factorial 5)
;; E1 [n:5] (* n (factorial (- n 1))) -> GLOBAL
;; E2 [n:4] (* n (factorial (- n 1))) -> GLOBAL
;; E3 ...

;; iterative
(define (factorial-iter n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)
      )
  )

;; global env (factorial-iter 5)
;; E1 [n:5] (fact-iter 1 1 n) -> GLOBAL
;; E2 [product:1, counter:1, max-count:5]
;;    (fact-iter (* counter product) (inc counter) max-count) -> GLOBAL
;; E3 ...
