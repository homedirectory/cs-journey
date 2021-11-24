#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.65: Use the series
;ln(2) = 1 - 1/2 + 1/3 - 1/4 + ...
;to compute three sequences of approximations to the natural
;logarithm of 2, in the same way we did above for π.
;How rapidly do these sequences converge?

;ln(2) ~ 0.6931471
;-----------------------------------------------------

; Ordinary convergence
(define ln2-summands
  (stream-map
   (lambda (x)
     (if (= (remainder x 2) 0)
         (- (/ 1.0 x))
         (/ 1.0 x)
         )
     )
   integers
   )
  )

(define ln2-stream
  (partial-sums ln2-summands)
  )

; Euler transform (n=15 is good)
(define ln2-euler (euler-transform ln2-stream))

; Accelerated (n=6 is good)
(define ln2-accel (accelerated-sequence euler-transform ln2-stream))