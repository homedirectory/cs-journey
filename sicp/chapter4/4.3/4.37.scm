#lang sicp

; Exercise 4.37
; Ben Bitdiddle claims that the following method for generating
; Pythagorean triples is more efficient than the one in Exercise 4.35.
; Is he correct?
; (Hint: Consider the number of possibilities that must be explored.)

;(define (a-pythagorean-triple-between low high)
;  (let ((i (an-integer-between low high))
;        (hsq (* high high)))
;    (let ((j (an-integer-between i high)))
;      (let ((ksq (+ (* i i) (* j j))))
;        (require (>= hsq ksq))
;        (let ((k (sqrt ksq)))
;          (require (integer? k))
;          (list i j k))))))

; - Ben is correct that this method is more efficient in terms of
; the amount of different timelines explored. Since it does not
; choose k "ambivalently" there is one point of choice less.