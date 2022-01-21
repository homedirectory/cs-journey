#lang sicp

; Exercise 4.32: Give some examples that illustrate the difference
; between the streams of Chapter 3 and the “lazier” lazy lists
; described in this section. How can you take advantage of this extra laziness?

; The main difference lies in the lazy lists' ability to delay the evaluation of
; the element in the "car", that is, the head of the list.
; Using lazy lists in the below snipet:
(define (func x) (newline) (display "func ") (display x))
(define a (cons (func 1) 2))
; Evaluating a will not display "func 1", but evaluating (car a) will.

; I guess that the advantage of this method can be taken when you need the 1st
; item of the stream to be delayed, such as in the integrating method:
;(define (solve f y0 dt)
;  (define y (integral dy y0 dt))
;  (define dy (map f y))
;  y)