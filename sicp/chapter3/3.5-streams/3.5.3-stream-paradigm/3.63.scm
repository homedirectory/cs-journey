#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.63: Louis Reasoner asks why the sqrt-stream
;procedure was not written in the following more straight-
;forward way, without the local variable guesses:

(define (sqrt-stream x)
  (cons-stream 1.0 (stream-map
                    (lambda (guess)
                      (newline)
                      (print "guess: " guess)
                      (sqrt-improve guess x))
                    (sqrt-stream x))))

;Alyssa P. Hacker replies that this version of the procedure
;is considerably less efficient because it performs redundant
;computation. Explain Alyssa’s answer. Would the two ver-
;sions still differ in efficiency if our implementation of delay
;used only (lambda () ⟨exp ⟩) without using the optimiza-
;tion provided by memo-proc (Section 3.5.1)?

;----------------------------------------------------------

; This version indeed is less efficient, since each call to
; (sqrt-stream x) creates a whole new stream. Thus, memoization
; will have no effect. Accessing nth element of the stream will
; be done by computing all the previous n-1 elements.

; The two version would be the same in terms of efficiency had our
; implementation of delay not used memoization.