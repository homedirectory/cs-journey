#lang sicp

(#%require "../stream.scm")
(#%require "./inf-stream.scm")

;Exercise 3.53: Without running the program, describe the
;elements of the stream defined by

(define s (cons-stream 1 (add-streams s s)))

;----------------------------------------------------

; s = (1, s + s)
; s = (1, 2, 4, 8, 16, 32, 64, ...)
(print-stream s 10)

