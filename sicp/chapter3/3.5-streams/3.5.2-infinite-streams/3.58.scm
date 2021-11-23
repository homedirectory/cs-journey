#lang sicp

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;Exercise 3.58: Give an interpretation of the stream com-
;puted by the following procedure:

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;(quotient is a primitive that returns the integer quotient of
;two integers.) What are the successive elements produced
;by (expand 1 7 10)? What is produced by (expand 3 8 10)?;

(define s1 (expand 1 7 10)) ; 1 / 7
; (cons-stream
;  1
;  (expand 3 7 10))
;        |
;        v
; (cons-stream
;  4
;  (expand 2 7 10))
;        |
;        v
; (cons-stream
;  2
;  (expand 6 7 10))
;        |
;        v
; (cons-stream
;  8
;  (expand 4 7 10))
;        |
;        v
;       ...
(print-stream s1 10) ; 0.1428571428

(define s2 (expand 3 8 10)) ; 3 / 8
; (cons-stream
;  3
;  (expand 6 8 10))
;        |
;        v
; (cons-stream
;  7
;  (expand 4 8 10))
;        |
;        v
; (cons-stream
;  5
;  (expand 0 8 10))
;        |
;        v
;        0
;        |
;        v
;        0
;        |
;        v
;       ...
(print-stream s2 10) ; 0.375