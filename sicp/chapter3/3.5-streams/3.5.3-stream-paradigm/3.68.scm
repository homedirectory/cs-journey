#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

; for debugging
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1))
       )
      )
  )

;Exercise 3.68: Louis Reasoner thinks that building a stream
;of pairs from three parts is unnecessarily complicated. In-
;stead of separating the pair (S0, T0) from the rest of the pairs
;in the first row, he proposes to work with the whole first
;row, as follows:

(define (pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (pairs (stream-cdr s) (stream-cdr t))
   )
  )

;Does this work? Consider what happens if we evaluate (pairs
;integers integers) using Louis’s definition of pairs.

;-------------------------------------------
;(define int-pairs (pairs integers integers))

; pairs procedure will go into an infinite loop, since the second
; argument to interleave will never get evaluated and thus the
; interleave procedure will never get to construct a stream
