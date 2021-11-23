#lang sicp

(#%require "./inf-stream.scm")
(#%require "../stream.scm")
(#%require "./3.59.scm")
(#%require "./3.60.scm")

;Exercise 3.61: Let S be a power series (Exercise 3.59) whose
;constant term is 1. Suppose we want to find the power se-
;ries 1/S, that is, the series X such that S*X = 1. Write
;S = 1 + Sr where Sr is the part of S after the constant
;term.
;X = 1 - Sr * X

;------------------------------------

(define (invert-unit-series s)
  (define x
    (cons-stream
     1
     (stream-neg (mul-series (stream-cdr s) x))
     )
    )
  x
  )

; TEST
(define cosine-inv (invert-unit-series cosine-series))
;(print-stream sine-inv 8)
(define sx (mul-series cosine-series cosine-inv))
;(print-stream sx 8)

;-----------------------------------------------
(#%provide invert-unit-series)