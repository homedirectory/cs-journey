#lang sicp

(#%require "./inf-stream.scm")
(#%require "../stream.scm")
(#%require "./3.59.scm")

;Exercise 3.60: With power series represented as streams
;of coefficients as in Exercise 3.59, adding series is imple-
;mented by add-streams. Complete the definition of the fol-
;lowing procedure for multiplying series:


(define (mul-series s1 s2)
  (cons-stream
   (* (stream-car s1) (stream-car s2))
   (add-streams
    (mul-series s1 (stream-cdr s2))
    (mul-series (stream-cdr s1) s2)
    (stream-neg (cons-stream
                 0
                 (mul-series (stream-cdr s1) (stream-cdr s2))
                 )
                )
    )
   )
  )

; TEST
(define s (add-streams
           (mul-series sine-series sine-series)
           (mul-series cosine-series cosine-series)
           )
  )

;(print-stream s 6)

;------------------------------------------------
(#%provide mul-series)