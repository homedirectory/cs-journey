#lang sicp

;Exercise 3.55: Define a procedure partial-sums that takes
;as argument a stream S and returns the stream whose ele-
;ments are S 0 , S 0 +S 1 , S0 +S 1 +S 2 , . . .. For example,
;(partial-sums integers) should be the stream 1, 3, 6, 10, 15, . . ..

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;-------------------------------------------

(define (partial-sums stream)
  (define s (cons-stream
             (stream-car stream)
             (add-streams s (stream-cdr stream))
             )
    )
  s
  )

; TEST
(define s1 (partial-sums integers))
(print-stream s1 10)