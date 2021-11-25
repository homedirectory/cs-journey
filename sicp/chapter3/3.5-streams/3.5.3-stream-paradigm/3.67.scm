#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.66.scm")

;Exercise 3.67: Modify the pairs procedure so that (pairs
;integers integers) will produce the stream of all pairs of
;integers (i, j) (without the condition i ≤ j). Hint: You will
;need to mix in an additional stream.

;-------------------------------------------------

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     )
    (pairs (stream-cdr s) (stream-cdr t))
    )
   )
  )

; Pairs of the form (i, j) of all integers
(define int-pairs (pairs integers integers))
