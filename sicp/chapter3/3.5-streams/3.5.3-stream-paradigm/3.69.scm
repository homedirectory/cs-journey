#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.66.scm")

;Exercise 3.69: Write a procedure triples that takes three
;infinite streams, S, T, and U and produces the stream of
;triples (Si, Tj, Uk) such that i ≤ j ≤ k. Use triples to gen-
;erate the stream of all Pythagorean triples of positive inte-
;gers, i.e., the triples (i, j, k) such that i ≤ j and i^2 + j^2 = k^2

;---------------------------------------------------------

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (p) (append (list (stream-car s)) p))
     (stream-cdr (pairs t u))
     )
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))
    )
   )
  )

;-------------------------------------
(define int-triples (triples integers integers integers))

(define pyth-triples
  (stream-filter
   (lambda (p) (= (+ (square (car p)) (square (cadr p))) (square (caddr p))))
   int-triples
   )
  )