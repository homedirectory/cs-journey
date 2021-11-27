#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.70.scm")

;Exercise 3.71: Numbers that can be expressed as the sum of
;two cubes in more than one way are sometimes called Ra-
;manujan numbers, in honor of the mathematician Srinivasa
;Ramanujan. Ordered streams of pairs provide an elegant
;solution to the problem of computing these numbers. To
;find a number that can be written as the sum of two cubes
;in two different ways, we need only generate the stream of
;pairs of integers (i, j) weighted according to the sum i^3 + j^3,
;then search the stream for two consecutive pairs with the same
;weight. Write a procedure to generate the Ramanujan numbers.
;The first such number is 1,729. What are the next five?

;--------------------------------------------------------------------
(define (search-same-weight stream weight)
  (let ((first (stream-car stream))
        (rest (stream-cdr stream)))
    (if (= (weight first) (weight (stream-car rest)))
        (cons-stream
         first
         (search-same-weight (stream-cdr rest) weight)
         )
        (search-same-weight rest weight)
        )
    )
  )

(define (sum-cubes pair)
  (let ((i (car pair))
        (j (cadr pair)))
    (+ (expt i 3) (expt j 3))
    )
  )

(define stream-by-sum-cubes (weighted-pairs integers integers sum-cubes))
(define ramanujan-numbers (stream-map sum-cubes
                                      (search-same-weight stream-by-sum-cubes sum-cubes))
  )