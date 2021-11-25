#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.66.scm")

;Exercise 3.70: It would be nice to be able to generate streams
;in which the pairs appear in some useful order, rather than
;in the order that results from an ad hoc interleaving pro-
;cess. We can use a technique similar to the merge procedure
;of Exercise 3.56, if we define a way to say that one pair of
;integers is “less than” another. One way to do this is to de-
;fine a “weighting function” W (i, j) and stipulate that (i1, j1)
;is less than (i2, j2) if W(i1, j1) < W(i2, j2).
;Write a procedure merge-weighted that is like merge, except that
;merge-weighted takes an additional argument weight, which is a
;procedure that computes the weight of a pair, and is used
;to determine the order in which elements should appear in
;the resulting merged stream.

;--------------------------------------------------------------------

(define (merge-weighted s1 s2 weight)
  (cond
    ((stream-null? s1) s2)
    ((stream-null? s2) s1)
    (else
     (let ((s1car (stream-car s1))
           (s2car (stream-car s2)))
       (cond
         ((< (weight s1car) (weight s2car))
          (cons-stream
           s1car
           (merge-weighted (stream-cdr s1) s2 weight))
          )
         ((> (weight s1car) (weight s2car))
          (cons-stream
           s2car
           (merge-weighted s1 (stream-cdr s2) weight))
          )
         (else
          (cons-stream
           s1car
           (merge-weighted (stream-cdr s1)
                           (stream-cdr s2) weight))
          )
         )
       )
     )
    )
  )


(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight
    )
   )
  )



; a. the stream of all pairs of positive integers (i, j) with
;i ≤ j ordered according to the sum i + j,
(define (sum p)
  (newline)
  (print "p: " p)
  (+ (car p) (cadr p))
  )
(define stream-a (weighted-pairs integers integers sum))

; b. the stream of all pairs of positive integers (i, j) with
;i ≤ j, where neither i nor j is divisible by 2, 3, or 5, and
;the pairs are ordered according to the sum 2i + 3j + 5ij.