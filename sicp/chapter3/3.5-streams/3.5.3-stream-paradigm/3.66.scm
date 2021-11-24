#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.66: Examine the stream (pairs integers integers).
;Can you make any general comments about the order in
;which the pairs are placed into the stream? For example,
;approximately how many pairs precede the pair (1, 100)?
;the pair (99, 100)? the pair (100, 100)? (If you can make pre-
;cise mathematical statements here, all the better. But feel
;free to give more qualitative answers if you find yourself
;getting bogged down.)


; A way to combine 2 infinite streams
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream
       (stream-car s1)
       (interleave s2 (stream-cdr s1))
       )
      )
  )

; Pairs of the form (i, j) of all integers, where i <= j
(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t))
    )
   )
  )

(define int-pairs (pairs integers integers))


(define powers-of-two
  (cons-stream
   1
   (stream-map
    (lambda (x) (expt 2 x))
    integers
    )
   )
  )

; index starts from 1
; pair (n, k)
(define (pair-index n k)
  (let ((sum-nn (sum (stream->list powers-of-two n))))
    (cond
      ((= n k) sum-nn)
      ((= k (inc n))
       (+ (expt 2 (dec n)) sum-nn))
      (else (+ sum-nn (expt 2 (dec n)) (* (expt 2 n) (- k n 1))))
      )
    )
  )

; how many pairs precede the pair (1, 100)?
;(pair-index 1 100)
; 198 - 1

; the pair (99, 100)?
;(pair-index 99 100)
; 950737950171172051122527404031 - 1

; the pair (100, 100)?
;(pair-index 100 100)
; 1267650600228229401496703205375 - 1