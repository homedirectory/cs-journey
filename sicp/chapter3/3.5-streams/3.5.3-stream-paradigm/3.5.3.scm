#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")


(define (sqrt-improve guess x)
  (average guess (/ x guess))
  )

; Iteration as a stream process
(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)


; Euler transform is a technique for accelerating the convergence of
; a sequence
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; S n-1
        (s1 (stream-ref s 1)) ; S n
        (s2 (stream-ref s 2)) ; S n+1
        ) 
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))
    )
  )


; A stream of streams (tableau). Each stream is the transform of the
; preceeding one
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s)))
  )

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s))
  )

; Approximation of Pi
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))
(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define pi-approx (accelerated-sequence euler-transform pi-stream))
;(print-stream pi-approx 10)


;------------------------------------------------
(#%provide sqrt-improve euler-transform make-tableau sqrt-stream
           accelerated-sequence)