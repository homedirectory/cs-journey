#lang sicp

; Exercise 4.25
; Suppose that (in ordinary applicative-order Scheme) we define unless
; as shown above and then define factorial in terms of unless as

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; What happens if we attempt to evaluate (factorial 5)?
; - infinite recursion, since unless will always evaluate its arguments first
; meaning that factorial will never yield a value.

; Will our definitions work in a normal-order language?
; Yes, it will. Here is the implementation.

(define-syntax unless-lazy
  (syntax-rules ()
    ((unless-lazy condition usual-value except-value)
     (if condition except-value usual-value))
    )
  )

(define (factorial-lazy n)
  (unless-lazy (= n 1)
    (* n (factorial-lazy (- n 1)))
    1))
