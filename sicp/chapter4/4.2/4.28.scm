#lang sicp

; Exercise 4.28:
; eval uses actual-value rather than eval to evaluate the operator
; before passing it to apply, in order to force the value of the operator.
; Give an example that demonstrates the need for this forcing.

(define (func proc x)
  (proc x))
(func inc 5)
; Here inc will be delayed since it is an argument, that is,
; it will become a thunk. Therefore, there is a need to force it.

