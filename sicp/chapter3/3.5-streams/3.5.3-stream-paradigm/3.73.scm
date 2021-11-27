#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.73: We can model electrical circuits using streams
;to represent the values of currents or voltages at a sequence
;of times. ...

;------------------------------------------------------------

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (lambda (stream v0)
    (add-streams
     (scale-stream stream R)
     (integral (scale-stream stream (/ 1.0 C))  v0 dt)
     )
    )
  )

(define RC1 (RC 5 1 0.5))