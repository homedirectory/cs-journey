#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")

;Exercise 3.81: Exercise 3.6 discussed generalizing the random-
;number generator to allow one to reset the random-number
;sequence so as to produce repeatable sequences of “random”
;numbers. Produce a stream formulation of this same
;generator that operates on an input stream of requests to
;generate a new random number or to reset the sequence
;to a specified value and that produces the desired stream of
;random numbers. Don’t use assignment in your solution.

;----------------------------------------------
(define (rand-init)
  (random 1.0)
  )

(define (random-stream requests-stream)
  (stream-map
   (lambda (req)
     (cond
       ((eq? req 'generate)
        (rand-init))
       ; ('reset n)
       (else
        (cadr req))
       )
     )
   requests-stream)
  )

; TEST
(define requests (list->stream (list 'generate 'generate (list 'reset 4) 'generate)))
(define random-numbers (random-stream requests))