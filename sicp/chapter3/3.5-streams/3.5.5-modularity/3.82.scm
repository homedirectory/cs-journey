#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")
(#%require "./3.5.5.scm")

;Exercise 3.82: Redo Exercise 3.5 on Monte Carlo integra-
;tion in terms of streams. The stream version of estimate-
;integral will not have an argument telling how many tri-
;als to perform. Instead, it will produce a stream of estimates
;based on successively more trials.

;----------------------------------------------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))
    )
  )

(define (rect-area x1 y1 x2 y2)
  (* (- x2 x1) (- y2 y1))
  )

(define (estimate-integral p x1 y1 x2 y2)
  (define (p-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2))
    )

  (let ((experiment-stream (stream-from-proc p-test)))
    (let ((area (rect-area x1 y1 x2 y2)))
      (stream-map
       (lambda (x) (* x area))
       (monte-carlo-stream experiment-stream 0.0 0.0)
       )
      )
    )
  )


; TEST
(define pi-stream
  (let ((unit-circle-predicate
         (lambda (x y)
           (<= (+ (square x) (square y)) 1.0)
           )
         ))
    (estimate-integral unit-circle-predicate -1.0 -1.0 1.0 1.0)
    )
  )