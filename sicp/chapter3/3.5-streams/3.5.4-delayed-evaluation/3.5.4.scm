#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")

;----------------------------------------

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream
     initial-value
     (let ((integrand (force delayed-integrand)))
       (add-streams (scale-stream integrand dt) int))))
  int)


; solve procedure from the book doesn't work in DrRacket implementation
; of Scheme, so it had to be modified slightly.
(define (solve f y0 dt)
  ((lambda ()
     (let (
           (y 1)
           (dy 1)
           )
       (set! y (integral (delay dy) y0 dt))
       (set! dy (stream-map f y))
       y
       )
     )
   )
  )

;--------------------------------------------
(#%provide solve)