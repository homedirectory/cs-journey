#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")
(#%require "./3.5.4.scm")

;Exercise 3.79: Generalize the solve-2nd procedure of Ex-
;ercise 3.78 so that it can be used to solve general second-
;order differential equations d^2*y/dt^2 = f(dy/dt, y).

;----------------------------------------------------------

(define (solve-2nd f y0 dy0 dt)
  ((lambda ()
     (let (
           (y 1)
           (dy 1)
           (ddy 1)
           )
       (set! y (integral (delay dy) y0 dt))
       (set! dy (integral (delay ddy) dy0 dt))
       (set! ddy (stream-map f dy y))
       y
       )
     ))
  )

;---------------------------------------------------------
(#%provide solve-2nd)