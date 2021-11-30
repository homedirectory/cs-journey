#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")
(#%require "./3.5.4.scm")

;Exercise 3.77: The integral procedure used above was
;analogous to the “implicit” definition of the infinite stream
;of integers in Section 3.5.2. Alternatively, we can give a def-
;inition of integral that is more like integers-starting-
;from (also in Section 3.5.2):

;When used in systems with loops, this procedure has the
;same problem as does our original version of integral.
;Modify the procedure so that it expects the integrand as
;a delayed argument and hence can be used in the solve
;procedure shown above.

;---------------------------------------------------------------

(define (integral delayed-integrand initial-value dt)
;  (print "delayed-integrand: " delayed-integrand)
;  (print "y0 :" initial-value)
  (cons-stream
   initial-value
   (if (stream-null? delayed-integrand)
       the-empty-stream
       (begin
         (let ((integrand (force delayed-integrand)))
;           (print "integrand: " integrand)
;           (newline)
           (integral (delay (stream-cdr integrand))
                     (+ (* dt (stream-car integrand))
                        initial-value)
                     dt)
           )
         )
       )
   )
  )

; TEST
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

(define a (solve (lambda (y) y) 1 0.001))