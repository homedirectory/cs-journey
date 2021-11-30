#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")
(#%require "./3.5.4.scm")

;Exercise 3.80: A series RLC circuit

;----------------------------------------------

; produces a pair of streams of states v_C and i_L
(define (RLC R L C dt)
  (lambda (vC0 iL0)
    ((lambda ()
       (let (
             (vC 1.0)
             (iL 1.0)
             (dvC 1.0)
             (diL 1.0)
             )
         (set! vC (integral (delay dvC) vC0 dt))
         (set! iL (integral (delay diL) iL0 dt))
         (set! dvC (scale-stream iL (- (/ 1.0 C))))
         (set! diL (add-streams
                    (scale-stream vC (/ 1.0 L))
                    (scale-stream iL (- (/ R L)))))
         ; return
         (cons vC iL)
         )
       ))
    )
  )

; could this be implemented in a more general way using `solve` procedure?

; TEST
(define pair ((RLC 1 1 0.2 0.1) 10 0))
(define voltage-stream (car pair))
(define current-stream (cdr pair))