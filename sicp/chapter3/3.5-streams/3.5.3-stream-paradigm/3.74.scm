#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.74: Alyssa P. Hacker is designing a system to
;process signals coming from physical sensors. One impor-
;tant feature she wishes to produce is a signal that describes
;the zero crossings of the input signal.

;-----------------------------------------------------------

(define (sign-change-detector new-val old-val)
  (cond
    ((and (>= new-val 0) (< old-val 0))
     1)
    ((and (< new-val 0) (>= old-val 0))
     -1)
    (else 0)
    )
  )

(define (make-zero-crossings input-stream)
  (stream-map sign-change-detector
              input-stream
              (cons-stream 0 input-stream)
              )
  )

; TEST
(define zc (make-zero-crossings
            (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))))

;---------------------------------------------
(#%provide sign-change-detector make-zero-crossings)