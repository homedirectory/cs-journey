#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.74.scm")

;Exercise 3.76: Eva Lu Ator has a criticism of Louis’s ap-
;proach in Exercise 3.75. The program he wrote is not mod-
;ular, because it intermixes the operation of smoothing with
;the zero-crossing extraction. For example, the extractor should
;not have to be changed if Alyssa finds a better way to con-
;dition her input signal. Help Louis by writing a procedure
;smooth that takes a stream as input and produces a stream
;in which each element is the average of two successive in-
;put stream elements. Then use smooth as a component to
;implement the zero-crossing detector in a more modular
;style.

;-----------------------------------------------------------

(define (smooth stream)
  (stream-map
   (lambda (x y) (average x y))
   stream
   (stream-cdr stream)
   )
  )

(define (smoothed-zero-crossings input-stream)
  (make-zero-crossings (smooth input-stream))
  )

; TEST
(define zc (smoothed-zero-crossings
            (list->stream (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))))






