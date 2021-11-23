#lang sicp

;Exercise 3.54: Define a procedure mul-streams, analogous
;to add-streams, that produces the elementwise product of
;its two input streams. Use this together with the stream of
;integers to complete the following definition of the stream
;whose n th element (counting from 0) is n + 1 factorial:

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;-----------------------------------------------------

(define (mul-streams . argstreams)
  (apply stream-map (cons * argstreams))
  )


(define factorials
  (cons-stream 1 (mul-streams
                  factorials integers))
  )

(print-stream factorials 10)