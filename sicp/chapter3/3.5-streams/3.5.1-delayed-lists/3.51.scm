#lang sicp

;;Exercise 3.51

(#%require "../../../helpers.scm")
(#%require "stream.scm")

(define (show x)
  (print x)
  x)

(print "define x")
(define x
  (begin
    (stream-map show (stream-enumerate-interval 0 10))
    )
  )

(newline)
(print "stream-ref x 5")
(stream-ref x 5)

(newline)
(print "stream-ref x 7")
(stream-ref x 7)