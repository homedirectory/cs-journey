#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")

;Exercise 3.64: Write a procedure stream-limit that takes
;as arguments a stream and a number (the tolerance). It should
;examine the stream until it finds two successive elements
;that differ in absolute value by less than the tolerance, and
;return the second of the two elements. Using this, we could
;compute square roots up to a given tolerance

;------------------------------------------------------------------

(define (stream-limit stream tolerance)
  (let ((first (stream-car stream))
        (rest (stream-cdr stream)))
        (if (< (abs (- first (stream-car rest))) tolerance)
            (stream-car rest)
            (stream-limit rest tolerance)
            )
        )
    )

; TEST
(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance)
  )

(define sqrt-ten (sqrt 10 0.001))