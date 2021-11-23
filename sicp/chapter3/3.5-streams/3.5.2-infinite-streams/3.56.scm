#lang sicp

;Exercise 3.56: A famous problem, first raised by R. Ham-
;ming, is to enumerate, in ascending order with no repeti-
;tions, all positive integers with no prime factors other than
;2, 3, or 5. One obvious way to do this is to simply test each
;integer in turn to see whether it has any factors other than
;2, 3, and 5. But this is very inefficient, since, as the integers
;get larger, fewer and fewer of them fit the requirement.

;• S begins with 1.
;• The elements of (scale-stream S 2) are also ele-
;ments of S.
;• The same is true for (scale-stream S 3) and (scale-
;stream 5 S).
;• These are all the elements of S.

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;----------------------------------------------

(define s (cons-stream 1
                       (merge (scale-stream s 2)
                              (merge (scale-stream s 3)
                                     (scale-stream s 5))
                              )
                       )
  )

(print-stream s 20)