#lang sicp

;;Exercise 3.50: Complete the following definition, which
;;generalizes stream-map to allow procedures that take mul-
;;tiple arguments.

(#%require "./stream.scm")

;----------------------------------------

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (apply stream-map (cons proc
                               (map (lambda (s) (stream-cdr s))
                                    argstreams)))
       )
      )
  )

;; TEST
(define s1 (stream-enumerate-interval 3 5))
(define s2 (stream-enumerate-interval 10 12))
(define s3 (stream-enumerate-interval 25 27))

(display-stream (stream-map + s1 s2 s3)) ; (38 41 44)

(display-stream (stream-map inc s1)) ; (4 5 6)