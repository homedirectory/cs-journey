;;Exercise 3.51

(load "stream.mit.scm")

(define (print x)
  (display x)
  (newline)
  )

(define (show x)
  (print x)
  x)

(define x
  (begin
    (print 'a)
    (stream-map show (stream-enumerate-interval 0 10))
    )
  )

(stream-ref x 5) ;; 5
(stream-ref x 7) ;; 7
