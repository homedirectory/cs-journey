#lang sicp

;;Exercise 3.21: Define a procedure print-queue that takes a queue as in-
;;put and prints the sequence of items in the queue.

(#%require "./queue.scm")

;---------------------------------------

(define (print-queue q)

  (define (print ptr)
    (display (car ptr))
    (if (not (null? (cdr ptr)))
        (begin
          (display ", ")
          (print (cdr ptr))
          )
        )
    )
  (display "(")
  (print (front-ptr q))
  (display ")")
  (newline)
  )

;; TEST
(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(insert-queue! q 'd)
(print-queue q) ;; (a, b, c, d)
(delete-queue! q)
(insert-queue! q 'z)
(print-queue q) ;; (b, c, d, z)
