#lang sicp

;;Exercise 3.1: An accumulator is a procedure that is called
;;repeatedly with a single numeric argument and accumu-
;;lates its arguments into a sum. Each time it is called, it
;;returns the currently accumulated sum. Write a procedure
;;make-accumulator that generates accumulators, each main-
;;taining an independent sum. Thee input to make-accumulator
;;should specify the initial value of the sum.

;------------------------------------------

(define (make-accumulator initial)
  (lambda (x)
    (set! initial (+ initial x))
    initial
    )
  )