#lang sicp

; Exercise 4.21
; It is possible to specify a recursive procedure without using
; letrec or define

;((lambda (n)
;   ((lambda (fact) (fact fact n))
;    (lambda (ft k) (if (= k 1) 1 (* k (ft ft (- k 1)))))))
; 10)

; a. Devise an analogous expression for computing Fibonacci numbers.
; fib(13) = 144
((lambda (n)
   ((lambda (fib) (fib fib 0 1 (- n 2)))
    (lambda (fib a b k) (if (= k 0) b (fib fib b (+ a b) (- k 1))))))
 13)

; b. Devise a similar procedure to the one below without using letrec or define.
;(define (f x)
;  (define (even? n)
;    (if (= n 0) true (odd? (- n 1))))
;  (define (odd? n)
;    (if (= n 0) false (even? (- n 1))))
;  (even? x))

((lambda (x)
   ((lambda (even? odd?) (even? even? odd? x))
    (lambda (even? odd? n) (if (= n 0) true (odd? odd? even? (- n 1))))
    (lambda (odd? even? n) (if (= n 0) false (even? even? odd? (- n 1)))))
   )
   10)