; helper DSL to make implementation and representation of exercises more
; enjoyable
#lang sicp

(define (print . args)

  (define (pprint args-list)
    (if (null? args-list)
        (newline)
        (and (display (car args-list))
             (pprint (cdr args-list)))
        )
    )

  (pprint args)
  )

(define (square x) (* x x))

(define (sum args)
  (define (f lst s)
    (if (null? lst)
        s
        (f (cdr lst) (+ s (car lst)))
        )
    )
  (f args 0)
  )

(define (average . args)
  (/ (sum args) (length args))
  )


; pow
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))
        )
  )


(define (expt x n)
  (fast-expt-iter x n 1)
  )


(define (divisible? x y)
  (= (remainder x y) 0)
  )

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;---------------------------------------------
 
(#%provide print square average sum expt divisible? tagged-list?)
