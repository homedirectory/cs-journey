#lang sicp

;;Exercise 3.33: Using primitive multiplier, adder, and con-
;;stant constraints, define a procedure averager that takes
;;three connectors a , b, and c as inputs and establishes the
;;constraint that the value of c is the average of the values of
;;a and b.

;        _____
; a --- |     |
;       | avg | --- c
; b --- |_____|

(#%require "./constraint-network.scm")

;---------------------------------------------------

(define (averager a b c)

  (define (avg x y)
    (/ (+ x y) 2)
    )

  (define (avg-inv x res)
    (- (* 2 res) x)
    )

  (define (process-new-value)
    (cond ((and (has-value? a) (has-value? b))
           (set-value! c
                       (avg (get-value a) (get-value b))
                            me))
           ((and (has-value? a) (has-value? c))
            (set-value! b
                        (avg-inv (get-value a) (get-value c))
                        me))
           ((and (has-value? b) (has-value? c))
            (set-value! a
                        (avg-inv (get-value b) (get-value c))
                        me))
      )
    )

  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value)
    )

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: AVERAGER" request))
          )
    )

  (connect a me)
  (connect b me)
  (connect c me)
  me
  )


;; TEST
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(averager a b c)

(probe "a" a)
(probe "b" b)
(probe "c" c)

(set-value! a 98 'user)
(set-value! b 10 'user)