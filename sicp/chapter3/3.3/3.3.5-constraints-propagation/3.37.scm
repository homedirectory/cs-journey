#lang sicp

;;Exercise 3.37: The celsius-fahrenheit-converter pro-
;;cedure is cumbersome when compared with a more expression-
;;oriented style of definition, such as

;(define (celsius-fahrenheit-converter x)
;  (c+ (c* (c/ (cv 9) (cv 5))
;          x)
;      (cv 32)))
;(define C (make-connector))
;(define F (celsius-fahrenheit-converter C))

;;Here c+, c*, etc. are the “constraint” versions of the arith-
;;metic operations. For example, c+ takes two connectors as
;;arguments and returns a connector that is related to these
;;by an adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z)
  )

;;Define analogous procedures c-, c*, c/, and cv (constant
;;value) that enable us to define compound constraints as in
;;the converter example above.

(#%require "./constraint-network.scm")

;----------------------------------------
;; divider
(define (divider dividend divisor result)

  (define (process-new-value)
    (cond ((and (has-value? dividend) (has-value? divisor))
           (set-value! result
                       (/ (get-value dividend) (get-value divisor))
                       me))
          ((and (has-value? result) (has-value? dividend))
           (set-value! divisor
                       (/ (get-value result) (get-value dividend))
                       me))
          ((and (has-value? result) (has-value? divisor))
           (set-value! dividend
                       (* (get-value result) (get-value divisor))
                       me))
          )
    )

  (define (process-forget-value)
    (forget-value! result me)
    (forget-value! dividend me)
    (forget-value! divisor me)
    (process-new-value)
    )
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: DIVIDER" request))
          )
    )

  (connect dividend me)
  (connect divisor me)
  (connect result me)
  
  me
  )


(define (c* m1 m2)
  (let ((p (make-connector)))
    (multiplier m1 m2 p)
    p)
  )

(define (c/ dividend divisor)
  (let ((result (make-connector)))
    (divider dividend divisor result)
    result)
  )

(define (cv value)
  (let ((c (make-connector)))
    (constant value c)
    c)
  )

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

;; TEST
(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(probe 'celcius C)
(probe 'fahrenheit F)

(set-value! F 100 'user) ;; C = 340 / 9 = 37.7
(forget-value! F 'user)
(set-value! C 20 'user) ;; F = 68