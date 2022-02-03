#lang sicp


; Exercise 4.51
; Implement a new kind of assignment called permanent-set! that is not undone upon
; failure. For example, we can choose two distinct elements from a list and count
; the number of trials required to make a successful choice as follows:
 
;(define count 0)
;(let ((x (an-element-of '(a b c)))
;      (y (an-element-of '(a b c))))
;  (permanent-set! count (+ count 1))
;  (require (not (eq? x y)))
;  (list x y count))
;;;; Starting a new problem
;;;; Amb-Eval value:
;(a b 2)
;;;; Amb-Eval input:
;try-again
;;;; Amb-Eval value:
;(a c 3)

; What values would have been displayed if we had used set! here rather than permanent-set! ?

; - count would have always been displayed as 1.

(define (set!? exp)
  (tagged-list? exp 'set!))
(define (permanent-set!? exp)
  (tagged-list? exp 'permanent-set!))

(define (assignment? exp)
  (or (set!? exp)
      (permanent-set!? exp)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (if (set!? exp)
        ; set!
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   ; *1*
                   (let ((old-value (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok
                              (lambda ()
                                ; *2*
                                (set-variable-value!
                                 var old-value env)
                                (fail2)))))
                 fail))
        ; permanent-set!
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   ; *1*
                   (let ((old-value (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok fail2)))
                 fail)))))



