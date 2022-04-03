#lang sicp

; Exercise 4.52
; Implement a new construct called if-fail that permits the user
; to catch the failure of an expression. if-fail takes two expressions. It
; evaluates the first expres- sion as usual and returns as usual if the
; evaluation suc- ceeds. If the evaluation fails, however, the value of the second
; expression is returned, as in the following example:
 
;;;; Amb-Eval input:
;(if-fail (let ((x (an-element-of '(1 3 5))))
;           (require (even? x))
;           x)
;         'all-odd)
;;;; Starting a new problem
;;;; Amb-Eval value:
;all-odd

;;;; Amb-Eval input:
;(if-fail (let ((x (an-element-of '(1 3 5 8))))
;           (require (even? x))
;           x)
;         'all-odd)
;;;; Starting a new problem
;;;; Amb-Eval value:
;8

; (if-fail <exp> <alt>)
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-exp exp)
  (cadr exp))
(define (if-fail-alt exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (lambda (env succeed fail)
    ((analyze (if-fail-exp exp))
     env succeed
     (lambda ()
       ((analyze (if-fail-alt exp))
        env succeed fail)))))







