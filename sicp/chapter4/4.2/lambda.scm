#lang sicp

(#%require "../../helpers.scm")
;---------------------------------------------------------------

; lambda expressions are lists that begin with the symbol lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (append (list 'lambda parameters) body))


;---------------------------------------------------------------
(#%provide (all-defined))
