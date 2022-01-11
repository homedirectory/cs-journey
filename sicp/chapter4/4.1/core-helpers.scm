#lang sicp

(#%require "../../helpers.scm")
;---------------------------------------------------------------

; The only self-evaluating items are numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; Variables are represented by symbols
(define (variable? exp) (symbol? exp))

; Quotations have the form (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (make-quotation text) (list 'quote text))

; Assignments have the form (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; applicaton is any compound expression that is not one of the above
; applicaton = (<operator> <operands list>)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; 4.1.3
; Testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; Exercise 4.5
(define (make-application operator operands)
  (cons operator operands)
  )


;--------------------------------------------------------------
(#%provide (all-defined))