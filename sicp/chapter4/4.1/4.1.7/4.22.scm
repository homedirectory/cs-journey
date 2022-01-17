#lang sicp

; Exercise 4.22: Extend the evaluator in this section to support
; the special form let.

(define (make-procedure parameters body-proc env)
  (list 'procedure parameters body-proc env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze-application (let->combination exp))) ; <---
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))