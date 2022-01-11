#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")
;---------------------------------------------------------------

; begin packages a sequence of expressions into a single expression.
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cadr exp))
(define (make-begin seq) (list 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

;---------------------------------------------------------------
(#%provide (all-defined))