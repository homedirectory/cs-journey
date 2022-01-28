#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")
;---------------------------------------------------------------

; begin packages a sequence of expressions into a single expression.
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (make-begin seq) (append (list 'begin) seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


;---------------------------------------------------------------
(#%provide (all-defined))