#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")
;---------------------------------------------------------------

; Conditionals begin with if and have a predicate, consequent,
; and an (optional) alternative. No alternative = false as the alternative
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;---------------------------------------------------------------
(#%provide (all-defined))