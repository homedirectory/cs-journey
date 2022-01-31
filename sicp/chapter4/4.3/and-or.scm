#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "if.scm")

;Exercise 4.3: Exercise 4.4: Recall the definitions of the special forms and
;and or from Chapter 1:
;...
;Install and and or as new special forms for the evaluator by
;defining appropriate syntax procedures and evaluation procedures
;eval-and and eval-or. Alternatively, show how to
;implement and and or as derived expressions.

;----------------------------------------------------------

; and expression is of the form:
; ('and <exp1> <exp2> ... <expn>)
(define (and? exp) (tagged-list? exp 'and))
(define (and-exps exp) (cdr exp))

; or expression is of the form:
; ('or <exp1> <exp2> ... <expn>)
(define (or? exp) (tagged-list? exp 'or))
(define (or-exps exp) (cdr exp))

; and as a derived expression
(define (and->if exp)
  (define (f exps)  
    (if (null? exps)
        'true
        (make-if (car exps)
                 (f (cdr exps))
                 'false)))
  (f (and-exps exp)))

; or as a derived expression
(define (or->if exp)
  (define (f exps)  
    (if (null? exps)
        'false
        (make-if (car exps)
                 'true
                 (f (cdr exps)))))
  (f (or-exps exp)))


;---------------------------------------------------------
(#%provide (all-defined))
