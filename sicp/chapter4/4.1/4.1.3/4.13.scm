#lang sicp

(#%require "../core.scm")
(#%require "4.12.scm")

; Exercise 4.13
; A way to get rid of bindings.

;------------------------------------------------------------
; Unbind the first found symbol.

; form: (cons 'make-unbound! <var>)
(define (unbind? exp)
  (tagged-list? exp 'make-unbound!))
(define (unbind-var exp)
  (cdr exp))
(define (unbind var env)
  (let (
        ; not the best way to remove an element from a list
        (found-action (lambda (res frame) (begin (set-car! res '()) (set-cdr! res '()))))
        (miss-action (error "Undefined variable" var))
        )
    (search-env var env found-action miss-action)
    ))

