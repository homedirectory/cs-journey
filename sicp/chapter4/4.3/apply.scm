#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")

; (apply <proc> (list <arg1> ... <argn>))
(define (apply? exp)
  (tagged-list? exp 'apply))

(define (apply-proc exp) (cadr exp))
(define (apply-args exp) (cdaddr exp))

(define (apply->application exp)
  (append (list (apply-proc exp)) (apply-args exp)))


;-------------------------------------------------------------------------------
(#%provide (all-defined))