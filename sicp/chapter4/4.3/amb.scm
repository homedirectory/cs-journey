#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;-------------------------------------------------------------------------------
(#%provide (all-defined))