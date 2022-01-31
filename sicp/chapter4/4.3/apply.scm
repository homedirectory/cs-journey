#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm")

; (apply <proc> <args-list>)
(define (apply? exp)
  (tagged-list? exp 'apply))

(define (apply-proc exp) (cadr exp))
(define (apply-arg exp) (caddr exp))

(define (list? exp)
  (tagged-list? exp 'list))

; case 1: list is constructed in the argument
; (apply + (list 1 2 3))
;  |
;  |
;  v
; (+ 1 2 3)

; UNSUPPORTED (see note)
; case 2: list is passed as a variable
; (define lst (list 1 2 3))
; (apply + lst)
;  |
;  |
;  v
; (+ (list-ref 0 lst) (list-ref 1 lst) (list-ref 2 lst))
; Note: this would mean that the length of the list must be inferred somehow
; during the analysis phase...

(define (apply->application exp)
  (define (var->list-items var)
    (error "apply->application: arg not list is UNSUPPORTED"))
  
  (let ((proc (apply-proc exp))
        (arg (apply-arg exp)))
    (if (list? arg)
        (cons proc (cdr arg))
        (cons proc (var->list-items)))))

;-------------------------------------------------------------------------------
(#%provide (all-defined))