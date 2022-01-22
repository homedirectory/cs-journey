#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "lambda.scm" "env.scm")
;---------------------------------------------------------------

; Definitions have the form:
; (define <var> <value>)
; or the form:
; (define (<var> <param1> ... <paramn>) <body>),
; which is the same as :
; (define <var> (lambda <param1> ... <paramn>) <body>))
(define (definition? exp) (tagged-list? exp 'define))

; Exercise 4.16
(define (definition-variable? exp)
  (symbol? (cadr exp)))

(define (definition-variable exp)
  (if (definition-variable? exp)
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (definition-variable? exp)
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body

; make-definition is used only in the underlying scheme (e.g. test purposes)
(define (make-definition var . exps)
  (if (= 1 (length exps))
      (list 'define var (car exps))
      (append (list 'define var) exps)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals))))
      )
    
    (scan (frame-variables frame) (frame-values frame))
    )
  )


;---------------------------------------------------------------
(#%provide (all-defined))