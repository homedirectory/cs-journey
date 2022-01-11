#lang sicp

(#%require "../../../helpers.scm"
           "../core-helpers.scm" "../lambda.scm")

;Exercise 4.6: Implement a syntactic transformation let->combination
;that reduces evaluating let expressions to evaluating com-
;binations of the type shown above, and add the appropriate
;clause to eval to handle let expressions.

;(let ((<var1> <exp1>) ... (<varn> <expn>))
;  <body>)

;is equivalent to

;((lambda (<var1> ... <varn>)
;   <body>)
; <exp1>
; ...
; <expn>)

;----------------------------------------------------------
; let expression has the form of:
; ('let <statements> <body>)
; <statements>: (<stat1> ... <statn>)
; <stati>: (<vari> <expi>)
; <body>: sequence of expression
(define (let? exp)
  (tagged-list? exp 'let)
  )
(define (let-statements exp)
  (cadr exp)
  )
(define (let-stat-var stat)
  (car stat)
  )
(define (let-stat-exp stat)
  (cadr stat)
  )
(define (let-body exp)
  (caddr exp)
  )

(define (let->combination exp)
  (let (
        (parameters (map let-stat-var (let-statements exp)))
        (body (let-body exp))
        (arguments (map let-stat-exp (let-statements exp)))
        )
    (make-application
     (make-lambda parameters body)
     arguments
     )
    )
  )

(define (make-let statements body)
  (list 'let statements body)
  )


