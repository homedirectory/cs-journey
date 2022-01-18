#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "define.scm"
           "lambda.scm")

; Exercise 4.6
; let expression has the form of:
; ('let <statements> <body>)
; <statements>: (<stat1> ... <statn>) ;; (list)
; <stati>: (<vari> <expi>) ;; (list)
; <body>: sequence of expression
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-statements exp)
  (cadr exp))
(define (let-stat-var stat)
  (car stat))
(define (let-stat-exp stat)
  (cadr stat))
(define (let-body exp)
  (cddr exp))
(define (make-let statements body)
  (append (list 'let statements) body))

; Exercise 4.7
; let*
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (iter statements body)
    (if (null? (cdr statements))
        (make-let (car statements) body)
        (make-let (car statements) (iter (cdr statements) body))
        )
    )

  (iter (let-statements exp) (let-body exp))
  )

; Exercise 4.8
; named let
;(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)
(define (named-let? exp)
  (symbol? (cadr exp)))
(define (let->combination exp)
  ;; named let?
  (if (named-let? exp)
      (let (
            (var (cadr exp))
            (parameters (map let-stat-var (caddr exp)))
            (arguments (map let-stat-exp (caddr exp)))
            (body (cdddr exp))
            )
        (let ((lambda-body (list
                            (make-definition (cons var parameters) body)
                            (make-application var arguments))))
          ; create lambda with no parameters and call it immediately
          ; so that the name used by named let is only visible inside this lambda
          ; the body of this lambda consists of 2 expressions
          ; 1. define the procedure
          ; 2. call it with supplied arguments extracted from named let bindings
          (make-application
           (make-lambda '() lambda-body)
           '()
           )
          )
        )
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
  )

; Exercise 4.20: letrec
; letrec has the form:
; (letrec ((<var1> <exp1>) ... (<varn> <expn>)) <body>)
; (list 'letrec statements body)

(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec-statements exp)
  (cadr exp))
(define (letrec-body exp)
  (cddr exp))
(define (make-letrec statements body)
  (append (list 'letrec statements) body))

; a. letrec as a derived expression (letrec -> let)
(define (letrec->let exp)
  (define (new-statements statements)
    (map (lambda (s)
           (list (let-stat-var s) (make-quotation '*unassigned*)))
         statements))
  (define (new-body e)
    (let ((orig-statements (letrec-statements e))
          (orig-body (letrec-body e)))
      (append
       (map (lambda (s) (list 'set! (let-stat-var s) (let-stat-exp s))) orig-statements)
       orig-body)
      ))
  
  (make-let
   (new-statements (letrec-statements exp))
   (new-body exp))
  )


(#%provide (all-defined))