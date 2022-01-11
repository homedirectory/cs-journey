#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "if.scm" "lambda.scm" "begin.scm" "cond.scm" "let.scm"
           "apply.scm" "define.scm" "and-or.scm" "for.scm"
           "env.scm")
;---------------------------------------------------------------

(define apply-in-underlying-scheme apply)

; ***** 4.1.1 The Core of the Evaluator *****

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (m-apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; Representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Exercise 4.16 - scanning out variable definitions in procedure body
(define (scan-out-defines body)
  (define (scan-exps exps defines not-defines)
    (if (null? exps)
        (if (not (null? defines))
            (transform defines not-defines)
            body
            )
        (if (and (definition? (car exps)) (definition-variable? (car exps)))
            (scan-exps (cdr exps) (append defines (list (car exps))) not-defines)
            (scan-exps (cdr exps) defines (append not-defines (list(car exps)))))
        ))

  (scan-exps body '() '()))

(define (transform defines rest-body)
  (define (defines->statements defs)
    (map (lambda (d) (list (cadr d) (make-quotation '*unassigned*))) defs))
  (define (extend-let-body defines body)
    (if (null? defines)
        body
        (extend-let-body (cdr defines)
                         (cons
                          (list 'set! (cadar defines) (caddar defines))
                          body))))

  (let ((statements (defines->statements defines)))
    (list
     (make-let
      statements
      (extend-let-body defines rest-body))))
  )

; 4.1.4 Running the Evaluator as a Program
(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ;⟨more primitives⟩
        ))
(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; driver loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;(driver-loop)


;-----------------------------------------------------------------------------------
(#%provide (all-defined))