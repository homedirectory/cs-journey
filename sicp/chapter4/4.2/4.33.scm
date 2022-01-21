#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm" "lazy-core.scm"
           "if.scm" "lambda.scm" "begin.scm" "cond.scm" "let.scm"
           "define.scm" "and-or.scm" "for.scm" "env.scm" "thunk.scm")

; Exercise 4.33: Ben Bitdiddle tests the lazy list implementation
; given above by evaluating the expression:
; (car '(a b c))
; To his surprise, this produces an error. After some thought,
; he realizes that the “lists” obtained by reading in quoted
; expressions are different from the lists manipulated by the
; new definitions of cons, car, and cdr. Modify the evaluator’s
; treatment of quoted expressions so that quoted lists
; typed at the driver loop will produce true lazy lists.

;------------------------------------------------------------------------
(define (make-cons a b)
  (make-application 'cons (list a b)))

(define (quoted-list->cons q-list)
  (define (iter lst)
    (if (null? lst)
        '()
        (make-cons (car lst) (iter (cdr lst)))
        ))

  (iter q-list)
  )

(define (eval-quoted exp env)
  (let ((obj (cadr exp)))
    (if (pair? obj)
        (eval (quoted-list->cons obj) env)
        obj)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (eval-quoted exp env)) ; <---- changed
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
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (m-apply (actual-value (operator exp) env)
                  (operands exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))

;---------------------------------------------------------------------------------
; Include all procedures below since they must call "eval" as defined in this file

(define (m-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type: APPLY" procedure))))

(define (actual-value exp env)
  (force-it (eval exp env)))

; Thunks
(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ; replace exp with its value
           (set-cdr! (cdr obj) '())    ; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

; begin.scm
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; Representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
;  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
; === SETUP ===
(define test-env (setup-environment))

(define lazy-cons
  (list
   ; (define (cons x y) (lambda (m) (m x y)))
   (make-definition
    (list 'cons 'x 'y)
    (make-lambda (list 'm) (list (make-application 'm (list 'x 'y)))))
   ; (define (car z) (z (lambda (p q) p)))
   (make-definition
    (list 'car 'z)
    (make-application 'z (list (make-lambda (list 'p 'q) (list 'p)))))
   ; (define (cdr z) (z (lambda (p q) q)))
   (make-definition
    (list 'cdr 'z)
    (make-application 'z (list (make-lambda (list 'p 'q) (list 'q)))))
   ))
(map (lambda (exp) (eval exp test-env)) lazy-cons)

; driver loop
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input test-env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
;---------------------------------------------------------------------------------