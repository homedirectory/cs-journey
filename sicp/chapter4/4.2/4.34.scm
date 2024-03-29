#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "if.scm" "lambda.scm" "begin.scm" "cond.scm" "let.scm"
           "define.scm" "and-or.scm" "for.scm" "env.scm" "thunk.scm")

; Exercise 4.34: Modify the driver loop for the evaluator so
; that lazy pairs and lists will print in some reasonable way.
; (What are you going to do about infinite lists?) You may
; also need to modify the representation of lazy pairs so that
; the evaluator can identify them in order to print them.

; ----------------------------------------------------------------------

; Lazy pairs and lists will be printed up to 10 elements.
; In:  (define a '(1 2 3 4 5 6 7 8 9 10 11 12))
; Out: ok
; In:  (display a)
; Out: (1 2 3 4 5 6 7 8 9 10 ...)

; Lazy pairs will be represented as pairs:
; ('lazy-pair proc), where proc is the result of cons

; I chose the approach of defining the respective printing procedures
; inside the evaluator itself. Therefore I use both lazy and non-lazy
; pairs. For lazy ones I defined "cons", "car" & "cdr" procedures.
; Non-lazy ones are defined by the same names prepended by "scheme-",
; e.g. "scheme-cons".
; I also include "scheme-display" and the evaluator defined "display".

; However, I realise that there is another approach which does not involve
; programming on the level of the evaluator, but rathere in the underlying
; Scheme itself.


; === lazy-core.scm included for tests ===

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
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((application? exp)
         (m-apply (actual-value (operator exp) env)
                  (operands exp) env))
        (else
         (error "Unknown expression type: EVAL" exp))))

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
(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; begin.scm
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

; define.scm
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

; if.scm
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; Representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
;  (list 'procedure parameters body env))
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
            (scan-exps (cdr exps) defines (append not-defines (list (car exps)))))
        ))

  (scan-exps body '() '()))

(define (transform defines rest-body)
  (define (defines->statements defs)
    (map (lambda (d) (list (cadr d) (make-quotation '*unassigned*))) defs))
  (define (extend-let-body defines body)
    (if (null? defines)
        body
        ;{{define b {+ a x}} {define a 5}}
        (append
         (map (lambda (d)
                (list 'set! (cadr d) (caddr d)))
              defines)
         body)
        ))

  (let ((statements (defines->statements defines)))
    (list
     (make-let
      statements
      (extend-let-body defines rest-body))))
  )

; 4.1.4 Running the Evaluator as a Program
(define primitive-procedures
  (list (list 'scheme-car car)
        (list 'scheme-cdr cdr)
        (list 'scheme-cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'pair? pair?)
        (list 'newline newline)
        (list 'scheme-display display)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
        (list 'length length)
        (list 'eq? eq?)
        (list 'not not)
        (list 'equal? equal?)
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

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; driver loop
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop env)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop env))

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
; === END OF lazy-core.scm ===

; === lazy-lists.scm with slight modification to support new lazy pairs representation ===
(define lazy-cons
  (list
   ; (define (cons x y) (cons 'lazy-pair (lambda (m) (m x y))))
   (make-definition
    (list 'cons 'x 'y)
    (make-application 'scheme-cons
                      (list "lazy-pair" (make-lambda (list 'm) (list (make-application 'm (list 'x 'y)))))))
   ; (define (car z) ((scheme-cdr z) (lambda (p q) p)))
   (make-definition
    (list 'car 'z)
    (make-application (make-application 'scheme-cdr (list 'z)) (list (make-lambda (list 'p 'q) (list 'p)))))
   ; (define (cdr z) ((scheme-cdr z) (lambda (p q) q)))
   (make-definition
    (list 'cdr 'z)
    (make-application (make-application 'scheme-cdr (list 'z)) (list (make-lambda (list 'p 'q) (list 'q)))))
   ))
; === END OF lazy-lists.scm with slight modifications ===

; === TEST ===
(define test-env (setup-environment))

; define null as a variable
(eval (make-definition 'nil '()) test-env)

(map (lambda (exp) (eval exp test-env)) lazy-cons)

(define (run) (driver-loop test-env))
; setup lazy cons, car & cdr
(map (lambda (exp) (eval exp test-env)) lazy-cons)
; define a primitive pair
(define pp-def (make-definition 'pp (make-application 'cons (list 1 2))))
(eval pp-def test-env)
; define a list of 12 elements INSIDE the evaluator while testing, cause doing it manually is a pain.
;(define lst (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 (cons 10 (cons 11 (cons 12)))))))))))))

; === custom display procedures defined INSIDE the evaluator ===
;(define (lazy-pair? obj)
;  (if (pair? obj)
;      (equal? "lazy-pair" (scheme-car obj))
;      false))
(define lazy-pair?-def
  (make-definition
   (list 'lazy-pair? 'obj)
   ;body
   (make-if (make-application 'pair? (list 'obj))
            (make-application 'equal? (list "lazy-pair" (make-application 'scheme-car (list 'obj))))
            'false)))
(eval lazy-pair?-def test-env)

;(define (display obj)
;  (if (pair? obj)
;      (if (equal? "lazy-pair" (scheme-car obj))
;          (display-lazy-pair obj)
;          (scheme-display obj))
;      (scheme-display obj)))
(define display-def
  (make-definition
   (list 'display 'obj)
   ;body
   (make-if (make-application 'pair? (list 'obj))
            (make-if (make-application 'lazy-pair? (list 'obj))
                     (make-application 'display-lazy-pair (list 'obj))
                     (make-application 'scheme-display (list 'obj)))
            (make-application 'scheme-display (list 'obj)))))
(eval display-def test-env)

;(define (display-lazy-pair p)
;  (define (iter lp count)
;    (if (= 10 count)
;        (scheme-cons "..." '())
;        (if (lazy-pair? lp)
;            (scheme-cons (car lp) (iter (cdr lp) (+ count 1)))
;            (if (null? lp)
;                '()
;                (scheme-cons lp '())))))
;  (iter p 0))
(define display-lazy-pair-def
  (make-definition
   (list 'display-lazy-pair 'p)
   ;body
   (make-definition
    (list 'iter 'lp 'count)
    ;iter body
    (make-if (make-application '= (list 10 'count))
             (make-application 'scheme-cons (list "..." '()))
             (make-if (make-application 'lazy-pair? (list 'lp))
                      (make-application 'scheme-cons (list
                                                      (make-application 'car (list 'lp))
                                                      (make-application 'iter
                                                                        (list (make-application 'cdr (list 'lp))
                                                                              (make-application '+ (list 'count 1))))))
                      (make-if (make-application 'null? (list 'lp))
                               '()
                               (make-application 'scheme-cons (list 'lp '()))))))
   (make-application 'iter (list 'p 0))))
(eval display-lazy-pair-def test-env)

