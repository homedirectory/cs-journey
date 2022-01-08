#lang sicp

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
        ((application? exp)
         (m-apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        ((let? exp)
         (eval (let->combination exp) env))
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
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


(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))


(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)


(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


; The only self-evaluating items are numbers and strings
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


; Variables are represented by symbols
(define (variable? exp) (symbol? exp))


; Quotations have the form (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


; Assignments have the form (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


; Definitions have the form:
; (define <var> <value>)
; or the form:
; (define (<var> <param1> ... <paramn>) <body>),
; which is the same as :
; (define <var> (lambda <param1> ... <paramn>) <body>))
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) ; formal parameters
                   (cddr exp)))) ; body
(define (make-definition var value)
  (list 'define var value)
  )


; lambda expressions are lists that begin with the symbol lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


; Conditionals begin with if and have a predicate, consequent,
; and an (optional) alternative. No alternative = false as the alternative
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (cons 'if (list predicate consequent alternative)))


; begin packages a sequence of expressions into a single expression.
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq) (cons 'begin seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))


; applicaton is any compound expression that is not one of the above
; applicaton = (<operator> <operands list>)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
; from Exercise 4.5
(define (make-application operator operands)
  (cons operator operands)
  )


; cond can be written in terms of nested if else clauses.
; such expressions are called derived expressions.
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false      ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; Exercise 4.4
; and expression is of the form:
; ('and <exp1> <exp2> ... <expn>)
(define (and-exps exp)
  (cdr exp)
  )

(define (eval-and exp env)
  (define (eval-and-exps exps last-val)
    (if (null? exps)
        last-val
        (let ((val (eval (car exps) env)))
          (if (true? val)
              (eval-and-exps (cdr exps) val)
              'false
              )
          )
        )
    )

  (eval-and-exps (and-exps exp) 'true)
  )

; or expression is of the form:
; ('or <exp1> <exp2> ... <expn>)
(define (or-exps exp)
  (cdr exp)
  )

(define (eval-or exp env)
  (define (eval-or-exps exps)
    (if (null? exps)
        'false
        (let ((val (eval (car exps) env)))
          (if (true? val)
              val
              (eval-or-exps (cdr exps))
              )
          )
        )
    )

  (eval-or-exps (or-exps exp))
  )

; Exercise 4.6
; let expression has the form of:
; ('let <statements> <body>)
; <statements>: (<stat1> ... <statn>) ;; (list)
; <stati>: (<vari> <expi>) ;; (cons)
; <body>: sequence of expression
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-statements exp)
  (cadr exp))
(define (let-stat-var stat)
  (car stat))
(define (let-stat-exp stat)
  (cdr stat))
(define (let-body exp)
  (cddr exp))
(define (make-let statements body)
  (cons 'let (cons statements body)))

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
(define (let->combination exp)
  ;; named let?
  (if (= (length exp) 4)
      (let (
            (var (cadr exp))
            (parameters (map let-stat-var (caddr exp)))
            (arguments (map let-stat-exp (caddr exp)))
            (body (cadddr exp))
            )
        (let ((lambda-body (cons
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

; Exercise 4.9
; "for" iteration construct
(define (for? exp) (tagged-list? exp 'for))
(define (for-inner-proc-name exp) (caadr exp))
(define (for-inner-arg-name exp) (cdadr exp))
(define (for-iter-var exp) (caaddr exp))
(define (for-iter-ds exp) (cdaddr exp))
(define (for-body exp) (cadddr exp))
(define (make-for inner-proc-name inner-arg-name iter-var iter-ds body)
  (list 'for (cons inner-proc-name inner-arg-name) (cons iter-var iter-ds) body))

(define (for->combination exp)
  (let (
        (inner-proc-name (for-inner-proc-name exp))
        (inner-arg-name (for-inner-arg-name exp))
        (iter-var (for-iter-var exp))
        (iter-ds (for-iter-ds exp))
        )
    (let ((body (make-if
                 (make-application 'not (make-application 'null? inner-arg-name))
                 (make-begin (list
                              (make-let (list (cons inner-arg-name (make-application 'car inner-arg-name)))
                                        (for-body exp))
                              (make-application inner-proc-name (make-application 'cdr inner-arg-name))
                              )
                             (for-body exp)))))
      (let ((lambda-body (cons
                          (make-definition (cons inner-proc-name inner-arg-name) body)
                          (make-application inner-proc-name iter-ds))))
        (make-application
         (make-lambda '() lambda-body)
         '()
         )
        )
      )
    )
  )


; 4.1.3
; Testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; Representing procedures
; Exercise 4.16 - scanning out defines in procedure body
(define (scan-out-defines body)
  (define (scan-exps exps defines not-defines)
    (if (null? exps)
        (transform defines not-defines)
        (if (definition? (car exps))
            (scan-exps (cdr exps) (cons (car exps) defines) not-defines)
            (scan-exps (cdr exps) defines (cons (car exps) not-defines))
            )
        )
    )

  (scan-exps body '() '())
  )

(define (transform defines rest-body)
  (define (defines->statements defs)
    (map (lambda (d) (cons (cadr d) '*unassigned*)) defs)
    )
  (define (extend-let-body defines body)
    (if (null? defines)
        body
        (extend-let-body (cdr defines)
                         (cons
                          (list 'set! (cadar defines) (caddar defines))
                          body))
        )
    )

  (let ((statements (defines->statements defines)))
    (make-let
     statements
     (extend-let-body defines rest-body)
     )
    )
  )

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

; Operations on Environments
; environment is a sequence of frames, where each frame is a table
; of bindings that associate variables with their values
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; a frame is represented as a pair of lists
; 1. list of vars bound in that frame
; 2. list of associated values
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

; looking up a variable in an environment starts with scanning the first frame
; if not found, we scan the enclosing environment and so on
; until we find it or signal an "unbound variable" error.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals))))
      )
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))
    )
  
  (env-loop env)
  )

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))
    )
  
  (env-loop env)
  )

; When defining a new variable we work only with the first frame
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

; 4.1.4 Running the Evaluator as a Program

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'map map)
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
(define input-prompt
  ";;; M-Eval input:")
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

;-----------------------------------------------------------------------------------
(#%provide eval m-apply list-of-values eval-if eval-sequence eval-assignment
           eval-definition self-evaluating? variable? quoted? text-of-quotation
           tagged-list? assignment? assignment-variable assignment-value
           definition? definition-variable definition-value make-definition lambda?
           lambda-parameters lambda-body make-lambda if? if-predicate if-consequent
           if-alternative make-if begin? begin-actions last-exp? first-exp rest-exps
           make-begin sequence->exp application? operator operands first-operand
           rest-operands cond? cond-clauses cond-else-clause? cond-predicate
           cond-actions cond->if expand-clauses make-application let? let-statements
           let-stat-var let-stat-exp let-body let->combination make-let let*?
           let*->nested-lets for? for-inner-proc-name for-inner-arg-name for-iter-var
           for-iter-ds for-body make-for for->combination make-procedure compound-procedure?
           procedure-body procedure-parameters procedure-environment make-frame
           frame-variables frame-values add-binding-to-frame! extend-environment
           lookup-variable-value set-variable-value! define-variable! setup-environment
           primitive-procedure? primitive-implementation primitive-procedures
           primitive-procedure-names primitive-procedure-objects apply-primitive-procedure
           input-prompt output-prompt driver-loop prompt-for-input announce-output
           user-print)