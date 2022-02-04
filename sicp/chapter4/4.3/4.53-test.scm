#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "define.scm" "if.scm" "lambda.scm" "begin.scm"
           "cond.scm" "env.scm" "let.scm" "and-or.scm"
           "amb.scm" "apply.scm")

; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
; 4.52
(define (if-fail? exp)
  (tagged-list? exp 'if-fail))
(define (if-fail-exp exp)
  (cadr exp))
(define (if-fail-alt exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (lambda (env succeed fail)
    ((analyze (if-fail-exp exp))
     env succeed 
     (lambda ()
       ((analyze (if-fail-alt exp))
        env succeed fail)))))

; 4.51
(define (set!? exp)
  (tagged-list? exp 'set!))
(define (permanent-set!? exp)
  (tagged-list? exp 'permanent-set!))

(define (assignment? exp)
  (or (set!? exp)
      (permanent-set!? exp)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (if (set!? exp)
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   ; *1*
                   (let ((old-value (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok
                              (lambda ()
                                ; *2*
                                (set-variable-value!
                                 var old-value env)
                                (fail2)))))
                 fail))
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   ; *1*
                   (let ((old-value (lookup-variable-value var env)))
                     (set-variable-value! var val env)
                     (succeed 'ok fail2)))
                 fail)))))
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((amb? exp) (analyze-amb exp))
        ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp)) ; +++
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze-application (let->combination exp)))
        ((apply? exp) (analyze (apply->application exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

; self-evaluating
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

; quotation
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

; variable lookup
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

; variable definition
; (it is assumed that internal definitions are scanned out)
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (make-procedure parameters body-proc env)
  (list 'procedure parameters body-proc env))

; lambda
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

; if
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

; sequence of expressions
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

; application
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs)
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args) fail3))
          fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION"
                proc))))

; amb
(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

; procedures
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'caar caar) (list 'cddr cddr) (list 'caddar caddar)
        (list 'cadr cadr) (list 'caddr caddr) (list 'cdadr cdadr)
        (list 'cdar cdar) (list 'caadr caadr) (list 'cadddr cadddr)
        (list 'cadar cadar)
        (list 'cons cons)
        (list 'append append)
        (list 'null? null?) (list 'pair? pair?)
        (list 'list list)
        (list 'newline newline)
        (list 'display display)
        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'inc inc)
        (list 'dec dec)
        (list 'length length)
        (list 'member member) (list 'memq memq)
        (list 'abs abs) (list 'min min) (list 'max max)
        (list 'even? even?) (list 'remainder remainder)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '< <)
        (list '>= >=)
        (list '<= <=)
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

(define apply-in-underlying-scheme apply)

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

; driver loop
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline) (display ";;; Starting a new problem ")
            (ambeval
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output output-prompt)
               (user-print val)
               (internal-loop next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no more values of")
               (user-print input)
               (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline) (display ";;; There is no current problem")
     (driver-loop))))

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

; == Compound procedures to be installed inside the Amb evaluator ==
(define installed-procedures
  (list
   '(define (require p) (if (not p) (amb)))
   '(define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items))))
   '(define (an-integer-starting-from n)
      (amb n (an-integer-starting-from (+ n 1))))
   '(define (list-ref n lst)
      (define (iter l c)
        (if (= c n)
            (car l)
            (iter (cdr l) (inc c))))
      (iter lst 0))
   ))

(define (ambeval-definition def)
  (ambeval def the-global-environment
           (lambda (val fail) 'ok)
           (lambda () 'fail)))

(map ambeval-definition installed-procedures)

; == TEST ==
(define (ambeval-test exp env)
  (ambeval exp env
           (lambda (val fail) val)
           (lambda () 'fail)))

;-------------------------------------------------------------------------------
(map ambeval-definition
     (list '(define (prime-sum-pair list1 list2)
              (let ((a (an-element-of list1))
                    (b (an-element-of list2)))
                (require (prime? (+ a b)))
                (list a b)))
           '(define (square x) (* x x))
           '(define (next x)
              (cond ((= x 2) 3)
                    (else (+ x 2))))
           '(define (smallest-divisor n) (find-divisor n 2))
           '(define (find-divisor n test-divisor)
              (cond ((> (square test-divisor) n) n)
                    ((divides? test-divisor n) test-divisor)
                    (else (find-divisor n (next test-divisor)))))
           '(define (divides? a b) (= (remainder b a) 0))
           '(define (prime? n)
              (= n (smallest-divisor n)))))

;(let ((pairs '()))
;  (if-fail
;   (let ((p (prime-sum-pair '(1 3 5 8)
;                            '(20 35 110))))
;     (permanent-set! pairs (cons p pairs))
;     (amb))
;   pairs))