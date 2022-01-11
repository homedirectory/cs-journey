#lang sicp

(#%require "../core-helpers.scm"
           "../define.scm" "../let.scm")

;Exercise 4.16: In this exercise we implement the method
;just described for interpreting internal definitions. We assume
;that the evaluator supports let.

;a. Change lookup-variable-value (Section 4.1.3) to signal an error
;if the value it finds is the symbol *unassigned*.

;(define (lookup-variable-value var env)
;  (define (env-loop env)
;    (define (scan vars vals)
;      (cond ((null? vars)
;             (env-loop (enclosing-environment env)))
;            ((eq? var (car vars))
;             (if (eq? (car vals) '*unassigned*)
;                 (error "Unassigned variable" var)
;                 (car vals))
;             )
;            (else (scan (cdr vars) (cdr vals))))
;      )
;    
;    (if (eq? env the-empty-environment)
;        (error "Unbound variable" var)
;        (let ((frame (first-frame env)))
;          (scan (frame-variables frame)
;                (frame-values frame))))
;    )
;  
;  (env-loop env)
;  )

;b. Write a procedure scan-out-defines that takes a procedure body
;and returns an equivalent one that has no internal definitions,
;by making the transformation described above.

(define (definition-variable? exp)
  (symbol? (cadr exp)))

; body = sequence of expressions
(define (scan-out-defines body)
  (define (scan-exps exps defines not-defines)
    (if (null? exps)
        (if (not (null? defines))
            (transform defines not-defines)
            body
            )
        ; this should also check if a variable is being defined (not procedure)
        (if (and (definition? (car exps)) (definition-variable? (car exps)))
            (scan-exps (cdr exps) (append defines (list (car exps))) not-defines)
            (scan-exps (cdr exps) defines (append not-defines (list(car exps))))
            )
        )
    )

  (scan-exps body '() '())
  )

(define (transform defines rest-body)
  (define (defines->statements defs)
    (map (lambda (d) (list (cadr d) (make-quotation '*unassigned*))) defs)
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
    (list
     (make-let
      statements
      (extend-let-body defines rest-body)
      )
     )
    )
  )

(#%provide (all-defined))

;c. Install scan-out-defines in the interpreter, either in make-procedure
;or in procedure-body. Which place is better? Why?


; TEST
;(define test-body (list (list 'define 'a (list 'list 1 2)) (list 'define 'b (list 'list 3 4))
;                        (list 'list (list 'car 'a) (list 'car 'b))))
(define test-body (list (make-definition 'a 1)
                        (list '+ 'a 'x)))
(define test-body1 (list (list '+ 1 2)))
(define test-body2 (list (make-definition 'a 1)
                         (list 'define (list 'g 'x)
                               (list '+ 'x 1))
                         (list '+ (list '* (list 'g 'x) 'y) 'a)))
;(scan-out-defines test-body)


(define (f x)
  (define (g x) (+ 1 x))
  (g (+ x 1))
  )

(define (ff x)
  (define a 1)
  (+ x a)
  )

(define (ff2 x)
  (define a 1)
  (define b 10)
  (+ x a b)
  )