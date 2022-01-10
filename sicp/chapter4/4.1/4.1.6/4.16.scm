#lang sicp

(#%require "../core.scm")

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
;             (if (eq? (car vals) '*unassigned)
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

; body = sequence of expressions
(define (scan-out-defines body)
  (define (scan-exps exps defines not-defines)
    (if (null? exps)
        (if (not (null? defines))
            (transform defines not-defines)
            body
            )
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
    (map (lambda (d) (list (cadr d) '*unassigned*)) defs)
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


;c. Install scan-out-defines in the interpreter, either in make-procedure
;or in procedure-body. Which place is better? Why?


; TEST
;(define test-body (list (list 'define 'a (list 'list 1 2)) (list 'define 'b (list 'list 3 4))
;                        (list 'list (list 'car 'a) (list 'car 'b))))
;(scan-out-defines test-body)
