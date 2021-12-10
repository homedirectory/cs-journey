#lang sicp

(#%require "../core.scm")

;Exercise 4.3: Exercise 4.4: Recall the definitions of the special forms and
;and or from Chapter 1:
;...
;Install and and or as new special forms for the evaluator by
;defining appropriate syntax procedures and evaluation procedures
;eval-and and eval-or. Alternatively, show how to
;implement and and or as derived expressions.

;----------------------------------------------------------

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


; and as a derived expression
(define (and->if exp)
  (define (f exps last-val)  
    (if (null? exps)
        last-val
        (make-if (car exps)
                 (f (cdr exps) (eval (car exps)))
                 'false)
        )
    )

  (f (and-exps exp) 'true)
  )

; or as a derived expression
(define (or->if exp)
  (define (f exps)  
    (if (null? exps)
        'false
        (make-if (car exps)
                 (eval (car exps))
                 (f (cdr exps)))
        )
    )

  (f (or-exps exp))
  )