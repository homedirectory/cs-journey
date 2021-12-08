#lang sicp

(#%require "../core.scm")

;Exercise 4.3: Rewrite eval so that the dispatch is done
;in data-directed style. Compare this with the data-directed
;differentiation procedure of Exercise 2.73. (You may use the
;car of a compound expression as the type of the expres-
;sion, as is appropriate for the syntax implemented in this
;section.)

;----------------------------------------------------------

(define (eval exp env)
  (if (self-evaluating? exp)
      exp
      ((get 'eval (exp-type exp)) (exp-exp exp) env)
      )
  )

(define (exp-type exp)
  (car exp)
  )

(define (exp-exp exp)
  (cadr exp)
  )

(put 'eval 'self-eval (lambda (exp env) exp))
(put 'eval 'variable lookup-variable-value)
(put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'assignment eval-assignment)
(put 'eval 'definition eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
                                                     (lambda-body exp)
                                                     env)))
(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env) (eval (cond->if exp) env)))
(put 'eval 'application (lambda (exp env) (apply (eval (operator exp) env)
                                                 (list-of-values (operands exp) env))))

