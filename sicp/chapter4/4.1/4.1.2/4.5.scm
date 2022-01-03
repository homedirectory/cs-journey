#lang sicp

(#%require "../core.scm")

;Exercise 4.5: Scheme allows an additional syntax for cond
;clauses, (⟨test⟩ => ⟨recipient⟩). If ⟨test ⟩ evaluates to a
;true value, then ⟨recipient ⟩ is evaluated. Its value must be a
;procedure of one argument; this procedure is then invoked
;on the value of the ⟨test⟩, and the result is returned as the
;value of the cond expression. For example
;(cond ((assoc 'b '((a 1) (b 2))) => cadr)
;      (else false))
;returns 2. Modify the handling of cond so that it supports
;this extended syntax.

;----------------------------------------------------------
(define (make-application operator operands)
  (cons operator operands)
  )

; ordinary clause has the form: (<predicate> <action>)
; clause with recipient has the form: (<test> => <recipient>)
(define (cond-clause-with-recipient? clause)
  (eq? (cadr clause) '=>)
  )
(define (cond-recipient clause) (caddr clause))
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
            (if (cond-clause-with-recipient? first)
                ; Can't and shouldn't eval here, no env anyway.
                ; The value of evaluated predicates would have to be stored in an environment
                ; and then used by the consequent expression.
                ; I see no way of doing this at the current stage of the book, so
                ; will have to do with evaluating the predicate twice.
                (make-if (cond-predicate first)
                         (make-application (cond-recipient first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))
            )
        )
      )
  )
