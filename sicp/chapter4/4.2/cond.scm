#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm" "begin.scm" "if.scm")
;---------------------------------------------------------------

; cond can be written in terms of nested if else clauses.
; such expressions are called derived expressions.
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cadr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cadr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (sequence? exp) (not (or (self-evaluating? exp) (variable? exp) (quoted? exp))))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false      ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (let ((actions (cond-actions first)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (if (sequence? actions)
                      (sequence->exp actions)
                      actions)
                  (error "ELSE clause isn't last: COND->IF"
                         clauses))
              (if (sequence? actions)
                  (make-if (cond-predicate first)
                           (sequence->exp actions)
                           (expand-clauses rest))
                  (make-if (cond-predicate first)
                           actions
                           (expand-clauses rest))))))))

(define (make-cond clauses)
  (list 'cond clauses))
(define (make-cond-clause predicate actions)
  (list predicate actions))


;---------------------------------------------------------------
(#%provide (all-defined))
