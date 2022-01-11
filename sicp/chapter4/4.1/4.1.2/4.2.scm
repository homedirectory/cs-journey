#lang sicp

(#%require "../../../helpers.scm" "../core-helpers.scm")

;Exercise 4.2: Louis Reasoner plans to reorder the cond clauses
;in eval so that the clause for procedure applications ap-
;pears before the clause for assignments. He argues that this
;will make the interpreter more efficient: Since programs
;usually contain more applications than assignments, def-
;initions, and so on, his modified eval will usually check
;fewer clauses than the original eval before identifying the
;type of an expression.

;----------------------------------------------------------

; a. What is wrong with Loui's plan?
; consider (define x 3)

; The evaluator will think that define is an operator, x and 3 are operands.
; The problem is that it will try to evaluate x before applying define,
; and since x might be undefined an error will occur.


;b. Louis is upset that his plan didn’t work. He is willing
;to go to any lengths to make his evaluator recognize
;procedure applications before it checks for most
;other kinds of expressions. Help him by changing the
;syntax of the evaluated language so that procedure
;applications start with call. For example, instead of
;(factorial 3) we will now have to write (call factorial 3)
;and instead of (+ 1 2) we will have to write (call + 1 2).

(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))