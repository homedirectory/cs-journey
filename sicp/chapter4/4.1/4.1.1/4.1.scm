#lang sicp

(#%require "../core-helpers.scm")

;Exercise 4.1: Notice that we cannot tell whether the metacir-
;cular evaluator evaluates operands from left to right or from
;right to left. Its evaluation order is inherited from the un-
;derlying Lisp: If the arguments to cons in list-of-values
;are evaluated from left to right, then list-of-values will
;evaluate operands from left to right; and if the arguments to
;cons are evaluated from right to left, then list-of-values
;will evaluate operands from right to left.
;Write a version of list-of-values that evaluates operands
;from left to right regardless of the order of evaluation in the
;underlying Lisp. Also write a version of list-of-values
;that evaluates operands from right to left.

;--------------------------------------------------------------

; left-to-right
(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
        (cons first-value
              (list-of-values (rest-operands exps) env))
        )
      )
  )

; right-to-left
(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest)
        )
      )
  )
