#lang sicp

; Exercise 4.27
; Suppose we type in the following definitions to the lazy evaluator:
(define count 0)
(define (id x) (set! count (+ count 1)) x)

; Give the missing values in the following sequence of interactions,
; and explain your answers.
(define w (id (id 10)))

;;; L-Eval input:
; count
;;; L-Eval value:
; 1
; ---
; Explanation: (id (id 10)) returns delayed (id 10),
; thus count is incremented only once.

;;; L-Eval input:
; w
;;; L-Eval value:
; 10
; ---
; Explanation: w is evaluated, that is, (id 10) is evaluated,
; thus count is incremented once again, return value of (id 10) is 10.

;;; L-Eval input:
; count
;;; L-Eval value:
; 2