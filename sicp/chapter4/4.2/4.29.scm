#lang sicp

; Exercise 4.29
; Exhibit a program that you would expect to run much more slowly
; without memoization than with memoization.

(define (func x proc)
  (if (and (= x 0) (> (proc) 10))
      1
      (func (- x 1) proc)))
; (func 10 (factorial 200))
; Without memoization (factorial 200) will have to be computed each time
; in the if statement.

; Also, consider the following interaction, where the id procedure
; is defined as in Exercise 4.27 and count starts at 0:
(define count 0)
(define (id x) (set! count (+ count 1)) x)

; (Give the responses both when the evaluator memoizes and when it does not.)
(define (square x) (* x x))
;;; L-Eval input:
; (square (id 10))
;;; L-Eval value:
; 100

;;; L-Eval input:
; count
;;; L-Eval value:
; Memo: 1
; No memo: 2
