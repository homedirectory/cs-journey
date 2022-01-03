#lang sicp

(#%require "../core.scm")

;Exercise 4.7: let* is similar to let, except that the bind-
;ings of the let* variables are performed sequentially from
;left to right, and each binding is made in an environment in
;which all of the preceding bindings are visible. For example
;(let* ((x 3) (y (+ x 2)) (z (+ x y 5)))
;  (* x z))
;returns 39.
;----------------------------------------------------------
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (iter statements body)
    (if (null? (cdr statements))
        (make-let (car statements) body)
        (make-let (car statements) (iter (cdr statements) body))
        )
    )

  (iter (let-statements exp) (let-body exp))
  )