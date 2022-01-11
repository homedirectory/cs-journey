#lang sicp

(#%require "../core.scm")

;Exercise 4.8: “Named let” is a variant of let that has the
;form:
;(let ⟨var⟩ ⟨bindings⟩ ⟨body⟩)

;----------------------------------------------------------
(define (named-let? exp)
  (symbol? (cadr exp)))
(define (let->combination exp)
  ;; named let?
  (if (named-let? exp)
      (let (
            (var (cadr exp))
            (parameters (map let-stat-var (caddr exp)))
            (arguments (map let-stat-exp (caddr exp)))
            (body (cadddr exp))
            )
        (let ((lambda-body (cons
                            (make-definition (cons var parameters) body)
                            (make-application var arguments))))
          ; create lambda with no parameters and call it immediately
          ; so that the name used by named let is only visible inside this lambda
          ; the body of this lambda consists of 2 expressions
          ; 1. define the procedure
          ; 2. call it with supplied arguments extracted from named let bindings
          (make-application
           (make-lambda '() lambda-body)
           '()
           )
          )
        )
      (let (
            (parameters (map let-stat-var (let-statements exp)))
            (body (let-body exp))
            (arguments (map let-stat-exp (let-statements exp)))
            )
        (make-application
         (make-lambda parameters body)
         arguments
         )
        )
      )
  )

(define (make-let statements body)
  (list 'let statements body)
  )


