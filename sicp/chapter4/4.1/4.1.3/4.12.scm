#lang sicp

; Exercise 4.12
; The procedures set-variable-value!, define-variable! and lookup-variable-value
; can be expressed in terms of more abstract procedures for traversing the
; environment structure. Define abstractions that capture the common patterns and
; redefine the three procedures in terms of these abstractions.
;--------------------------------------------------------------------------------

; --- supporting procedures from previous excercises ---
(define empty-frame '())
(define (make-empty-frame) '())

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-empty-environment) '())

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))
(define (frame-binds frame)
  (cdr frame))
(define (frame-variables frame) (map car (frame-binds frame)))
(define (frame-values frame) (map cdr (frame-binds frame)))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (frame-binds frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
; ------------------------------------------------------

; I am using the frame representation from Exercise 4.11, that is, as a list of bindings.

(define (bind-var bind) (car bind))
(define (bind-val bind) (cdr bind))

; found-action: (lambda (res frame) <body>)
; miss-action: (lambda (frame) <body>)
(define (search-frame var frame found-action miss-action)
  (let ((res (assoc var (frame-binds frame))))
    (if res
        (found-action res frame)
        (miss-action frame))
    ))

(define (search-env var env found-action miss-action)
  (define (env-loop e)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (search-frame var frame found-action miss-action)))
    )
  
  (env-loop env)
  )

(define (lookup-variable-value var env)
  (let (
        (found-action (lambda (res frame) (bind-val res)))
        (miss-action (lambda (frame) (error "Unbound variable" var)))
        )
    (search-env var env found-action miss-action)
    )
  )

(define (set-variable-value! var val env)
  (let (
        (found-action (lambda (res frame) (set-cdr! res val)))
        (miss-action (lambda (frame) (error "Unbound variable" var)))
        )
    (search-env var env found-action miss-action)
    )
  )

(define (define-variable! var val env)
  (let (
        (found-action (lambda (res frame) (set-cdr! res val)))
        (miss-action (lambda (frame) (add-binding-to-frame! var val frame)))
        )
    (search-frame (first-frame env) found-action miss-action)
    )
  )

(#%provide search-env search-frame)

; TEST
;(define test-env the-empty-environment)
;(define-variable! 'a 1 test-env)
;(define-variable! 'b 2 test-env)
;(lookup-variable-value 'a test-env)