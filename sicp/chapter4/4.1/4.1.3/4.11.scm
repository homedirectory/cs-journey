#lang sicp

; Exercise 4.11
; Instead of representing a frame as a pair of lists, we can represent a frame
; as a list of bindings, where each binding is a name-value pair.
;-------------------------------------------------------------------------------

; I chose the approach of using associative list to represent frames.

; define empty frame
(define empty-frame '())
(define (make-empty-frame) '())

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-empty-environment) '())

; redefine environment, since there is no way to work with null (empty env)
; (cons 'env (cons <first-frame> <enclosing>))
;(define the-empty-environment (cons 'env (cons '() '())))
;(define (make-empty-environment) (cons 'env (cons '() '())))
;(define (empty-env? env) (null? (cdr env)))
;(define (enclosing-environment env) (cddr env))
;(define (first-frame env) (cadr env))
;(define (make-env frame enc-env)
;  (cons 'env (cons frame enc-env)))

; frame = (cons 'frame (list <bind1> <bind2>))
; bind = (cons <var> <val>)
(define (make-frame variables values)
  (cons 'frame (map cons variables values)))
(define (frame-binds frame)
  (cdr frame))
(define (frame-variables frame) (map car (frame-binds frame)))
(define (frame-values frame) (map cdr (frame-binds frame)))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (frame-binds frame))))

; no change
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (let ((res (assoc var (frame-binds frame))))
        (if res
            (cdr res)
            (env-loop (enclosing-environment env)))
        ))
    
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame)))
    )
  
  (env-loop env)
  )

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (let ((res (assoc var (frame-binds frame))))
        (if res
            (set-cdr! res val)
            (env-loop (enclosing-environment env)))
        ))

    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame)))
    )
  
  (env-loop env)
  )

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan frame)
      (let ((res (assoc var (frame-binds frame))))
        (if res
            (set-cdr! res val)
            (add-binding-to-frame! var val frame))
        ))

    (scan frame)
    )
  )

; TEST
; how to properly represent environments?
;(define test-env (make-empty-environment))
;(define-variable! 'a 1 test-env)
;(define-variable! 'b 2 test-env)
;(lookup-variable-value 'a test-env)