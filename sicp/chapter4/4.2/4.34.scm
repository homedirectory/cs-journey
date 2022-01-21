#lang sicp

; Exercise 4.34: Modify the driver loop for the evaluator so
; that lazy pairs and lists will print in some reasonable way.
; (What are you going to do about infinite lists?) You may
; also need to modify the representation of lazy pairs so that
; the evaluator can identify them in order to print them.

; Lazy pairs and lists will be printed up to 10 elements.
; (define a '(1 2 3 4 5 6 7 8 9 10 11 12))
; - ok
; a
; - (1 2 3 4 5 6 7 8 9 10 ...)

(define (lazy-pair? input env)
  (if (variable? input)
      (let ((val (lookup-variable-value input env)))
        (and (pair? val) (eq? 'lazy-pair (car val))))
      #f))
(define (lazy-pair-represent input env)
  (define (iter pair res)
    (let ((val (car pair)))
      (if (null? val)
          res
          (iter (cdr pair))))
    )
  
  (iter (lookup-variable-value input env) '()))

(define (handle-output input env)
  (if (lazy-pair? input env)
      (lazy-pair-represent input env)
      (actual-value input env)))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (handle-output
            input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))