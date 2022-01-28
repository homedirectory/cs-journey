#lang sicp

(#%require "../../helpers.scm" "core-helpers.scm"
           "if.scm" "begin.scm" "let.scm" "define.scm"
           "lambda.scm")

; Exercise 4.9
; Iteration constructs
;--------------------------------------------------------------------------

; for
; (for (<inner-proc-name> <inner-arg-name>) (<iter-var> <iter-ds>) <body>)
(define (for? exp) (tagged-list? exp 'for))
(define (for-inner-proc-name exp) (caadr exp))
(define (for-inner-arg-name exp) (cdadr exp))
(define (for-iter-var exp) (caaddr exp))
(define (for-iter-ds exp) (cdaddr exp))
(define (for-body exp) (cadddr exp))
(define (make-for inner-proc-name inner-arg-name iter-var iter-ds body)
  (list 'for (cons inner-proc-name inner-arg-name) (cons iter-var iter-ds) body))

;(for (f ds) (i (list 1 2 3)) (display (* 2 i)) (newline)))
;                        |
;                        v
;((lambda ()
;   (define (f ds)
;     (if (not (null? ds))
;         (begin
;           (let ((i (car ds)))
;             (display (* 2 i)) (newline)
;             )
;           (f (cdr ds))))
;     )
;   (f (list 1 2 3))
;   ))

(define (for->combination exp)
  (let (
        (inner-proc-name (for-inner-proc-name exp))
        (inner-arg-name (for-inner-arg-name exp))
        (iter-var (for-iter-var exp))
        (iter-ds (for-iter-ds exp))
        )
    (let ((body (make-if
                 (make-application 'not (make-application 'null? inner-arg-name))
                 (make-begin (list
                              (make-let (list inner-arg-name (make-application 'car inner-arg-name))
                                        (for-body exp))
                              (make-application inner-proc-name (make-application 'cdr inner-arg-name))
                              )
                             (for-body exp)))))
      (let ((lambda-body (list
                          (make-definition (list inner-proc-name inner-arg-name) body)
                          (make-application inner-proc-name iter-ds))))
        (make-application
         (make-lambda '() lambda-body)
         '()
         )
        )
      )
    )
  )


;--------------------------------------------------------------------------
(#%provide (all-defined))
