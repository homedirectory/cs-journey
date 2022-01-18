#lang sicp

; Exercise 4.26
; Show how to implement unless as a derived expression (like cond or let)

; (unless condition usual-value except-value)
(define (unless-condition exp) (cadr exp))
(define (unless-usual exp) (caddr exp))
(define (unless-except exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-except exp)
           (unless-usual exp)))

; Give an example of a situation where it might be useful to have unless
; available as a procedure, rather than as a special form.

; No idea. Here is ivanjovanovic's anwser:
; ---
; if unless would be implemented as the special syntax form, then it would
; not be procedure object that is first-order value in the evaluation process and thus
; could not be passed to other functions.
; ---
; https://github.com/ivanjovanovic/sicp/blob/master/4.2/e-4.26.scm