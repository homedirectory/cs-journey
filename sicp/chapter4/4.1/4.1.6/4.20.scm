#lang sicp

(#%require "../../../helpers.scm"
           "../core-helpers.scm"
           "../let.scm")

; Exercise 4.20: letrec
; letrec has the form:
; (letrec ((<var1> <exp1>) ... (<varn> <expn>)) <body>)
; (list 'letrec statements body)

(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec-statements exp)
  (cadr exp))
(define (letrec-body exp)
  (cddr exp))
(define (make-letrec statements body)
  (append (list 'letrec statements) body))

; a. letrec as a derived expression (letrec -> let)
(define (letrec->let exp)
  (define (new-statements statements)
    (map (lambda (s)
           (list (let-stat-var s) (make-quotation '*unassigned*)))
         statements))
  (define (new-body e)
    (let ((orig-statements (letrec-statements e))
          (orig-body (letrec-body e)))
      (append
       (map (lambda (s) (list 'set! (let-stat-var s) (let-stat-exp s))) orig-statements)
       orig-body)
      ))
  
  (make-let
   (new-statements (letrec-statements exp))
   (new-body exp))
  )

; a. TEST
(define test-letrec-a (make-letrec (list (list 'a 4) (list 'b 3)) (list (list '+ 'a 'b))))
(letrec->let test-letrec-a)

; b.
; 1. using let
; let is transformed into lambda and we get:
; ((lambda (even? odd?) <body>) <arguments>)
; where <arguments> are: {even-proc odd-proc}
; even-proc:
;   (lambda (n)
;     (if (= n 0) true
;         (odd? (- n 1)))) <--- odd? is undefined in current environment, thus an error
;
; 2. using letrec
; first even? and odd? are initialized to '*unassigned*, therefore they exist
; in current environment and we won't get an error such as in 1.