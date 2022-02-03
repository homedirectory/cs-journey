#lang sicp

(#%require "../../helpers.scm")

; Exercise 4.50
; Implement a new special form ramb that is like amb except that it searches
; alternatives in a random order, rather than from left to right.
; Show how this can help with Alyssa’s problem in Exercise 4.49.

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((r (random (length choices))))
              ((list-ref r choices)
               env
               succeed
               (lambda () (try-next (list-remove-at r choices)))))))
      (try-next cprocs))))

; as revc pointed out in http://community.schemewiki.org/?sicp-ex-4.50
; such a method as mine is flawed, since it does not treat all options equally.
; This means that a procedure such as an-integer-between will always prefer lower
; numbers.

;(define (an-integer-between low high)
;  (require (<= low high))
;  (ramb low (an-integer-between (inc low) high)))

; (an-integer-between 1 5)
; 1) (ramb 1 <rest>) | 1 - 1/2 , [2, 3, 4, 5] - 1/2
; 2) (ramb 2 <rest>) | 2 - 1/4 , [3, 4, 5]    - 1/4
; 3) (ramb 3 <rest>) | 3 - 1/8 , [4, 5]       - 1/8
; 4) (ramb 4 <rest>) | 4 - 1/16, [5]          - 1/16
; 5) (ramb 5 <rest>) | 5 - 1/32

; It is important to note, however, that my method satisfies the exercies requirements,
; since it searches alternatives in a random order, rather than from left to right,
; where alternatives are arguments to ramb.


; Alyssa's problem in 4.49
(define (parse-word word-list)
  (require (not (null? word-list)))
  (ramb (car word-list) (parse-word (cdr word-list))))

; The sentence structure, however, will not be random.