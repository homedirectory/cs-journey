#lang sicp

(#%require "machine.scm" "../helpers.scm")

; factorial

;(controller
;  test-counter
;  (test (op >) (reg counter) (reg n))
;  (branch (label end))
;  (assign product (op *) (reg product) (reg counter))
;  (assign counter (op +) (reg counter) (const 1))
;  (goto (label test-counter))
;  end)

(define factorial-machine
  (make-machine
    '(n product counter)
    (list (list '> >) (list '* *) (list '+ +))
    '(test-counter (test (op >) (reg counter) (reg n))
                   (branch (label end))
                   (assign product (op *) (reg product) (reg counter))
                   (assign counter (op +) (reg counter) (const 1))
                   (goto (label test-counter))
                   end)))

; TEST
(define (factorial n)
  (set-register-contents! factorial-machine 'n n)
  (set-register-contents! factorial-machine 'product 1)
  (set-register-contents! factorial-machine 'counter 1)
  (start factorial-machine)
  (get-register-contents factorial-machine 'product))

(print (factorial 5))
(print (factorial 10))
(print (factorial 15))
