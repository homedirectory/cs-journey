#lang sicp

(#%require "machine.scm" "machines.scm" "../helpers.scm")

(define test-machine
  (extend-machine
    (make-simple-machine)
    '(a)
    '()
    '((jz a increment)
      (goto (label end))
      increment
      (assign a (op +) (reg a) (const 10))
      end)
    ))

; TEST
(define (a-is-incremented)
  (set-register-contents! test-machine 'a 0)
  (start test-machine)
  (get-register-contents test-machine 'a))
(print (a-is-incremented))

(define (a-is-not-incremented)
  (set-register-contents! test-machine 'a 5)
  (start test-machine)
  (get-register-contents test-machine 'a))
(print (a-is-not-incremented))
