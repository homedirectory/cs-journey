#lang sicp

(#%require "machine.scm" "machines.scm" "../helpers.scm")

(define machine
  (extend-machine
    (make-simple-machine)
    '(a)
    '()
    '((assign a (const 10))
      (save a)
      (restore a))))

(define (good)
     (set-register-contents! machine 'a 0)
     (start machine))

(good)

(define fail-machine
  (extend-machine
    (make-simple-machine)
    '(a)
    '()
    '((assign a (const 10))
      (save a)
      (perform (op initialize-stack))
      (restore a))))

(define (fail)
  (set-register-contents! fail-machine 'a 0)
  (start fail-machine))

(fail)
