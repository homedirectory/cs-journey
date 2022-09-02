#lang sicp

(#%require "5.11.scm")

(define machine 
  (make-machine
    '(a b)
    '()
    '((assign a (const 5))
      (assign b (const 20))
      (save a)
      (save b)
      (restore a)
      (restore b))))

(define (test)
  (start machine)
  (cons (get-register-contents machine 'a) (get-register-contents machine 'b)))

(test)


