#lang sicp

(#%require "machine.scm" "machines.scm" "../helpers.scm")
(#%provide (all-defined))

; a. Recursive exponentiation:
(define (expta b n)
  (if (= n 0)
    1
    (* b (expta b (- n 1)))))

; regs: b, n, val
(define expt-rec-machine
  (extend-machine
    (make-simple-machine)
    '(b n val continue)
    '()
    '((assign continue (label end))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label base-case))
      (assign n (op -) (reg n) (const 1))
      (save continue)
      (assign continue (label after-expt))
      (goto (label expt-loop))

      after-expt
      ; val now contains (expt b (- n 1))
      (assign val (op *) (reg b) (reg val))
      (restore continue)
      (goto (reg continue))

      base-case
      (assign val (const 1))
      (goto (reg continue))

      end)))

; TEST
(define (expt-rec base n)
  (set-register-contents! expt-rec-machine 'b base)
  (set-register-contents! expt-rec-machine 'n n)
  (set-register-contents! expt-rec-machine 'val 0)
  (start expt-rec-machine)
  (get-register-contents expt-rec-machine 'val))

(print (expt-rec 2 3))
(print (expt-rec 4 4))
(print (expt-rec 2 10))

; b. Iterative exponentiation:
(define (exptb b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
                 (* b product))))
  (expt-iter n 1))

; regs: b, n (both n & counter), p (product)
(define expt-iter-machine
  (extend-machine
    (make-simple-machine)
    '(b n p)
    '()
    '((assign p (const 1))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label end))
      (assign n (op -) (reg n) (const 1))
      (assign p (op *) (reg b) (reg p))
      (goto (label expt-loop))
      end
      )))

(define (expt-iter base n)
  (set-register-contents! expt-iter-machine 'b base)
  (set-register-contents! expt-iter-machine 'n n)
  (set-register-contents! expt-iter-machine 'p 1)
  (start expt-iter-machine)
  (get-register-contents expt-iter-machine 'p))

(print (expt-iter 2 3))
(print (expt-iter 4 4))
(print (expt-iter 2 10))
