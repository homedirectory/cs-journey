#lang sicp

(#%require "machine.scm" "../machines.scm" "../../helpers.scm")

(define machine
  (make-machine
    basic-ops
    '(controller
       (assign val (const 1))
       (assign continue (label fact-done))
       ;set up final return address
       fact-loop
       (test (op <=) (reg n) (const 1))
       (branch (label base-case))
       ;; Set up for the recursive call by saving n and continue.
       ;; Set up continue so that the computation will continue
       ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val)) ;val now contains n(n - 1)!
       (goto (reg continue))
       ;return to caller
       base-case
       (assign val (const 1))
       ;base case: 1! = 1
       (goto (reg continue))
       ;return to caller
       fact-done)
    ))

(define (factorial n)
  (reset machine)
  (set-register-contents! machine 'n n)
  (start machine)
  (print "factorial " n " = " (get-register-contents machine 'val)) 
  (print-stack-stats machine))

(for-each factorial (range 1 10))

; total-pushes = maximum-depth = 2 * (n-1)
