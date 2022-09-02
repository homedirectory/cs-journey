#lang sicp

(#%require "../../helpers.scm" "machine.scm")

(define machine
  (make-machine
    '((assign p (const 1))
      (assign p (const 1))
      expt-loop
      (test (op =) (reg n) (const 0))
      (branch (label end))
      (assign n (op -) (reg n) (const 1))
      (assign p (op *) (reg b) (reg p))
      (goto (label expt-loop))
      end)
    ))

(set-register-contents! machine 'n 4)
(set-register-contents! machine 'b 2)

(machine 'trace-on)
;(trace-all-registers machine)
;(untrace-register machine 'pc)

(set-breakpoint machine 3)
(start machine)
(print "n: " (get-register-contents machine 'n) 
       " flag: " (get-register-contents machine 'flag))
;(rm-all-breakpoints machine)
; TODO infinite while loop
(while (not (eq? (proceed machine) 'done))
       (print "n: " (get-register-contents machine 'n) 
              " flag: " (get-register-contents machine 'flag)))
