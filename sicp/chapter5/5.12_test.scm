#lang sicp

(#%require "5.12.scm" "../helpers.scm")

; Fibbonacci machine
(define machine
  (make-machine
    '(n val continue)
    (list (list '+ +) (list '- -) (list '= =) (list '< <))
    '((assign continue (label fib-done))
      fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1)) 
      (goto (label fib-loop))
      afterfib-n-1
      (restore n)
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
      (restore val)
      afterfib-n-2
      (assign n (reg val))
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
      immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)
    ))


(print "##### instructions-by-type #####")
(for-each (lambda (x)
            (print (car x) ": " (cdr x))) 
          (machine 'instructions-by-type))

(newline)
(print "##### entry-point-registers #####")
(print (machine 'entry-point-registers))

(newline)
(print "##### save-restore-registers #####")
(print (machine 'save-restore-registers))

(newline)
(print "##### registers-sources #####")
(for-each (lambda (x)
            (print (car x) ": " (cdr x))) 
          (machine 'registers-sources))
