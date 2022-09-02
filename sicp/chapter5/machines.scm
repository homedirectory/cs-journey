#lang sicp

(#%require "machine.scm")
(#%provide (all-defined))

(define basic-ops 
  (list (list '> >) (list '= =) (list '< <)
        (list '>= >=) (list '<= <=)
        (list '* *) (list '+ +) (list '- -)
        (list '/ /)))

(define (extend-machine machine register-names ops controller-text)
  (for-each
    (lambda (register-name)
      ((machine 'allocate-register) register-name))
    register-names)
  ((machine 'install-operations) ops)
  ((machine 'install-instruction-sequence)
   (assemble controller-text machine))
  machine)


(define (make-simple-machine)
  (make-machine
    '() ; no registers
    basic-ops
    '() ; no controller text
    ))
