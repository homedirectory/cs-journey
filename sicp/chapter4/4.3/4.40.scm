#lang sicp

(#%require "amb-core.scm" "4.38.scm")

; Exercise 4.40
; In the multiple dwelling problem, how many sets of assignments
; are there of people to floors, both be- fore and after the requirement that
; floor assignments be distinct? It is very inefficient to generate all possible
; assignments of people to floors and then leave it to backtracking to
; eliminate them. For example, most of the restrictions de- pend on only one or
; two of the person-floor variables, and can thus be imposed before floors have
; been selected for all the people. Write and demonstrate a much more efficient
; nondeterministic procedure that solves this problem based upon
; generating only those possibilities that are not already ruled out by previous
; restrictions. (Hint: This will require a nest of let expressions.)

; My optimized version from 4.39
(define (multiple-dwelling)
  (let ((cooper (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
      (require (> (abs (- smith fletcher)) 1))
      (let ((baker (amb 1 2 3 4 5)))
        (require (> (abs (- fletcher cooper)) 1))
        (require (not (= baker 5)))
        (require (not (= cooper 1)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require
          (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher) (list 'miller miller)
              (list 'smith smith))))))

(define (multiple-dwelling-alt)
  (let ((fletcher (amb 1 2 3 4 5)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (require (> (abs (- fletcher cooper)) 1))
      (let ((miller (amb 1 2 3 4 5)))
        (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
          (require (> (abs (- smith fletcher)) 1))
          (let ((baker (amb 1 2 3 4 5)))
            (require (not (= baker 5)))
            (require
              (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher) (list 'miller miller)
                  (list 'smith smith))))))))

; This is even more efficient, but I'm not sure if it fits into the deterministic
; paradigm we have been following.
(define (multiple-dwelling-opt)
  (let ((cooper (amb 2 4 5))
        (miller (amb 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4))
          (smith (amb 1 2 4 5)))
      (require (> (abs (- smith fletcher)) 1))
      (require (> (abs (- fletcher cooper)) 1))
      (let ((baker (amb 1 2 3 4)))
        (require
          (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'baker baker)
              (list 'cooper cooper)
              (list 'fletcher fletcher) (list 'miller miller)
              (list 'smith smith))))))