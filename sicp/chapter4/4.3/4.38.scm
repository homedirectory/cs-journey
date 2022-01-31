#lang sicp

(#%require "amb-core.scm")

; Exercise 4.38
; Modify the multiple-dwelling procedure to omit the requirement
; that Smith and Fletcher do not live on adjacent floors.
; How many solutions are there to this modified puzzle?

; Puzzle:
; Baker, Cooper, Fletcher, Miller, and Smith live on different
; floors of an apartment house that contains only five floors.
; Baker does not live on the top floor. Cooper does not live on the
; bottom floor. Fletcher does not live on either the top or
; the bottom floor. Miller lives on a higher floor than does Cooper.
; Smith does not live on a floor adjacent to Fletcher’s.
; Fletcher does not live on a floor adjacent to Cooper’s.
; Where does everyone live?

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))

; - With the omitted requirement there are 5 solutions:
; 5 | 4 | 3 | 2 | 1
; M | C | B | F | S
; M | C | S | F | B
; S | F | M | C | B
; M | F | S | C | B
; M | F | B | C | S

(define (multiple-dwelling-omitted)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher) (list 'miller miller)
          (list 'smith smith))))
