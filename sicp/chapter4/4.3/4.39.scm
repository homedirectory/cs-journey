#lang sicp

(#%require "amb-core.scm" "4.38.scm")

; Exercise 4.39
; Does the order of the restrictions in the multiple-dwelling
; procedure affect the answer? Does it affect the time to find an answer?
; If you think it matters, demonstrate a faster program obtained from
; the given one by reordering the restrictions. If you think it does not
; matter, argue your case.

; Total cases: 5^5 = 3125
; distinct: 5! = 120 
; miller > cooper: 10 * 5^3 = 1250

; In my opinion distinct? procedure call must be the last one, since it's
; the most computationally expensive. "miller > cooper" restriction eliminates
; almost 2000 cases at once with a simple check, thus it must definitely be
; the first one. Non-adjacency restrictions eliminate more cases then those
; which check a particular floor of a resident, so they are also placed at the start,
; after the "miller > cooper" check.

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (> (abs (- smith fletcher)) 1))
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
          (list 'smith smith))))

; Here is another, even more efficient version. I got inspired by revc's solution
; at: http://community.schemewiki.org/?sicp-ex-4.39
; It breaks up the one let statement into many to avoid redundant computation.
; For example, the first check (miller > cooper) does not make use of
; fletcher, miller & smith.
; However, if the evaluator is making use of lazy evaluation, then the below
; solution is the same in terms of efficiency.


(define (multiple-dwelling-opt)
  (let ((cooper (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
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
                  (list 'smith smith)))))))

