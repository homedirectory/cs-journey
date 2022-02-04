#lang sicp

; Exercise 4.57
; Define a rule that says that person 1 can replace person 2 if either
; person 1 does the same job as person 2 or someone who does person 1’s job
; can also do person 2’s job, and if person 1 and person 2 are not the same person.
; Using your rule, give queries that find the following:

(rule (can-replace ?person1 ?person2)
      (and (not (same ?person1 ?person2))
           (or (and (job ?person1 ?job1)
                    (job ?person2 ?job1))
               (and (job ?person2 ?job2)
                    (can-do-job ?job1 ?job2)))))

; a. all people who can replace Cy D. Fect
(can-replace ?person (Cy D. Fect))

; b. all people who can replace someone who is being paid more than they are,
;    together with the two salaries.
(and (salary ?person1 ?amount1)
     (salary ?person2 ?amount2)
     (lisp-value > ?amount2 ?amount1)
     (can-replace ?person1 ?person2))
 
