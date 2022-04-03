#lang sicp

; Exercise 4.69
; Beginning with the data base and the rules you formulated in Exercise 4.63,
; devise a rule for adding “greats” to a grandson relationship.
; This should enable the system to deduce that Irad is the great-grandson of
; Adam, or that Jabal and Jubal are the great-great-great-great-great-grandsons
; of Adam.
; (Hint: Represent the fact about Irad, for example, as ((great grandson) Adam Irad).
; Write rules that determine if a list ends in the word grandson.
; Use this to express a rule that allows one to derive the relationship ((great . ?rel) ?x ?y),
; where ?rel is a list ending in grandson.)
; Check your rules on queries such as ((great grandson) ?g ?ggs) and
; (?relationship Adam Irad).
;-----------------------------------------------------------------------------------------------

(rule (ends-with-grandson ?lst)
      (last-pair ?lst (grandson)))
(rule (starts-with-grandson (?car . ?cdr))
      (same grandson ?car))

(rule (greats (great . ?rel) ?g ?ggs)
      (and (ends-with-grandson ?rel)
           (sons ?f ?ggs)
           (or 
            (and (grandsons ?g ?f)
                 (starts-with-grandson ?rel))
            (greats ?rel ?f ?ggs))))
