#lang sicp

; Exercise 4.64
; Louis Reasoner mistakenly deletes the outranked-by rule (Section 4.4.1)
; from the data base. When he realizes this, he quickly reinstalls it.
; Unfortunately, he makes a slight change in the rule, and types it in as

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

; Just after Louis types this information into the system, DeWitt Aull comes by
; to find out who outranks Ben Bitdiddle. He issues the query
(outranked-by (Bitdiddle Ben) ?who)
; After answering, the system goes into an infinite loop. Explain why.
;-------------------------------------------------------------------------------

; The recursive call to outranked-by comes before (supervisor ?staff-person ?middle-manager),
; thus there is no termination point, as the rule will try to recursively call itself each time to find
; the intersection:
;(and (outranked-by ?middle-manager ?boss)
;     (supervisor ?staff-person ?middle-manager))

; Were the rule reinstalled correctly like the following:
(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))
; It would terminate when (supervisor ?staff-person ?middle-manager) returned an empty set

