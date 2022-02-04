#lang sicp

; === Rules ===

; Rules are means for abstracting queries (query procedures).

;(rule (lives-near ?person-1 ?person-2)
;      (and (address ?person-1 (?town . ?rest-1))
;           (address ?person-2 (?town . ?rest-2))
;           (not (same ?person-1 ?person-2))))

;(rule (same ?x ?x))

; Rules without bodies, as in same, are interpreted to mean that the rule conclusion
; is satisfied by any values of the variables.