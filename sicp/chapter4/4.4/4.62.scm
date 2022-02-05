#lang sicp

; Exercise 4.62
; Define rules to implement the last-pair operation of Exercise 2.17,
; which returns a list containing the last element of a nonempty list.

(rule (last-pair (?last) (?last)))
(rule (last-pair (?car . ?cdr) (?last))
      (last-pair ?cdr (?last)))

; Check your rules on queries such as (last-pair (3) ?x),
; (last-pair (1 2 3) ?x) and (last-pair (2 ?x) (3)).
; Do your rules work correctly on queries such as (last-pair ?x (3)) ?

(last-pair (3) ?x)
; - (last-pair (3) (3))

(last-pair (1 2 3) ?x)
; - (last-pair (1 2 3) (3))

(last-pair (2 ?x) (3))
; - (last-pair (2 3) (3))