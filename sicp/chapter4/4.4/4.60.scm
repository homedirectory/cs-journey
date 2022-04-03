#lang sicp

; Exercise 4.60
; By giving the query
(lives-near ?person (Hacker Alyssa P))

; Alyssa P. Hacker is able to find people who live near her, with whom she can
; ride to work. On the other hand, when she tries to find all pairs of people
; who live near each other by querying
(lives-near ?person-1 ?person-2) 

; she notices that each pair of people who live near each other is listed twice;
; for example,
(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

; Why does this happen? Is there a way to find a list of people who live near each other,
; in which each pair appears only once? Explain.

; Here is the lives-near rule:
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
; This happens because the pattern matching mechanism in the process of substituting variables
; with values will go through these steps among other persons that may exist:
; (lives-near (Hacker Alyssa P) (Fect Cy D))
; (lives-near (Fect Cy D) (Hacker Alyssa P))

; The problem is that some rules have symmetric arguments, that is, the order
; of values passed inside the rule does not matter (e.g. lives-near),
; while some rules are the opposite (e.g. (can-replace ?person ?person-to-replace))

; There is a way around this problem that is to implement a special form to specify
; when the order of the arguments does not matter.