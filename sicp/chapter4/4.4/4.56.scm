#lang sicp

; Exercise 4.56
; Formulate compound queries that retrieve the following information:

; a. the names of all people who are supervised by Ben Bitdiddle,
;    together with their addresses
(and (supervisor ?name (Ben Bitdiddle))
     (address ?name . ?rest))

; b. all people whose salary is less than Ben Bitdiddle’s,
;    together with their salary and Ben Bitdiddle’s salary
(and (salary (Ben Bitdiddle) ?salary-ben)
     (salary ?name ?salary)
     (lisp-value < ?salary ?salary-ben))

;  c. all people who are supervised by someone who is not in the computer division,
;     together with the supervisor’s name and job
(and (not (job ?super-name (computer . ?rest)))
     (supervisor ?name ?super-name))