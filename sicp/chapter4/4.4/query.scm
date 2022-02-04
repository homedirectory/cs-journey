#lang sicp

; === Simple queries ===

; The query language allows users to retrieve information from the data base by
; posing queries in response to the system’s prompt. For example, to find all
; computer programmers one can say
;;;; Query input: (job ?x (computer progammer))

; The system will respond with the following items:
;;;; Query results:
;(job (Hacker Alyssa P) (computer programmer))
;(job (Fect Cy D) (computer programmer))

; The “anything” that can be the second item in the matching list is specified by
; a pattern variable, ?x. The general form of a pattern is ? and symbol.

; For example, The query
;(job ?x (computer ?type))
; matches all job entries whose third item is a two-element list whose first
; item is computer:
;(job (Bitdiddle Ben) (computer wizard))
;(job (Hacker Alyssa P) (computer programmer))
;(job (Fect Cy D) (computer programmer))
;(job (Tweakit Lem E) (computer technician))

; In case the pattern has no variables, the query reduces to a determination
; of whether that pattern is in the data base. If so, the empty assignment, which
; assigns no values to variables, satisfies that pattern for that data base.



; === Compound queries ===

; One thing that makes the query language a logic programming language
; is that the means of combination mirror the means of combination used
; in forming logical expressions: and, or & not.

; (and <query_1> ... <query_n>)
; Find addresses of all computer programmers:
;(and (job     ?person               (computer programmer))
;     (address ?person               ?where))

;(and (job     (Hacker Alyssa P)     (computer programmer))
;     (address (Hacker Alyssa P)     (Cambridge (Mass Ave)      78)))
;(and (job     (Fect Cy D)           (computer programmer))
;     (address (Fect Cy D)           (Cambridge (Ames Street)   3)))

; (or <query_1> ... <query_n>)
; Find all employees supervised either by Ben or Alyssa:
;(or (supervisor ?x                (Bitdiddle Ben))
;    (supervisor ?x                (Hacker Alyssa P)))

;(or (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
;    (supervisor (Hacker Alyssa P) (Hacker Alyssa P)))
;(or (supervisor (Fect Cy D)       (Bitdiddle Ben))
;    (supervisor (Fect Cy D)       (Hacker Alyssa P)))
;(or (supervisor (Tweakit Lem E)   (Bitdiddle Ben))
;    (supervisor (Tweakit Lem E)   (Hacker Alyssa P)))
;(or (supervisor (Reasoner Louis)  (Bitdiddle Ben))
;    (supervisor (Reasoner Louis)  (Hacker Alyssa P)))

; (not <query>)
; Straightforward. Returns all assignments to the pattern that to do not satisfy
; the <query>.


; (lisp-value <predicate> <arg_1> ... <arg_n>)
; Find all people whose salary is > 30000$:
;(and (salary ?person ?amount) (lisp-value > ?amount 30000))
