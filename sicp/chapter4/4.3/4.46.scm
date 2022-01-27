#lang sicp

; Exercise 4.46
; The evaluators in Section 4.1 and Section 4.2 do not determine
; what order operands are evaluated in. We will see that the amb evaluator
; evaluates them from left to right. Explain why our parsing program wouldn’t
; work if the operands were evaluated in some other order.
 
; Take a look at the following procedure for example:

;(define (parse-verb-phrase)
;  (define (maybe-extend verb-phrase)
;    (amb verb-phrase
;         (maybe-extend
;          (list 'verb-phrase
;                verb-phrase
;                (parse-prepositional-phrase)))))
;  (maybe-extend (parse-word verbs)))

; Evaluating amb arguments from right-to-left would cause an infinite
; recursion into the maybe-extend procedure.