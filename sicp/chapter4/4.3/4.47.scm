#lang sicp

; Exercise 4.47
; Louis Reasoner suggests that, since a verb phrase is either a
; verb or a verb phrase followed by a prepo- sitional phrase, it would be much
; more straightforward to define the procedure parse-verb-phrase as follows
; (and similarly for noun phrases):
;
;(define (parse-verb-phrase)
;  (amb (parse-word verbs)
;       (list 'verb-phrase
;             (parse-verb-phrase)
;             (parse-prepositional-phrase))))
;
; Does this work? Does the program’s behavior change if we interchange the order
; of expressions in the amb?

; -----------------------------------------------------------------------------

; Let's analyze both the original implementation and the proposed one, starting
; with the original:

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; the amb evaluator will generate the following sequence:
; 1st try: (amb verb-phrase)
; 2nd try: (amb verb-phrase
;               (amb verb-phrase1))
; 3rd try: (amb verb-phrase
;               (amb verb-phrase1
;                    (amb verb-phrase2)))

; amb has 2 arguments, where the 2nd one is always yet another amb

; Now let's look at the proposed one:
(define (parse-verb-phrase)
  (amb (parse-word verbs)
       (list 'verb-phrase
             (parse-verb-phrase)
             (parse-prepositional-phrase))))

; 1st try: (amb (parse-word verbs))
; 2nd try: (amb (parse-word verbs)
;               (list 'verb-phrase (amb (parse-word verbs)) (parse-prep)))
; 3rd try: (amb (parse-word verbs)
;               (list 'verb-phrase (amb (parse-word verbs)
;                                       (list 'verb-phrase (amb (parse-word verbs)) (parse-prep)))
;                                   (parse-prep)))

; Here amb also has 2 arguments, but the 2nd one is list

; I think that this proposed approach should work as well as the original one.

; After having ran the program with the proposed implementation of parse-verb-phrase
; it was made clear for me that the proposed approach fails, since it enters an
; infinite recursion loop in case amb fails to find any more values. The cause
; is the call to parse-verb-phrase procedure inside the list construction.











 
