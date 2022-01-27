#lang sicp

(#%require "parse.scm")

; Exercise 4.48
; Extend the grammar given above to handle more complex sentences.
; For example, you could extend noun phrases and verb phrases to include
; adjectives and adverbs, or you could handle compound sentences.

;------------------------------------------------------------------------

; Adjectives might be placed between a noun and an article.
; An adjective might be followed by an arbitrary amount of adjectives.
; For example, "A large blue pretty cat sleeps"

; Therefore the representation of a noun-phrase must be changed.
; There can no longer exist a simple-noun-phrase since it does not allow
; anything between an article and a noun.

; The new representation shall be as follows:
; ('simple-noun-phrase <article> <adjective_1> ... <adjective_n> <noun>)

(define adjectives '(adj blue large pretty natural boring))

(define (parse-adjectives)
  (define (maybe-extend adj-list)
    (amb adj-list
         (maybe-extend (append adj-list
                               (list (parse-word adjectives))))))
  (maybe-extend (list (parse-word adjectives))))

(define (parse-simple-noun-phrase)
  ; append is needed since parse-adjectives returns a list
  (append
   (append (list 'simple-noun-phrase (parse-word articles))
           (parse-adjectives))
   (list (parse-word nouns))))


; Adverbs might be placed right after a verb.
; An adverb might be followed by an arbitrary amount of adverbs.

; Example: The professor lectures gracefully to the student.

; There is a need for a new verb-phrase representation:
; ('verb-phrase <verb> <adverb_1> ... <adverb_2> <adjective> <prep>)

(define adverbs '(adverb slowly peacefully obediently gracefully unexpectedly))

(define (parse-adverbs)
  ; returns a list
  (define (maybe-extend adv-list)
    (amb adv-list
         (maybe-extend (append adv-list
                               (list (parse-word adverbs))))))
  (maybe-extend (list (parse-word adverbs))))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (append
           (append (list 'verb-phrase verb-phrase)
                   (parse-adverbs))
           (list (parse-prepositional-phrase))))))
  (maybe-extend (parse-word verbs)))



