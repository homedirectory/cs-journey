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

(define (parse-article)
  ; an article may be extended with adjectives
  (define (maybe-extend article)
    (amb article
         (maybe-extend
          (list article
                (parse-word adjectives)))))
  (maybe-extend (parse-word articles)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-article)
        (parse-word nouns)))


; Adverbs might be placed right after a verb.
; An adverb might be followed by an arbitrary amount of adverbs.

; Example: The professor lectures gracefully to the student.

; There is a need for a new verb-phrase representation:
; ('verb-phrase <verb> <adverb_1> ... <adverb_2> <adjective> <prep>)

(define adverbs '(adverb slowly peacefully obediently gracefully unexpectedly))

(define (parse-adverb)
  ; an adverb might be extended with other adverbs
  (define (maybe-extend adverb)
    (amb adverb
         (maybe-extend (list adverb
                             (parse-word adverbs)))))
  (maybe-extend (parse-word adverbs)))

(define (parse-verb)
  ; a verb might be extended with an adverb
  (define (maybe-extend verb)
    (amb verb
         (maybe-extend (list verb
                             (parse-adverb)))))
  (maybe-extend (parse-word verbs)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend
          (list 'verb-phrase
                verb-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-verb)))