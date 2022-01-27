#lang sicp

(#%require "amb-core.scm")

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define prepositions '(prep for to in by with))
(define articles '(article the a))
(define adverbs '(adverb slowly peacefully obediently gracefully unexpectedly))
(define adjectives '(adj blue large pretty natural boring))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                noun-phrase
                (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

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

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))

(#%provide (all-defined))