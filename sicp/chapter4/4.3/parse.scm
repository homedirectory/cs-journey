#lang sicp

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

(define (parse-sentence)
  (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))
