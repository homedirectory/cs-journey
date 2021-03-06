#lang sicp

(#%require "parse.scm" "../../helpers.scm")

; Exercise 4.49: Alyssa P. Hacker is more interested in generating interesting
; sentences than in parsing them. She reasons that by simply changing the
; procedure parse-word so that it ignores the “input sentence” and instead
; always succeeds and generates an appropriate word, we can use the programs
; we had built for parsing to do generation instead. Implement Alyssa’s idea,
; and show the first half-dozen or so sentences generated

(define (parse-word word-list)
  (require (not (null? word-list)))
  (amb (car word-list) (parse-word (cdr word-list))))

(define (generate-sentence noun-preps noun-adjs noun-prep-nouns
                           verb-preps verb-advs verb-prep-nouns)
  (list (parse-noun-phrase noun-preps noun-adjs noun-prep-nouns)
        (parse-verb-phrase verb-preps verb-advs verb-prep-nouns)))

(define (generate-sentence noun-preps noun-adjs
                           noun-prep-nouns noun-prep-adjs
                           verb-preps verb-advs
                           verb-prep-nouns verb-prep-adjs)
  (flatten
   (list (parse-noun-phrase noun-preps noun-adjs noun-prep-nouns noun-prep-adjs)
         (parse-verb-phrase verb-preps verb-advs verb-prep-nouns verb-prep-adjs))))

(define (generate-sentence-simple)
  (generate-sentence 0 1 0 0
                     0 1 0 0))

(define (generate-sentence-medium)
  (generate-sentence 1 0 1 0
                     1 0 1 0))

; all calls to parse-word must pass (cdr <word-list>) as an argument instead of
; <word-list>, since first item in the list is the name of part of speech
; (e.g. "noun", "article")

(define (parse-prepositional-phrase noun-depth)
  (list (parse-word (cdr prepositions))
        (parse-noun-phrase (dec noun-depth) noun-depth)))

(define (parse-noun-phrase preps-depth adj-depth prep-noun-depth prep-noun-adj-depth)
  (define (maybe-extend noun-phrase preps-c)
    (if (= 0 preps-depth)
        (amb noun-phrase)
        (begin
          (require (<= preps-c preps-depth))
          (amb noun-phrase
               (maybe-extend
                (list noun-phrase
                      (parse-prepositional-phrase prep-noun-depth prep-noun-adj-depth))
                (inc preps-c))))))
  (maybe-extend (parse-simple-noun-phrase adj-depth) 0))

(define (parse-article adj-depth)
  ; an article may be extended with adjectives
  (define (maybe-extend article adj-c)
    (require (<= adj-c adj-depth))
    (amb article
         (maybe-extend (list article
                             (parse-word (cdr adjectives)))
                       (inc adj-c))))
  (maybe-extend (parse-word (cdr articles)) 0))

(define (parse-simple-noun-phrase adj-depth)
  (list (parse-article adj-depth)
        (parse-word (cdr nouns))))

(define (parse-verb adverb-depth)
  ; a verb may be extended with adverbs
  (define (maybe-extend adverb adverb-c)
    (require (<= adverb-c adverb-depth))
    (amb adverb
         (maybe-extend (list adverb
                             (parse-word (cdr adverbs)))
                       (inc adverb-c))))
  (maybe-extend (parse-word (cdr verbs)) 0))

(define (parse-verb-phrase preps-depth adverb-depth prep-noun-depth prep-noun-adj-depth)
  (define (maybe-extend verb-phrase preps-c)
    (if (= 0 preps-depth)
        (amb verb-phrase)
        (begin
          (require (<= preps-c preps-depth))
          (amb verb-phrase
               (maybe-extend
                (list verb-phrase
                      (parse-prepositional-phrase prep-noun-depth prep-noun-adj-depth))
                (inc preps-c))))))
  (maybe-extend (parse-verb adverb-depth) 0))
