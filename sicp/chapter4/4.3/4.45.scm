#lang sicp

; Exercise 4.45
; With the grammar given above, the following sentence can be
; parsed in five different ways: “The professor lectures to the student in the
; class with the cat.” Give the five parses and explain the differences in
; shades of meaning among them.
 
(sentence
 ; START: noun-phrase
 ; START: simple-noun-phrase
 (simple-noun-phrase (article the) (noun professor))
 ; END: simple-noun-phrase
 ; END: noun-phrase
 ; START: verb-phrase
 ; === SPLIT ===
 (verb-phrase (verb lectures)
              ; START: prep-phrase
              (prep-phrase (prep to)
                           ; START: noun-phrase
                           ; START: simple-noun-phrase
                           (simple-noun-phrase (article the) (noun student))
                           ; END: simple-noun-phrase
                           ; === SPLIT ===
                           ; START: prep-phrase
                           (prep-phrase (prep in)
                                        ; START: noun-phrase
                                        ; START: simple-noun-phrase
                                        (simple-noun-phrase (article the) (noun class))
                                        ; END: simple-noun-phrase
                                        ; === SPLIT ===
                                        ; START: prep-phrase
                                        (prep-phrase (prep with)
                                                     ; START: noun-phrase
                                                     ; START: simple-noun-phrase
                                                     (simple-noun-phrase (article the) (noun cat))
                                                     ; END: simple-noun-phrase
                                                     ; END: noun-phrase
                                                     )
                                        ; END: prep-phrase
                                        ; END: noun-phrase
                                        )
                           ; END: prep-phrase
                           ; END: noun-phrase
                           )
              ; END: prep-phrase
              )
 ; END: verb-phrase
 )

; 1. the student is in the class
;    the class is with the cat
; ---------------------------------------------------------------------
; noun-phrase:
;     simple-noun-phrase: The professor
; verb-phrase:
;     verb: lectures
;     prep-phrase:
;         prep: to
;         noun-phrase:
;             simple-noun-phrase: the student
;             prep-phrase:
;                 prep: in
;                 noun-phrase:
;                     simple-noun-phrase: the class
;                     prep-phrase:
;                         prep: with
;                         noun-phrase:
;                            simple-noun-phrase: the cat
; ---------------------------------------------------------------------


; 2. professor lectures in the class
;    the class is with the cat
; ---------------------------------------------------------------------
; noun-phrase:
;     simple-noun-phrase: The professor
; verb-phrase:
;      verb-phrase:
;      |   verb: lectures
;      |   prep-phrase:
;      |       prep: to
;      |       noun-phrase:
;      |           simple-noun-phrase: the student
;      prep-phrase:
;          prep: in
;          noun-phrase:
;              simple-noun-phrase: the class
;              prep-phrase:
;                  prep: with
;                  noun-phrase:
;                     simple-noun-phrase: the cat
; ---------------------------------------------------------------------


; 3. professor lectures in the class
;    professor lectures with the cat
; ---------------------------------------------------------------------
; noun-phrase:
;     simple-noun-phrase: The professor
; verb-phrase:
;     verb-phrase:
;     |    verb-phrase:
;     |    |   verb: lectures
;     |    |   prep-phrase:
;     |    |       prep: to
;     |    |       noun-phrase:
;     |    |           simple-noun-phrase: the student
;     |   prep-phrase:
;     |       prep: in
;     |       noun-phrase:
;     |           simple-noun-phrase: the class
;     prep-phrase:
;         prep: with
;         noun-phrase:
;            simple-noun-phrase: the cat
; ---------------------------------------------------------------------


; 4. the student is in the class
;    the student is with the cat
; ---------------------------------------------------------------------
; noun-phrase:
;     simple-noun-phrase: The professor
; verb-phrase:
;      verb-phrase:
;          verb: lectures
;          prep-phrase:
;              prep: to
;              noun-phrase:
;                  noun-phrase:
;                  |   simple-noun-phrase: the student
;                  |   prep-phrase:
;                  |       prep: in
;                  |       noun-phrase:
;                  |           simple-noun-phrase: the class
;                  prep-phrase:
;                      prep: with
;                      noun-phrase:
;                         simple-noun-phrase: the cat
; ---------------------------------------------------------------------


; 5. the student is in the class
;    professor lectures with the cat
; ---------------------------------------------------------------------
; noun-phrase:
;     simple-noun-phrase: The professor
; verb-phrase:
;      verb-phrase:
;      |   verb: lectures
;      |   prep-phrase:
;      |       prep: to
;      |       noun-phrase:
;      |           simple-noun-phrase: the student
;      |           prep-phrase:
;      |               prep: in
;      |               noun-phrase:
;      |                   simple-noun-phrase: the class
;      prep-phrase:
;          prep: with
;          noun-phrase:
;             simple-noun-phrase: the cat
; ---------------------------------------------------------------------