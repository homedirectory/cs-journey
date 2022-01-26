#lang sicp

(#%require "../../helpers.scm")

; Exercise 4.42
; Solve the following “Liars” puzzle (from Phillips 1934):
; Five schoolgirls sat for an examination. Their parents—so they thought—showed an
; undue degree of interest in the result. They therefore agreed that, in writing
; home about the examination, each girl should make one true statement and one
; untrue one. The following are the relevant passages from their letters:
;
; • Betty: “Kitty was second in the examination. I was only third.”
; • Ethel: “You’ll be glad to hear that I was on top. Joan was 2nd.”
; • Joan: “I was third, and poor old Ethel was bottom.”
; • Kitty: “I came out second. Mary was only fourth.”
; • Mary: “I was fourth. Top place was taken by Betty.”
;
; What in fact was the order in which the five girls were placed?


(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (liars-puzzle)
  (define (make-true-statement girl-index place)
    (lambda (girls)
      (let ((item (list-ref girl-index girls)))
        ; if item is empty (= -1) then there has been yet no claim about it
        ; if item = place, then this statement reaffirms the previously made one
        ; it item != place, then there is a contradiction
        (if (or (= -1 item) (= place item))
            (list-replace-at girl-index place girls)
            #f))))
  (define (make-false-check girl-index place)
    (lambda (girls)
      (not (= place (list-ref girl-index girls)))))
  (define (make-statement-pair girl-index place)
    (cons (make-true-statement girl-index place)
          (make-false-check girl-index place)))
   
  (define (check statements)
    ; statements: list
    ; Each list item represents a particular passage in the form of a nested pair:
    ; (1st statement . 2nd statement)
    ; statement: (true form . false form)
    ; Where true form is a procedure that accepts a list and returns a new list
    ; or signals false,
    ; and false form is a procedure that accepts a list and returns a boolean value.
    ;
    ; First, all true statements are applied, then all false statements are checked.
    (define (check-false-statements checks girls)
      (if (null? checks)
          girls
          (if ((car checks) girls)
              (check-false-statements (cdr checks) girls)
              (begin
                (print girls)
                '()))))
    (define (iter stats girls false-checks)
      ; girls: list represents the schoolgirls' placements in the following order:
      ; [Betty, Ethel, Joan, Kitty, Mary]
      (print girls)
      (cond ((not girls) '())
            ((null? stats)
             (if (distinct? girls)
                 (check-false-statements false-checks girls)
                 '()))
            (else (let ((s (car stats)))
                    (let ((s1 (car s))
                          (s2 (cdr s)))
                      (let ((s1-true (car s1))
                            (s1-false (cdr s1))
                            (s2-true (car s2))
                            (s2-false (cdr s2)))
                        (cons
                         (iter (cdr stats) (s1-true girls) (append false-checks (list s2-false)))
                         (iter (cdr stats) (s2-true girls) (append false-checks (list s1-false))))
                        ))))))
    
    (iter statements (list -1 -1 -1 -1 -1) '())
    )

  ; note: [Betty, Ethel, Joan, Kitty, Mary]
  ;          0      1      2     3     4
  (check (list
          ; Betty
          (cons
           (make-statement-pair 3 2)  ; kitty 2
           (make-statement-pair 0 3)) ; betty 3
          ; Ethel
          (cons
           (make-statement-pair 1 1)  ; ethel 1
           (make-statement-pair 2 2)) ; joan 2
          ; Joan:
          (cons
           (make-statement-pair 2 3)  ; joan 3
           (make-statement-pair 1 5)) ; ethel 5
          ; Kitty
          (cons
           (make-statement-pair 3 2)  ; kitty 2
           (make-statement-pair 4 4)) ; mary 4
          ; Mary
          (cons
           (make-statement-pair 4 4)  ; mary 4
           (make-statement-pair 0 1)) ; betty 1
          )))

(define solutions (liars-puzzle))
; (3 5 2 -1 4)
; -1 for Kitty means that she takes the remaining place, that is, 1st place.
; Solution:
; Girl  | Place
; ------|-------
; Kitty | 1
; Joan  | 2
; Betty | 3
; Mary  | 4
; Ethel | 5