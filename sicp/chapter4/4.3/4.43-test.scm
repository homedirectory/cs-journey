#lang sicp

(#%require "amb-core.scm")

; Exercise 4.43
; Use the amb evaluator to solve the following puzzle:
; Mary Ann Moore’s father has a yacht and so has each of his four friends:
; Colonel Downing, Mr. Hall, Sir Barnacle Hood, and Dr. Parker. Each of the five also
; has one daugh- ter and each has named his yacht after a daughter of one of the
; others. Sir Barnacle’s yacht is the Gabrielle, Mr. Moore owns the Lorna; Mr.
; Hall the Rosalind. The Melissa, owned by Colonel Downing, is named after Sir
; Barnacle’s daugh- ter. Gabrielle’s father owns the yacht that is named after
; Dr. Parker’s daughter. Who is Lorna’s father? Try to write the program so that
; it runs efficiently (see Ex- ercise 4.40). Also determine how many solutions
; there are if we are not told that Mary Ann’s last name is
; Moore.
 
; Father          | Daughter    | Yacht
; ----------------|-------------|----------
; Mr. Moore       | Mary Ann    | Lorna
; Mr. Hall        | Gabrielle   | Rosalind
; Colonel Downing | Lorna       | Melissa
; Sir Barnacle    | Melissa     | Gabrielle
; Dr. Parker      | Rosalind    | Mary Ann

; Solution: Lorna's father is Colonel Downing

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define fathers (list "Mr.Moore" "Mr.Hall" "Colonel Downing" "Sir Barnacle" "Dr. Parker"))
(define daughters (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))

(define (father-of-daughter-has-yacht daughter yacht fathers-d fathers-y)
  (cond ((null? fathers-d) false)
        ((and (equal? (car fathers-d) daughter) (equal? (car fathers-y) yacht)) true)
        (else (father-of-daughter-has-yacht daughter yacht (cdr fathers-d) (cdr fathers-y)))))

(define (yacht-puzzle)
  (let ((moore-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
        (moore-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
    ; moore is mary ann's father
    (require (equal? moore-d "Mary Ann"))
    ; moore owns lorna yacht
    (require (equal? moore-y "Lorna"))
    (let ((barnacle-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
          (barnacle-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
      ; barnacle owns gabrielle yacht
      (require (equal? barnacle-y "Gabrielle"))
      ; barnacle is melissa's father
      (require (equal? barnacle-d "Melissa"))
      (let ((hall-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
            (hall-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
        ; hall owns rosalind yacht
        (require (equal? hall-y "Rosalind"))
        (require (distinct? (list hall-d hall-y)))
        (let ((downing-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
              (downing-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
          ; downing owns melissa yacht
          (require (equal? downing-y "Melissa"))
          (require (distinct? (list downing-d downing-y)))
          (let ((parker-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
                (parker-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
            ; parker cannot be gabrielle's father
            (require (not (equal? parker-d "Gabrielle")))
            (require (distinct? (list parker-d parker-y)))
            ; gabrielle's father owns parker-d yacht
            (let ((fathers-d (list moore-d hall-d downing-d barnacle-d parker-d))
                  (fathers-y (list moore-y hall-y downing-y barnacle-y parker-y)))
              (require (father-of-daughter-has-yacht "Gabrielle" parker-d fathers-d fathers-y))
              (require (distinct? fathers-d))
              (require (distinct? fathers-y))
              (list fathers fathers-d fathers-y)
              ))))))
  )

(define solutions (yacht-puzzle))

; Determine how many solutions there are if we are not told that Mary Ann’s last name is Moore.
(define (yacht-puzzle-alt)
  (let ((moore-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
        (moore-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
    ; moore owns lorna yacht
    (require (equal? moore-y "Lorna"))
    (require (distinct? (list moore-d moore-y)))
    (let ((barnacle-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
          (barnacle-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
      ; barnacle owns gabrielle yacht
      (require (equal? barnacle-y "Gabrielle"))
      ; barnacle is melissa's father
      (require (equal? barnacle-d "Melissa"))
      (let ((hall-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
            (hall-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
        ; hall owns rosalind yacht
        (require (equal? hall-y "Rosalind"))
        (require (distinct? (list hall-d hall-y)))
        (let ((downing-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
              (downing-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
          ; downing owns melissa yacht
          (require (equal? downing-y "Melissa"))
          (require (distinct? (list downing-d downing-y)))
          (let ((parker-d (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind")))
                (parker-y (apply amb (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))))
            ; parker cannot be gabrielle's father
            (require (not (equal? parker-d "Gabrielle")))
            (require (distinct? (list parker-d parker-y)))
            ; gabrielle's father owns parker-d yacht
            (let ((fathers-d (list moore-d hall-d downing-d barnacle-d parker-d))
                  (fathers-y (list moore-y hall-y downing-y barnacle-y parker-y)))
              (require (father-of-daughter-has-yacht "Gabrielle" parker-d fathers-d fathers-y))
              (require (distinct? fathers-d))
              (require (distinct? fathers-y))
              (list fathers fathers-d fathers-y)
              ))))))
  )

(define solutions-alt (yacht-puzzle-alt))
