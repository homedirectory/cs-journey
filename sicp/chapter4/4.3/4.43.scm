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

; Father          | Daughter    | Yacht
; ----------------|-------------|----------
; Mr. Moore       |    Mary Ann | Lorna
; Mr. Hall        |             | Rosalind
; Colonel Downing |             | Melissa
; Sir Barnacle    |     Melissa | Gabrielle
; Dr. Parker      |             | Mary Ann

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define fathers (list "Mr.Moore" "Mr.Hall" "Colonel Downing" "Sir Barnacle" "Dr. Parker"))
(define daughters (list "Mary Ann" "Gabrielle" "Lorna" "Melissa" "Rosalind"))

(define (father-of-daughter-has-yacht daughter yacht fathers-d fathers-y)
  (cond ((null? fathers-d) #f)
        ((and (equal? (car fathers-d) daughter) (equal? (car fathers-y) yacht)) #t)
        (else (father-of-daughter-has-yacht daughter yacht (cdr fathers-d) (cdr fathers-y)))))

(define (yacht-puzzle)
  (let ((moore-d (amb daughters))
        (moore-y (amb daughters)))
    ; moore is mary ann's father
    (require (equal? moore-d "Mary Ann"))
    ; moore owns lorna yacht
    (require (equal? moore-y "Lorna"))
    (let ((barnacle-d (amb daughters))
          (barnacle-y (amb daughters)))
      ; barnacle owns gabrielle yacht
      (require (equal? barnacle-y "Gabrielle"))
      ; barnacle is melissa's father
      (require (equal? barnacle-d "Melissa"))
      (let ((hall-y (amb daughters))
            (hall-d (amb daughters)))
        ; hall owns rosalind yacht
        (require (equal? hall-y "Rosalind"))
        (require (distinct? (list hall-d hall-y)))
        (let ((downing-d (amb daughters))
              (downing-y (amb daughters)))
          ; downing owns melissa yacht
          (require (equal? downing-y "Melissa"))
          (require (distinct? (list downing-d downing-y)))
          (let ((parker-d (amb daughters))
                (parker-y (amb daughters)))
            ; parker cannot be gabrielle's father
            (require (not (equal? parker-d "Gabrielle")))
            (require (distinct? (list parker-d parker-y)))
            ; gabrielle's father owns parker-d yacht
            (let ((fathers-d (list moore-d hall-d downing-d barnacle-d parker-d))
                  (fathers-y (list moore-y hall-y downing-y barnacle-y parker-y)))
              (require (father-of-daughter-has-yacht "Gabrielle" parker-d fathers-d fathers-y))
              (require (distinct? fathers-d))
              (require (distinct? fathers-y))
              (map (lambda (father daughter yacht) (list father daughter yacht))
                   fathers fathers-d fathers-y)
              ))))))
  )

(define solutions (yacht-puzzle))

; Determine how many solutions there are if we are not told that Mary Ann’s last name is Moore.
(define (yacht-puzzle-alt)
  (let ((moore-d (amb daughters))
        (moore-y (amb daughters)))
    ; moore owns lorna yacht
    (require (equal? moore-y "Lorna"))
    (require (distinct? (list moore-d moore-y)))
    (let ((barnacle-d (amb daughters))
          (barnacle-y (amb daughters)))
      ; barnacle owns gabrielle yacht
      (require (equal? barnacle-y "Gabrielle"))
      ; barnacle is melissa's father
      (require (equal? barnacle-d "Melissa"))
      (let ((hall-y (amb daughters))
            (hall-d (amb daughters)))
        ; hall owns rosalind yacht
        (require (equal? hall-y "Rosalind"))
        (require (distinct? (list hall-d hall-y)))
        (let ((downing-d (amb daughters))
              (downing-y (amb daughters)))
          ; downing owns melissa yacht
          (require (equal? downing-y "Melissa"))
          (require (distinct? (list downing-d downing-y)))
          (let ((parker-d (amb daughters))
                (parker-y (amb daughters)))
            ; parker cannot be gabrielle's father
            (require (not (equal? parker-d "Gabrielle")))
            (require (distinct? (list parker-d parker-y)))
            ; gabrielle's father owns parker-d yacht
            (let ((fathers-d (list moore-d hall-d downing-d barnacle-d parker-d))
                  (fathers-y (list moore-y hall-y downing-y barnacle-y parker-y)))
              (require (father-of-daughter-has-yacht "Gabrielle" parker-d fathers-d fathers-y))
              (require (distinct? fathers-d))
              (require (distinct? fathers-y))
              (map (lambda (father daughter yacht) (list father daughter yacht))
                   fathers fathers-d fathers-y)
              ))))))
  )

(define solutions-alt (yacht-puzzle-alt))