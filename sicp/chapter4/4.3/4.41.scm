#lang sicp

(#%require "../../helpers.scm")

; Exercise 4.41
; Write an ordinary Scheme program to solve the multiple dwelling puzzle.

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (group-list items)
  ; a single use procedure to group the flattened list
  (define (iter lst temp-item temp-lst res c-pair c-lst)
    (cond ((null? lst) res)
          ((= c-pair 2)
           (let ((new-temp-lst (append temp-lst (list (list temp-item (car lst))))))
             (if (= c-lst 5)
                 (iter (cdr lst) '() '() (append res (list new-temp-lst)) 1 1)
                 (iter (cdr lst) '() new-temp-lst res 1 (inc c-lst)))))
          ((= c-pair 1)
           (iter (cdr lst) (car lst) temp-lst res (inc c-pair) c-lst))))
  (iter items '() '() '() 1 1))

(define (multiple-dwelling)
  (define (fletcher F-curr F-end)
    (define (cooper C-curr C-end)
      (define (miller M-curr M-end)
        (define (smith S-curr S-end)
          (define (baker B-curr B-end)
            (if (> B-curr B-end)
                '()
                (if (and (not (= B-curr 5)) (distinct? (list B-curr C-curr F-curr M-curr S-curr)))
                    (cons (list (list 'baker B-curr)
                                (list 'cooper C-curr)
                                (list 'fletcher F-curr) 
                                (list 'miller M-curr)
                                (list 'smith S-curr))
                          (baker (inc B-curr) B-end))
                    (baker (inc B-curr) B-end))))
          ; smith
          (if (> S-curr S-end)
              '()
              (if (> (abs (- S-curr F-curr)) 1)
              ;(if #t
                  (cons (baker 1 5) (smith (inc S-curr) S-end))
                  (smith (inc S-curr) S-end))))
        ; miller
        (if (> M-curr M-end)
            '()
            (if (> M-curr C-curr)
                (cons (smith 1 5) (miller (inc M-curr) M-end))
                (miller (inc M-curr) M-end))))
      ; cooper
      (if (> C-curr C-end)
          '()
          (if (and (not (= C-curr 1)) (> (abs (- F-curr C-curr)) 1))
              (cons (miller 1 5) (cooper (inc C-curr) C-end))
              (cooper (inc C-curr) C-end))))
    ; fletcher
    (if (> F-curr F-end)
        '()
        (if (and (not (= F-curr 5)) (not (= F-curr 1)))
            (cons (cooper 1 5) (fletcher (inc F-curr) F-end))
            (fletcher (inc F-curr) F-end))))
  
  (filter (lambda (x) (not (null? x))) (flatten (fletcher 1 5))))

(define solutions (group-list (multiple-dwelling)))
