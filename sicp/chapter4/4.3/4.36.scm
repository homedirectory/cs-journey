#lang sicp

(#%require "amb-core.scm")

; Exercise 4.36
; Exercise 3.69 discussed how to generate the stream of all
; Pythagorean triples, with no upper bound on ;the size of the integers
; to be searched.


; Explain why simply replacing an-integer-between by
; an-integer-starting-from in the procedure in Exercise 4.35
; is not an adequate way to generate arbitrary Pythagorean triples.

; - Because try-again would backtrack only to the choice of the last integer,
; that is, to the choice of k. Below is the timeline visualization:
; ================ point of choice: i -> [1 2 3]     ================
;                    i=1                           i=2             i=3
;                     |                             |               |
;                     v                             v               v
;  -------------------------------------      -------------        ---
;      |              |            |           |         |          |
;      v              v            v           v         v          v
;     j=1            j=2          j=3         j=2       j=3        j=3
; -------------    --------       ---       --------    ---        ---
;  |    |    |      |    |         |         |    |      |          |
;  v    v    v      v    v         v         v    v      v          v
; k=1  k=2  k=3    k=2  k=3       k=3       k=2  k=3    k=3        k=3

; Now imagine that high is not 3, but infinity. In such a case backtracking would
; never reach j=2, it would be stuck in the left-most branch,
; trying new values of k for i=1,j=1.



; Write a procedure that actually will accomplish this.
; (That is, write a procedure for which repeatedly typing try-again would in
; principle eventually generate all Pythagorean triples.)

; def (triples i j k)
; start with (i j k)
; interleave:
;     - (i (cdr (pairs j k)))
;     - (triples i+1 j+1 k+1)

; pairs:
; (1 1) (1 2) (2 2) (1 3) (2 3) (1 4) (3 3) (1 5) (2 4) (1 6) (3 4) (1 7) (2 5)
; ----
; (1 1)
; (1 2) (2 2)
; (1 3) (2 3) (3 3)
; (1 4) (2 4) (3 4) (4 4)

(define (a-pythagorean-triple-from n)
  (let ((triple (amb-triple n n n)))
    (require (pythagorean-triple? triple))
    triple))

(define (pythagorean-triple? t)
  (let ((i (car t))
        (j (cadr t))
        (k (caddr t)))
    (= (+ (* i i) (* j j)) (* k k))))

(define (amb-pair n1 n2)
  (define (move i j next-list)
    (if (= i j)
        (let ((new-next-list (list (cons i (inc j)) (cons (inc i) (inc j)))))
          (cond ((null? next-list)
                 (amb (list i j)
                      (move (caar new-next-list) (cdar new-next-list)
                            (cdr new-next-list))))
                ((= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            new-next-list)))
                (else (amb (list i j)
                           (move (caar next-list) (cdar next-list)
                                 (list-append-at (dec i) new-next-list (cdr next-list)))))))
        (let ((next-pair (cons i (inc j))))
          (cond ((null? next-list)
                 (amb (list i j)
                      (move (inc i) (inc i)
                            (list next-pair))))
                ((= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list next-pair))))
                (else (amb (list i j)
                           (move (caar next-list) (cdar next-list)
                                 (list-insert-at (dec i) next-pair (cdr next-list))))))))
    )
  (move n1 n2 '()))

; triples:
; (1 1 1), (1 1 2),  (2 2 2), (1 2 2), (2 2 3), (1 1 3),  (3 3 3),  (1 2 3), (2 3 3),
; (1 1 4), (3 3 4),  (1 3 3), (2 2 4), (1 1 5), (4 4 4),  (1 2 4),  (2 3 4), (1 1 6),
; (3 4 4), (1 3 4),  (2 2 5), (1 1 7), (4 4 5), (1 2 5),  (2 4 4),  (1 1 8), (3 3 5),
; (1 4 4), (2 2 6),  (1 1 9), (5 5 5), (1 2 6), (2 3 5),  (1 1 10), (3 4 5), (1 3 5),
; (2 2 7), (1 1 11), (4 5 5), (1 2 7), (2 4 5), (1 1 12), (3 3 6),  (1 4 5), (2 2 8),
; (1 1 13), (5 5 6), (1 2 8), (2 3 6), (1 1 14)
; ----
; (1 1 1)
; (1 1 2) (2 2 2)
; (1 2 2) (2 2 3) (3 3 3)
; (1 1 3) (2 3 3) (3 3 4) (4 4 4)

; Unfortunately amb-triple can not be expressed in terms of amb-pair

(define (amb-triple n1 n2 n3)
  (define (insert-and-fill n item lst)
    (define (fill l c)
      (if (= c n)
          l
          (fill (append l (list '())) (inc c))))
    (define (iter l c)
      (if (= c n)
          (if (null? (car l))
              (cons item (cdr l))
              (cons item l))
          (cons (car l) (iter (cdr l) (inc c)))))
    (if (>= n (length lst))
        (append (fill lst (length lst)) (list item))
        (iter lst 0)))
  (define (next-index start-point times)
    (define (iter ind t)
      (if (<= t 1)
          (if (> ind 0)
              (dec ind)
              0)
          (iter (inc (* 2 ind)) (dec t))))
    (iter start-point (- times (dec n1))))
  (define (move-call triple next-list)
    (move (car triple) (cadr triple) (caddr triple)
          next-list))
  (define (move i j k next-list)
    (let ((triple (list i j k)))
      (cond ((= i j k)
             (let ((triple1 (list i j (inc k)))
                   (triple2 (list (inc i) (inc j) (inc k)))
                   (triple3 (list i (inc j) (inc k))))
               (cond ((null? next-list)
                      (amb triple
                           (move-call triple1 (list triple2 triple3))))
                     (else
                      (amb triple
                           (move-call (car next-list)
                                      (insert-and-fill
                                       (next-index 2 i)
                                       triple3 (insert-and-fill
                                                (next-index 1 i)
                                                triple2 (insert-and-fill
                                                         (next-index 0 i)
                                                         triple1 (cdr next-list))))))))))
            ((= j k)
             (let ((triple1 (list i j (inc k)))
                   (triple2 (list i (inc j) (inc k))))
               (amb triple
                    (move-call (car next-list)
                               (insert-and-fill
                                (next-index 3 j)
                                triple2 (insert-and-fill
                                         (next-index 3 (dec j))
                                         triple1 (cdr next-list)))))))
            (else
             (let ((next-triple (list i j (inc k))))
               (amb triple
                    (move-call (car next-list)
                               (insert-and-fill (next-index 3 j) next-triple
                                                (cdr next-list)))))))))
  (move n1 n2 n3 '()))