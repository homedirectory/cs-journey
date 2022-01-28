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
    (cond ((null? next-list)
           (amb (list i j)
                (move i (inc j) (list (cons (inc i) (inc j))))))
          ((= i j)
           (let ((new-next-list (list (cons i (inc j)) (cons (inc i) (inc j)))))
             (if (= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            new-next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list-append-at (dec i) new-next-list (cdr next-list)))))))
          (else
           (let ((next-pair (cons i (inc j))))
             (if (= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list next-pair)))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list-insert-at (dec i) next-pair (cdr next-list)))))))
          )
    )
  (move n1 n2 '()))

; triples:
; (1 1 1) (1 1 2) (2 2 2) (1 2 2) (2 2 3) (1 1 3) (3 3 3) (1 2 3) (2 3 3) (1 1 4)
; ----
; (1 1 1)
; (1 1 2) (2 2 2)
; (1 2 2) (2 2 3) (3 3 3)
; (1 1 3) (2 3 3) (3 3 4) (4 4 4)

; Unfortunately amb-triple can not be expressed in terms of amb-pair

(define (amb-triple n1 n2 n3)
  (define (move i j k next-list)
    (cond ((null? next-list)
           (amb (list i j k)
                (move i j (inc k) (list (list (inc i) (inc j) (inc k))))))
          ((= i j k)
           (let ((new-next-list (list (list i j (inc k))
                                      (list (inc i) (inc j) (inc k)))))
             (if (= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            new-next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list-append-at (dec i) new-next-list (cdr next-list)))))))
          (else
           (let ((next-pair (cons i (inc j))))
             (if (= 1 (length next-list))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list next-pair)))
                 (amb (list i j)
                      (move (caar next-list) (cdar next-list)
                            (list-insert-at (dec i) next-pair (cdr next-list)))))))
          )
    )
  (move n1 n2 n3 '()))




