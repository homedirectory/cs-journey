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

(define (a-pythagorean-triple-from n)
  (let ((triple (amb-triple n n n)))
    (require (pythagorean-triple? triple))
    triple))

(define (pythagorean-triple? t)
  (let ((i (car t))
        (j (cadr t))
        (k (caddr t)))
    (= (+ (* i i) (* j j)) (* k k))))

(define (interleave proc1 proc2)
  (amb (proc1) (interleave proc2 proc1)))

(define (amb-pair i j)
  (amb (list i j)
       (interleave
        ((lambda (x) (lambda () (set! x (inc x)) (list i x))) j)
        (amb-pair (inc i) (inc j)))))

(define (amb-triple i j k)
  (amb (list i j k)
       (interleave
        (list i (amb-pair j k)) ; need to take cdr of pairs
        (amb-triple (inc i) (inc j) (inc k)))))
