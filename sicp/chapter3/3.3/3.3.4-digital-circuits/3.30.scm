#lang sicp

;;Exercise 3.30: Figure 3.27 shows a ripple-carry adder formed
;;by stringing together n full-adders. is is the simplest form
;;of parallel adder for adding two n-bit binary numbers. e
;;inputs A 1 , A 2 , A 3 , . . ., A n and B 1 , B 2 , B 3 , . . ., Bn are the
;;two binary numbers to be added (each A k and B k is a 0 or
;;a 1). e circuit generates S 1 , S 2 , S 3 , . . ., Sn , the n bits of
;;the sum, and C, the carry from the addition. Write a proce-
;;dure ripple-carry-adder that generates this circuit. e
;;procedure should take as arguments three lists of n wires
;;each—the A k , the B k , and the S k —and also another wire C.
;;e major drawback of the ripple-carry adder is the need
;;to wait for the carry signals to propagate. What is the delay
;;needed to obtain the complete output from an n-bit ripple-
;;carry adder, expressed in terms of the delays for and-gates,
;;or-gates, and inverters?


;------------------------------------------

;; half-adder delay = 1 or + 2 and + 1 not
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok
    )
  )

;; full-adder delay = 2 half-adder + 1 or
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok
    )
  )

;; riple-carry-adder delay = n full-adder = 2n half-adder + n or =
;; = 2n or + 4n and + 2n not + n or = 3n or + 4n and + 2n not
(define (riple-carry-adder a-list b-list s-list c)
  (if (null? a-list)
      'ok
      (let ((c-out (make-wire)))
        (full-adder (car a-list) (car b-list) c (car s-list) c-out)
        (riple-carry-adder (cdr a-list) (cdr b-list) (cdr s-list) c-out)
        )
      )
  )