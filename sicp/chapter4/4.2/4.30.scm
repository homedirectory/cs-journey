#lang sicp

; Exercise 4.30
; a. Explain why Ben is right about the behavior of for-each.
; - Ben is right, since the evaluator, while evaluating a sequence
; of expressions, will call apply for each of them and inevitably
; (if the expression is a procedure call) will call apply-primitive-procedure
; that will force the actual values.


; b.
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
; What are the values of (p1 1) and (p2 1) with the original eval-sequence?
; - (p1 1) -> (1 2)
; - (p2 1) -> 1, since the value of e is not needed, its merely looked up (not forced).

; What would the values be with Cy’s proposed change to eval-sequence?
; - (p1 1) -> (1 2)
; - (p2 1) -> (1 2), since each expression is forced.


; c. Cy also points out that changing eval-sequence as he proposes does not
; affect the behavior of the example in part a. Explain why this is true.
; - Already explained in my answer to a.


; d. How do you think sequences ought to be treated in the lazy evaluator?
; Do you like Cy’s approach, the approach in the text, or some other approach?
; - I like the approach in the text, since it is more efficient in that it
; does not do any redundant computation, as shown in part b.
; If one would really want (p2 1) to yield (1 2) using the evaluator in the text:
(define (p2_ x)
  (define (p proc)
    (let ((v (proc)))
      ; use v
      x))
  (p (lambda () (set! x (cons x '(2))))))