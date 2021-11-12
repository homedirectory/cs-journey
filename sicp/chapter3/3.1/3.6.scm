#lang sicp

;;Exercise 3.6: It is useful to be able to reset a random-number
;;generator to produce a sequence starting from a given value.
;;Design a new rand procedure that is called with an ar-
;;gument that is either the symbol generate or the symbol
;;reset and behaves as follows: (rand 'generate) produces
;;a new random number; ((rand 'reset) ⟨new-value ⟩) re-
;;sets the internal state variable to the designated ⟨new-value ⟩.
;;Thus, by resetting the state, one can generate repeatable se-
;;quences. These are very handy to have when testing and
;;debugging programs that use random numbers.

;---------------------------------------------

(define (rand-init)
  (random 1.0)
  )

(define rand
  (let ((x (rand-init)))

    (define (reset newx)
      (set! x newx)
      newx
      )

    (define (dispatch m)
      (cond
        ((eq? m 'generate)
         (begin (set! x (rand-init)) x))
        ((eq? m 'reset) reset)
        ((eq? m 'get) x)
        (else (error "Unknown operation"))
        )
      )
    dispatch
    )
  )