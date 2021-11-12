#lang sicp

;;Exercise 3.5: Monte Carlo integration

(#%require "../../helpers.scm")

;--------------------------------------

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))
    )
  )

(define (rect-area x1 y1 x2 y2)
  (* (- x2 x1) (- y2 y1))
  )

(define (estimate-integral p x1 y1 x2 y2 trials)

  (define (p-test)
    (p (random-in-range x1 x2) (random-in-range y1 y2))
    )

  (* (monte-carlo trials p-test) (rect-area x1 y1 x2 y2))
  )

(define (estimate-pi)
  (let ((unit-circle-predicate
         (lambda (x y)
           (<= (+ (square x) (square y)) 1.0)
           )
         ))
    (estimate-integral unit-circle-predicate -1.0 -1.0 1.0 1.0 10000)
    )
  )

(define (monte-carlo trials experiment)
  
  (define (iter trials-remaining trials-passed)
    (cond
      ((= trials-remaining 0)
       (/ trials-passed trials))
      ((experiment)
       (iter (- trials-remaining 1)
             (+ trials-passed 1)))
      (else
       (iter (- trials-remaining 1)
             trials-passed)))
    )
  
  (iter trials 0)
  )