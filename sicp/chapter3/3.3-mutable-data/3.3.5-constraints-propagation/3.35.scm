#lang sicp

;;Exercise 3.35: Ben Bitdiddle tells Louis that one way to
;;avoid the trouble in Exercise 3.34 is to define a squarer
;;as a new primitive constraint. Fill in the missing portions
;;in Ben’s outline for a procedure to implement such a con-
;;straint:

(#%require "./constraint-network.scm")

;--------------------------------------------------------

(define (squarer a b)
  
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (error "square less than 0: SQUARER"
                      (get-value b))
               (set-value! a (sqrt (get-value b)) me)
               ))
          ((has-value? a)
           (set-value! b (* (get-value a) (get-value a)) me))
          )
    )
  
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value)
    )
  
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: SQUARER" request))
          )
    )
  
  (connect a me)
  (connect b me)
  
  me)


;; TEST
(define x (make-connector))
(define res (make-connector))

(squarer x res)

(probe 'x x)
(probe 'res res)

(set-value! x 4 'user) ;; res = 16

(forget-value! x 'user)

(set-value! res 25 'user) ;; x = 5