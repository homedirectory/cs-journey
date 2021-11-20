#lang sicp

(#%require "./constraint-network.scm")

(define (squarer a b)
  (multiplier a a b))

(define x (make-connector))
(define res (make-connector))

(squarer x res)

(probe "x" x)
(probe "res" res)

(set-value! x 4 'user)

(forget-value! x 'user)

(set-value! x 5 'user)

(forget-value! x 'user)
(forget-value! res 'user)

(set-value! res 36 'user)

(define a (make-connector))
(define b (make-connector))

;; reset all
(forget-value! x 'user)
(forget-value! res 'user)

(adder a b x)

(set-value! a 2 'user)
(set-value! b 3 'user)

;; hmmm, the only flaw i see is that the squarer box works only in one way
;; that is, x -> res (x*x), but not x (sqrt res) <- res