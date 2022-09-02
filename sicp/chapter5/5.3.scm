#lang sicp

(#%require "machine.scm" "machines.scm" "../helpers.scm")

; inputs: x, guess
; regs: x, guess, t
;(controller
;  sqrt-loop
;  (assign x (op read))
;  (assign guess (op read))
;  test-guess ; (< (abs (- (square guess) x)) 0.001)
;  ; t = (square guess)
;  (assign t (op *) (reg guess) (reg guess))
;  ; t = (- t x)
;  (assign t (op -) (reg t) (reg x))
;  ; t = (abs t)
;  (test (op <) (reg t) 0)
;  (assign t (op neg) (reg t))
;  ; (test (< t 0.001))
;  (test (op <) (reg t) (const 0.001))
;  (branch (label end))
;
;  ; improve guess
;  ; guess = (/ (+ guess (/ x guess)) 2)
;  ; t = (/ x guess)
;  (assign t (op /) (reg x) (reg guess))
;  ; t = (+ guess t)
;  (assign t (op +) (reg t) (reg guess))
;  ; guess = (/ t 2)
;  (assign guess (op /) (reg t) (const 2))
;  ; end improve
;  (goto (label test-guess))
;  end
;  (perform (op print) (reg guess))
;  (goto (label sqrt-loop))
;  )

;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))

(define sqrt-machine 
  (extend-machine
    (make-simple-machine)
    '(x guess t)
    '()
    '(test-guess ; (< (abs (- (square guess) x)) 0.001)
       ; t = (square guess)
       (assign t (op *) (reg guess) (reg guess))
       ; t = (- t x)
       (assign t (op -) (reg t) (reg x))
       ; t = (abs t)
       (test (op <) (reg t) (const 0))
       (branch (label neg))
       after-neg
       ; (test (< t 0.001))
       (test (op <) (reg t) (const 0.001))
       (branch (label end))
       ; improve guess
       ; guess = (/ (+ guess (/ x guess)) 2)
       ; t = (/ x guess)
       (assign t (op /) (reg x) (reg guess))
       ; t = (+ guess t)
       (assign t (op +) (reg t) (reg guess))
       ; guess = (/ t 2)
       (assign guess (op /) (reg t) (const 2.0))
       ; end improve
       (goto (label test-guess))
       neg
       (assign t (op *) (reg t) (const -1))
       (goto (label after-neg))
       end)
    ))

; TEST
(define (sqrt-newton x)
  (set-register-contents! sqrt-machine 'x x)
  (set-register-contents! sqrt-machine 'guess (/ x 2.0))
  (set-register-contents! sqrt-machine 't 0)
  (start sqrt-machine)
  (get-register-contents sqrt-machine 'guess))

(print (sqrt-newton 1))
(print (sqrt-newton 9))
(print (sqrt-newton 64))
(print (sqrt-newton 625))
(print (sqrt-newton (* 46 46)))
