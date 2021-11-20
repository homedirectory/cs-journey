#lang sicp

;;Exercise 3.28: Define an or-gate as a primitive function
;;box. Your or-gate constructor should be similar to and-
;;gate.

;--------------------------------------------

(define (or-gate in1 in2 out)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! out new-value)))
        )
    )
  (add-action! in1 or-action-procedure)
  (add-action! in2 or-action-procedure)
  )
