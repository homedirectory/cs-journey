#lang sicp

(#%require "./agenda.scm")
(#%require "../../../helpers.scm")

(define the-agenda (make-agenda))

;; A Wire
(define (make-wire name)
  (let ((signal-value 0) (action-procedures '()))

    (define (call-each procedures)
      (if (null? procedures)
          'done
          (begin ((car procedures))
                 (call-each (cdr procedures)))))
    
    (define (set-my-signal! new-value)
      (print "set " name " to " new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            ((eq? m 'name) name)
            (else (error "Unknown operation: WIRE" m))))
    
    dispatch)
  )

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (wire-name wire) (wire 'name))


;; Simulation related procedures
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (newline)
        (print "TIME: " (current-time the-agenda))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; some other procedures
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display ": New-value = ")
                 (display (get-signal wire))
                 (newline))))


;; TEST

;; ---------- LOGICAL GATES -----------
(define (logical-or s1 s2)
  (cond ((and (= s1 1) (= s2 0)) 1)
        ((and (= s1 0) (= s2 1)) 1)
        ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))
        )
  )

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

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))
        )
  )

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))
      )
    )
  (add-action! input invert-input)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 0)) 0)
        ((and (= s1 0) (= s2 1)) 0)
        ((and (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2))
        )
  )

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
;      (print (wire-name a1) "(" (get-signal a1) ")" " AND "
;             (wire-name a2) "(" (get-signal a2) ")" " -> "
;             (wire-name output) "(" new-value ")")
      (after-delay
       and-gate-delay
       (lambda () (set-signal! output new-value)))
      )
    )
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire 'd)) (e (make-wire 'e)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok
    )
  )

;; -------------------------------------------

(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire 'in1))
(define input-2 (make-wire 'in2))
(define sum (make-wire 'sum))
(define carry (make-wire 'carry))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

;(print-agenda the-agenda)

(print "--- PROPAGATE ---")
(propagate)

(set-signal! input-2 1)

(print "--- PROPAGATE ---")
(propagate)

(print "sum: " (sum 'get-signal))
(print "carry: " (carry 'get-signal))

(print-agenda the-agenda)