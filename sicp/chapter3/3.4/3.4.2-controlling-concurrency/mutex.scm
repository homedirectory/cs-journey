#lang sicp

;; Mutex
;; false = unlocked
;; true = locked
(define (make-mutex)
  (let ((cell (list false)))
    
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ; retry
            ((eq? m 'release) (clear! cell))
            ((eq? m 'locked?) (car cell))
            (else (error "MUTEX: Unknown operation" m))
            )
      )
    
    the-mutex
    )
  )

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

;; Mutex Interface
(define (mutex-locked? mutex)
  (mutex 'locked?)
  )

(define (mutex-acquire mutex)
  (mutex 'acquire)
  )

(define (mutex-release mutex)
  (mutex 'release)
  )

;------------------------------------------------

(#%provide make-mutex mutex-acquire mutex-release mutex-locked? test-and-set!)

;; TEST
;(define mut (make-mutex))
;(mutex-acquire mut)
;(mutex-locked? mut) ; true
;(mutex-release mut)
;(mutex-locked? mut) ; false


