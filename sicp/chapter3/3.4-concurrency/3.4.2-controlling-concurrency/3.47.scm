#lang sicp

;;Exercise 3.47: A semaphore (of size n) is a generalization of
;;a mutex. Like a mutex, a semaphore supports acquire and
;;release operations, but it is more general in that up to n
;;processes can acquire it concurrently. Additional processes
;;that attempt to acquire the semaphore must wait for release
;;operations. Give implementations of semaphores
;;a. in terms of mutexes
;;b. in terms of atomic test-and-set! operations.

(#%require "./mutex.scm")

;--------------------------------------------------

;; a. in terms of mutexes
(define (make-semaphore-a n)
  (let (
        (access-mutex (make-mutex))
        (mutex (make-mutex))
        (procs 0)
        )
    (define (acquire)
      (mutex-acquire mutex)
      (mutex-acquire access-mutex)
      (cond ((< procs n)
             (set! procs (inc procs))
             ;; release if there is room for another process
             (if (< procs n)
                 (mutex-release mutex)
                 )
             )
            )
      (mutex-release access-mutex)
      )

    (define (release)
      (mutex-acquire access-mutex)
      (set! procs (dec procs))
      (mutex-release mutex)
      (mutex-release access-mutex)
      )
    
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             acquire)
            ((eq? m 'release)
             release)
            )
      )

    dispatch
    )
  )

;; TEST
(define sem-a  (make-semaphore-a 3))
((sem-a 'acquire))
((sem-a 'acquire))
((sem-a 'acquire))
((sem-a 'release))
((sem-a 'acquire))
;(display 1)
;((sem-a 'acquire)) ;; hangs
;(display 2)


;; b. in terms of atomic test-and-set! operations
(define (make-semaphore-b n)
  (let (
        (access-cell (list false))
        (cell (list false))
        (procs 0)
        )

    (define (access-acquire)
      (if (test-and-set! access-cell)
          (access-acquire)
          )
      )
    
    (define (acquire)
      (if (test-and-set! cell)
          (acquire)
          (begin
            (access-acquire)
            (cond ((< procs n)
                   (set! procs (inc procs))
                   ;; release if there is room for another process
                   (if (< procs n)
                       (clear! cell)
                       )
                   )
                  )
            (clear! access-cell)
            )
          )
      )

    (define (release)
      (access-acquire)
      (set! procs (dec procs))
      (clear! cell)
      (clear! access-cell)
      )
  
    (define (dispatch m)
      (cond ((eq? m 'acquire)
             acquire)
            ((eq? m 'release)
             release)
            )
      )
           
    dispatch
    )
  )


;; TEST
(newline) (display "b.")
(define sem-b (make-semaphore-b 3))
((sem-b 'acquire))
((sem-b 'acquire))
((sem-b 'acquire))
;(newline) (display 1)
;((sem-b 'acquire)) ;; hangs
;(newline) (display 2)
((sem-b 'release))
((sem-b 'acquire))
((sem-b 'release))
((sem-b 'release))
((sem-b 'release))
((sem-b 'acquire))