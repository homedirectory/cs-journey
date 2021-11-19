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

(define (make-mutex-list n)
  (define (f lst c)
    (if (= c 0)
        lst
        (f (cons (make-mutex) lst) (dec c))
        )
    )

  (f (list) n)
  )

;; a. in terms of mutexes
;; this would be a really bad implementation
;; consider a case where you have 4 mutexes
;; all 4 are unlocked at the beginning
;; (f f f f)
;; then you acquire
;; (t f f f)
;; now another process wants to acquire
;; it iterates over the list and tries to acquire the first mutex
;; and it hangs until it is released instead of simply acquiring
;; the next mutex


;; b. in terms of atomic test-and-set! operations
;; below implementation is sort of a mix of a. and b.
(define (make-semaphore n)
  (let ((mutexes (make-mutex-list n)))

    (define (acquire)
      (define (f mutex-list)
        (cond ((null? mutex-list)
               (acquire)) ;; retry
              ((mutex-locked? (car mutex-list))
               (f (cdr mutex-list))) ;; check next mutex
              (else ;; lock mutex and return
               (mutex-acquire (car mutex-list))
               false
               )
              )
        )

      (f mutexes)
      )

    (define (release)
      ;; find first locked mutex and unlock it
      (define (f mutex-list)
        (cond ((null? mutex-list)
               (error "RELEASE: Somehow all mutexes are already unlocked"))
              ((mutex-locked? (car mutex-list)) ;; locked found -> release
               (mutex-release (car mutex-list)))
              (else (f (cdr mutex-list))) ;; continue

              )
        )

      (f mutexes)
      )

    ;; debug procedures
    (define (show-locked)
      (map (lambda (m) (mutex-locked? m))
           mutexes)
      )
    
    (define (semaphore m)
      (cond ((eq? m 'acquire) acquire)
            ((eq? m 'release) release)
            ;; debug
            ((eq? m 'show-locked) show-locked)
            (else (error "SEMAPHORE: Unknown operation" m))
            )
      )

    semaphore
    )
  )

;; Semaphore Interface
(define (semaphore-acquire sem)
  ((sem 'acquire))
  )

(define (semaphore-release sem)
  ((sem 'release))
  )

;; debug
(define (semaphore-show-locked sem)
  ((sem 'show-locked))
  )

;-------------------------------------
(#%provide make-semaphore)

;; TEST
(define s (make-semaphore 4))

(semaphore-acquire s)
(semaphore-show-locked s) ;; (t f f f)
(semaphore-acquire s)
(semaphore-acquire s)
(semaphore-acquire s)
(semaphore-show-locked s) ;; (t t t t)
;(semaphore-acquire s) ;; hangs
;(display 123) (newline)
(semaphore-release s)
(semaphore-show-locked s) ;; (f t t t)
(semaphore-release s)
(semaphore-acquire s)
(semaphore-show-locked s) ;; (t f t t)