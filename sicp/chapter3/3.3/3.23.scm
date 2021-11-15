#lang sicp


;;Exercise 3.23: A deque (“double-ended queue”) is a sequence
;;in which items can be inserted and deleted at either the
;;front or the rear. Operations on deques are the constructor
;;make-deque, the predicate empty-deque?, selectors front-
;;deque and rear-deque, mutators front-insert-deque!,
;;rear-insert-deque!, front-delete-deque!, and rear-delete-
;;deque!. Show how to represent deques using pairs, and
;;give implementations of the operations.23 All operations
;;should be accomplished in Θ(1) steps.

(#%require "../../helpers.scm")

;-----------------------------------

(define (make-deque)

  ;; node data structure
  ;; node has pointers to prev and next
  (define (make-node)
    (let (
          (value '())
          (prev '())
          (next '())
          )

      (define (set-value! item)
        (set! value item)
        )

      (define (set-prev! item)
        (set! prev item)
        )

      (define (set-next! item)
        (set! next item)
        )

      (define (null-node?)
        (null? value)
        )

      (define (print-node)
        (print "value:" value " | " "prev:" prev " | " "next:" next)
        )
      
      (define (dispatch m)
        (cond
          ((eq? m 'value) value)
          ((eq? m 'prev) prev)
          ((eq? m 'next) next)
          ((eq? m 'set-value!) set-value!)
          ((eq? m 'set-prev!) set-prev!)
          ((eq? m 'set-next!) set-next!)
          ((eq? m 'null-node?) (null-node?))
          ((eq? m 'print-node) (print-node))
          (else (error "MAKE-NODE: Unknown operation:") m)
          )
        )

      dispatch
      )
    )
  
  (let (
        (front-ptr '())
        (rear-ptr '())
        )

    (define (set-front-ptr! node)
      (set! front-ptr node)
      )

    (define (set-rear-ptr! node)
      (set! rear-ptr node)
      )

    (define (empty-deque?)
      (null? front-ptr)
      )

    (define (front-insert-deque! item)
      (let ((new-node (make-node)))
        ((new-node 'set-value!) item)
        (cond
          ((empty-deque?)
           (set-rear-ptr! new-node)
           )
          (else
           ((front-ptr 'set-prev!) new-node)
           ((new-node 'set-next!) front-ptr)
           )
          )
        (set-front-ptr! new-node)
        )
      )

    (define (rear-insert-deque! item)
      (let ((new-node (make-node)))
        ((new-node 'set-value!) item)
        ((rear-ptr 'set-next!) new-node)
        ((new-node 'set-prev!) rear-ptr)
        (if (empty-deque?) ;; if empty
            (set-front-ptr! new-node)
            )
        (set-rear-ptr! new-node)
        )
      )

    (define (front-delete-deque!)
      (if (empty-deque?)
          (error "FRONT-DELETE! called with an empty queue")
          (begin
            (set-front-ptr! (front-ptr 'next))
            ((front-ptr 'set-prev!) '())
            )
          )
      )

    (define (rear-delete-deque!)
      (if (empty-deque?)
          (error "REAR-DELETE! called with an empty queue")
          (begin
            (set-rear-ptr! (rear-ptr 'prev))
            ((rear-ptr 'set-next!) '())
            )
          )
      )

    (define (print-deque)

      (define (prnt node)
        (if (not (null? node))
            (begin
              (display (node 'value))
              (if (not (eq? node rear-ptr))
                  (begin
                    (display ", ")
                    (prnt (node 'next))
                    )
                  )
              )
            )
        )
      (display "(")
      (prnt front-ptr)
      (display ")")
      (newline)
      )

    (define (dispatch m)
      (cond
        ((eq? m 'empty-deque?) (empty-deque?))
        ((eq? m 'front-deque) front-ptr)
        ((eq? m 'rear-deque) rear-ptr)
        ((eq? m 'front-insert-deque!) front-insert-deque!)
        ((eq? m 'rear-insert-deque!) rear-insert-deque!)
        ((eq? m 'front-delete-deque!) (front-delete-deque!))
        ((eq? m 'rear-delete-deque!) (rear-delete-deque!))
        ((eq? m 'print-deque) (print-deque))
        (else (error "No such operation:" m))
        )
      )

    dispatch
    )
  )

;; TEST
(define dq (make-deque))
((dq 'front-insert-deque!) 'a)
((dq 'front-insert-deque!) 'b)
((dq 'rear-insert-deque!) 'c)
(dq 'print-deque) ; (b, a, c)
((dq 'rear-insert-deque!) 'd)
(dq 'front-delete-deque!)
(dq 'print-deque) ; (a, c, d)
(dq 'rear-delete-deque!)
((dq 'front-insert-deque!) 'z)
(dq 'print-deque) ; (z, a, c)



