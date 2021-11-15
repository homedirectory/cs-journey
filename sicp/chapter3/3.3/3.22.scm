#lang sicp

;;Exercise 3.22: Instead of representing a queue as a pair of
;;pointers, we can build a queue as a procedure with local
;;state. The local state will consist of pointers to the beginning
;;and the end of an ordinary list

;--------------------------------------

(define (make-queue)
  (let (
        (front-ptr '())
        (rear-ptr '())
        )

    (define (set-front-ptr! item)
      (set! front-ptr item)
      )

    (define (set-rear-ptr! item)
      (set! rear-ptr item)
      )

    (define (empty-queue?)
      (null? front-ptr)
      )

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          front-ptr
          )
      )

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond
          ((empty-queue?)
           (set-front-ptr! new-pair)
           (set-rear-ptr! new-pair)
           )
          (else
           (set-cdr! rear-ptr new-pair)
           (set-rear-ptr! new-pair)
           )
          )
        )
      )

    (define (delete-queue!)
      (cond
        ((empty-queue?)
         (error "DELETE! called with an empty queue")
         )
        (else
         (set-front-ptr! (cdr front-ptr))
         )
        )
      )

    (define (print-queue)

      (define (print ptr)
        (display (car ptr))
        (if (not (null? (cdr ptr)))
            (begin
              (display ", ")
              (print (cdr ptr))
              )
            )
        )
      (display "(")
      (print front-ptr)
      (display ")")
      (newline)
      )

    (define (dispatch m)
      (cond
        ((eq? m 'front-ptr) front-ptr)
        ((eq? m 'rear-ptr) rear-ptr)
        ((eq? m 'empty-queue?) (empty-queue?))
        ((eq? m 'front-queue) front-queue)
        ((eq? m 'insert-queue!) insert-queue!)
        ((eq? m 'delete-queue!) (delete-queue!))
        ((eq? m 'print-queue) (print-queue))
        (else (error "Operation not found:" m))
        )
      )

    dispatch
    )
  )

;; TEST
(define q (make-queue))
((q 'insert-queue!) 'a)
((q 'insert-queue!) 'b)
((q 'insert-queue!) 'c)
((q 'insert-queue!) 'd)
(q 'print-queue) ;; (a, b, c, d)
(q 'delete-queue!)
((q 'insert-queue!) 'z)
(q 'print-queue) ;; (b, c, d, z)