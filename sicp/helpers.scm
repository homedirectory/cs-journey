; helper DSL to make implementation and representation of exercises more
; enjoyable
#lang sicp

(define (print . args)

  (define (pprint args-list)
    (if (null? args-list)
      (newline)
      (and (display (car args-list))
       (pprint (cdr args-list)))
      )
    )

  (pprint args)
  )

(define (square x) (* x x))

(#%provide print square)
