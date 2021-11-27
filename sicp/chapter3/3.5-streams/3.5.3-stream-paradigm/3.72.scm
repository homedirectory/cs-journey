#lang sicp

(#%require "../stream.scm")
(#%require "./3.5.3.scm")
(#%require "../../../helpers.scm")
(#%require "./3.70.scm")

;Exercise 3.72: In a similar way to Exercise 3.71 generate a
;stream of all numbers that can be written as the sum of two
;squares in three different ways (showing how they can be
;so written).

;------------------------------------------------------------

; n - how many same consecutive elements to look for?
(define (search-same-consecutive stream n)
  ; we need a counter to know where to continue
  ; example: search for 3 same consecutive elements
  ; stream = (1, 2, 2, 2, 2, 2, 2, 4, ...)
  ; we dont want to include 2 twice, so we will skip up to 4
  (define (f stream prev c)
    (if (stream-null? (stream-cdr stream))
        the-empty-stream
        (let ((scar (stream-car stream))
              (scdr (stream-cdr stream))
              (scadr (stream-car (stream-cdr stream))))
          (cond
            ((= c n)
             (if (= scar scadr)
                 (cons-stream scar (f scdr scar (inc c)))
                 (cons-stream scar (f scdr scar 1))
                 )
             )
            (else
             (if (= scar scadr)
                 (f scdr scar (inc c))
                 (f scdr scar 1)
                 )
             )
            )
          )
        )
    )

  ; starting point
  (if (= (stream-car stream) (stream-car (stream-cdr stream)))
      (f (stream-cdr stream) (stream-car stream) 2)
      (f (stream-cdr stream) (stream-car stream) 1)
      )
  )

(define (sum-squares pair)
  (+ (square (car pair)) (square (cadr pair)))
  )

(define numbers (search-same-consecutive
                 (stream-map sum-squares
                             (weighted-pairs integers integers sum-squares)) 3))


; TEST
;(define s (search-same-consecutive (list->stream (list 1 2 3 3 3 4 4 4 4 5 5 6 7 7 7 8)) 3))