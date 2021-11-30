#lang sicp

(#%require "../stream.scm")
(#%require "../../../helpers.scm")

; 3.5.5 Modularity of Functional Programs and Modularity of Objects

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))


(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo-stream
      (stream-cdr experiment-stream) passed failed))
    )
  
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))
      )
  )

;----------------------------------------------------------------
(#%provide map-successive-pairs monte-carlo-stream)