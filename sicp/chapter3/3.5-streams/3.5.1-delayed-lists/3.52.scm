#lang sicp

;;Exercise 3.52

(#%require "../../../helpers.scm")
(#%require "./stream.scm")

;-----------------------------------------------
(define (stream-enumerate-interval low high)
  ;  (print 'stream-enumerate-interval " " low " " high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))
      )
  )
(define (stream-map proc s)
  ;  (print 'stream-map)
  ;  (print "sum = " sum)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))
      )
  )



(define sum 0)
(define (accum x)
;  (print 'accum " " x)
;  (print "sum = " sum)
  (set! sum (+ x sum))
  sum
  )

; stream-enumerate-interval will evaluate only the first element, i.e., 1
; stream-map will also map only the first element of the stream, i.e., 1
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(print "sum = " sum) ; 1
; seq = (1, 3, 6, 10, 15, ...)
; note that elements after 1 are all delayed and not evaluated yet


; stream-filter will, at first, check 1 (already evaluated) and exclude it
; then it will need to evaluate (stream-map accum (stream-enumerate-interval 2 20))
; (accum 2) will be called and sum becomes 3
; the element in the stream is mapped: 2 -> 3 (since accum returned 3)
; (even? 3) - no, continue
; (stream-map accum (stream-enumerate-interval 3 20))
; (accum 3), sum becomes 6
; the element in the stream is mapped: 3 -> 6 (since accum returned 6)
; (even? 6) - yes, stop
(define y (stream-filter even? seq))
(print "sum = " sum) ; 6
y ; (6, 10 = map(4), 15 = map(5), ...)


; (6 % 5 == 0)? - no, continue
; (accum 4), sum becomes 10
; map: 4 -> 10
; (10 % 5 == 0)? - yes, stop
;(newline)
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
(print "sum = " sum) ; 10
z ; (10, 15 = map(5), ...)


; 7 more elements will get filtered and 12 more will get mapped
; iter | raw element | mapped    | even? | index | sum
; -----|-------------|-----------|-------|-------|------
; 0    | 6           | already   | yes   | 0     | 10
; 1    | 10          | already   | yes   | 1     | 10
; 2    | 5           | 5 -> 19   | no    | -     | 15
; 3    | 6           | 6 -> 25   | no    | -     | 21
; 4    | 7           | 7 -> 32   | yes   | 2     | 28
; 5    | 8           | 8 -> 40   | yes   | 3     | 36
; 6    | 9           | 9 -> 49   | no    | -     | 45
; ...  | ...         | ...       | ...   | ...   | ...
; 13   | 16          | 16 -> 136 | yes   | 7     | 136
(newline)
(stream-ref y 7)
(print "sum = " sum) ; 136


; the whole filter (x % 5 == 0) will be evaluated
; thus each element will be mapped
; sum = 136 + 17 + 18 + 19 + 20 = 210
(newline)
(display-stream z)
(print "sum = " sum) ; 210


; Would these responses differ if we had implemented (delay ⟨exp ⟩)
; simply as (lambda () ⟨exp ⟩) without using the optimization
; provided by memo-proc? Explain.

; Yes, they would. For example, in the last call (display-stream z)
; each element up to 16 (included) that was already mapped would get
; mapped again and sum would be equal to 210 + 1 + ... + 15