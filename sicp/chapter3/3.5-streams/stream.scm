#lang sicp

(#%require "../../helpers.scm")

; ----- Streams -----


(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define the-empty-stream '())
(define stream-null? null?)


; Thank you, ivanjovanovic.
; So this is called a macro and we have to use it here instead of a regular
; procedure definition so that the argument `b` does not get evaluated
; before being passed to the delay function. This is an essential requirement
; for streams.
; This is how I understand macros:
; whenever `cons-stream` is encountered by the interpreter it asks the
; macro-expander to transform `cons-stream a b` into `(cons a (delay b))` or
; whatever it might be that you defined according to the rules you defined.
; Here I defined `cons-stream` to be interpreted as a procedure that takes
; 2 arguments.
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))
    )
  )

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map (lambda (s) (stream-car s)) argstreams))
       (apply stream-map (cons proc
                               (map (lambda (s) (stream-cdr s))
                                    argstreams)))
       )
      )
  )

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))
      )
  )

; prints fist n elements of the stream
(define (print-stream stream n)
  (define (f s c)
    (if (and (< c n) (not (stream-null? s)))
        (begin
          (display (stream-car s))
          (display ", ")
          (f (stream-cdr s) (inc c))
          )
        )
    )

  (display "(")
  (f stream 0)
  (display ")")
  (newline)
  )

;-------------------------------------------------
(#%provide the-empty-stream stream-null? cons-stream stream-car stream-cdr
           stream-ref stream-map stream-filter stream-for-each
           stream-enumerate-interval display-stream print-stream)
