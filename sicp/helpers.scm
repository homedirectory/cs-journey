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

(define (sum args)
  (define (f lst s)
    (if (null? lst)
        s
        (f (cdr lst) (+ s (car lst)))
        )
    )
  (f args 0)
  )

(define (average . args)
  (/ (sum args) (length args))
  )


; pow
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))
        )
  )


(define (expt x n)
  (fast-expt-iter x n 1)
  )


(define (divisible? x y)
  (= (remainder x y) 0)
  )

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (flatten items)
  ; items: list
  (cond ((null? items) '())
        ((pair? items)
         (let ((car-flat (flatten (car items)))
               (cdr-flat (flatten (cdr items))))
           (append car-flat cdr-flat)))
        (else (list items))))

(define (filter pred items)
  ; items: list
  ; pred: procedure
  (if (null? items)
      '()
      (let ((first (car items))
            (rest (cdr items)))
        (if (pred first)
            (cons first (filter pred rest))
            (filter pred rest)))))

(define (list-ref n lst)
  ; indexing starts at 0
  (define (iter l c)
    (cond ((null? l)
           (error "list-ref: n >= list length"))
          ((= c n) (car l))
          (else (iter (cdr l) (inc c)))))
  (iter lst 0))

(define (list-replace-at n item lst)
  (define (iter l c new-lst)
    (cond ((null? l)
           (error "list-replace-at: n >= list length"))
          ((= c n) (append (append new-lst (list item)) (cdr l)))
          (else (iter (cdr l) (inc c) (append new-lst (list (car l)))))))
  (iter lst 0 '()))

(define (list-insert-at n item lst)
  ; items are shifted to the right
  ; e.g. for [1, 2, 3] insert 4 at 1 results in [1, 4, 2, 3]
  ; to insert at the end, n = (length lst) would have to be specified
  (define (iter l c)
    (if (= c n)
        (cons item l)
        (cons (car l) (iter (cdr l) (inc c)))))
  
  (cond ((>= n (length lst)) (append lst (list item)))
        ((= n 0) (cons item lst))
        (else (iter lst 0))))

(define (list-append-at n items lst)
  ; items are shifted to the right
  ; e.g. for [1, 2, 3] insert 4 at 1 results in [1, 4, 2, 3]
  ; to insert at the end, n = (length lst) would have to be specified
  (define (iter l c)
    (if (= c n)
        (append items l)
        (cons (car l) (iter (cdr l) (inc c)))))
  
  (cond ((>= n (length lst)) (append lst items))
        ((= n 0) (append items lst))
        (else (iter lst 0))))

(define (list-remove-at n lst)
  (define (iter l c)
    (if (= c n)
        (cdr l)
        (cons (car l) (iter (cdr l) (inc c)))))
  
  (if (>= n (length lst))
      (error "list-remove-at: n >= (length lst)")
      (iter lst 0)))
;---------------------------------------------
 
(#%provide (all-defined))
