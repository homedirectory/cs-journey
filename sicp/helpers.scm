; helper DSL to make implementation and representation of exercises more
; enjoyable
#lang sicp

(#%provide (all-defined))

(define (xor-gate a b)
  (and (or a b) (not (and a b))))
(define (xor . args)
  (if (list-length<? 2 args) 
    (error "xor: at least 2 args expected" args)
    (let ((x (xor-gate (car args) (cadr args))))
      (if (null? (cddr args))
        x
        (apply xor (cons x (cddr args)))))))

(define (list-length>? n lst)
  (if (null? lst)
    (< n 0)
    (list-length>? (- n 1) (cdr lst))))

(define (list-length>=? n lst)
  (if (null? lst)
    (<= n 0)
    (list-length>=? (- n 1) (cdr lst))))

(define (list-length<? n lst)
  (not (list-length>=? n lst)))

(define (list-length<=? n lst)
  (not (list-length>? n lst)))

(define (void . args)
  (display ""))

(define (print . args)
  (define (pprint args-list)
    (if (null? args-list)
        (newline)
        (and (display (car args-list))
             (pprint (cdr args-list)))))
  (pprint args))

(define (square x) (* x x))

(define (sum args)
  (define (f lst s)
    (if (null? lst)
        s
        (f (cdr lst) (+ s (car lst)))))
  (f args 0))

(define (average . args)
  (/ (sum args) (length args)))


; pow
(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))


(define (expt x n)
  (fast-expt-iter x n 1))


(define (divisible? x y)
  (= (remainder x y) 0))

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

(define (do-list-index-of item lst equal?p)
  (define (iter l n)
    (cond ((null? l) -1)
          ((equal?p (car l) item) n)
          (else (iter (cdr l) (inc n)))))
  (iter lst 0))

(define-syntax list-index-of
  (syntax-rules ()
    ((_ item lst equal?p)
     (do-list-index-of item lst equal?p))
    ((_ item lst)
     (do-list-index-of item lst equal?))))

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

(define (do-list-remove item lst equalp)
  (define (iter new-lst rest)
    (cond ((null? rest) (error "list-remove: empty list" lst))
        ((equalp item (car rest)) (append (reverse new-lst) (cdr rest)))
        (else (iter (cons (car rest) new-lst) (cdr rest)))))
  (iter '() lst))

(define-syntax list-remove
  (syntax-rules ()
    ((_ item lst equalp)
     (do-list-remove item lst equalp))
    ((_ item lst)
     (do-list-remove item lst equal?))))

(define (do-list-find item lst equalp)
  ; we can save the trouble of passing around item and equalp which never change
  ; by having a nested procedure
  ; what about an additional copy of lst ? is it created when it gets passed as an argument?
  ; NO! lists are passed by reference
  (define (iter l)
    (cond ((null? l) #f)
          ((equalp item (car l)) l)
          (else (iter (cdr l)))))
  (iter lst))

(define-syntax list-find
  (syntax-rules ()
    ((_ item lst equalp)
     (do-list-find item lst equalp))
    ((_ item lst)
     (do-list-find item lst equal?))))

(define (cons-if-absent item rest)
  (if (eq? #f (member item rest))
    (cons item rest)
    rest))

(define (range-iter start end lst)
  (if (= start end)
    (reverse lst)
    (range-iter (inc start) end (cons start lst))))

(define-syntax range
  (syntax-rules ()
    ((range end)
     (range-iter 0 end '()))
    ((range start end)
     (range-iter start end '()))))

(define (do-when pred proc)
  (if pred (proc) (void)))

(define-syntax when
  (syntax-rules ()
    ((when pred body ...)
     (do-when pred (lambda () body ...)))))

; don't use promises, because they act as thunks (compute once and reuse result)
; we want to recompute every time
(define (fwhile pred-proc body-proc last-val)
  (if (pred-proc)
    (let ((val (body-proc)))
      (fwhile pred-proc body-proc val))
    last-val))

(define-syntax while
  (syntax-rules ()
    ((_ pred proc ...)
     (fwhile (lambda () pred) (lambda () proc ...) (void)))
    ((_ pred)
     (fwhile (lambda () pred) void (void)))))

(define (numberize lst)
  (map cons (range (length lst)) lst))

(define (map-apply proc args-list)
  (map (lambda (args) (apply proc args))
       args-list))
