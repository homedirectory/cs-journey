#lang sicp

;;Exercise 3.25: Generalizing one- and two-dimensional ta-
;;bles, show how to implement a table in which values are
;;stored under an arbitrary number of keys and different val-
;;ues may be stored under different numbers of keys. The
;;lookup and insert! procedures should take as input a list
;;of keys used to access the table.

;---------------------------------------------

(define (make-table table-name same-key?)

  (define (assoc key records)
    (cond ((null? records) false)
          ((pair? (car records))
           (if (same-key? key (caar records))
               (car records)
               (assoc key (cdr records))
               )
           )
          (else
           (if (same-key?  key ((car records) 'table-name))
               (car records)
               (assoc key (cdr records))
               )
           )
          )
    )
  
  (let ((table (list table-name)))

    (define (lookup keys)
      (cond ((not (pair? keys)) (lookup (list keys)))
            (else
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (if (= (length keys) 1)
                       (cdr record) ;; return record
                       ((record 'lookup) (cdr keys)) ;; search subtable
                       )
                   false)
               )
             )
            )
      )
    
    (define (insert! keys value)
      (cond ((not (pair? keys)) (insert! (list keys) value))
            (else
             (let ((record (assoc (car keys) (cdr table))))
               (if record
                   (if (= (length keys) 1)
                       (set-cdr! record value)  ;; insert value
                       ((record 'insert!) (cdr keys) value) ;; delegate to subtable
                       )
                   ;; key not found
                   (if (= (length keys) 1)
                       (set-cdr! table (cons
                                        (cons (car keys) value)
                                        (cdr table)))  ;; create record
                       (let ((subtable (make-table (car keys) same-key?)))
                         (set-cdr! table (cons subtable (cdr table)))
                         ((subtable 'insert!) (cdr keys) value)
                         ) ;; create empty subtable and delegate to it
                       )
                   )
               )
             )
            )
      )
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            ((eq? m 'table-name) table-name)
            ((eq? m 'table) table)
            (else (error "Unknown operation: TABLE" m))
            )
      )
    
    dispatch)
  )

;; TEST
(define t (make-table '*table* equal?))
((t 'insert!) 'a 5)
((t 'lookup) 'a)
((t 'insert!) 'b 12)
((t 'lookup) 'b)
((t 'insert!) (list 'x 'y) 20)
;((t 'lookup) (list 'x 'y))

