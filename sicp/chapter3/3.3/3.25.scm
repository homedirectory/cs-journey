#lang sicp

;;Exercise 3.25: Generalizing one- and two-dimensional ta-
;;bles, show how to implement a table in which values are
;;stored under an arbitrary number of keys and different val-
;;ues may be stored under different numbers of keys. The
;;lookup and insert! procedures should take as input a list
;;of keys used to access the table.

;---------------------------------------------

(define (make-table same-key?)

  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))
          )
    )
  
  (let ((local-table (list '*table*)))
    
    (define (lookup keys)
      (define (lookup-keys table keyz)
        (let ((record (assoc (car keyz) (cdr table))))
          (if record
              (if (= (length keyz) 1)
                  (cdr record)
                  (lookup-keys record (cdr keyz))
                  )
              false)
          )
        )
      (if (pair? keys)
          (lookup-keys local-table keys)
          (lookup-keys local-table (list keys))
          )
      )
    
    (define (insert! keys value)
      (define (insert-keys! table keyz)
        (let ((record (assoc (car keyz) (cdr table))))
          (if record
              (if (= (length keyz) 1)
                  (set-cdr! record value)
                  (insert-keys! (cdr record) (cdr keyz))
                  )
              (if (= (length keyz) 1)
                  (set-cdr! table (cons (cons (car keyz) value) (cdr table)))
                  (begin
                    ;; create new table
                    (set-cdr! table (cons (cons (car keyz) '()) (cdr table)))
                    ;; insert into the new table
                    (insert-keys! (cdr table) (cdr keyz))
                    )
                  )
              )
          )
        )
      (if (pair? keys)
          (insert-keys! local-table keys)
          (insert-keys! local-table (list keys))
          )
      )
    
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation: TABLE" m))
            )
      )
    
    dispatch)
  )

;; TEST
(define t (make-table equal?))
((t 'insert!) 'a 5)
((t 'insert!) 'b 12)
((t 'lookup) 'a)
((t 'lookup) 'b)
((t 'insert!) (list 'x 'y) 20)
((t 'lookup) (list 'x 'y))

