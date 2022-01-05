#lang sicp

;Exercise 3.25 
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
                  (insert-keys! record (cdr keyz))
                  )
              (if (= (length keyz) 1)
                  (set-cdr! table (cons (cons (car keyz) value) (cdr table)))
                  (begin
                    ;; create new table
                    (set-cdr! table (cons (cons (car keyz) '()) (cdr table)))
                    ;; insert into the new table
                    (insert-keys! (cadr table) (cdr keyz))
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

(define (lookup table key)
  ((table 'lookup) key))

(define (insert! table keys value)
  ((table 'insert!) keys value))
;------------------------------------------------
(#%provide make-table lookup insert!)

;------------------------------------------------
; TEST
;(define t (make-table equal?))
;((t 'insert!) 'a 5)
;((t 'insert!) 'b 12)
;((t 'lookup) 'a)
;((t 'lookup) 'b)
