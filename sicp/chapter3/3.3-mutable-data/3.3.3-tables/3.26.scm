#lang sicp

;;Exercise 3.26: To search a table as implemented above, one
;;needs to scan through the list of records. This is basically
;;the unordered list representation of Section 2.3.3. For large
;;tables, it may be more efficient to structure the table in a dif-
;;ferent manner. Describe a table implementation where the
;;(key, value) records are organized using a binary tree, as-
;;suming that keys can be ordered in some way (e.g., numer-
;;ically or alphabetically).
;---------------------------------------------
(define (make-table same-key? key-gt?)

  (define (make-tree tree-key tree-value)

    (define (make-node node-key node-value)

      (define (set-key! k)
        (set! node-key k)
        )

      (define (set-value! v)
        (set! node-value v)
        )

      (define (empty?)
        (null? node-key)
        )
      
      (define (dispatch m)
        (cond ((eq? m 'node-key) node-key)
              ((eq? m 'node-value) node-value)
              ((eq? m 'set-key!) set-key!)
              ((eq? m 'set-value!) set-value!)
              ((eq? m 'empty?) (empty?))
              (else (error "NODE: Unknown operation:" m))
              )
        )
      
      dispatch
      )
    
    (let (
          (node (make-node tree-key tree-value))
          (left '())
          (right '())
          )

      (define (empty-tree?)
        (node 'empty?)
        )

      (define (set-node! key value)
        (set! node (make-node key value))
        dispatch
        )

      (define (set-left! tree)
        (set! left tree)
        left
        )

      (define (set-right! tree)
        (set! right tree)
        right
        )

      (define (safe-search key branch)
        (if (null? branch)
            false
            ((branch 'search) key)
            )
        )

      (define (search key)
        (cond ((empty-tree?) false)
              ((same-key? (node 'node-key) key) dispatch)
              ((key-gt? key (node 'node-key)) (safe-search key right))
              (else (safe-search key left))
              )
        )

      (define (safe-insert key value branch which-branch)
        (if (null? branch)
            (if (eq? which-branch 'right)
                (set-right! (make-tree key value))
                (set-left! (make-tree key value))
                )
            ((branch 'insert) key value)
            )
        )

      (define (insert key value)
        (cond ((empty-tree?) (set-node! key value))
              ((same-key? (node 'node-key) key) ((node 'set-value!) value)) ;;upd node
              ((key-gt? key (node 'node-key)) (safe-insert key value right 'right))
              (else (safe-insert key value left 'left))
              )
        )
    
      (define (dispatch m)
        (cond ((eq? m 'node) node)
              ((eq? m 'left) left)
              ((eq? m 'right) right)
              ((eq? m 'empty-tree?) (empty-tree?))
              ((eq? m 'set-node!) set-node!)
              ((eq? m 'search) search)
              ((eq? m 'insert) insert)
              (else (error "TREE: Unknown operation:" m))
              )
        )

      dispatch
      )
    )

  (define (get-tree-key tree)
    ((tree 'node) 'node-key)
    )

  (define (get-tree-value tree)
    ((tree 'node) 'node-value)
    )

  ;; records = binary tree
  (define (assoc key records)
    ((records 'search) key)
    )
  
  (let ((local-table (make-tree '*table* (make-tree '() '()))))
    
    (define (lookup keys)
      (define (lookup-keys table keyz)
        (let ((record (assoc (car keyz) (get-tree-value table))))
          (if record
              (if (= (length keyz) 1)
                  (get-tree-value record)
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
        (let ((table-records (get-tree-value table)))
          (let ((record (assoc (car keyz) table-records)))
            (if record
                (if (= (length keyz) 1)
                    ((record 'insert) (car keyz) value)
                    (insert-keys! record (cdr keyz))
                    )
                (if (= (length keyz) 1)
                    ((table-records 'insert) (car keyz) value)
                    ;; create new table
                    (let ((new-table
                           ((table-records 'insert) (car keyz) (make-tree '() '()))
                           ))
                      ;; insert into the new table
                      (insert-keys! new-table (cdr keyz))
                      )
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
            (else (error "TABLE: Unknown operation:" m))
            )
      )
    
    dispatch)
  )

;; TEST
(define t (make-table equal? >))
((t 'insert!) 1 5)
((t 'insert!) 2 12)
((t 'lookup) 1)
((t 'lookup) 2)

((t 'insert!) (list 3 4) 100)
((t 'lookup) (list 3 4))

((t 'insert!) (list 9 1 56 912 87) 'hello)
((t 'lookup) (list 9 1 56 912 87))

((t 'insert!) (list 9 1 56 912 87) 'world)
((t 'lookup) (list 9 1 56 912 87))

((t 'insert!) (list 9 1 56) 'oops)
((t 'lookup) (list 9 1 56))