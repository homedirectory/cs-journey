#lang sicp

(#%require "machine.scm" "../helpers-test.scm" "../helpers.scm")

; a. recursive count-leaves
(define (count-leaves-a tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves-a (car tree))
                 (count-leaves-a (cdr tree))))))

(define machine-a
  (make-machine
    (list (list 'list list)
          (list 'car car)
          (list 'cdr cdr)
          (list 'pair? pair?)
          (list 'null? null?))
    '((assign continue (label end))
      (assign n (const 0))
      start
      (test (op null?) (reg tree))
      (branch (label tree-null))
      (test (op pair?) (reg tree))
      (branch-not (label tree-not-pair))
      (goto (label tree-else))
      tree-null
      (assign n (const 0))
      (goto (reg continue))
      tree-not-pair
      (assign n (const 1))
      (goto (reg continue))
      tree-else
      (save continue)
      (assign continue (label after-car))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label start))
      after-car
      (restore tree)
      (save n)
      (assign tree (op cdr) (reg tree))
      (assign continue (label after-cdr))
      (goto (label start))
      after-cdr
      (restore tmp)
      (assign n (op +) (reg n) (reg tmp))
      (restore continue)
      (goto (reg continue))
      end)))

(define (machine-a-count-leaves tree)
  (set-register-contents! machine-a 'tree tree)
  (start machine-a)
  (get-register-contents machine-a 'n))

; test
(let ((trees (list
               (cons (cons 1 2) (cons 3 4))
               (cons '() '())
               (cons 1 (cons (cons 2 3) 4)))))
  (for-each assert-equal? 
            (map count-leaves-a trees)
            (map machine-a-count-leaves trees))
  ;(for-each (lambda (tree) (print (count-leaves tree))) trees)
  )

; b. iterative count-leaves
(define (count-leaves-b tree)
  (define (iter tr n)
    (cond ((null? tr) n)
          ((not (pair? tr)) (+ n 1))
          (else (iter (cdr tr) (iter (car tr) n)))))
  (iter tree 0))

(define machine-b
  (make-machine
    (list (list 'list list)
          (list 'car car)
          (list 'cdr cdr)
          (list 'pair? pair?)
          (list 'null? null?))
    '((assign continue (label end))
      (assign n (const 0))
      start
      (test (op null?) (reg tree))
      (branch (label tree-null))
      (test (op pair?) (reg tree))
      (branch-not (label tree-not-pair))
      (goto (label tree-else))
      tree-null
      (goto (reg continue))
      tree-not-pair
      (assign n (op +) (reg n) (const 1))
      (goto (reg continue))
      tree-else
      (save continue)
      (assign continue (label after-car))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label start))
      after-car
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (restore continue)
      (goto (label start))
      end)))

(define (machine-b-count-leaves tree)
  (set-register-contents! machine-b 'tree tree)
  (start machine-b)
  (get-register-contents machine-b 'n))

; test
(let ((trees (list
               (cons (cons 1 2) (cons 3 4))
               (cons '() '())
               (cons 1 (cons (cons 2 3) 4)))))
  (for-each (lambda (proc-res machine-res) 
              (print proc-res " " machine-res)
              (assert-equal? proc-res machine-res)) 
            (map count-leaves-b trees)
            (map machine-b-count-leaves trees))
  )
