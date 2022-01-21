#lang sicp

(#%require "lazy-core.scm" "core-helpers.scm" "define.scm"
           "lambda.scm" "env.scm" "if.scm" "cond.scm")

;-------------------------------------------------------------------

(define test-env (setup-environment))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value
            input test-env)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
;----------------------------------------------

(define lazy-cons
  (list
   ; (define (cons x y) (lambda (m) (m x y)))
   (make-definition
    (list 'cons 'x 'y)
    (make-lambda (list 'm) (list (make-application 'm (list 'x 'y)))))
   ; (define (car z) (z (lambda (p q) p)))
   (make-definition
    (list 'car 'z)
    (make-application 'z (list (make-lambda (list 'p 'q) (list 'p)))))
   ; (define (cdr z) (z (lambda (p q) q)))
   (make-definition
    (list 'cdr 'z)
    (make-application 'z (list (make-lambda (list 'p 'q) (list 'q)))))
   ))

(define stream-procs
  (list
   ;      (define (list-ref items n)
   ;        (if (= n 0)
   ;            (car items)
   ;            (list-ref (cdr items) (- n 1))))
   (make-definition
    (list 'list-ref 'items 'n)
    (make-if (make-application '= (list 'n 0))
             (make-application 'car (list 'items))
             (make-application
              'list-ref
              (list (make-application 'cdr (list 'items)) (make-application '- (list 'n 1))))))
   ;      (define (map proc items)
   ;        (if (null? items)
   ;            '()
   ;            (cons (proc (car items)) (map proc (cdr items)))))
   (make-definition
    (list 'map 'proc 'items)
    (make-if (make-application 'null? (list 'items))
             '()
             (make-application
              'cons
              (list (make-application
                     'proc
                     (list (make-application 'car (list 'items))))
                    (make-application
                     'map
                     (list 'proc (make-application 'cdr (list 'items))))))))
   ;      (define (scale-list items factor)
   ;        (map (lambda (x) (* x factor)) items))
   (make-definition
    (list 'scale-list 'items 'factor)
    (make-application
     'map
     (list (make-lambda (list 'x) (list (make-application '* (list 'x 'factor)))) 'items)))
   ;      (define (add-lists list1 list2)
   ;        (cond ((null? list1) list2)
   ;              ((null? list2) list1)
   ;              (else (cons (+ (car list1) (car list2))
   ;                          (add-lists (cdr list1) (cdr list2))))))
   (make-definition
    (list 'add-lists 'list1 'list2)
    (cons 'cond
          (list
           (cons (make-application 'null? (list 'list1)) (list 'list2))
           (cons (make-application 'null? (list 'list2)) (list 'list1))
           (cons 'else (list
                        (make-application
                         'cons
                         (list (make-application
                                '+
                                (list (make-application 'car (list 'list1))
                                      (make-application 'car (list 'list2))))
                               (make-application
                                'add-lists
                                (list (make-application 'cdr (list 'list1))
                                      (make-application 'cdr (list 'list2))))))))
           )))
   ;(define ones (cons 1 ones))
   (make-definition 'ones (make-application 'cons (list 1 'ones)))
   ;(define integers (cons 1 (add-lists ones integers)))
   (make-definition 'integers
                    (make-application
                     'cons
                     (list 1 (make-application 'add-lists (list 'ones 'integers)))))
   ))

(map (lambda (exp) (eval exp test-env)) lazy-cons)
(map (lambda (exp) (eval exp test-env)) stream-procs)









