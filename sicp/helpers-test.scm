#lang sicp

(#%require "helpers.scm")
(#%provide assert-equal?)

; <<<<<<<<<< ASSERT PROCEDURES <<<<<<<<<<
(define (assert-equal? x y)
  (if (equal? x y)
    #t
    (error "assert-equal?" (list 'EXPECTED x 'ACTUAL y))))
; >>>>>>>>>> ASSERT PROCEDURES >>>>>>>>>>

; <<<<<<<<<< MACROS <<<<<<<<<<
;(define-syntax define-test
;  (syntax-rules ()
;    ((_ (proc-id) body ...)
;     ((define (proc-id) body ...)
;      (proc-id)))))
; >>>>>>>>>> MACROS >>>>>>>>>>


(define (test-list-remove)
  (assert-equal? (list-remove 3 (list 1 5 3 4 2 10))
                 (list 1 5 4 2 10)))
(test-list-remove)

(define (test-while)
  (let ((x 3))
    (while (> x 0)
           (set! x (- x 1)))
    (assert-equal? x 0)))
(test-while)

(define (test-list-length>?)
  (for-each
    assert-equal?
    (list #t #f #t #f #f)
    (map (lambda (n-lst-pair)
           (list-length>? (car n-lst-pair) (cdr n-lst-pair)))
         (list
           (cons 0 (list 1))
           (cons 1 (list 1))
           (cons 1 (list 1 2))
           (cons 1 (list))
           (cons 0 (list))))))
(test-list-length>?)

(define (test-xor)
  (for-each
    assert-equal?
    (list #f #t #t #f #t #f #f #t)
    (map-apply xor
         (list (list #f #f)
               (list #t #f)
               (list #f #t)
               (list #t #t)
               (list #f #f #t #f)
               (list #t #t #t #f #t)
               (list #t #t #t #t)
               (list #f #f #t #f #t #t)))))
(test-xor)
