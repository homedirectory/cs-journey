#lang sicp

;;Exercise 3.17: Devise a correct version of the count-pairs
;;procedure of Exercise 3.16 that returns the number of dis-
;;tinct pairs in any structure. (Hint: Traverse the structure,
;;maintaining an auxiliary data structure that is used to keep
;;track of which pairs have already been counted.)

(#%require "./3.12.scm") ; append!
;-------------------------------------------

(define (count-pairs lst)

  (define (cp x count?)
    (if (pair? x)
        (+ count? (cp (car x) 1) (cp (cdr x) 0))
        0
        )
    )

  (cp lst 0)
  )

(define (count-pairs-distinct lst)
  (let ((storage (list)))
    (define (exists? x)
      (define (iter strg)
        (cond
          ((null? strg) #f)
          ((eq? x (car strg)) #t)
          (else (iter (cdr strg)))
          )
        )
      (iter storage)
      )
    (define (store x)
;      (display x)
;      (display " ")
;      (display storage)
;      (newline)
      (set! storage (append storage (list x)))
      )

    (define (cp x count?)
      (if (pair? x)
          (if (= count? 1)
              (if (exists? x)
                  0
                  (begin
                    (store x)
                    (+ 1 (cp (car x) 1) (cp (cdr x) 0))
                    )
                  )
              (+ (cp (car x) 1) (cp (cdr x) 0))
              )
          0
          )
      )
    
    (cp lst 0)
    )
  )