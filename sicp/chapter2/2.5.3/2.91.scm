#lang sicp

;;Exercise 2.91: A univariate polynomial can be divided by
;;another one to produce a polynomial quotient and a poly-
;;nomial remainder

(#%require "./poly.scm")
(#%require "./2.88.scm")

;----------------------------------------------------

;; little help
(define (sub-terms L1 L2)
  (add-terms L1 (map neg-term L2))
  )

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((result (div-terms (term-list p1) (term-list p2))))
        (list
         ;; quotient
         (make-poly (variable p1)
                    (car result))
         ;; remainder
         (make-poly (variable p1)
                    (cdr result))
         )
        )
      (error "Polys not in same var: DIV-POLY" (list p1 p2))))


(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (let ((new-L1 (sub-terms
                                    L1
                                    (mul-terms (list (make-term new-o new-c)) L2)
                                    )))
                       (div-terms new-L1 L2)
                       )
                     ))
                (list
                 (adjoin-term (make-term new-o new-c) (car rest-of-result))
                 (cadr rest-of-result))
                )
              )
            )
        )
      )
  )

(#%provide div-poly div-terms)