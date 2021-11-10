#lang sicp

;;Exercise 2.96

(#%require "./poly.scm")
(#%require "./2.91.scm") ; div-poly & div-terms

;-------------------------------------------------

;;a. Implement the procedure pseudoremainder-terms, which
;;   is just like remainder-terms except that it multiplies
;;   the dividend by the integerizing factor described above
;;   before calling div-terms. Modify gcd-terms to use
;;   pseudoremainder-terms, and verify that greatest-
;;   common-divisor now produces an answer with inte-
;;   ger coefficients on the example in Exercise 2.95.

(define (order-poly p)
  (order (first-term (term-list p)))
  )

(define (pseudoremainder-terms a b)
  (let (
        (order-a (order (first-term) a))
        (order-b (order (first-term) b))
        (c (coeff (first-term b)))
        )
    (let ((factor (exp c (+ 1 (- order-a order-b)))))
      ;; multiply by factor only if order-a >= order-b
      (if (>= order-a order-b) 
          (cadr (div-terms (map (lambda (term) (make-term
                                                (order term)
                                                (mul factor (coeff term))))
                                a)
                           b))
          (cadr (div-terms a b))
          )
      )
    )
  )

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))


;;b. The GCD now has integer coefficients, but they are
;;   larger than those of P 1 . Modify gcd-terms so that it
;;   removes common factors from the coefficients of the
;;   answer by dividing all the coefficients by their (inte-
;;   ger) greatest common divisor.

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((gcd-a-coeffs (apply gcd (map coeff a))))
        ;; divide each term's coeff by gcd
        (map (lambda (term) (make-term (order term) (div (coeff term) gcd-a-coeffs)))
             a)
        )
      (gcd-terms b (pseudoremainder-terms a b))))


