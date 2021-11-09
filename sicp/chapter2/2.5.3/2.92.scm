#lang sicp

;;Exercise 2.92: By imposing an ordering on variables, ex-
;;tend the polynomial package so that addition and multipli-
;;cation of polynomials works for polynomials in different
;;variables. (This is not easy!)

(#%require "./poly.scm")
(#%require "./2.87.scm")

;----------------------------------------------------

;; converts term to var and outputs a polynomial
(define (convert-term term var-to var-from)
  ;; if coeff is a number
  (if (number? (coeff term))
      ;; order = 0?
      (if (= 0 (order term))
          (make-poly var-to (list (make-term 0 (coeff term))))
          (make-poly var-to
                     (list (make-term 0
                                      (make-poly var-from (list (make-term (order term) 1))))))
          )
      ;; recursive conversion check
      (let ((coeff-var (variable (coeff term))))
        (if (same-variable? var-to coeff-var)
            ;; term's var must become the converted term's coeff
            ;; create a polynomial from term's var and order
            ;; e.g term x^2 (y+1), where 'x' is var-from
            ;; converted term's coeff will be x^2 (not as a term, but as a polynomial)
            (let ((term-to-poly
                   (make-poly var-from (list (make-term (order term) 1)))
                   ))
              ;; for each term ('cterm') in the coefficient (which is a polynomial)
              ;; multiply the coefficient of 'cterm' by 'term-to-poly'
              (let ((new-term-list
                     (map
                      (lambda (cterm)
                        (make-term (order cterm) (mul-poly (coeff cterm) term-to-poly))
                        )
                      (term-list (coeff term)))
                     ))
                (make-poly var-to new-term-list)
                )
              )
            ;; else: go deeper and first convert the coeff
            (convert-term (convert-poly (coeff term) var-to)
                          var-to var-from)
            )
        )
      )
  )

(define (convert-poly p var)
  ;; already converted?
  (if (same-variable? (variable p) var)
      p
      ;; else: convert every term and add them up
      (let ((converted-terms
             (map (lambda (term) (convert-term term var (variable p))) (term-list p))
             ))
        (accumulate add-poly (make-poly var the-empty-termlist)
                    converted-terms)
        )
      )
  )

(define (num-to-poly n var)
  (make-poly var (list (make-term 0 n)))
  )


;; representing a number as a polynomial of degree 0
;; both add-poly and mul-poly were modified to add this feature

(define (add-poly p1 p2)
  (cond
    ;; both numbers - nah
    ;; p1 is a number?
    ((number? p1) (add-poly (num-to-poly p1 (variable p2)) p2))
    ;; p2 is a number?
    ((number? p2) (add-poly p1 (num-to-poly p2 (variable p1))))
    ((same-variable? (variable p1) (variable p2))
     (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
     )
    ;; else convert
    (else (let ((p2-converted (convert-poly p2 (variable p1))))
            (add-poly p1 p2-converted)
            ))
    )
  )

(define (mul-poly p1 p2)
  (cond
    ;; both numbers - nah
    ;; p1 is a number?
    ((number? p1) (mul-poly (num-to-poly p1 (variable p2)) p2))
    ;; p2 is a number?
    ((number? p2) (mul-poly p1 (num-to-poly p2 (variable p1))))
    ((same-variable? (variable p1) (variable p2))
     (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2)))
     ;; convert
     (else (let ((p2-converted (convert-poly p2 (variable p1))))
             (mul-poly p1 p2-converted)
             ))
     )
    )
  )