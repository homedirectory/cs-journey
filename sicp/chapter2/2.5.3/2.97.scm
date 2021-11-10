#lang sicp

;; Exercise 2.97:

(#%require "./poly.scm")
(#%require "./2.96.scm")

;-------------------------------------------------

;; multiply term-list by coeff
(define (mul-tl-c term-list c)
  (map (lambda (term) (make-term (order term) (mul (coeff term) c)))
       term-list)
  )

;; divide term-list by coeff
(define (div-tl-c term-list c)
  (map (lambda (term) (make-term (order term) (div (coeff term) c)))
       term-list)
  )


;;a. Implement this algorithm as a procedure reduce-terms
;;that takes two term lists n and d as arguments and re-
;;turns a list nn, dd, which are n and d reduced to low-
;;est terms via the algorithm given above. Also write a
;;procedure reduce-poly, analogous to add-poly, that
;;checks to see if the two polys have the same variable.
;;If so, reduce-poly strips off the variable and passes
;;the problem to reduce-terms, then reattaches the vari-
;;able to the two term lists supplied by reduce-terms.

;; 1. compute GCD of num and denom
;; 2. multiply num and denom by a factor
;;    factor = c ^ (1 + O1 - O2)
;;    c - leading coeff of GCD
;;    O1 - max of orders of num and denom
;;    O2 - order of GCD
;; 3. divide both num and denom by the GCD
;; 4. divide resulting num and resulting denom by a factor
;;    factor = GCD of (coeffs of num, coeffs of denom)

(define (reduce-terms num denom)
  ;; 1.
  (let ((nd-gcd (gcd-terms num denom)))
    ;; 2.
    (let (
          (c (coeff (first-term nd-gcd)))
          (O1 (max (order (first-term num)) (order (first-term denom))))
          (O2 (order nd-gcd))
          (let ((factor-mul (exp c (+ 1 (- O1 O2)))))
            (let (
                  (num2 (mul-tl-c num factor-mul))
                  (denom2 (mul-tl-c denom factor-mul))
                  )
              ;; 3.
              (let (
                    (num3 (div-terms num2 nd-gcd))
                    (denom3 (div-terms denom2 nd-gcd))
                    )
                ;; 4.
                (let ((nd-coeffs-gcd
                       (apply gcd
                              (append
                               (map coeff num3)
                               (map coeff denom3))
                              )
                       ))
                  (list
                   (div-tl-c num3 nd-coeffs-gcd)
                   (div-tl-c denom3 nd-coeffs-gcd)
                   )
                  )
                )
              )
            )
          )
      )
    )
  )


(define (reduce-poly num-p denom-p)
  (if (not (same-variable? num-p denom-p))
      (error "Polynomials are not in the same variable: REDUCE-POLY" num-p denom-p)
      (map
       (lambda (term-l) (make-poly (variable num-p) term-l))
       (reduce-terms (term-list num-p) (term-list denom-p)))
      )
  )

;;b. Define a procedure analogous to reduce-terms that
;;does what the original make-rat did for integers:

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;;and define reduce as a generic operation that calls
;;apply-generic to dispatch to either reduce-poly (for
;;polynomial arguments) or reduce-integers (for scheme-
;;number arguments). You can now easily make the rational-
;;arithmetic package reduce fractions to lowest terms
;;by having make-rat call reduce before combining the
;;given numerator and denominator to form a ratio-
;;nal number. The system now handles rational expres-
;;sions in either integers or polynomials.


;; addition to polynomial package
(put 'reduce '(polynomial polynomial) (lambda (p1 p2) (tag (reduce-poly p1 p2))))

;; addition to scheme-number package
(put 'reduce '(scheme-number scheme-number) (lambda (a b) (tag (reduce-integers a b))))

(define (reduce num denom)
  (apply-generic 'reduce num denom)
  )

;; modification of rational package
(define (make-rat n d)
  (reduce n d)
  )

















