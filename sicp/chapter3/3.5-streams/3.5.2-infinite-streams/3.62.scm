#lang sicp

(#%require "./inf-stream.scm")
(#%require "../stream.scm")
(#%require "./3.59.scm")
(#%require "./3.60.scm")
(#%require "./3.61.scm")

;Exercise 3.62: Use the results of Exercise 3.60 and Exer-
;cise 3.61 to define a procedure div-series that divides two
;power series. div-series should work for any two series,
;provided that the denominator series begins with a nonzero
;constant term. (If the denominator has a zero constant term,
;then div-series should signal an error.) Show how to use
;div-series together with the result of Exercise 3.59 to gen-
;erate the power series for tangent.

;---------------------------------------

;It is important to scale the denominator series before inverting it,
;so that its constant term is 1.
;Scaling the result of the inverse operation is also necessary,
;since the constant term of den-series can be > 0, as well as, > 1.
(define (div-series num-series den-series)
  (let ((den-const (stream-car den-series)))
    (if (= den-const 0)
        (error "DIV-SERIES: denominator series has a zero constant term")
        (let ((den-const-inv (/ 1 den-const)))
          (mul-series
           num-series
           (scale-stream
            (invert-unit-series (scale-stream den-series den-const-inv))
            den-const-inv
            )
           )
          )
        )
    )
  )

; TEST
(define tan-series (div-series sine-series cosine-series))


