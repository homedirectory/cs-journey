#lang sicp

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;Exercise 3.59: In Section 2.5.3 we saw how to implement
;a polynomial arithmetic system representing polynomials
;as lists of terms. In a similar way, we can work with power
;series, such as
;
;* no latex here :(    (but you know the power series) *
;
;We will represent the power series of a form:
;  a0 + a1*x + a2*x^2 + a3*x^3 + ...
;as the stream whose elements are the coefficients a0, a1, ...


; a. The integral of the series a0 + a1*x + a2*x^2 + ...
;is the series
;c + a0*x + 1/2*a1*x^2 + 1/3*a2*x3 + ...
;where c is any constant. Define a procedure integrate-
;series that takes as input a stream a0, a1, a2, ...
;representing a power series and returns the stream
;a0, 1/2*a1, 1/3*a2, ... of coefficients of the non-constant terms
;of the integral of the series. (Since the result has no
;constant term, it doesn’t represent a power series; when
;we use integrate-series, we will cons on the appropriate constant.)

(define fractions
  (stream-map (lambda (x) (/ 1 x)) integers))

(define (integrate-series stream)
  (mul-streams stream fractions)
  )


; b. The function x -> e^x is its own derivative. This imples
;that e^x and the integral of e^x are the same series, except
;for the constant term, which is e^0 = 1.
(define exp-series
  (cons-stream 1 (integrate-series exp-series))
  )

;Show how to generate the series for sine and cosine,
;starting from the facts that the derivative of sine is
;cosine and the derivative of cosine is the negative of
;sine:
;       derivative
;sin(x)    --->     cos(x)
;cos(x)    --->    -sin(x)

;cos(x) = 1 + (- Integral(sin(x)))
(define cosine-series
  (cons-stream 1 (stream-neg (integrate-series sine-series)))
  )

;sin(x) = Integral(cos(x))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series))
  )


;------------------------------------------
(#%provide integrate-series cosine-series sine-series)