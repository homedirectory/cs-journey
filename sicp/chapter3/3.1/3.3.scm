#lang sicp

;;Exercise 3.3: Modify the make-account procedure so that
;;it creates password-protected accounts. That is, make-account
;;should take a symbol as an additional argument, as in
;;(define acc (make-account 100 'secret-password))
;;The resulting account object should process a request only
;;if it is accompanied by the password with which the ac-
;;count was created, and should otherwise return a complaint:

;------------------------------------------

(define (make-account balance password)

  (define (deposit n)
    (set! balance (+ balance n))
    balance
    )

  (define (withdraw n)
    (if (>= balance n)
        (begin
          (set! balance (- balance n))
          balance)
        "Insufficient funds"
        )
    )

  (define (dispatch pass m)
    (if (eq? pass password)
        (cond
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))
          )
        (error "Incorrect password")
        )
    )

  dispatch

  ;; introducing this procedure seems to make the program more modular
  ;; for example, instead of calling ((acc 'password123 'withdraw) 50)
  ;; one would call (((acc 'password123) 'withdraw) 50)
  ;  (define (pass-protect pass)
  ;    (if (eq? pass password)
  ;        dispatch
  ;        (error "Incorrect password")
  ;     )
  ;    )
  ;; and then we don't need to modify dispatch to check passwords

  ;  pass-protect
  )