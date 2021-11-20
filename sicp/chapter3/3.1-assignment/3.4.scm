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
  (let ((incorrect-counter 0))

    (define (call-the-cops)
      (error "Password was entered incorrectly >= 7 consecutive times. Hello, 911?")
      )

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
          (begin
            (set! incorrect-counter (inc incorrect-counter))
            (if (>= incorrect-counter 7)
                (call-the-cops)
                (error "Incorrect password")
                )
            )
          )
      )

    dispatch
    )
  )