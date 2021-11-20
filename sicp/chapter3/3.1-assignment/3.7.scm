#lang sicp

;;Exercise 3.7: Consider the bank account objects created by
;;make-account, with the password modification described
;;in Exercise 3.3. Suppose that our banking system requires
;;the ability to make joint accounts. Define a procedure make-
;;joint that accomplishes this. make-joint should take three
;;arguments. The first is a password-protected account. The
;;second argument must match the password with which the
;;account was defined in order for the make-joint operation
;;to proceed. The third argument is a new password. make-
;;joint is to create an additional access to the original ac-
;;count using the new password.

(#%require "./3.3.scm")

;--------------------------------------------------------

;; returns acc that can be unlocked with new-pass
(define (make-joint acc acc-pass new-pass)
  (lambda (pass msg)
    (if (eq? new-pass pass)
        (acc acc-pass msg)
        (error "Incorrect password")
        )
    )
  )

;; TEST
(define peter-acc (make-account 100 'peter1))
(define paul-acc (make-joint peter-acc 'peter1 'paul1))

((paul-acc 'paul1 'withdraw) 90) ; returns 10

;---------------

;;You may wish to modify your solution to Exercise 3.3 to accommodate this
;;new feature.

(define (make-account1 balance password)

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

  (define (make-joint new-pass)
    (pass-protect new-pass)
    )

  (define (dispatch m)
    (cond
      ((eq? m 'withdraw) withdraw)
      ((eq? m 'deposit) deposit)
      ((eq? m 'make-joint) make-joint)
      (else (error "Unknown request: MAKE-ACCOUNT" m))
      )
    )

  (define (pass-protect real-pass)
    (define (pass-check input-pass)
      (if (eq? input-pass real-pass)
          dispatch
          (error "Incorrect password")
          )
      )
    pass-check
    )

  (pass-protect password)
  )

;; TEST
(define peter-acc1 (make-account1 300 'peter1))
(define paul-acc1 (((peter-acc1 'peter1) 'make-joint) 'paul1))

(((paul-acc1 'paul1) 'withdraw) 140) ; returns 160