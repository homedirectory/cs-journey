#lang sicp

;;Exercise 2.85: This section mentioned a method for “sim-
;;plifying” a data object by lowering it in the tower of types
;;as far as possible. Design a procedure drop that accom-
;;plishes this for the tower described in Exercise 2.83. The
;;key is to decide, in some general way, whether an object
;;can be lowered. For example, the complex number 1.5 + 0i
;;can be lowered as far as real, the complex number 1 + 0i
;;can be lowered as far as integer, and the complex number
;;2 + 3i cannot be lowered at all. Here is a plan for determin-
;;ing whether an object can be lowered: Begin by defining
;;a generic operation project that “pushes” an object down
;;in the tower. For example, projecting a complex number
;;would involve throwing away the imaginary part. Then a
;;number can be dropped if, when we project it and raise
;;the result back to the type we started with, we end up with
;;something equal to what we started with. Show how to im-
;;plement this idea in detail, by writing a drop procedure that
;;drops an object as far as possible. You will need to design
;;the various projection operations53 and install project as a
;;generic operation in the system. You will also need to make
;;use of a generic equality predicate, such as described in
;;Exercise 2.79. Finally, use drop to rewrite apply-generic
;;from Exercise 2.84 so that it “simplifies” its answers.

(#%require "./2.83.scm")

;; integer -> rational -> real -> complex

;----------------------------------------

;; this procedure assumes that type is present in the tower
(define (get-lower-type t)

  (define (iter twr)
    (if (equal? (cadr twr) t)
        (car twr)
        (iter (cdr twr))
        )
    )

  ;; return it if its the lowest type in the tower
  (if (equal? t (car tower))
      t
      (iter tower)
      )
  )

;; no need to write project procedure for every type, since that would just be
; repeating the coercion logic
;; instead we can use the already existing coercions
(define (project x)
  (let ((type-x (type-tag x)))
    (let ((lower-type (get-lower-type type-x)))
      (if (equal? type-x lower-type)
          x
          ((get-coercion type-x lower-type) x)
          )
      )
    )
  )

(define (can-be-lowered? x)
  (equal? (type-tag x) (get-lower-type x))
  )

(define (drop x)
  (if (can-be-lowered? x)
      (let ((projected-x (project x)))
        (if (equal? (raise projected-x) x)
            (drop projected-x)
            x
            )
        )
      x
      )
  )

;; use drop to rewrite apply-generic, so that it “simplifies” its answers
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          ;; add drop here
          (drop (apply proc (map contents args)))
          ; else try the same with raised args
          (let ((raised-args
                 (apply-generic op (successive-raise args))))
            (if (get op (map type-tag raised-args))
                ;; add drop here
                (drop (apply (map type-tag raised-args) (map contents raised-args)))
                (error "gg")
                )
            )
          )
      )
    )
  )






