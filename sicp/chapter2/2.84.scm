#lang sicp

;;Exercise 2.84: Using the raise operation of Exercise 2.83,
;;modify the apply-generic procedure so that it coerces its
;;arguments to have the same type by the method of succes-
;;sive raising, as discussed in this section. You will need to
;;devise a way to test which of two types is higher in the
;;tower. Do this in a manner that is “compatible” with the
;;rest of the system and will not lead to problems in adding
;;new levels to the tower.

(#%require "./2.83.scm")
;------------------------------------------

;; this whole raising thing works under the assumption that any type can be raised to
;any type that is higher in the tower

;; btw this is a bad strategy, since we haven't yet introduced the opposite of raising: lowering
;; since this strategy uses raising only, it means that you have to coerce all args to the
;highest of the types among the args
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          ; else try the same with raised args
          (let ((raised-args
                 (apply-generic op (successive-raise args))))
            (if (get op (map type-tag raised-args))
                (apply (map type-tag raised-args) (map contents raised-args))
                (error "gg")
                )
            )
          )
      )
    )
  )

;; 1. find the type that is the highest in the tower
;; 2. raise all args to that type

(define (successive-raise args)
  (if (< (length args) 2)
      args
      (let ((top-type (highest-type args)))
        (map (lambda (x t) (raise-to x top-type)) args)
        )
      )
  )

(define (highest-type args)

  (define (iter args top-type)
    (if (null? args)
        top-type
        (if (higher-type? (type-tag (car args)) top-type)
            (iter (cdr args) (type-tag (car args)))
            (iter (cdr args) top-type)
            )
        )
    )

  (iter args '())
  )

(define (higher-type? t1 t2)
  (if (<= (length (memq t1 tower)) (length (memq t2 tower)))
      t1
      t2
      )
  )

(define (raise-to x type)
  (if (equal? (type-tag x) type)
      x
      (raise x)
      )
  )

;----------------------------------------------------------
(#%provide successive-raise)