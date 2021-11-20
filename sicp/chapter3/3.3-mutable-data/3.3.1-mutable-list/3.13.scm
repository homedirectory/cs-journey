#lang sicp

(#%require "./3.12.scm")

;;Exercise 3.13: Consider the following make-cycle proce-
;;dure, which uses the last-pair procedure defined in Exer-
;;cise 3.12:
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

;;Draw a box-and-pointer diagram that shows the structure
;;z created by
(define z (make-cycle (list 'a 'b 'c)))
;What happens if we try to compute (last-pair z)?

;;   ---------------------- \
;;  /                       |
;;  |                       |
;;  v                       |
;; [   |   ]                |
;;  |    |                  |
;;  v    v                  |
;;  a   [   |   ]           |
;;        |   |             |
;;        v   v             |
;;        b   [   |   ]     |
;;              |   |       |
;;              v   v       /
;;              c   --------

;; (last-pair z) will hang forever

(#%provide make-cycle)