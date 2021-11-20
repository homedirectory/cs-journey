#lang sicp

;;Exercise 3.49: Give a scenario where the deadlock-avoid-
;;ance mechanism described above does not work. (Hint: In
;;the exchange problem, each process knows in advance which
;;accounts it will need to get access to. Consider a situation
;;where a process must get access to some shared resources
;;before it can know which additional shared resources it will
;;require.)

;----------------------------------------------

;; If, for example, the exchange procedure takes as an argument a single account
;; and that account has a pointer inside it to another account with which the
;; exchange needs to be done.
;; Then, to find out the number (id) of that second account you first need to
;; lock the first account. This way there is no way of enforcing the ordering.

;; 2 concurrent processes:
;; time 0: exchange (acc1)
;; time 1: lock acc1
;; time 2: acc1.exchange_with = acc2; (need access to acc2, but it's locked)
;;    
;; time 0: exchange (acc2)
;; time 1: lock acc2
;; time 2: acc2.exchange_with = acc1; (need access to acc1, but it's locked)
