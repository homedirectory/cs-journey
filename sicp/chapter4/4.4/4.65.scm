#lang sicp

; Exercise 4.65
; Cy D. Fect, looking forward to the day when he will rise in the organization,
; gives a query to find all the wheels (using the wheel rule of Section 4.4.1):

;(wheel ?who)

; To his surprise, the system responds

;;;; Query results:
;(wheel (Warbucks Oliver))
;(wheel (Bitdiddle Ben))
;(wheel (Warbucks Oliver))
;(wheel (Warbucks Oliver))
;(wheel (Warbucks Oliver))

; Why is Oliver Warbucks listed four times?
;-------------------------------------------------------------------------------

; The rule:
(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

; The database:
(supervisor (Hacker Alyssa P) (Bitdiddle Ben)) ; <-- (wheel (Warbucks Oliver))
(supervisor (Fect Cy D) (Bitdiddle Ben)) ; <-- (wheel (Warbucks Oliver))
(supervisor (Tweakit Lem E) (Bitdiddle Ben)) ; <-- (wheel (Warbucks Oliver))
(supervisor (Reasoner Louis) (Hacker Alyssa P)) ; <-- (wheel (Bitdiddle Ben))
(supervisor (Bitdiddle Ben) (Warbucks Oliver)) 
(supervisor (Scrooge Eben) (Warbucks Oliver))
(supervisor (Cratchet Robert) (Scrooge Eben)) ; <-- (wheel (Warbucks Oliver))
(supervisor (Aull DeWitt) (Warbucks Oliver))