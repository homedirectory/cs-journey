#lang sicp

; Exercise 4.58
; Define a rule that says that a person is a “big shot” in a division if the
; person works in the division but does not have a supervisor who works in the division.

(rule (big-shot-in-division ?person ?division)
      (and (job ?person (?division . ?rest))
           (or (supervisor ?person ())
               (and (supervisor ?person ?sup)
                    (not (job ?sup (?division . ?rest1)))))))