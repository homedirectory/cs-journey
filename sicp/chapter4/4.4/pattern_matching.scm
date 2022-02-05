#lang sicp

; === Pattern matching ===

; pattern matcher inputs:
; - pattern
; - datum
; - frame (specifies bindings for pattern variables)
; checks whether datum matches pattern with bindings in the frame

; For, example (job ?x (computer programmer))
; Scan all records in DB and select those that match, with an initially empty frame.
; For each match use the frame returned by the match to instantiate the pattern with
; a value for ?x.