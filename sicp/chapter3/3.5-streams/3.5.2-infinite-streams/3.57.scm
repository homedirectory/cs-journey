#lang sicp

;Exercise 3.57: How many additions are performed when
;we compute the n th Fibonacci number using the definition
;of fibs based on the add-streams procedure? Show that
;the number of additions would be exponentially greater
;if we had implemented (delay ⟨exp⟩) simply as
;(lambda () ⟨exp⟩), without using the optimization provided by the
;memo-proc procedure described in Section 3.5.1.64

(#%require "./inf-stream.scm")
(#%require "../stream.scm")

;-------------------------------------------

(define fibs
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs) fibs))
   )
  )

; fibs 6
; (0, 1, 1, 2, 3, 5)

; 0: no add
; 1: no add
; 1: 1 + 0
; 2: 1 + 1
; 3: 2 + 1
; 5: 3 + 2

; total: O(n) additions

; In case we had not used memo-proc in our implementation,
; we would have to redo all addition operations from the beginning
; for each element of the fibs stream.