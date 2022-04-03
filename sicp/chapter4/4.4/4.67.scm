#lang sicp

; Exercise 4.67
; Devise a way to install a loop detector in the
; query system so as to avoid the kinds of simple loops illustrated
; in the text and in Exercise 4.64. The general idea is
; that the system should maintain some sort of history of its
; current chain of deductions and should not begin processing a query
; that it is already working on. Describe what
; kind of information (patterns and frames) is included in
; this history, and how the check should be made. (After you
; study the details of the query-system implementation in
; Section 4.4.4, you may want to modify the system to include
; your loop detector.)
;-------------------------------------------------------------------------

; The recursive call cannot be a part of top-level "or" statement, because
; "or" will try to merge the queries it contains, thus it needs to reach a termination
; point.
; It also cannot be the first query in the top-level "and" statement, because
; it will never reach the termination point. However, if it comes after the 1st query,
; which may result in an empty set, then the termination point may be reached.

; If the pattern and the frame created by the caller is symmetrical to the current one,
; then a loop is detected. Example in the text:
; (rule (married ?x ?y) (married ?y ?x))