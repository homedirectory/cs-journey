#lang sicp

(define (list-ref n lst)
  ; indexing starts at 0
  (define (iter l c)
    (cond ((null? l)
           (error "list-ref: n >= list length"))
          ((= c n) (car l))
          (else (iter (cdr l) (inc c)))))
  (iter lst 0))

(define (gen-column pos size)
  (define (iter k col)
    (cond ((= size k) col)
          ((= pos k) (iter (+ k 1) (append col (list 1))))
          (else (iter (+ k 1) (append col (list 0))))))
  (iter 0 '()))

(define (adjoin-pos pos board size)
  (let ((column (gen-column pos size)))
    (if (null? board)
        (list column)
        (append board (list column)))))

(define (board-row k board)
  (map (lambda (col) (list-ref k col)) board))

(define (board-diag-left-up-from k board)
  ; k - row position in the last column
  ; diagonal length is calculated by taking a minimum of remaining rows till the
  ; end of the board and remaining columns
  (define (iter row col diag-len diag)
    (if (= diag-len (length diag))
        diag
        (iter (- row 1)
              (- col 1)
              diag-len
              (append diag (list (list-ref (- row 1)
                                           (list-ref (- col 1) board)))))))
  (let ((col (- (length board) 1)))
    (iter k col (min k col) '())))

(define (board-diag-left-down-from k board size)
  ; k - row position in the last column
  ; diagonal length is calculated by taking a minimum of remaining rows till the
  ; end of the board and remaining columns
  (define (iter row col diag-len diag)
    (if (= diag-len (length diag))
        diag
        (iter (+ row 1)
              (- col 1)
              diag-len
              (append diag (list (list-ref (+ row 1)
                                           (list-ref (- col 1) board)))))))
  (let ((col (- (length board) 1))
        (row (- size k 1)))
    (iter k col (min row col) '())))

(define board (adjoin-pos 1 '() 8))
(define board1 (adjoin-pos 2 board 8))
(define board2 (adjoin-pos 7 board1 8))
(define row0 (board-row 0 board2))
(define row1 (board-row 1 board2))
(define du (board-diag-left-up-from 3 board2))
(define dd (board-diag-left-down-from 1 board2 8))