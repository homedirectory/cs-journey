(define (sum args)
  (define (f lst s)
    (if (null? lst)
        s
        (f (cdr lst) (+ s (car lst)))))
  (f args 0))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (inc low) high)))

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
  (define (iter rows)
    (if (null? rows) 
      '()
      (cons (list-ref k (car rows)) (iter (cdr rows)))))
  (iter board))

(define (board-diag-left-up-from k board)
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

(define (safe-last? board pos board-size)
  (let ((size (length board)))
    (if (< size 2)
        true
        (let ((row (board-row pos board))
              (diag-left-up (board-diag-left-up-from pos board))
              (diag-left-down (board-diag-left-down-from pos board board-size)))
          (and (= 1 (sum row))
               (or (= 0 (length diag-left-up)) (= 0 (sum diag-left-up)))
               (or (= 0 (length diag-left-down)) (= 0 (sum diag-left-down))))))))

(define (queens-puzzle n)
  (define (iter k board)
    (if (= k n)
        board
        (let ((queen-pos (an-integer-between 0 (- n 1))))
          (let ((new-board (adjoin-pos queen-pos board n)))
            (require (safe-last? new-board queen-pos n))
            (iter (+ k 1) new-board))
          )))
  (iter 0 '()))

(define solutions (queens-puzzle 8))
