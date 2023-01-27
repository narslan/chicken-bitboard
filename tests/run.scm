(import test
	srfi-178
	)

(include-relative "../defboard.scm")


(define (pp-tree t)
  (let loop ((attr (bb->alist t)))
    (cond ((null? attr) 'done)
          (else
            (display (caar attr)
		     )(display ": ")
            (display (bitvector->string (cdar attr)))(newline)
            (loop (cdr attr))))))

(define (display-bitboard t)
  (let loop ((attr (bb->alist t)))
    (cond ((null? attr) 'done)
          (else
            (display (caar attr)
		     )(display ": ")
            (display (bitvector->string (cdar attr)))(newline)
            (loop (cdr attr))))))



(define (game-def)
  (let* (
	 [board2 (make-bb)]
	 [board3 (update-board board2 'h8 'blackRook)]
	 [board4 (update-board board3 'h7 'blackPawn)]
	 [board5 (update-board board4 'h6 'blackRook)]
	 )
    (pp-tree board5)
    ))




(game-def)
;;(game-hash)
(test-exit)

