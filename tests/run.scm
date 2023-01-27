(import test
	srfi-178
	vector-lib ; vector library
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

(define (draw-vec board-vec)
(vector-for-each (lambda (i x) (print i " " x)) board-vec
  ))

(define (game-def)
  (let* (
	 [board2 (make-bb)]
	 [board3 (update-board board2 'a1 'blackRook)]
	 [board4 (update-board board3 'h1 'blackPawn)]
	 [board5 (update-board board4 'h8 'blackKnight)]
	 )
    (draw-vec (bbindex board5))))


(game-def)
;;(game-hash)
(test-exit)


