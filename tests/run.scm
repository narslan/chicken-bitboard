(import test
	srfi-178
	(only vector-lib vector-for-each) ; vector library
	
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

(define (draw-board board-vec)
  (vector-for-each (lambda (i x)
		     (begin
			   (case x
			     ['blackPawn (display " ♟ ")]
			     ['blackKnight (display " ♞ ")]
			     ['blackBishop (display " ♝ ")]
			     ['blackRook (display " ♜ ")]
			     ['blackQueen (display " ♛ ")]
			     ['blackKing (display " ♚ ")]
			     [else (display " - ")]
			     )
			   (if
			    (equal? (modulo (+ i 1) 8) 0)
			    (newline)
			    )))
		   board-vec))

(define start-pos
  `((h8 blackRook)
    (f8 blackBishop)
    (g8 blackKnight)
    (d8 blackQueen)
    ))

(define (new-game bitboard)
  (let loop (
	     [pos start-pos]
	     [board bitboard]
	     )
    (cond
     ((null? pos) board)
     (else (loop (cdr pos) (update-board board (caar pos) (cadar pos)) 

		 )))))


(time (draw-board (bbindex (new-game (make-bb)))))

;;(update-board board (car pos) (cadr pos))
;; (define (game-def)
;;   (let* (
;; 	 [board2 (make-bb)]
;; 	 [board3 (update-board board2 'a1 'blackRook)]
;; 	 [board4 (update-board board3 'h1 'blackPawn)]
;; 	 [board5 (update-board board4 'e1 'blackKing)]
;; 	 [board6 (update-board board5 'h8 'blackKnight) ]
;; 	 [board7 (update-board board6 'e2 'blackQueen)]
;; 	 [board8 (update-board board7 'e4 'blackPawn)]
;; 	 )
;;     (draw-vec (bbindex board8))))


					;(new-game)
;;(game-hash)
(test-exit)


