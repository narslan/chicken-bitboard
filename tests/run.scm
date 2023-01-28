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

			     ['whitePawn (display " ♙ ")]
			     ['whiteKnight (display " ♘ ")]
			     ['whiteBishop (display " ♗ ")]
			     ['whiteRook (display " ♖ ")]
			     ['whiteQueen (display " ♕ ")]
			     ['whiteKing (display " ♔ ")]
			     [else (display " - ")])
			   (if (equal? (modulo (+ i 1) 8) 0)
			       (newline))))
		   board-vec))

(define start-pos
  `((a1 whiteRook)
    (b1 whiteKnight)
    (c1 whiteBishop)
    (d1 whiteQueen)
    (e1 whiteKing)
    (f1 whiteBishop)
    (g1 whiteKnight)
    (h1 whiteRook)
    (a2 whitePawn)
    (b2 whitePawn)
    (c2 whitePawn)
    (d2 whitePawn)
    (e2 whitePawn)
    (f2 whitePawn)
    (g2 whitePawn)
    (h2 whitePawn)

    (a8 blackRook)
    (b8 blackKnight)
    (c8 blackBishop)
    (d8 blackQueen)
    (e8 blackKing)
    (f8 blackBishop)
    (g8 blackKnight)
    (h8 blackRook)
    (a7 blackPawn)
    (b7 blackPawn)
    (c7 blackPawn)
    (d7 blackPawn)
    (e7 blackPawn)
    (f7 blackPawn)
    (g7 blackPawn)
    (h7 blackPawn)))

(define (new-game bitboard)
  (let loop ([pos start-pos]
	     [board bitboard])
    (cond
     ((null? pos) board)
     (else (loop (cdr pos) (update-board board (caar pos) (cadar pos)) )))))


;;(time (draw-board (bbindex (new-game (make-bb)))))
(time (draw-board (bbindex (update-board (new-game (make-bb)) 'e5 'whiteKing  'e1)  )))


;; (print (bitvector->string
;; 	(set-bitvector-of (make-bitvector 64 0 ) 1) ))
;; (print (bitvector->string
;; 	(null-bitvector-at  1) ))
(print (bitvector->string
	(nullify-at-bitvector-of (set-bitvector-of (make-bitvector 64 0 ) 2) 2 )))


(test-exit)


