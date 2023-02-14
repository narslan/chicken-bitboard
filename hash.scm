(import
  srfi-178
  (chicken random)
  (only vector-lib vector-for-each vector-unfold) 
  )

(include "fen.scm")
;; 1. Generate a 64 x 12 array

;;builds a new bit vector with random value.
(define (generate-random-bits)
  (bitvector-unfold (lambda (_) (pseudo-random-integer 2)) 64))

;; constructs an array of length 12.
(define (vector-twelve)
  (vector-unfold (lambda (i x) (values x (generate-random-bits))) 12 (generate-random-bits)))

;; constructs a vector from a vector of twelve bitvectors.
(define init-zobrist
  (vector-unfold (lambda (i x) (values x (vector-twelve))) 64 (vector-twelve)))

(define black-to-move (generate-random-bits))

(define (zobrist-hash board-vec)
  (let ([h (make-bitvector 64 0)])
    (vector-for-each
     (lambda (i x)
       (begin
	 (case x
	   ['blackPawn   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 0))]
	   ['blackKnight (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 1))]
	   ['blackBishop (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 2))]
	   ['blackRook   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 3))]
	   ['blackQueen  (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 4))]
	   ['blackKing   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 5))]
	   
	   ['whitePawn   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 6))]
	   ['whiteKnight (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 7))]
	   ['whiteBishop (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 8))]
	   ['whiteRook   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 9))]
	   ['whiteQueen  (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 10))]
	   ['whiteKing   (bitvector-xor! h (vector-ref (vector-ref init-zobrist i) 11))]
	   )))
     board-vec)
    h))

(print (bitvector->string (zobrist-hash (bbindex (build-board-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))))

