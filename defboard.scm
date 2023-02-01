(import
  
  defstruct
  test
  (chicken format)
  (chicken string)
  (only srfi-1 list-index)
  srfi-178 ;; bitvectors
  	(only vector-lib vector-for-each) ; vector library
  )

;;builds a new bit vector with id-th index set 1.
(define (set-bitvector-at id )
  (bitvector-unfold (lambda (_ b)
		      (values (if (equal? _ id) (not b) b) b))
		    64
                    #f))

(define (null-bitvector-at id )
  (bitvector-unfold (lambda (_ b)
		      (values (if (equal? _ id) (not b) b) b))
		    64
                    #t))

;;builds a new bit vector from another bit vector while index id of vector is set to 1
(define (set-bitvector-of bv id)
  (let ([nbv (set-bitvector-at id)])
    (bitvector-ior nbv bv)))

(define (nullify-at-bitvector-of bv id)
  (let ([nbv (null-bitvector-at id)])
    (bitvector-and nbv bv)))

(define all-square-symbols `(
			     a8 b8 c8 d8 e8 f8 g8 h8
			     a7 b7 c7 d7 e7 f7 g7 h7
			     a6 b6 c6 d6 e6 f6 g6 h6
			     a5 b5 c5 d5 e5 f5 g5 h5
			     a4 b4 c4 d4 e4 f4 g4 h4
			     a3 b3 c3 d3 e3 f3 g3 h3
			     a2 b2 c2 d2 e2 f2 g2 h2
			     a1 b1 c1 d1 e1 f1 g1 h1))


(: square->index (symbol --> fixnum))
(define (square->index square)
  (list-index (lambda (x) (equal? square x )) all-square-symbols))

(defstruct bb
  (blackPawn (make-bitvector 64 0) )
  (blackKnight (make-bitvector 64 0) )
  (blackBishop (make-bitvector 64 0) )
  (blackRook (make-bitvector 64 0) )
  (blackQueen (make-bitvector 64 0) )
  (blackKing (make-bitvector 64 0) )
  (blackAll (make-bitvector 64 0) )
  
  (whitePawn (make-bitvector 64 0) )
  (whiteKnight (make-bitvector 64 0) )
  (whiteBishop (make-bitvector 64 0) )
  (whiteRook (make-bitvector 64 0) )
  (whiteQueen (make-bitvector 64 0) )
  (whiteKing (make-bitvector 64 0) )
  (whiteAll (make-bitvector 64 0) ))

(define (bitvector-all-bits bit-vector)
  (let ([len (bitvector-length bit-vector)])
    (let loop ( [i 0] [acc '()])
      (cond ((>= i len) i acc)
            ((= (bitvector-ref/int bit-vector i) 1)
             (loop (+ i 1) (cons i acc)))
            (else
             (loop (+ i 1) acc))))))




;; functionally update 
(define (updateBlackAll board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     blackAll: (nullify-at-bitvector-of (set-bitvector-of (bb-blackAll board) square) oldSquare) )])  nb))

(define  (updateBlackPawn board square #!optional oldSquare)
  (let ([nb (update-bb
	    board
	    blackPawn: (nullify-at-bitvector-of (set-bitvector-of (bb-blackPawn board) square) oldSquare) )])
   (updateBlackAll nb square oldSquare)))

(define  (updateBlackKnight  board square #!optional oldSquare)
 (let ([nb (update-bb
	    board
	    blackKnight: (nullify-at-bitvector-of (set-bitvector-of (bb-blackKnight board) square) oldSquare) )])
   (updateBlackAll nb square oldSquare)))

(define  (updateBlackBishop  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     blackBishop: (nullify-at-bitvector-of (set-bitvector-of (bb-blackBishop board) square) oldSquare) )])
    (updateBlackAll nb square oldSquare)))

(define  (updateBlackRook  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     blackRook: (nullify-at-bitvector-of (set-bitvector-of (bb-blackRook board) square) oldSquare) 
	     )])
    (updateBlackAll nb square oldSquare)))

(define  (updateBlackQueen  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     blackQueen:
	     (nullify-at-bitvector-of (set-bitvector-of (bb-blackQueen board) square) oldSquare) 
	     )])
    (updateBlackAll nb square oldSquare)))

(define  (updateBlackKing  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     blackKing: (nullify-at-bitvector-of (set-bitvector-of (bb-blackKing board) square) oldSquare) )])
    (updateBlackAll nb square oldSquare)))


(define (updateWhiteAll board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     whiteAll: (nullify-at-bitvector-of (set-bitvector-of (bb-blackAll board) square) oldSquare) )])  nb))

(define  (updateWhitePawn  board square #!optional oldSquare)
 (let ([nb (update-bb
	    board
	    whitePawn: (nullify-at-bitvector-of (set-bitvector-of (bb-whitePawn board) square) oldSquare) )])
   (updateWhiteAll nb square oldSquare)))

(define  (updateWhiteKnight  board square #!optional oldSquare)
 (let ([nb (update-bb
	    board
	    whiteKnight: (nullify-at-bitvector-of (set-bitvector-of (bb-whiteKnight board) square) oldSquare)
		      )])
   (updateWhiteAll nb square oldSquare)))

(define  (updateWhiteBishop  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     whiteBishop: (nullify-at-bitvector-of (set-bitvector-of (bb-whiteBishop board) square) oldSquare)
	    )])
    (updateWhiteAll nb square oldSquare)))

(define  (updateWhiteRook  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     whiteRook: (nullify-at-bitvector-of (set-bitvector-of (bb-whiteRook board) square) oldSquare))])
    (updateWhiteAll nb square oldSquare)))

(define  (updateWhiteQueen  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     whiteQueen: (nullify-at-bitvector-of (set-bitvector-of (bb-whiteQueen board) square) oldSquare)
	     )])
    (updateWhiteAll nb square oldSquare)))


(define  (updateWhiteKing  board square #!optional oldSquare)
  (let ([nb (update-bb
	     board
	     whiteKing: (nullify-at-bitvector-of (set-bitvector-of (bb-whiteKing board) square) oldSquare))])
    (updateWhiteAll nb square)))

(define (update-board board newSquare piece #!optional oldSquare)
  (let ([sq (square->index newSquare)]
	[osq (square->index oldSquare)])
    (case piece
     ['blackPawn  (updateBlackPawn board sq osq)]
     ['blackKnight  (updateBlackKnight board sq osq)]
     ['blackBishop (updateBlackBishop board sq osq)]
     ['blackRook (updateBlackRook board sq osq)]
     ['blackQueen (updateBlackQueen board sq osq)]
     ['blackKing (updateBlackKing board sq osq)]

     ['whitePawn (updateWhitePawn board sq osq)]
     ['whiteKnight (updateWhiteKnight board sq osq)]
     ['whiteBishop (updateWhiteBishop board sq osq)]
     ['whiteRook (updateWhiteRook board sq osq)]
     ['whiteQueen (updateWhiteQueen board sq osq)]
     ['whiteKing (updateWhiteKing board sq osq)]
     [else board] )))
 
;; bbindex maps pieces to their indices ... 
(define (bbindex board)
  (let loop ((attr (bb->alist board))
	     (v (make-vector 64 #f)))
    (cond
     ((null? attr) v)
     (else
      (let* ([piece (caar attr)]
	     [bv (cdar attr)]
	     (bits (bitvector-all-bits  bv)))
	(case piece
	  ['blackAll (loop (cdr attr)  v)] 
	  ['whiteAll (loop (cdr attr)  v)]
	  [else (loop (cdr attr)
		      (begin
			(for-each (lambda (idx) (vector-set! v idx piece)) bits) v))]))))))

;; TODO: this needs bbindex, unite them together.
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
