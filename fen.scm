(import test
	(prefix abnf abnf:) 
	(prefix abnf-consumers abnf:)
	(only srfi-1 make-list fold append-map)
	srfi-69 ;; hash-tables
	)

(include "board.scm")

(define :? abnf:optional-sequence)
(define :! abnf:drop-consumed)
(define :* abnf:repetition)
(define :+ abnf:repetition1)

;; rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
;; rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1
;; r1bqkb1r/ppp1n2p/2n2p2/3pp1p1/Q3P3/2PP1N2/PP2BPPP/RNB2RK1 b kq - 1 7
;; r1bk2qr/1pp5/p1n2b2/4pp1p/QPPP4/3n2PP/P2BqP1K/R1R5 w - - 2 23
;; 4k3/3p2p1/8/pP6/4P2P/8/8/4K3 w - a6 0 5
;; 4k3/6p1/8/pP1pP3/7P/8/8/4K3 w - d6 0 6
(define piece  (abnf:set-from-string "KNRBQPknrbqp12345678" ))
(define side   (abnf:bind-consumed->string (abnf:set-from-string "wb" )))

(define castling-right
  (abnf:bind-consumed->string
   (abnf:alternatives
    (:+ (abnf:set-from-string "KQkq" ))
    (abnf:char #\-))))

;;TODO: narrow the character range of en passant macher.
(define enpassant
  (abnf:bind-consumed->string
   (abnf:alternatives
    (abnf:repetition-n 2 (abnf:set-from-string "abcdefgh12345678" ))  (abnf:char #\-))))

(define half-move
  (abnf:bind-consumed->string
   (abnf:concatenation
    (:+ abnf:decimal))))

(define full-move half-move)

(define piece-placement
  (abnf:concatenation
    (:+ piece)))

(define board-positions
  (abnf:bind-consumed->string
   (abnf:concatenation
    (abnf:repetition-n 7
		       (abnf:concatenation
			piece-placement
			(:! (abnf:char #\/))))
    piece-placement)))

(define fen-record
  (abnf:concatenation
   board-positions
   abnf:wsp
   side
   abnf:wsp
   castling-right
   abnf:wsp
   enpassant
   abnf:wsp
   half-move
   abnf:wsp
   full-move))

(define (string->input-stream s) (string->list s))
(define (err s)
  (print "pgn message error on stream: " s)
  (list))


(define board-positions2
  (abnf:concatenation
   (abnf:repetition-n 7
		      (abnf:concatenation
		       piece-placement
		       (:! (abnf:char #\/))))
   piece-placement))


;; We turn a fen board field string, the first eight fields, into a list of characters. 
;; The characters are converted to one character string each by (make-string 1 c).
;; If the character is a number n, then a list equal to length n will be produced.
;; The letters (prnbqkPRNBQK) stand for chess pieces.
;; Parsed fen strings will be mapped onto a 64 length string.
;; input:  rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR 
;; output: rnbqkbnrpppppppp--------------------------------PPPPPPPPRNBQKBNR		   
(define (parse-fen s)
  (let* ([positions (board-positions car err `(() ,(string->input-stream s)))] ;; parsed string 
	 [position-chars (string->input-stream (car positions))]) ;;produce a list of character from a field
    (map (lambda (x y) (cons x y) ) ;;associate squares with pieces  
	 (append-map
	  (lambda (c)
	    (let ([empty-squares (string->number (make-string 1 c))])  
	      (if empty-squares
		  (make-list empty-squares #f)
		  (cons c '())))) position-chars)
	 all-square-symbols))) ;;

;; converting a character to a symbol:
(define (char->symbol ch)
  (string->symbol (string ch)))


(define fen-symbols-table
  (let* ([lst `((p . blackPawn)
		(n . blackKnight)
		(b . blackBishop)
		(r . blackRook)
		(q . blackQueen)
		(k . blackKing)
		
		(P . whitePawn)
		(N . whiteKnight)
		(B . whiteBishop)
		(R . whiteRook)
		(Q . whiteQueen)
		(K . whiteKing))])
    (alist->hash-table lst)))

(define (fen-symbols-lookup sym)
  (hash-table-ref fen-symbols-table sym))

; (update-board boardfen (fen-symbols-lookup (car p)) (cdr p) )
(define (build-board-from-fen s)
  (let loop ([pos (parse-fen s)]
	     [boardfen (make-bb)]) ;;bitboard from fen
    (cond
     ((null? pos) boardfen)
     ((and (caar pos)) (loop (cdr pos) (update-board boardfen (cdar pos) (fen-symbols-lookup (char->symbol (caar pos))) )))
     
     (else (loop (cdr pos) boardfen)))))

;;(draw-board (bbindex (build-board-from-fen "4k3/3p2p1/8/pP6/4P2P/8/8/4K3 w - a6 0 5")))

;;(time (draw-board (bbindex (build-board-from-fen "4k3/3p2p1/8/pP6/4P2P/8/8/4K3 w - a6 0 5"))))

;;(draw-board (bbindex (build-board-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))


;;(time (draw-board (bbindex (build-board-from-fen "r1bk2qr/1pp5/p1n2b2/4pp1p/QPPP4/3n2PP/P2BqP1K/R1R5 w - - 2 23"))))

;;(time (draw-board (bbindex (build-board-from-fen "r1bk2qr/1pp5/p1n2b2/4pp1p/QPPP4/3n2PP/P2BqP1K/R1R5 w - - 2 23"))))



;; rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1
;; r1bqkb1r/ppp1n2p/2n2p2/3pp1p1/Q3P3/2PP1N2/PP2BPPP/RNB2RK1 b kq - 1 7
;; r1bk2qr/1pp5/p1n2b2/4pp1p/QPPP4/3n2PP/P2BqP1K/R1R5 w - - 2 23
;; 4k3/3p2p1/8/pP6/4P2P/8/8/4K3 w - a6 0 5
;; 4k3/6p1/8/pP1pP3/7P/8/8/4K3 w - d6 0 6
