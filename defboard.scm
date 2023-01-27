(import
  
  defstruct
  test
  (chicken format)
  (chicken string)
  (only srfi-1 list-index)
  srfi-178 ;; bitvectors
  srfi-69 ;; hash tables
  )

;; return a bitvector with idx set
(define (make-bitvector-at idx)
  (let ([m1 (make-bitvector 64 0)])
    (begin
      (bitvector-set! m1 idx 1) m1)))

;;builds a new bit vector with id-th index set 1.
(define (set-bitvector-at id)
  (bitvector-unfold (lambda (_ b)
		      (values (if (equal? _ id) (not b) b) b))
		    64
                    #f))

;;builds a new bit vector from another bit vector while index id of vector is set to 1
(define (set-bitvector-of bv id)
  (let ([nbv (set-bitvector-at id)])
    (bitvector-ior nbv bv)))

(define all-square-symbols `(
 a8 b8 c8 d8 e8 f8 g8 h8
 a7 b7 c7 d7 e7 f7 g7 h7
 a6 b6 c6 d6 e6 f6 g6 h6
 a5 b5 c5 d5 e5 f5 g5 h5
 a4 b4 c4 d4 e4 f4 g4 h4
 a3 b3 c3 d3 e3 f3 g3 h3
 a2 b2 c2 d2 e2 f2 g2 h2
 a1 b1 c1 d1 e1 f1 g1 h1))

;; 'a1 -> 0 'h8-> 63
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


(define (bbindex board)
  (let loop ((attr (bb->alist board))
	     (v (make-vector 64 #f))
	     )
    (cond
     ((null? attr) v)
     (else
      (let* ([piece (caar attr)]
	     [bv (cdar attr)]
	     (bits (bitvector-all-bits  bv))
	    )
	(case piece
	  ['blackAll (loop (cdr attr)  v)]
	  ['whiteAll (loop (cdr attr)  v)]
	  [else (loop (cdr attr)
		      (begin
			(for-each (lambda (idx) (vector-set! v idx piece)) bits)
			v
			))]))))))



;; [ 'blackPawn (append lst  (bitvector-all-bits  bv ))]
;; 	  [ 'blackKnight (append  lst (bitvector-all-bits  bv )) ]
;; 	  [ 'blackBishop (append lst (bitvector-all-bits  bv ))]
;; 	  [ 'blackRook (merge lst (bitvector-all-bits  bv ))]
;; 	  [ 'blackQueen (append lst  (bitvector-all-bits  bv ))]
;; 	  [ 'blackKing (append lst  (bitvector-all-bits  bv ))]

;; functionally update 
(define (updateBlackAll board square)
  (let ([nb (update-bb board blackAll: (set-bitvector-of (bb-blackAll board) square))])  nb))

(define  (updateBlackPawn  board square)
 (let ([nb (update-bb board blackPawn: (set-bitvector-of (bb-blackPawn board) square))])
   (updateBlackAll nb square)))

(define  (updateBlackKnight  board square)
 (let ([nb (update-bb board blackKnight: (set-bitvector-of (bb-blackKnight board) square))])
   (updateBlackAll nb square)))

(define  (updateBlackBishop  board square)
  (let ([nb (update-bb board blackBishop: (set-bitvector-of (bb-blackBishop board) square))])
    (updateBlackAll nb square)))

(define  (updateBlackRook  board square)
  (let ([nb (update-bb board blackRook: (set-bitvector-of (bb-blackRook board) square))])
    (updateBlackAll nb square)))

(define  (updateBlackQueen  board square)
  (let ([nb (update-bb board blackQueen: (set-bitvector-of (bb-blackQueen board) square))])
    (updateBlackAll nb square)))

(define  (updateBlackKing  board square)
  (let ([nb (update-bb board blackKing: (set-bitvector-of (bb-blackKing board) square))])
    (updateBlackAll nb square)))

(define (update-board board asquare piece)
  (let ([sq (square->index asquare)])
    (cond
     [(equal? piece 'blackPawn) (updateBlackPawn board sq)]
     [(equal? piece 'blackKnight) (updateBlackKnight board sq)]
     [(equal? piece 'blackBishop) (updateBlackBishop board sq)]
     [(equal? piece 'blackRook) (updateBlackRook board sq)]
     [(equal? piece 'blackQueen) (updateBlackQueen board sq)]
     [(equal? piece 'blackKing) (updateBlackKing board sq)]
     [else board]
     )))
 
