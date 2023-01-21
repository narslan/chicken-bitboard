(import
  srfi-178 ;bitvector library
  defstruct
  test
  (chicken format)
  )


(defstruct
  bitboard
  blackPawn 
  blackKnight 
  blackBishop 
  blackRook   
  blackQueen  
  blackKing   
  whitePawn   
  whiteKnight 
  whiteBishop 
  whiteRook   
  whiteQueen  
  whiteKing   
  whitePieces 
  blackPieces 
  )

(define make-blank-board
  (make-bitboard
   blackPawn: (make-bitvector 64) 
   blackKnight: (make-bitvector 64) 
   blackBishop: (make-bitvector 64)  
   blackRook: (make-bitvector 64)    
   blackQueen: (make-bitvector 64)   
   blackKing: (make-bitvector 64)    
   whitePawn: (make-bitvector 64)    
   whiteKnight: (make-bitvector 64)  
   whiteBishop: (make-bitvector 64)  
   whiteRook: (make-bitvector 64)    
   whiteQueen: (make-bitvector 64)   
   whiteKing: (make-bitvector 64)    
   whitePieces: (make-bitvector 64)  
   blackPieces: (make-bitvector 64) 
))


(define (bitboard-all-bits bv)
  (bit-vector-ior (bitboard-whitePieces bv) (bitboard-blackPieces bv) )
  )
;; return a bitvector with idx set
(define (make-bitvector-at idx)
  (let ([m1 (make-bitvector 64 0)])
    (begin
      (bitvector-set! m1 idx 1) m1)))

(define (update-square bv square newPiece)
  (let ([mask (make-bitvector-at square)])
    (cond ((equal? newPiece 'p)
	   (begin
	     (bitboard-blackPawn-set! bv (bitvector-ior (bitboard-blackPawn bv) mask))
	     (bitboard-blackPieces-set! bv (bitvector-ior (bitboard-blackPieces bv) mask))
	     bv
	     ))
	  (else (error "not implemented"))
	  )))


(print (bitvector->string (update-square make-blank-board 5 'p)) )
;;(time(printf "bits: ~B\n" (bitboard-blackPieces (update-square make-blank-board 1 'p))))

;; (time(print (bitboard-blackQueen make-blank-board)))
;; (time(print (bitboard-all-bits make-blank-board)))
(test-exit)
