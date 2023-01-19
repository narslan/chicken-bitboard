(import
  iset
  srfi-69
  defstruct
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
   blackPawn: (make-bit-vector 64) 
   blackKnight: (make-bit-vector 64) 
   blackBishop: (make-bit-vector 64)  
   blackRook: (make-bit-vector 64)    
   blackQueen: (make-bit-vector 64)   
   blackKing: (make-bit-vector 64)    
   whitePawn: (make-bit-vector 64)    
   whiteKnight: (make-bit-vector 64)  
   whiteBishop: (make-bit-vector 64)  
   whiteRook: (make-bit-vector 64)    
   whiteQueen: (make-bit-vector 64)   
   whiteKing: (make-bit-vector 64)    
   whitePieces: (make-bit-vector 64)  
   blackPieces: (make-bit-vector 64) 
))


(define (bitboard-all-bits bv)
  (bit-vector-ior (bitboard-whitePieces bv) (bitboard-blackPieces bv) )
  )

(define (update-square bv square newPiece)
 (let ([mask (bit-vector-shift (make-bit-vector 64) square)])
   (cond ((equal? newPiece 'p) (begin
				 (bit-vector-ior (bitboard-blackPawns bv) mask)
				 (bit-vector-ior (bitboard-blackPieces bv) mask)
				 ))
	 )
   )

  )


(time(print (bitboard-blackQueen make-blank-board)))
(time(print (bitboard-all-bits make-blank-board)))
