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
(define squares
(let ([squares (make-hash-table)])
    (begin
      (hash-table-set! squares "a1" 0)
      (hash-table-set! squares "a2" 1)
      (hash-table-set! squares "a3" 2)
      (hash-table-set! squares "a4" 3)
      (hash-table-set! squares "a5" 4)
      (hash-table-set! squares "a6" 5)
      (hash-table-set! squares "a7" 6)
      (hash-table-set! squares "a8" 7)
      (hash-table-set! squares "b1" 8)
      (hash-table-set! squares "b2" 9)
      (hash-table-set! squares "b3" 10)
      (hash-table-set! squares "b4" 11)
      (hash-table-set! squares "b5" 12)
      (hash-table-set! squares "b6" 13)
      (hash-table-set! squares "b7" 14)
      (hash-table-set! squares "b8" 15)
      (hash-table-set! squares "c1" 16)
      (hash-table-set! squares "c2" 17)
      (hash-table-set! squares "c3" 18)
      (hash-table-set! squares "c4" 19)
      (hash-table-set! squares "c5" 20)
      (hash-table-set! squares "c6" 21)
      (hash-table-set! squares "c7" 22)
      (hash-table-set! squares "c8" 23)
      (hash-table-set! squares "d1" 24)
      (hash-table-set! squares "d2" 25)
      (hash-table-set! squares "d3" 26)
      (hash-table-set! squares "d4" 27)
      (hash-table-set! squares "d5" 28)
      (hash-table-set! squares "d6" 29)
      (hash-table-set! squares "d7" 30)
      (hash-table-set! squares "d8" 31)
      (hash-table-set! squares "e1" 32)
      (hash-table-set! squares "e2" 33)
      (hash-table-set! squares "e3" 34)
      (hash-table-set! squares "e4" 35)
      (hash-table-set! squares "e5" 36)
      (hash-table-set! squares "e6" 37)
      (hash-table-set! squares "e7" 38)
      (hash-table-set! squares "e8" 39)
      (hash-table-set! squares "f1" 40)
      (hash-table-set! squares "f2" 41)
      (hash-table-set! squares "f3" 42)
      (hash-table-set! squares "f4" 43)
      (hash-table-set! squares "f5" 44)
      (hash-table-set! squares "f6" 45)
      (hash-table-set! squares "f7" 46)
      (hash-table-set! squares "f8" 47)
      (hash-table-set! squares "g1" 48)
      (hash-table-set! squares "g2" 49)
      (hash-table-set! squares "g3" 50)
      (hash-table-set! squares "g4" 51)
      (hash-table-set! squares "g5" 52)
      (hash-table-set! squares "g6" 53)
      (hash-table-set! squares "g7" 54)
      (hash-table-set! squares "g8" 55)
      (hash-table-set! squares "h1" 56)
      (hash-table-set! squares "h2" 57)
      (hash-table-set! squares "h3" 58)
      (hash-table-set! squares "h4" 59)
      (hash-table-set! squares "h5" 60)
      (hash-table-set! squares "h6" 61)
      (hash-table-set! squares "h7" 62)
      (hash-table-set! squares "h8" 63)
      squares))  )


(define make-bitboard
  (let ([h (make-hash-table)])
    (begin
      (hash-table-set! h 'blackPawn (make-bitvector 64 0))
      (hash-table-set! h 'blackKnight (make-bitvector 64 0))
      (hash-table-set! h 'blackKing (make-bitvector 64 0))
      (hash-table-set! h 'whitePawn (make-bitvector 64 0))
      (hash-table-set! h 'whiteKnight (make-bitvector 64 0))
      (hash-table-set! h 'whiteKing (make-bitvector 64 0))
      (hash-table-set! h 'blackAll (make-bitvector 64 0)) ;;black all
      (hash-table-set! h 'whiteAll (make-bitvector 64 0)) ;;white all
      h)))

(define (display-bitboard bb)
  (begin
   (hash-table-walk bb (lambda (k v) (print k (bitvector->string v))))))

;; "a1" -> 0 "h8"-> 63
(: square->int (string --> fixnum))
(define (square->index square)
  (let* ([f squares])
    (hash-table-ref squares square)
    
    ))

(time (print (square->index "a2")))

;; (define testmask
;;   (let ([m1 (make-bitvector 64 0)]
;; 	[m2 (make-bitvector 64 1)]
;; 	[m3 (bitvector-unfold (lambda (_ b) (values b (not b))) 64 #f)]
;; 	)
;;     (begin (bitvector-set! m1 0 1) m1)
;;     (print (bitvector->string m1) )
;;     (print (bitvector->string m3) )
;;     ))
;; a1 a2 a3 a4 .... h7 h8
;; 0  1  2  3 .... 62 63




(define build-bitboard
  (let ([bb make-bitboard] )
    (begin
      (hash-table-set! bb 'blackKing (make-bitvector-at 0))
      (hash-table-set! bb 'blackAll (make-bitvector-at 2))
      bb)))
