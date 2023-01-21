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
  (let*-values ([f (string (car (string->list square)))]
		[r (string (cadr (string->list square)))]
		[y (substring-index-ci (car f) "abcdefgh")]
		[z (substring-index-ci (car r) "12345678")]
		[l (->number(cons (car y) z))]
		)
    (begin
      (print l)
      )
              
     ))

(time (square->index "a4"))

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
