(import
	srfi-178
	(chicken random)
	(only vector-lib vector-for-each vector-unfold) 
	)

;; 1. Generate and fill 64 x 12 array
;; 


;;builds a new bit vector with random value.
(define (generate-random-bits)
	(bitvector-unfold (lambda (_) (pseudo-random-integer 2)) 64))

(define (vec-sq-helper-rand new old index)
  (if (< index 0)
      new
      (begin (vector-set! new index (generate-random-bits))
						 (vec-sq-helper-rand new old (- index 1)))))


(define (vector-table-rand vec)
  (vec-sq-helper-rand (make-vector (vector-length vec))
		 vec
		 (- (vector-length vec) 1)))


(define (vec-sq-helper new old index)
  (if (< index 0)
      new
      (begin (vector-set! new index (vector-table-rand (make-vector 6)))
						 (vec-sq-helper new old (- index 1)))))


(define (vector-table vec)
  (vec-sq-helper (make-vector (vector-length vec))
		 vec
		 (- (vector-length vec) 1)))

(define vector-with-black
	(vector-unfold (lambda (i x) (generate-random-bits))  6 generate-random-bits)

	)


(define init-zobrist
	(let ([table (vector-table (make-vector 64))])
		(begin
			(vector-for-each (lambda (i x)
												 (vector-for-each
													(lambda (j y)
														(print (bitvector->integer y)))	x)) table))))

(vector-with-black)

;;(bitvector->integer ( generate-random-bits))

;;(bitvector->string ( generate-random-bits))

