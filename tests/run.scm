(import test
	srfi-178
	)

(include-relative "../defboard.scm")


(define (pp-tree t)
  (let loop ((attr (bb->alist t)))
    (cond ((null? attr) 'done)
          (else
            (display (caar attr))(display ": ")
            (display (bitvector->string (cdar attr)))(newline)
            (loop (cdr attr))))))

(define (game-def)
  (let ([board (make-bb)])
    (begin
      (pp-tree board))))

(define (game-hash)
  (let ([bb make-bitboard-hash])
    (begin
      (update-board bb 'a3 'blackKing)
      (display-bitboard bb)
      )))
(game-def)
(game-hash)
(test-exit)


