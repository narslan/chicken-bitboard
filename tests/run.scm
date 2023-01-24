(import test
	srfi-178
	)

(include-relative "../defboard.scm")

(define (game1)
  (let ([bb make-bitboard])
    (begin
      (update-board bb 'a3 'blackKing)
      (display-bitboard bb)
      )

    )
  )
(game1)
(test-exit)


