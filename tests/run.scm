(import test
	srfi-178
	)

(include-relative "../defboard.scm")


(define (pp-tree t)
  (let loop ((attr (bb->alist t)))
    (cond ((null? attr) 'done)
          (else
            (display (caar attr)
		     )(display ": ")
            (display (bitvector->string (cdar attr)))(newline)
            (loop (cdr attr))))))

(define (game-def)
  (let (
	[board2 (update-board (make-bb) 'a1 'blackRook)]
	)
    (begin
      (pp-tree (update-board (make-bb) 'a1 'blackRook)))))

(game-def)
;;(game-hash)
(test-exit)


