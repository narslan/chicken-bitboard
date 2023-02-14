### `chicken-bitboard` (WIP

`chicken-bitboard` implements a simple bitboard for chess.

### Implemented features 
  * Draw a board from fen string
  * Generate hash from given position  
  * Move pieces

```scheme
(draw-board (bbindex (build-board-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")))
```
![sample](sample.pgn)
