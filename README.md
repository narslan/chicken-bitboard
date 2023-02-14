### `chicken-bitboard` (WIP

`chicken-bitboard` implements a simple bitboard for chess.

### Implemented features 
  * Draw a board from fen string
  * Generate hash from given position  
  * Move pieces

```scheme
  (draw-board (bbindex (build-board-from-fen "4k3/3p2p1/8/pP6/4P2P/8/8/4K3 w - a6 0 5")))
```
![sample](sample.pgn)
