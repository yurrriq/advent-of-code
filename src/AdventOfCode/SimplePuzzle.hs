module AdventOfCode.SimplePuzzle
  ( runPuzzle,
    SimplePuzzle,
    emptyPuzzleState,
  )
where

import AdventOfCode.Puzzle (Puzzle (..))

type SimplePuzzle i a = Puzzle i () a

emptyPuzzleState :: ()
emptyPuzzleState = ()
