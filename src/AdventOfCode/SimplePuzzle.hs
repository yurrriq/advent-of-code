{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.SimplePuzzle
  ( runPuzzle,
    SimplePuzzle,
    emptyPuzzleState,
    evalPart,
  )
where

import AdventOfCode.Puzzle (Puzzle (..))
import Control.Monad.Logger (runStderrLoggingT)
import Relude

type SimplePuzzle r a = Puzzle r () a

emptyPuzzleState :: ()
emptyPuzzleState = ()

evalPart :: r -> SimplePuzzle r a -> IO a
evalPart input =
  runStderrLoggingT
    . evaluatingStateT emptyPuzzleState
    . usingReaderT input
    . runPuzzle
