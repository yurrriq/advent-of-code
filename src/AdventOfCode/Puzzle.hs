{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Puzzle where

import Relude

newtype Puzzle s a
  = Puzzle {runPuzzle :: StateT s IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState s,
      MonadFail
    )
