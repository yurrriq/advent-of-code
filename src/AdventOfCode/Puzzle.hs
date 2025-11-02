{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Puzzle where

import Control.Monad.Logger (LoggingT, MonadLogger)
import Relude

newtype Puzzle i s a = Puzzle
  {runPuzzle :: ReaderT i (StateT s (LoggingT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadState s,
      MonadReader i,
      MonadFail
    )
