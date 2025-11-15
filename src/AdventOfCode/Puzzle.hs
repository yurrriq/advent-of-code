{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Puzzle where

import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Relude

newtype Puzzle r s a = Puzzle
  {runPuzzle :: ReaderT r (StateT s (LoggingT IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadLogger,
      MonadState s,
      MonadReader r,
      MonadFail
    )

evalPart :: (MonadIO m) => r -> s -> Puzzle r s a -> m a
evalPart input initialState =
  liftIO
    . runStderrLoggingT
    . evaluatingStateT initialState
    . usingReaderT input
    . runPuzzle
