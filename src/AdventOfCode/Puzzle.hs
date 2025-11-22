{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Puzzle where

import Control.Lens (makeLenses)
import Control.Monad.Logger (LoggingT, MonadLogger, runStderrLoggingT)
import Generic.Data (GenericProduct (..))
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

type SimplePuzzle r a = Puzzle r () a

data GPuzzleState a b
  = GPuzzleState
  { _answerOne :: !a,
    _answerTwo :: !b
  }
  deriving (Eq, Generic, Show)
  deriving
    (Semigroup, Monoid)
    via (GenericProduct (GPuzzleState (Sum a) (Sum b)))

makeLenses ''GPuzzleState

type GPuzzleState1 a = GPuzzleState a a

evalPuzzle :: (MonadIO m) => r -> s -> Puzzle r s a -> m a
evalPuzzle input initialState =
  liftIO
    . runStderrLoggingT
    . evaluatingStateT initialState
    . usingReaderT input
    . runPuzzle

evaluatingPuzzle :: (MonadIO m, Monoid s) => Puzzle r s a -> r -> m a
evaluatingPuzzle puzzle input = evalPuzzle input mempty puzzle
