{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module AdventOfCode.Year2015.Day22.Types where

import Control.Lens (makeFields)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data SpellInstant
  = MagicMissile
  | Drain
  deriving (Eq, Generic, Ord, Show)

instance Hashable SpellInstant

data SpellEffect
  = Shield
  | Poison
  | Recharge
  deriving (Eq, Generic, Ord, Show)

instance Hashable SpellEffect

data SpellRule
  = Instant !SpellInstant
  | Effect !SpellEffect !Int
  deriving (Eq, Generic, Show)

instance Hashable SpellRule

type Spell = (Int, SpellRule)

data GameTurn
  = TurnBoss
  | TurnPlayer
  deriving (Eq, Generic, Ord, Show)

instance Hashable GameTurn

data Boss = Boss
  { _bossHitPoints :: !Int,
    _bossDamage :: !Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable Boss

makeFields ''Boss

data Player = Player
  { _playerHitPoints :: !Int,
    _playerArmor :: !Int,
    _playerMana :: !Int,
    _playerManaSpent :: !Int
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable Player

makeFields ''Player

data GameState = GameState
  { _gameStateTurn :: !GameTurn,
    _gameStateBoss :: !Boss,
    _gameStatePlayer :: !Player,
    _gameStateActiveEffects :: ![(SpellEffect, Int)],
    _gameStateHardMode :: !Bool
  }
  deriving (Eq, Generic, Ord, Show)

instance Hashable GameState

makeFields ''GameState
