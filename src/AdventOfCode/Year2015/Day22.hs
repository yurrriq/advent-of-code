{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : AdventOfCode.Year2015.Day22
-- Description : Advent of Code 2015 Day 22: Wizard Simulator 20XX
-- Copyright   : (c) Eric Bailey, 2024
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : POSIX
-- https://adventofcode.com/2015/day/22
module AdventOfCode.Year2015.Day22
  ( main,
    partOne,
    partTwo,
    examples,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (defaultMainMaybe, inputFilePath)
import AdventOfCode.Util ((<&&>), (<||>))
import AdventOfCode.Year2015.Day22.Types
import Control.Arrow (first)
import Control.Lens (Getter, over, view, views, (%~), (+~), (-~), (.~), (^.))
import Data.Function ((&))
import Data.Graph.AStar (aStar)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Text.Trifecta (Parser, natural, symbol)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = $(defaultMainMaybe)

partOne :: GameState -> Maybe Int
partOne =
  fmap (view (player . manaSpent) . last)
    . aStar neighbors distance spamMagicMissile isBossDead
  where
    spamMagicMissile = views (boss . hitPoints) ((* 53) . (+ 1) . (`div` 4))
    neighbors state =
      if isGameOver state
        then HashSet.empty
        else HashSet.fromList (map fst (runTurn state))
    distance = flip (HashMap.lookupDefault maxBound) . nexts
    nexts = HashMap.fromList . runTurn

partTwo :: GameState -> Maybe Int
partTwo = undefined

-- ---------------------------------------------------------------- [ Examples ]

examples :: IO [GameState]
examples = mapM mkExample [13, 14]

mkExample :: Int -> IO GameState
mkExample bossHitPoints =
  do
    initialState <- getInput
    pure $
      initialState
        { _gameStatePlayer = Player 10 0 250 0,
          _gameStateBoss = Boss bossHitPoints 8
        }

-- ------------------------------------------------------------------- [ Input ]

getInput :: IO GameState
getInput =
  do
    theBoss <- parseInput parseBoss $(inputFilePath)
    pure $
      GameState
        { _gameStateTurn = TurnPlayer,
          _gameStateBoss = theBoss,
          _gameStatePlayer =
            Player
              { _playerHitPoints = 50,
                _playerArmor = 0,
                _playerMana = 500,
                _playerManaSpent = 0
              },
          _gameStateActiveEffects = []
        }

parseBoss :: Parser Boss
parseBoss =
  Boss
    <$> (symbol "Hit Points:" *> (fromInteger <$> natural))
    <*> (symbol "Damage:" *> (fromInteger <$> natural))

-- ------------------------------------------------------------------ [ Spells ]

grimoire :: [Spell]
grimoire =
  [ (53, Instant MagicMissile),
    (73, Instant Drain),
    (113, Effect Shield 6),
    (173, Effect Poison 6),
    (229, Effect Recharge 5)
  ]

-- -------------------------------------------------------------- [ Turn Logic ]

runTurn :: GameState -> [(GameState, Int)]
runTurn st = case st ^. turn of
  TurnPlayer -> first (turn .~ TurnBoss) <$> playerAttacks (runEffects st)
  TurnBoss -> [(turn .~ TurnPlayer $ bossAttack (runEffects st), 0)]

runEffects :: GameState -> GameState
runEffects state =
  if isEffectActive Shield newState
    then newState
    else newState & (player . armor) .~ 0
  where
    newState =
      over activeEffects decrementTimers $
        foldr runEffect state (views activeEffects (map fst) state)
    decrementTimers = filter ((> 0) . snd) . map (fmap (subtract 1))

runEffect :: SpellEffect -> GameState -> GameState
runEffect Shield = (player . armor) .~ 7
runEffect Poison = (boss . hitPoints) -~ 3
runEffect Recharge = (player . mana) +~ 101

playerAttacks :: GameState -> [(GameState, Int)]
playerAttacks state =
  [ ((player . manaSpent) +~ cost $ (player . mana) -~ cost $ cast rule state, cost)
    | spell@(cost, rule) <- grimoire,
      canCast spell state
  ]

cast :: SpellRule -> GameState -> GameState
cast (Instant MagicMissile) = (boss . hitPoints) -~ 4
cast (Instant Drain) = (boss . hitPoints -~ 2) . (player . hitPoints +~ 2)
cast (Effect effect turns) = activeEffects %~ ((effect, turns) :)

bossAttack :: GameState -> GameState
bossAttack state = (player . hitPoints) -~ actualDamage $ state
  where
    playerArmor = view (player . armor) state
    actualDamage = views (boss . damage) (max 1 . subtract playerArmor) state

-- -------------------------------------------------------------- [ Predicates ]

canCast :: Spell -> GameState -> Bool
canCast (cost, spell) = hasEnoughMana cost <&&> go spell
  where
    go (Instant _) = const True
    go (Effect effect _) = not . isEffectActive effect

hasEnoughMana :: Int -> GameState -> Bool
hasEnoughMana = views (player . mana) . (<=)

isEffectActive :: SpellEffect -> GameState -> Bool
isEffectActive effect = views activeEffects ((effect `elem`) . map fst)

isGameOver :: GameState -> Bool
isGameOver = isBossDead <||> isPlayerDead -- <||> views (player . mana) (<= 0)

isDead :: (HasHitPoints a Int) => Getter GameState a -> GameState -> Bool
isDead who = views (who . hitPoints) (<= 0)

isBossDead :: GameState -> Bool
isBossDead = isDead boss

isPlayerDead :: GameState -> Bool
isPlayerDead = isDead player

-- --------------------------------------------------------------------- [ EOF ]
