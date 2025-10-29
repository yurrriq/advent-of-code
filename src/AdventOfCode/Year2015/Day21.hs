{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2015.Day21 where

import AdventOfCode.TH (defaultMain)
import Combinatorics (variate)
import Control.Arrow (second, (>>>))
import Control.Lens (views, (&), (+~), (-~), (^.), _1, _2)
import Control.Monad (ap)
import Data.List.HT (takeUntil)
import Data.Tuple.Extra (both)
import Linear (V3 (..), _x, _y, _yz, _z)

type Item = (String, V3 Int)

type Fighter = (String, V3 Int)

main :: IO ()
main = $(defaultMain)

partOne :: (Fighter, [(Int, Fighter)]) -> Int
partOne (boss, players) =
  fst . minimum . filterWinner "Player" $
    map (getResult boss) players

partTwo :: (Fighter, [(Int, Fighter)]) -> Int
partTwo (boss, players) =
  fst . maximum . filterWinner "Boss" $
    map (getResult boss) players

getInput :: IO (Fighter, [(Int, Fighter)])
getInput = pure (("Boss", V3 103 9 2), allPlayers)

allPlayers :: [(Int, Fighter)]
allPlayers =
  map (foldl' equip (0, initialPlayer)) $
    foldl'
      (liftA2 (++))
      [[]]
      [ variate 1 weapons,
        [] : variate 1 armors,
        [] : variate 1 rings ++ variate 2 rings
      ]

weapons :: [Item]
weapons =
  [ ("Dagger", V3 8 4 0),
    ("Shortsword", V3 10 5 0),
    ("Warhammer", V3 25 6 0),
    ("Longsword", V3 40 7 0),
    ("Greataxe", V3 74 8 0)
  ]

armors :: [Item]
armors =
  [ ("Leather", V3 13 0 1),
    ("Chainmail", V3 31 0 2),
    ("Splintmail", V3 53 0 3),
    ("Bandedmail", V3 75 0 4),
    ("Platemail", V3 102 0 5)
  ]

rings :: [Item]
rings =
  [ ("Damage +1", V3 25 1 0),
    ("Damage +2", V3 50 2 0),
    ("Damage +3", V3 100 3 0),
    ("Defense +1", V3 20 0 1),
    ("Defense +2", V3 40 0 2),
    ("Defense +3", V3 80 0 3)
  ]

initialPlayer :: Fighter
initialPlayer = ("Player", V3 100 0 0)

attack :: Fighter -> Fighter -> Fighter
attack defender attacker = defender & _2 . _x -~ damage
  where
    damage :: Int
    damage = max 1 (attacker ^. _2 . _y - defender ^. (_2 . _z))

fight :: (Fighter, Fighter) -> [(Fighter, Fighter)]
fight = takeUntil isFightOver . iterate (uncurry (ap (,) . attack))

isFightOver :: (Fighter, Fighter) -> Bool
isFightOver = both (views (_2 . _x) (<= 0)) >>> uncurry (||)

equip :: (Int, Fighter) -> Item -> (Int, Fighter)
equip (gold, player) item = (gold + item ^. _2 . _1, player & _2 . _yz +~ (item ^. _2 . _yz))

filterWinner :: String -> [(Int, (Fighter, Fighter))] -> [(Int, (Fighter, Fighter))]
filterWinner winner = filter ((== winner) . fst . fst . snd)

getResult :: Fighter -> (Int, Fighter) -> (Int, (Fighter, Fighter))
getResult boss = second (last . fight . (boss,))

example :: [(Fighter, Fighter)]
example = fight (("Boss", V3 12 7 2), ("Player", V3 8 5 5))
