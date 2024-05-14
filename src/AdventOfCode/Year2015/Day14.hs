{-# LANGUAGE TupleSections #-}

module AdventOfCode.Year2015.Day14 where

import AdventOfCode.Input
import AdventOfCode.TH
import Control.Lens
import Control.Monad (unless)
import Control.Monad.State
import Text.Trifecta hiding (position)

data ReindeerState = ReindeerState
  { _position :: Integer,
    _running :: Integer,
    _wait :: Integer,
    _score :: Integer
  }
  deriving (Eq, Show)

makeLenses ''ReindeerState

instance Ord ReindeerState where
  compare x y = compare (x ^. position) (y ^. position)

data Reindeer = Reindeer
  { _name :: String,
    _speed :: Integer,
    _stamina :: Integer,
    _rest :: Integer
  }
  deriving (Eq, Show)

makeLenses ''Reindeer

reindeer :: Parser Reindeer
reindeer =
  Reindeer
    <$> some letter
    <*> (string " can fly " *> natural <* string "km/s")
    <*> (string " for " *> natural <* string "seconds,")
    <*> (string " but then must rest for " *> natural <* string "seconds.")

main :: IO ()
main = $(defaultMain)

getInput :: IO [Reindeer]
getInput = parseInput (reindeer `sepEndBy` newline) $(inputFilePath)

partOne :: [Reindeer] -> Integer
partOne = _position . maximum . map (run 2503)

partTwo :: [Reindeer] -> Integer
partTwo flyers = maximum . map (_score . snd) $ foldl go xs [1 :: Integer .. 2503]
  where
    go ys = const $ zip flyers $ awardPoint $ map snd zs
      where
        zs = map (\(flyer, st) -> (flyer, execState (step flyer) st)) ys
    xs = map (,ReindeerState 0 0 0 0) flyers

run :: Integer -> Reindeer -> ReindeerState
run seconds flyer =
  foldl
    (const . execState (step flyer))
    (ReindeerState 0 0 0 0)
    [1 .. seconds]

step :: Reindeer -> State ReindeerState ()
step flyer =
  do
    waiting <- uses wait (> 0)
    wait -= 1
    unless waiting $
      do
        tired <- uses running (flyer ^. stamina ==)
        if tired
          then do
            running .= 0
            wait .= (flyer ^. rest) - 1
          else do
            running += 1
            position += (flyer ^. speed)

awardPoint :: [ReindeerState] -> [ReindeerState]
awardPoint xs = map go xs
  where
    winner = maximum xs
    go flyer =
      if flyer ^. position == winner ^. position
        then (score +~ 1) flyer
        else flyer
