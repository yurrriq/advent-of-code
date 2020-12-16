module AdventOfCode.Year2020.Day09
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (liftM2)
import Data.List (find, inits, splitAt, tails)
import Text.Trifecta (natural, some)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    answerOne <-
      case partOne input of
        Just answer -> print answer *> pure answer
        Nothing -> error "Failed!"
    putStr "Part Two: "
    case partTwo answerOne input of
      Just answer -> print answer
      Nothing -> error "Failed!"

getInput :: IO [Int]
getInput = parseInput (some (fromInteger <$> natural)) $(inputFilePath)

partOne :: [Int] -> Maybe Int
partOne xs = head . snd <$> find (null . go) (splitAt 25 <$> tails xs)
  where
    go (preamble, (z : _)) =
      [ (z, x, y)
        | length preamble == 25,
          (x : ys) <- tails preamble,
          x < z,
          y <- ys,
          y < z,
          z == x + y
      ]
    go _ = []

partTwo :: Int -> [Int] -> Maybe Int
partTwo n xs =
  liftM2 (+) minimum maximum
    <$> find ((n ==) . sum) (concatMap inits (tails xs))
