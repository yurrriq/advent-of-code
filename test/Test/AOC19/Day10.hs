module Test.AOC19.Day10 where

import Data.AOC19.Day10 (partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day10 :: TestTree
test_day10 =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        numAsteroids <- partOne
        numAsteroids @?= 326,
      testCase "Part Two" $ do
        numAsteroids <- partTwo
        numAsteroids @?= 1623
    ]
