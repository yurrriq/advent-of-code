module Test.AOC19.Day10 where

import Data.AOC19.Day10 (partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day10 :: TestTree
test_day10 =
  testGroup
    "Day 10"
    [ testCase "finds the best location for a new monitoring station" $ do
        numAsteroids <- partOne
        numAsteroids @?= 326,
      testCase "completely vaporizes correctly" $ do
        numAsteroids <- partTwo
        numAsteroids @?= 1623
    ]
