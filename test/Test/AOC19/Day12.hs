module Test.AOC19.Day12 where

import Data.AOC19.Day12 (partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day12 :: TestTree
test_day12 =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        sumEnergy <- partOne
        sumEnergy @?= 6423,
      testCase "Part Two" $ do
        numSteps <- partTwo
        numSteps @?= 327636285682704
    ]
