module Test.AOC19.Day12 where

import Data.AOC19.Day12 (partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day12 :: TestTree
test_day12 =
  testGroup
    "Day 12"
    [ testCase "finds the total energy in the system after 1000 steps" $ do
        sumEnergy <- partOne
        sumEnergy @?= 6423,
      testCase "counts the steps it takes to reach the first cycle" $ do
        numSteps <- partTwo
        numSteps @?= 327636285682704
    ]
