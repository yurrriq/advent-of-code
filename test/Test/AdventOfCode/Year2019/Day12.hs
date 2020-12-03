module Test.AdventOfCode.Year2019.Day12 where

import AdventOfCode.Year2019.Day12 (getInput, partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day12 :: TestTree
test_day12 =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        input <- getInput
        partOne input @?= 6423,
      testCase "Part Two" $ do
        input <- getInput
        partTwo input @?= 327636285682704
    ]
