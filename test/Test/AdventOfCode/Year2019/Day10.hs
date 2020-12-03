module Test.AdventOfCode.Year2019.Day10 where

import AdventOfCode.Year2019.Day10 (getInput, partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day10 :: TestTree
test_day10 =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        input <- getInput
        partOne input @?= 326,
      testCase "Part Two" $ do
        input <- getInput
        partTwo input @?= 1623
    ]
