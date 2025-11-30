module Test.AdventOfCode.Year2019.Day10 where

import AdventOfCode.Puzzle
import AdventOfCode.Year2019.Day10 (getInput, partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day10 :: TestTree
test_day10 =
  testGroup
    "Answers"
    [ testCase "Part One" $
        getInput >>= evaluatingPuzzle partOne >>= (@?= 326),
      testCase "Part Two" $ do
        getInput >>= evaluatingPuzzle partTwo >>= (@?= 1623)
    ]
