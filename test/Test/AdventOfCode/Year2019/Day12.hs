module Test.AdventOfCode.Year2019.Day12 where

import AdventOfCode.Puzzle
import AdventOfCode.Year2019.Day12 (getInput, partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day12 :: TestTree
test_day12 =
  testGroup
    "Answers"
    [ testCase "Part One" $
        getInput >>= evaluatingPuzzle partOne >>= (@?= 6423),
      testCase "Part Two" $
        getInput >>= evaluatingPuzzle partTwo >>= (@?= 327636285682704)
    ]
