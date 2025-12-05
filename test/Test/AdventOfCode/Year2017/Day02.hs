module Test.AdventOfCode.Year2017.Day02 where

import AdventOfCode.Puzzle
import AdventOfCode.Year2017.Day02
import Test.Tasty
import Test.Tasty.HUnit

test_day02_examples :: TestTree
test_day02_examples =
  testGroup
    "Examples"
    [ testCase "Part One" $
        evaluatingPuzzle
          partOne
          [ [5, 1, 9, 5],
            [7, 5, 3],
            [2, 4, 6, 8]
          ]
          >>= (@?= 18),
      testCase "Part Two" $ do
        evaluatingPuzzle
          partTwo
          [ [5, 9, 2, 8],
            [9, 4, 7, 3],
            [3, 8, 6, 5]
          ]
          >>= (@?= 9)
    ]

test_day02_answers :: TestTree
test_day02_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $
        getInput >>= evaluatingPuzzle partOne >>= (@?= 41887),
      testCase "Part Two" $
        getInput >>= evaluatingPuzzle partTwo >>= (@?= 226)
    ]
