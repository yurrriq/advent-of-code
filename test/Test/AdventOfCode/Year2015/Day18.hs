module Test.AdventOfCode.Year2015.Day18 where

import AdventOfCode.Year2015.Day18
import Test.Tasty
import Test.Tasty.HUnit

test_day18_examples :: TestTree
test_day18_examples =
  testGroup
    "Examples"
    [ testCase "Part One" $
        doPartOne 6 4 input @?= 4,
      testCase "Part Two" $
        doPartTwo 6 5 input @?= 17
    ]
  where
    input =
      mkGrid . map (map (== '#')) $
        [ ".#.#.#",
          "...##.",
          "#....#",
          "..#...",
          "#.#..#",
          "####.."
        ]

test_day18_answers :: TestTree
test_day18_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        input <- getInput
        partOne input @?= 768,
      testCase "Part Two" $ do
        input <- getInput
        partTwo input @?= 781
    ]
