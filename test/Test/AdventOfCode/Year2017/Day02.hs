module Test.AdventOfCode.Year2017.Day02 where

import AdventOfCode.Year2017.Day02
import Test.Tasty
import Test.Tasty.HUnit

test_day02_examples :: TestTree
test_day02_examples =
  testGroup
    "Examples"
    [ testCase "Part One" $
        do
          partOne
            [ [5, 1, 9, 5],
              [7, 5, 3],
              [2, 4, 6, 8]
            ]
            @?= 18,
      testCase "Part Two" $
        do
          partTwo
            [ [5, 9, 2, 8],
              [9, 4, 7, 3],
              [3, 8, 6, 5]
            ]
            @?= 9
    ]

test_day02_answers :: TestTree
test_day02_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $
        (41887 @=?) . partOne =<< getInput,
      testCase "Part Two" $
        (226 @=?) . partTwo =<< getInput
    ]
