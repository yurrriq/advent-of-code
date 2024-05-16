module Test.AdventOfCode.Year2015.Day23 where

import AdventOfCode.Year2015.Day23 (example, getInput, partOne, partTwo)
import Test.Tasty
import Test.Tasty.HUnit

test_day23_examples :: TestTree
test_day23_examples =
  testGroup
    "Examples"
    [ testCase "the example evaluates correctly" $
        (2 @?=) =<< example
    ]

test_day23_answers :: TestTree
test_day23_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $
        (184 @=?) . partOne
          =<< getInput,
      testCase
        "Part Two"
        $ (231 @=?) . partTwo =<< getInput
    ]
