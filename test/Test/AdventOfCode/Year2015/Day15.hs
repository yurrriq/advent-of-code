module Test.AdventOfCode.Year2015.Day15 where

import AdventOfCode.Year2015.Day15
import Test.Tasty
import Test.Tasty.HUnit

test_day15_examples :: TestTree
test_day15_examples =
  testGroup
    "Examples"
    [ testCase "Part One" $
        partOne input @?= 62842880,
      testCase "Part Two" $
        partTwo input @?= 57600000
    ]

input :: [Ingredient Integer]
input =
  [ Ingredient (-1) (-2) 6 3 8,
    Ingredient 2 3 (-2) (-1) 3
  ]
