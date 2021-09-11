module Test.AdventOfCode.Year2017.Day01 where

import AdventOfCode.Input (parseString)
import AdventOfCode.Year2017.Day01
import Data.Monoid (Sum)
import Data.Vector (Vector)
import Test.Tasty
import Test.Tasty.HUnit

test_day01_examples :: TestTree
test_day01_examples =
  testGroup
    "Examples"
    [ testGroup "Part One" . map (mkExample partOne) $
        [ ("1122", 3),
          ("1111", 4),
          ("1234", 0),
          ("91212129", 9)
        ],
      testGroup "Part Two" . map (mkExample partTwo) $
        [ ("1212", 6),
          ("1221", 0),
          ("123425", 4),
          ("123123", 12),
          ("12131415", 4)
        ]
    ]

test_day01_answers :: TestTree
test_day01_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $
        (1034 @=?) . partOne =<< getInput,
      testCase "Part Two" $
        (1356 @=?) . partTwo =<< getInput
    ]

mkExample :: (Vector (Sum Int) -> Int) -> (String, Int) -> TestTree
mkExample f (str, expected) =
  testCase str $
    do
      input <- parseString digits str
      f input @?= expected
