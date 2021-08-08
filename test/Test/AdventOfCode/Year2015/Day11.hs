module Test.AdventOfCode.Year2015.Day11 where

import AdventOfCode.Year2015.Day11
import Test.Tasty
import Test.Tasty.HUnit

test_day11_answers :: TestTree
test_day11_answers =
  testGroup
    "Answers"
    [ testCase "Part One" $ do
        input <- getInput
        partOne input @?= Just "cqjxxyzz",
      testCase "Part Two" $ do
        input <- getInput
        partTwo input @?= Just "cqkaabcc"
    ]

test_day11_examples :: TestTree
test_day11_examples =
  testGroup
    "Examples"
    [ let password = "hijklmmn"
       in testCase password $ do
            meetsTheFirstRequirement password
            failsTheSecondRequirement password,
      let password = "abbceffg"
       in testCase password $ do
            meetsTheThirdRequirement password
            failsTheFirstRequirement password,
      let password = "abbcegjk"
       in testCase password $ do
            failsTheThirdRequirement password,
      let password = "abcdefgh"
       in testCase password $ do
            findNextPassword password @?= Just "abcdffaa",
      let password = "ghijklmn"
       in testCase password $ do
            findNextPassword password @?= Just "ghjaabcc"
    ]

meetsTheFirstRequirement :: String -> Assertion
meetsTheFirstRequirement password =
  assertBool "meets the first requirement" $
    hasStraightR 3 (reverse password)

failsTheFirstRequirement :: String -> Assertion
failsTheFirstRequirement password =
  assertBool "fails the third requirement" $
    not (hasStraightR 3 (reverse password))

failsTheSecondRequirement :: String -> Assertion
failsTheSecondRequirement password =
  assertBool "fails the second requirement" $
    isConfusing password

meetsTheThirdRequirement :: String -> Assertion
meetsTheThirdRequirement password =
  assertBool "meets the third requirement" $
    countUniquePairs password >= 2

failsTheThirdRequirement :: String -> Assertion
failsTheThirdRequirement password =
  assertBool "fails the third requirement" $
    countUniquePairs password < 2
