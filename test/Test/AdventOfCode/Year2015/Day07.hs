{-# LANGUAGE LambdaCase #-}

module Test.AdventOfCode.Year2015.Day07 where

import AdventOfCode.Year2015.Day07 (circuit, evalCircuit)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Trifecta (Result (..), parseString)

test_day07_examples :: TestTree
test_day07_examples =
  testGroup
    "Examples"
    [ testCase "the example evaluates correctly" $
        do
          let input =
                fromJust
                  . \case Success c -> Just c; Failure _ -> Nothing
                  . parseString circuit mempty
                  . unlines
                  $ [ "123 -> x",
                      "456 -> y",
                      "x AND y -> d",
                      "x OR y -> e",
                      "x LSHIFT 2 -> f",
                      "y RSHIFT 2 -> g",
                      "NOT x -> h",
                      "NOT y -> i"
                    ]
          let expected =
                Map.fromList
                  [ ("d", 72),
                    ("e", 507),
                    ("f", 492),
                    ("g", 114),
                    ("h", 65412),
                    ("i", 65079),
                    ("x", 123),
                    ("y", 456)
                  ]
          evalCircuit input @?= expected
    ]
