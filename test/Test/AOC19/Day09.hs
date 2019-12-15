{-# LANGUAGE OverloadedLists #-}
module Test.AOC19.Day09 where

import           Conduit
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.AOC19.Day09 (await', evalStack)
import           Data.Digits      (digits)
import           Data.Vector      (fromList, (!))


test_day09 :: TestTree
test_day09 = testGroup "Day 9"
  [ testCase "takes no input and produces a copy of itself as output" $
    do let input = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
       output <- runConduit $ pure () .| evalStack input .| sinkList
       fromList output @?= input
  , testCase "should output a 16-digit number" $
    do let input = [1102,34915192,34915192,7,4,7,99,0]
       output <- runConduit $ pure () .| evalStack input .| await'
       length (digits 10 output) @?= 16
  , testCase "should output the large number in the middle" $
    do let input = [104,1125899906842624,99]
       output <- runConduit $ pure () .| evalStack input .| await'
       output @?= input ! 1
  ]
