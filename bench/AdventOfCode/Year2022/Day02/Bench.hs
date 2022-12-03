import AdventOfCode.Year2022.Day02 (getInput, partOne, partTwo)
import Criterion.Main (bench, bgroup, defaultMain, whnf, whnfIO)

main :: IO ()
main =
  do
    input <- getInput
    defaultMain
      [ bgroup
          "2022 Day 2"
          [ bench "parse" $ whnfIO getInput,
            bench "p1" $ whnf partOne input,
            bench "p2" $ whnf partTwo input
          ]
      ]
