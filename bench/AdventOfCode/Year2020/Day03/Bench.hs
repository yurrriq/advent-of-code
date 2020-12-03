import AdventOfCode.Year2020.Day03 (getInput, partOne, partTwo)
import Criterion.Main

main :: IO ()
main =
  do
    input <- getInput
    defaultMain
      [ bgroup
          "Day 3"
          [ bench "parse" $ whnfIO getInput,
            bench "p1" $ whnf partOne input,
            bench "p2" $ whnf partTwo input
          ]
      ]
