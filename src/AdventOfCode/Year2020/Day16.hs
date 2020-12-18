module AdventOfCode.Year2020.Day16
  ( main,
    getInput,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Monad (void)
import Data.Ix (inRange)
import Data.List ((\\), isPrefixOf, transpose)
import Data.Maybe (mapMaybe)
import Linear (V2 (..))
import Text.Trifecta (Parser, anyChar, char, commaSep, eof, manyTill, natural, symbol)

main :: IO ()
main =
  do
    input <- getInput
    putStr "Part One: "
    print $ partOne input
    putStr "Part Two: "
    print $ partTwo input

-- TODO: Use better types
getInput :: IO ([(String, V2 (Int, Int))], [Int], [[Int]])
getInput = flip parseInput $(inputFilePath) $
  do
    ticketRules <- manyTill ticketRule (symbol "your ticket:")
    myTicket <- commaSep posInt
    void $ symbol "nearby tickets:"
    nearbyTickets <- manyTill (commaSep posInt) eof
    pure (ticketRules, myTicket, nearbyTickets)

partOne :: ([(String, V2 (Int, Int))], [Int], [[Int]]) -> Int
partOne (rules, _, nearbyTickets) =
  sum $ concatMap (invalidFields rules) nearbyTickets

-- FIXME: Clean this up...
partTwo :: ([(String, V2 (Int, Int))], [Int], [[Int]]) -> Int
partTwo (rules, myTicket, nearbyTickets) =
  product
    $ map ((myTicket !!) . fst)
    $ filter (isPrefixOf "departure" . snd)
    $ zip [0 ..]
    $ concat
    $ iterate go possibleFields !! length nearbyTickets
  where
    go xs =
      flip map xs $ \ys ->
        foldr (\as bs -> if as == bs then as else bs \\ as) ys singles
      where
        singles = filter ((1 ==) . length) xs
    possibleFields =
      [ flip mapMaybe rules $ \(label, ranges) ->
          if all (\field -> any (`inRange` field) ranges) fields
            then Just label
            else Nothing
        | fields <- transpose validTickets
      ]
    validTickets = filter (not . any (invalidField rules)) nearbyTickets

ticketRule :: Parser (String, V2 (Int, Int))
ticketRule =
  do
    label <- manyTill anyChar (symbol ":")
    lhs <- intRange
    void (symbol "or")
    rhs <- intRange
    pure (label, V2 lhs rhs)

invalidFields :: [(String, V2 (Int, Int))] -> [Int] -> [Int]
invalidFields = filter . invalidField

invalidField :: [(String, V2 (Int, Int))] -> Int -> Bool
invalidField rules field = not (any (any (`inRange` field) . snd) rules)

intRange :: Parser (Int, Int)
intRange =
  do
    from <- posInt
    void (char '-')
    to <- posInt
    pure (from, to)

posInt :: Parser Int
posInt = fromInteger <$> natural
