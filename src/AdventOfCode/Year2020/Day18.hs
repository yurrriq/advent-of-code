module AdventOfCode.Year2020.Day18
  ( main,
    partOne,
    partTwo,
  )
where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr
import Text.Trifecta (Parser, natural, parens, some, symbol)

main :: IO ()
main =
  do
    putStr "Part One: "
    print =<< partOne
    putStr "Part Two: "
    print =<< partTwo

partOne :: IO Int
partOne = sum <$> parseInput (some expr) $(inputFilePath)
  where
    expr = makeExprParser (parens expr <|> posInt) [[addition, multiplication]]

partTwo :: IO Int
partTwo = sum <$> parseInput (some expr) $(inputFilePath)
  where
    expr = makeExprParser (parens expr <|> posInt) [[addition], [multiplication]]

addition :: Operator Parser Int
addition = InfixL ((+) <$ symbol "+")

multiplication :: Operator Parser Int
multiplication = InfixL ((*) <$ symbol "*")

posInt :: Parser Int
posInt = fromInteger <$> natural
