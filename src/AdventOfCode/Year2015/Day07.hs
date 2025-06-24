{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module AdventOfCode.Year2015.Day07 where

import AdventOfCode.Input (parseInput)
import AdventOfCode.TH (inputFilePath)
import Control.Applicative ((<|>))
import Control.Monad.Reader
import Data.Bits (Bits (..), complement, shiftL, shiftR, (.&.), (.|.))
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Word (Word16)
import Text.Parser.Token.Highlight
import Text.Trifecta
import Prelude hiding (and, not, or)

type Circuit = Map String Gate

instance {-# OVERLAPPING #-} Show Circuit where
  show = unlines . map show . Map.toList

circuit :: Parser Circuit
circuit = Map.fromList <$> (wire `sepEndBy` newline)

evalCircuit :: Circuit -> Map String Word16
evalCircuit circ = results
  where
    results = Map.map (flip runReader results . evalGate) circ

evalGate :: Gate -> Reader (Map String Word16) Word16
evalGate (PROVIDE v) = evalValue v
evalGate (x `AND` y) = evalBinop (.&.) x y
evalGate (x `LSHIFT` y) = evalBinop shiftL' x y
evalGate (NOT v) = complement <$> evalValue v
evalGate (x `OR` y) = evalBinop (.|.) x y
evalGate (x `RSHIFT` y) = evalBinop shiftR' x y

evalBinop :: (Word16 -> Word16 -> Word16) -> Value -> Value -> Reader (Map String Word16) Word16
evalBinop f = liftA2 f `on` evalValue

evalValue :: Value -> Reader (Map String Word16) Word16
evalValue (Name c) = asks (! c)
evalValue (Signal x) = pure x

data Gate
  = PROVIDE Value
  | AND Value Value
  | LSHIFT Value Value
  | NOT Value
  | OR Value Value
  | RSHIFT Value Value
  deriving (Eq)

instance Show Gate where
  show (PROVIDE x) = show x
  show (x `AND` y) = show x <> " AND " <> show y
  show (x `LSHIFT` y) = show x <> " LSHIFT " <> show y
  show (NOT x) = "NOT " <> show x
  show (x `OR` y) = show x <> " OR " <> show y
  show (x `RSHIFT` y) = show x <> " RSHIFT " <> show y

gate :: Parser Gate
gate = try and <|> try lshift <|> try not <|> try or <|> try rshift <|> try provide
  where
    and = binop AND "AND"
    lshift = binop LSHIFT "LSHIFT"
    not = NOT <$> (symbol "NOT" *> value)
    or = binop OR "OR"
    rshift = binop RSHIFT "RSHIFT"
    provide = PROVIDE <$> value
    binop f op = f <$> (value <* symbol op) <*> value

shiftL' :: (Bits a, Integral a) => a -> a -> a
shiftL' x y = shiftL x (fromIntegral y)

shiftR' :: (Bits a, Integral a) => a -> a -> a
shiftR' x y = shiftR x (fromIntegral y)

type Wire = (String, Gate)

instance {-# OVERLAPPING #-} Show Wire where
  show (k, v) = show v <> " -> " <> k

wire :: Parser Wire
wire = flip (,) <$> (gate <* symbol "->") <*> some lower

data Value
  = Name String
  | Signal Word16
  deriving (Eq)

value :: Parser Value
value = name <|> signal
  where
    name = Name <$> token (highlight Identifier (some lower))
    signal = Signal . fromIntegral <$> natural

instance Show Value where
  show (Name c) = c
  show (Signal x) = show x

-- main :: IO ()
-- main = putStrLn "Not yet implemented"

main :: IO ()
main =
  do
    input <- parseInput circuit $(inputFilePath)
    putStr "Part One: "
    let a = partOne input
    print a
    putStr "Part Two: "
    print $ partTwo (Map.insert "b" (PROVIDE (Signal a)) input)

partOne :: Circuit -> Word16
partOne = (! "a") . evalCircuit

partTwo :: Circuit -> Word16
partTwo = (! "a") . evalCircuit
