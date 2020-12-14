module AdventOfCode.Year2020.Day14 where

import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad.State
import Data.Bits
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)
import Text.Trifecta

data ProgState
  = ProgState
      { _mask :: Int -> Int,
        _memory :: IntMap Int
      }

makeLenses ''ProgState

data Instruction
  = SetMask (Int -> Int)
  | SetValue Int Int

instance Show Instruction where
  show (SetMask f) = "mask = ..."
  show (SetValue addr val) = "mem[" <> show addr <> "] = " <> show val

instruction :: Parser Instruction
instruction = setMask <|> setValue

setMask :: Parser Instruction
setMask =
  do
    symbol "mask"
    symbol "="
    maskSpec <- some (oneOf "X01") <* whiteSpace
    pure . SetMask $ foldr (.) id (catMaybes (zipWith go maskSpec (iterate pred 35)))
  where
    go '0' = pure . (flip clearBit)
    go '1' = pure . (flip setBit)
    go _ = const Nothing

setValue :: Parser Instruction
setValue =
  do
    symbol "mem"
    address <- between (symbol "[") (symbol "]") (fromInteger <$> natural)
    symbol "="
    value <- fromInteger <$> natural
    pure $ SetValue address value

getInput :: IO [Instruction]
getInput =
  do
    Just res <- parseFromFile (some instruction) "../../../input/2020/day14.txt"
    pure res

partOne :: [Instruction] -> Int
partOne instructions =
  execState (mapM_ runInstruction instructions) initialState ^. memory & sum

initialState :: ProgState
initialState = ProgState id IM.empty

execProgram :: [Instruction] -> Int
execProgram instructions =
  execState (mapM_ runInstruction instructions) initialState ^. memory & sum

runInstruction :: Instruction -> State ProgState ()
runInstruction (SetMask newMask) = mask .= newMask
runInstruction (SetValue addr val) =
  do
    applyMask <- view mask <$> get
    memory %= IM.insert addr (applyMask val)
