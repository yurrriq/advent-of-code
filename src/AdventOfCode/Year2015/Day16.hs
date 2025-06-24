module AdventOfCode.Year2015.Day16 where

import AdventOfCode.Input
import AdventOfCode.TH
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.Maybe (fromJust, listToMaybe)
import Text.Trifecta (Parser, commaSep, integer, letter, some, symbol)

type AuntsSue = IntMap AuntSue

type AuntSue = HashMap String Int

main :: IO ()
main = $(defaultMain)

partOne :: AuntsSue -> Int
partOne = fromJust . fingerAuntSue (\sue -> (==) . (sue HM.!))

partTwo :: AuntsSue -> Int
partTwo = fromJust . fingerAuntSue finger
  where
    finger sue k
      | k `elem` ["cats", "trees"] = (sue HM.! k >)
      | k `elem` ["pomeranians", "goldfish"] = (sue HM.! k <)
      | otherwise = (sue HM.! k ==)

getInput :: IO AuntsSue
getInput = parseInput auntsSue $(inputFilePath)

fingerAuntSue :: (AuntSue -> String -> Int -> Bool) -> AuntsSue -> Maybe Int
fingerAuntSue finger = fmap fst . find fingerSue
  where
    fingerSue = flip all tickerTape . uncurry . finger'
    finger' sue k v = not (HM.member k sue) || finger sue k v

tickerTape :: [(String, Int)]
tickerTape =
  [ ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)
  ]

auntsSue :: Parser AuntsSue
auntsSue = IM.fromList <$> some auntSue

auntSue :: Parser (Int, AuntSue)
auntSue =
  (,)
    <$> (symbol "Sue" *> int <* symbol ":")
    <*> (HM.fromList <$> commaSep keyValue)

keyValue :: Parser (String, Int)
keyValue = (,) <$> (some letter <* symbol ":") <*> int

int :: Parser Int
int = fromInteger <$> integer

find :: (a -> Bool) -> IntMap a -> Maybe (IM.Key, a)
find p = listToMaybe . IM.toList . IM.filter p
