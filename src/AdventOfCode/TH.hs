module AdventOfCode.TH where

import Data.List.Split (splitOn)
import Language.Haskell.TH

inputFilePath :: Q Exp
inputFilePath =
  stringE
    =<< do
      modName <- loc_module <$> location
      case splitOn "." modName of
        ["Test", "AdventOfCode", 'Y' : 'e' : 'a' : 'r' : year, 'D' : 'a' : 'y' : day] ->
          pure $ "input/" <> year <> "/day" <> day <> ".txt"
        ["AdventOfCode", 'Y' : 'e' : 'a' : 'r' : year, 'D' : 'a' : 'y' : day] ->
          pure $ "input/" <> year <> "/day" <> day <> ".txt"
        _ -> fail "Oops!"
