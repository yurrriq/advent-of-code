{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.TH where

import Control.Monad.Logger qualified as Logger
import Data.List.Split (splitOn)
import Language.Haskell.TH
import Relude

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
        _parts -> fail "Oops!"

defaultMain :: Q Exp
defaultMain =
  doE
    [ bindS (varP (mkName "input")) (varE (mkName "getInput")),
      noBindS [|$(doPartOne)|],
      noBindS [|$(doPartTwo)|]
    ]

defaultMainIO :: Q Exp
defaultMainIO =
  [|
    do
      input <- getInput
      putStr "Part One: "
      print =<< partOne input
      putStr "Part Two: "
      print =<< partTwo input
    |]

defaultMainMaybe :: Q Exp
defaultMainMaybe =
  doE
    [ bindS (varP (mkName "input")) (varE (mkName "getInput")),
      noBindS [|putStr "Part One: "|],
      noBindS [|maybe (print "failed!") print (partOne input)|],
      noBindS [|putStr "Part Two: "|],
      noBindS [|maybe (print "failed!") print (partTwo input)|]
    ]

doPartOne :: Q Exp
doPartOne = [|putStr "Part One: " *> print (partOne input)|]

doPartTwo :: Q Exp
doPartTwo = [|putStr "Part Two: " *> print (partTwo input)|]

evalPuzzle :: Q Exp
evalPuzzle =
  [|
    do
      input <- getInput
      $(varE 'Logger.runStderrLoggingT)
        $ evaluatingStateT emptyPuzzleState
        $ usingReaderT input
        $ runPuzzle
        $ do
          putStr "Part One: "
          print =<< partOne
          putStr "Part Two: "
          print =<< partTwo
    |]
