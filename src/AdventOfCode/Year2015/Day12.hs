{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Year2015.Day12 (main) where

import AdventOfCode.TH (defaultMain, inputFilePath)
import Control.Monad.Reader (Reader, asks, foldM, runReader)
import Data.Aeson (Array, Object, Value (..), decodeFileStrict')
import Data.Bool (bool)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import Data.Scientific (Scientific (coefficient))

main :: IO ()
main = $(defaultMain)

getInput :: IO Object
getInput = fromJust <$> decodeFileStrict' $(inputFilePath)

partOne :: Object -> Integer
partOne = sumFilter (const True)

partTwo :: Object -> Integer
partTwo = sumFilter ((String "red" `notElem`) . HM.elems)

sumFilter :: (Object -> Bool) -> Object -> Integer
sumFilter p = coefficient . flip runReader p . sumObject

sumObject :: Object -> Reader (Object -> Bool) Scientific
sumObject = foldM accumValue 0

accumValue :: Scientific -> Value -> Reader (Object -> Bool) Scientific
accumValue acc = fmap (acc +) . sumValue

sumValue :: Value -> Reader (Object -> Bool) Scientific
sumValue (Object obj) = asks ($ obj) >>= bool (pure 0) (sumObject obj)
sumValue (Array arr) = sumArray arr
sumValue (Number n) = pure n
sumValue _ = pure 0

sumArray :: Array -> Reader (Object -> Bool) Scientific
sumArray = foldM accumValue 0