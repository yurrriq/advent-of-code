module Day01 (
  partOne,
  partTwo
  ) where


import           Data.ByteString (ByteString)
import           Text.Trifecta   (Parser, Result (..), integer, many,
                                  parseByteString)


frequencyChanges :: Parser [Integer]
frequencyChanges = many integer


partOne :: ByteString -> Maybe Integer
partOne input =
    case parseByteString frequencyChanges mempty input of
         Failure _errDoc -> Nothing
         Success changes -> Just (sum changes)


partTwo :: String -> String
partTwo = undefined
