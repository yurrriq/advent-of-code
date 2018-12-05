module Day01 (
  partOne,
  partTwo
  ) where


import           Data.ByteString (ByteString)
import qualified Data.HashSet    as HS
import           Text.Trifecta   (Parser, Result (..), integer, many,
                                  parseByteString)


frequencyChanges :: Parser [Integer]
frequencyChanges = many integer


partOne :: ByteString -> Maybe Integer
partOne input =
    case parseByteString frequencyChanges mempty input of
         Failure _errDoc -> Nothing
         Success changes -> Just (sum changes)


partTwo :: ByteString -> Maybe Integer
partTwo input =
    case parseByteString frequencyChanges mempty input of
      Failure _errDoc -> Nothing
      Success changes -> findFirstDup HS.empty $ scanl (+) 0 (cycle changes)


findFirstDup :: HS.HashSet Integer -> [Integer] -> Maybe Integer
findFirstDup _ []           = Nothing
findFirstDup seen (x:xs)    = if HS.member x seen then
                                Just x
                              else
                                findFirstDup (HS.insert x seen) xs
