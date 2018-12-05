module Day02 (
  partOne,
  partTwo
  ) where


import           Control.Arrow       ((***))
import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.List           (find, tails)
import           Data.Maybe          (catMaybes)
import           Text.Trifecta       (Parser, Result (..), letter, newline,
                                      parseByteString, sepEndBy, some)
import           Util                (Frequencies, frequencies)


type BoxID = String

type Checksum = Integer


partOne :: ByteString -> Maybe Checksum
partOne input =
    case parseByteString boxIDs mempty input of
      Failure _errDoc -> Nothing
      Success ids     -> Just (checksum ids)


partTwo :: ByteString -> Maybe String
partTwo input =
    case parseByteString boxIDs mempty input of
      Failure _errDoc -> Nothing
      Success ids     -> case catMaybes (correctBoxIDs <$> tails ids) of
                           [(a,b)] -> Just (commonLetters a b)
                           _       -> Nothing


boxID :: Parser BoxID
boxID = some letter


boxIDs :: Parser [BoxID]
boxIDs = boxID `sepEndBy` newline


exactlyTwo :: (Eq a, Hashable a) => Frequencies a -> Frequencies a
exactlyTwo = HM.filter (==2)


exactlyThree :: (Eq a, Hashable a) => Frequencies a -> Frequencies a
exactlyThree = HM.filter (==3)


checksum :: [BoxID] -> Checksum
checksum = uncurry (*) . foldr go (0, 0)
  where
    go :: BoxID -> (Integer, Integer) -> (Integer, Integer)
    go bid =
        let freqs = frequencies bid
            f = if not (HM.null (exactlyTwo freqs)) then (+1) else id
            g = if not (HM.null (exactlyThree freqs)) then (+1) else id
        in f *** g


hammingDistance :: Eq a => [a] -> [a] -> Maybe Integer
hammingDistance (x:xs) (y:ys) | x /= y    = (+1) <$> recur
                              | otherwise = recur
  where recur = hammingDistance xs ys
hammingDistance [] []                     = Just 0
hammingDistance _ _                       = Nothing


correctBoxIDs :: [BoxID] -> Maybe (BoxID, BoxID)
correctBoxIDs (x:xs@(_:_)) = do y <- find ((== Just 1) . hammingDistance x) xs
                                pure (x,y)
correctBoxIDs [_]          = Nothing
correctBoxIDs []           = Nothing


commonLetters :: String -> String -> String
commonLetters (x:xs) (y:ys) | x == y    = x:recur
                            | otherwise = recur
  where recur = commonLetters xs ys
commonLetters _ _ = "" -- NOTE: This is bad.
