module Day03 (
  partOne,
  partTwo
  ) where


import           Data.ByteString (ByteString)
import           Data.Hashable   (Hashable (..))
import qualified Data.HashSet    as HS
import           Text.Trifecta   (Parser, Result (..), comma, digit, many,
                                  natural, parseByteString, some, space, symbol)


-- ------------------------------------------------------------------  [ Types ]

data Point = Point
  { _left :: Integer
  , _top  :: Integer
  }
  deriving (Eq)


instance Hashable Point where
    hashWithSalt salt (Point l t) = hashWithSalt salt (l, t)


instance Show Point where
    showsPrec _ (Point left top) =
        showString $ "(" <> show left <> ", " <> show top <> ")"


data Size = Size
  { _width  :: Integer
  , _height :: Integer
  }
  deriving (Eq)


instance Show Size where
    showsPrec _ (Size w h) = showString (show w <> "x" <> show h)


type ClaimID = String


data Claim = Claim
  { _id     :: ClaimID
  , _origin :: Point
  , _size   :: Size
  }
  deriving (Eq, Show)


-- ----------------------------------------------------------------- [ Parsers ]

claim :: Parser Claim
claim = Claim <$>
        (symbol "#" *> some digit <* space) <*>
        (symbol "@" *> point) <*>
        (symbol ":" *> size)


point :: Parser Point
point = Point <$> natural <*> (comma *> natural)


size :: Parser Size
size = Size <$> natural <*> (symbol "x" *> natural)


-- ----------------------------------------------------------------- [ Helpers ]

squaresCovered :: Claim -> HS.HashSet Point
squaresCovered (Claim _ (Point x0 y0) (Size w h)) =
    HS.fromList [ Point x1 y1 | x1 <- [x0..x0+w-1], y1 <- [y0..y0+h-1] ]


intersections :: (Eq a, Hashable a) => [HS.HashSet a] -> HS.HashSet a
intersections (x:xs) = HS.union (foldMap (HS.intersection x) xs) recur
  where recur = intersections xs
intersections []     = HS.empty


-- ------------------------------------------------------------------- [ Parts ]

partOne :: ByteString -> Maybe Int
partOne input =
    case parseByteString (many claim) mempty input of
      Failure _errDoc -> Nothing
      Success claims  ->
        Just (length (intersections (squaresCovered <$> claims)))


partTwo :: ByteString -> Maybe ()
partTwo = undefined
