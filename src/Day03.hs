module Day03 (
  partOne,
  partTwo
  ) where


import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM
import           Text.Trifecta       (Parser, Result (..), comma, digit, many,
                                      natural, parseByteString, some, space,
                                      symbol)
import           Util                (frequencies)


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

squaresCovered :: Claim -> [Point]
squaresCovered (Claim _ (Point x0 y0) (Size w h)) =
    [ Point x1 y1 | x1 <- [x0..x0+w-1], y1 <- [y0..y0+h-1] ]


-- ------------------------------------------------------------------- [ Parts ]

partOne :: ByteString -> Maybe Int
partOne input =
    case parseByteString (many claim) mempty input of
      Failure _errDoc -> Nothing
      Success claims  -> Just . length .
                         filter (>= 2) . HM.elems .
                         frequencies . concatMap squaresCovered $
                         claims


partTwo :: ByteString -> Maybe ()
partTwo = undefined
