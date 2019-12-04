module Data.AOC19.Day03 where

import           Control.Applicative ((<|>))
import           Control.Arrow       (second, (&&&))
import           Control.Category    ((>>>))
import           Data.Foldable       (minimumBy)
import           Data.Function       (on)
import           Data.Hashable       (Hashable (..))
import qualified Data.HashSet        as HS
import           Data.Ix             (Ix (..))
import           GHC.Arr             (unsafeIndex, unsafeRangeSize)
import           Text.Trifecta


-- ------------------------------------------------------------------- [ Types ]

data Segment
  = Segment Direction Int
  deriving (Eq, Show)


data Direction
  = D
  | L
  | R
  | U
  deriving (Eq, Show)


data Point = Point
  { _x :: Int
  , _y :: Int
  }
  deriving (Eq, Ord)


instance Hashable Point where
    hashWithSalt salt (Point x y) = hashWithSalt salt (x, y)


instance Ix Point where
    {-# SPECIALISE instance Ix Point #-}

    range (Point x0 y0, Point x1 y1) =
      [ Point x y | x <- range (x0, x1)
                  , y <- range (y0, y1) ]
    {-# INLINE range #-}

    unsafeIndex (Point x0 y0, Point x1 y1) (Point x y) =
      unsafeIndex (x0, x1) x * unsafeRangeSize (y0, y1) + unsafeIndex (y0, y1) y
    {-# INLINE unsafeIndex #-}

    inRange (Point x0 y0, Point x1 y1) (Point x y) =
      inRange (x0, x1) x && inRange (y0, y1) y
    {-# INLINE inRange #-}


instance Show Point where
    show (Point x y) = concat [ "(", show x, ",", show y, ")" ]

-- ----------------------------------------------------------------- [ Parsers ]

point :: Parser Point
point = Point <$> nonnegInt <*> (comma *> nonnegInt)


wires :: Parser ([Segment], [Segment])
wires = (,) <$> segments <*> segments


segments :: Parser [Segment]
segments = segment `sepBy` comma


segment :: Parser Segment
segment = Segment <$> direction <*> nonnegInt


direction :: Parser Direction
direction =
    (pure D <* char 'D') <|>
    (pure L <* char 'L') <|>
    (pure R <* char 'R') <|>
    (pure U <* char 'U')


nonnegInt :: Parser Int
nonnegInt = fromIntegral <$> natural


-- ----------------------------------------------------------------- [ Helpers ]

manhattanDistance :: Point -> Point -> Int
manhattanDistance = curry $ (distanceOn _x &&& distanceOn _y) >>> uncurry (+)
  where
    distanceOn :: Num a => (b -> a) -> (b, b) -> a
    distanceOn f = abs . uncurry (subtract `on` f)


findCrossings :: [Segment] -> [Segment] -> HS.HashSet Point
findCrossings =
    (HS.delete (Point 0 0) .) .
    HS.intersection `on` (snd . runSegments)


runSegments :: [Segment] -> (Point, HS.HashSet Point)
runSegments = foldl go (Point 0 0, HS.empty)
  where
    go (start, pointses) seg =
        second (HS.union pointses) (runSegment (start, seg))


runSegment :: (Point, Segment) -> (Point, HS.HashSet Point)
runSegment (from@(Point x y), Segment D distance) =
    let to = Point x (y - distance) in (to, HS.fromList (range (to, from)))
runSegment (from@(Point x y), Segment L distance) =
    let to = Point (x - distance) y in (to, HS.fromList (range (to, from)))
runSegment (from@(Point x y), Segment R distance) =
    let to = Point (x + distance) y in (to, HS.fromList (range (from, to)))
runSegment (from@(Point x y), Segment U distance) =
    let to = Point x (y + distance) in (to, HS.fromList (range (from, to)))


-- ---------------------------------------------------------------- [ Examples ]

exampleOne :: Result Int
exampleOne =
    runExample
    "R8,U5,L5,D3\
   \ U7,R6,D4,L4"


exampleTwo :: Result Int
exampleTwo =
    runExample
    "R75,D30,R83,U83,L12,D49,R71,U7,L72\
   \ U62,R66,U55,R34,D71,R55,D58,R83"


exampleThree :: Result Int
exampleThree =
    runExample
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\
   \ U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"


runExample :: String -> Result Int
runExample str =
    do crossings <- uncurry findCrossings <$> parseString wires mempty str
       pure . distance $ minimumBy (compare `on` distance) crossings
  where
    distance = manhattanDistance (Point 0 0)


-- ------------------------------------------------------------------- [ Parts ]

partOne :: IO Int
partOne =
    do res <- parseFromFile wires "../../../input/day03.txt"
       let crossings = maybe (error "Fail") (uncurry findCrossings) res
       pure . distance $ minimumBy (compare `on` distance) crossings
  where
    distance = manhattanDistance (Point 0 0)
