module Day04 (
  partOne,
  partTwo
  ) where


import           Control.Applicative ((<|>))
import           Control.Arrow       (first)
import           Data.ByteString     (ByteString)
import           Data.Hashable       (Hashable (..))
import qualified Data.HashMap.Strict as HM
import           Data.List           (maximumBy, sort)
import           Data.Ord            (comparing)
import           Text.Trifecta       (Parser, brackets, char, colon, many,
                                      natural, symbol, (<?>))
import           Util                (frequencies, maybeParseByteString)


-- ------------------------------------------------------------------- [ Types ]

data Time = Time
  { _tYear   :: Integer
  , _tMonth  :: Integer
  , _tDay    :: Integer
  , _tHour   :: Integer
  , _tMinute :: Integer
  }
  deriving (Eq, Ord, Show)


time :: Parser Time
time = brackets (Time <$>
                 ((natural <* char '-') <?> "year") <*>
                 ((natural <* char '-') <?> "month") <*>
                 (natural <?> "day") <*>
                 ((natural <* colon) <?> "hour") <*>
                 (natural <?> "minute"))
       <?> "time"


newtype Guard = Guard { unGuard :: Integer }
  deriving (Eq, Ord, Show)


instance Hashable Guard where
    hashWithSalt salt = hashWithSalt salt . unGuard


data Event = Shift Guard
           | Asleep
           | Awake
  deriving (Eq, Ord, Show)


shift :: Parser Event
shift = Shift . Guard <$> (symbol "Guard" *> char '#' *> natural <*
                           symbol "begins shift")


event :: Parser Event
event = shift <|>
        (Asleep <$ symbol "falls asleep") <|>
        (Awake <$ symbol "wakes up")


type Entry = (Time, Event)


entry :: Parser Entry
entry = (,) <$> time <*> event


type TimeCards = HM.HashMap Guard [[Entry]]


-- ----------------------------------------------------------------- [ Helpers ]

isShiftChange :: Entry -> Bool
isShiftChange (_, Shift _) = True
isShiftChange (_, _)       = False


napTimes :: [Entry] -> [Integer]
napTimes ((from,Asleep):(to,Awake):rest) =
    [_tMinute from .. _tMinute to - 1] ++ napTimes rest
napTimes _ = []


napsByGuard :: [Entry] -> HM.HashMap Guard [[Integer]]
napsByGuard = HM.map (map napTimes) . shifts


shifts :: [Entry] -> TimeCards
shifts = go HM.empty . sort
  where
    go :: TimeCards -> [Entry] -> TimeCards
    go acc ((_when, Shift who):entries) =
        let (events, rest) = break isShiftChange entries in
          go (HM.insertWith (++) who [events] acc) rest
    go acc _ = acc


sleepiestGuard :: [Entry] -> Maybe (Int, (Guard, [[Integer]]))
sleepiestGuard = HM.foldrWithKey go Nothing . napsByGuard
  where
    go :: Guard -> [[Integer]] -> Maybe (Int, (Guard, [[Integer]])) ->
          Maybe (Int, (Guard, [[Integer]]))
    go a xxs Nothing = Just (sum (length <$> xxs), (a, xxs))
    go a xxs old@(Just (m,(_,_))) =
        let n = sum (length <$> xxs) in
          if n > m then Just (n, (a,xxs)) else old


sleepiestMinute :: [[Integer]] -> (Integer, Integer)
sleepiestMinute = maximumBy (comparing snd) .
                  HM.toList . frequencies . concat


mostConsistentSleeper :: [Entry] -> Maybe ((Guard, Integer), Integer)
mostConsistentSleeper = HM.foldrWithKey go Nothing .
                        HM.map (filter (not . null)) .
                        napsByGuard
  where
    go :: Guard -> [[Integer]] -> Maybe ((Guard, Integer), Integer) ->
          Maybe ((Guard, Integer), Integer)
    go _ [] old           = old
    go guard naps Nothing = Just $ first ((,) guard) (sleepiestMinute naps)
    go guard naps old@(Just ((_, _), oldTimes)) =
      let (minute, times) = sleepiestMinute naps in
          if times > oldTimes then
            Just ((guard, minute), times)
          else
            old


-- ------------------------------------------------------------------- [ Parts ]

partOne :: ByteString -> Maybe Integer
partOne bstr =
    case sleepiestGuard =<< maybeParseByteString (many entry) bstr of
      Just (_, (Guard gid, naps)) -> Just (gid * (fst (sleepiestMinute naps)))
      _                           -> Nothing


partTwo :: ByteString -> Maybe Integer
partTwo bstr =
    case mostConsistentSleeper =<< maybeParseByteString (many entry) bstr of
      Just ((Guard gid, minute), _) -> Just (gid * minute)
      _                             -> Nothing
