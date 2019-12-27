{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.AOC19.Day12 where


import           Control.Arrow      ((&&&), (>>>))
import           Control.Lens       (makeLenses, view, (%~), (&), (^.), _1, _2,
                                     _3)
import           Data.Composition   ((.:))
import           Data.Function      (on)
import           System.Environment (getArgs)
import           Text.Trifecta      (Parser, angles, between, integer,
                                     parseFromFile, some, string)


type Dimensions = (Int, Int, Int)
type Position   = Dimensions
type Velocity   = Dimensions


-- instance {-# OVERLAPPING #-} Show Dimensions where
--   show (x, y, z) =
--     "<x=" ++ show x ++
--     ", y=" ++ show y ++
--     ", z=" ++ show z ++ ">"


data Moon = Moon
  { _position :: Position
  , _velocity :: Velocity }
  deriving (Eq)
makeLenses ''Moon


instance Show Moon where
  show (Moon pos vel) = "pos=" ++ show pos ++ ", vel=" ++ show vel


main :: IO ()
main =
    do putStr "Part One: "
       partOne =<< getInputFilename


partOne :: FilePath -> IO ()
partOne fname =
  do Just dims <- parseFromFile (some dimensions) fname
     let moons = flip Moon (0,0,0) <$> dims
     print (sum (fmap totalEnergy (((!! 1000) . iterate step) moons)))


dimensions :: Parser Dimensions
dimensions =
  angles $ (,,) <$>
  (fromIntegral <$> between (string "x=") (string ", ") integer) <*>
  (fromIntegral <$> between (string "y=") (string ", ") integer) <*>
  (fromIntegral <$>         (string "z=" *>             integer))


applyGravity :: Moon -> Moon -> Moon
applyGravity this that =
    this & velocity %~ ( (_1 %~ (+ dx)) . (_2 %~ (+ dy)) . (_3 %~ (+ dz)) )
  where
    dx = (velocityChange _1) (this, that)
    dy = (velocityChange _2) (this, that)
    dz = (velocityChange _3) (this, that)

    velocityChange dimension =
        uncurry (go `on` (view (position.dimension)))
      where
        go = \case { LT -> 1; EQ -> 0; GT -> -1 } .: compare


applyVelocity :: Moon -> Moon
applyVelocity moon =
  moon & position %~ ( (_1 %~ (+ (moon^.velocity._1))) .
                       (_2 %~ (+ (moon^.velocity._2))) .
                       (_3 %~ (+ (moon^.velocity._3))) )


step :: [Moon] -> [Moon]
step moons = [ applyVelocity (foldl applyGravity moon moons) | moon <- moons ]


potentialEnergy :: Moon -> Int
potentialEnergy (Moon (x, y, z) _) = abs x + abs y + abs z


kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (x, y, z)) = abs x + abs y + abs z


totalEnergy :: Moon -> Int
totalEnergy = potentialEnergy &&& kineticEnergy >>> uncurry (*)


getInputFilename :: IO FilePath
getInputFilename =
  getArgs >>= \case
  [fname] -> pure fname
  []      -> error "Must specify input filename"
  _       -> error "Too many args"
