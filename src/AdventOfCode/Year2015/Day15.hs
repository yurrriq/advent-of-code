{-# LANGUAGE DeriveTraversable #-}

module AdventOfCode.Year2015.Day15 where

import AdventOfCode.Input
import AdventOfCode.TH
import Control.Lens hiding (transform)
import Data.Monoid
import Text.Trifecta

data Ingredient a = Ingredient
  { _capacity :: a,
    _durability :: a,
    _flavor :: a,
    _texture :: a,
    _calories :: a
  }
  deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Ingredient

instance Applicative Ingredient where
  pure a = Ingredient a a a a a
  {-# INLINE pure #-}
  Ingredient a b c d e <*> Ingredient f g h i j = Ingredient (a f) (b g) (c h) (d i) (e j)
  {-# INLINE (<*>) #-}

instance (Semigroup a) => Semigroup (Ingredient a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Ingredient a) where
  mempty = pure mempty

main :: IO ()
main = $(defaultMain)

partOne :: [Ingredient Integer] -> Integer
partOne = bestCookie 100 id

partTwo :: [Ingredient Integer] -> Integer
partTwo = bestCookie 100 $ \x ->
  if 500 == _calories x
    then x
    else pure 0

bestCookie :: Int -> (Ingredient Integer -> Ingredient Integer) -> [Ingredient Integer] -> Integer
bestCookie tsp transform ingredients =
  maximum $
    score transform
      . zipWith (<>) (map (fmap Product) ingredients)
      . map (pure . Product . fromIntegral)
      <$> splits tsp (length ingredients)

score :: (Ingredient Integer -> Ingredient Integer) -> [Ingredient (Product Integer)] -> Integer
score transform =
  getProduct
    . product
    . (calories .~ mempty)
    . fmap Product
    . transform
    . fmap (max 0 . getSum)
    . mconcat
    . map (fmap (Sum . getProduct))

getInput :: IO [Ingredient Integer]
getInput = parseInput (some ingredient) $(inputFilePath)

ingredient :: Parser (Ingredient Integer)
ingredient =
  do
    _ <- some letter <* char ':' <* space
    [a, b, c, d, e] <- commaSep (token (some letter) *> integer)
    pure $ Ingredient a b c d e

splits :: (Integral a) => a -> a -> [[a]]
splits _ 0 = []
splits n 1 = [[n]]
splits n k =
  [ i : is
    | i <- [0 .. n],
      is <- splits (n - i) (k - 1)
  ]
