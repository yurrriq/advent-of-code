= Day 5

Thanks to [Justin Le][JL] for teaching me some [neat group theory tricks][blog]!


[JL]: https://github.com/mstksg
[blog]: https://blog.jle.im/entry/alchemical-groups.html


> {-# LANGUAGE DataKinds #-}

> module Day05 (
>   partOne,
>   partTwo
>   ) where


== Imports

> import           Data.Algebra.Free (foldMapFree, returnFree)
> import           Data.Char         (isLower, ord, toLower)
> import           Data.Function     (on)
> import           Data.Finite       (Finite, finites, packFinite)
> import           Data.Foldable     (foldMap)
> import           Data.Group        (invert)
> import qualified Data.Group.Free   as FG


== Types

<blockquote cite="https://adventofcode.com/2018/day/5">
Units' types are represented by letters...
</blockquote>

> type Unit = Finite 26

> fromChar :: Char -> Maybe (Either Unit Unit)
> fromChar c | isLower c = Left  <$> unit
>            | otherwise = Right <$> unit
>   where
>     unit = packFinite . fromIntegral $ ((-) `on` ord) (toLower c) 'a'


== Helpers

As per [the documentation][returnFree], `returnFree` is an [injective][] map
 that embeds generators into a [free algebra][] ([`FreeAlgebra`][]).

> inject :: Char -> FG.FreeGroupL Unit
> inject = foldMap (either returnFree (invert . returnFree)) . fromChar


> clean :: Unit -> FG.FreeGroupL Unit -> FG.FreeGroupL Unit
> clean c = foldMapFree go
>   where
>     go :: Unit -> FG.FreeGroupL Unit
>     go d | d == c    = mempty
>          | otherwise = returnFree d


> order :: FG.FreeGroupL a -> Int
> order = length . FG.toList


[returnFree]: https://hackage.haskell.org/package/free-algebras-0.6.0.0/docs/Data-Algebra-Free.html#v:returnFree
[injective]: https://en.wikipedia.org/wiki/Injective_function
[free algebra]: https://en.wikipedia.org/wiki/Free_algebra
[`FreeAlgebra`]: https://hackage.haskell.org/package/free-algebras-0.6.0.0/docs/Data-Algebra-Free.html#t:FreeAlgebra


== Parts

=== Part One

> partOne :: [Char] -> Int
> partOne = order . foldMap inject


=== Part Two

> partTwo :: [Char] -> Int
> partTwo = minimum . cleanedPolymers . foldMap inject
>   where
>     cleanedPolymers :: FG.FreeGroupL Unit -> [Int]
>     cleanedPolymers polymer = (order . flip clean polymer) <$> finites
