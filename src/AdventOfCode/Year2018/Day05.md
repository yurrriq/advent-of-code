# Day 5

Thanks to [Justin Le](https://github.com/mstksg) for teaching me some
[neat group theory
tricks](https://blog.jle.im/entry/alchemical-groups.html)\!

``` sourceCode literate haskell
{-# LANGUAGE DataKinds #-}
```

``` sourceCode literate haskell
module Day05 (
  partOne,
  partTwo
  ) where
```

## Imports

``` sourceCode literate haskell
import           Data.Algebra.Free (foldMapFree, returnFree)
import           Data.Char         (isLower, ord, toLower)
import           Data.Function     (on)
import           Data.Finite       (Finite, finites, packFinite)
import           Data.Foldable     (foldMap)
import           Data.Group        (invert)
import qualified Data.Group.Free2  as FG
```

## Types

<blockquote cite="https://adventofcode.com/2018/day/5">

Units’ types are represented by letters…

</blockquote>

``` sourceCode literate haskell
type Unit = Finite 26
```

``` sourceCode literate haskell
fromChar :: Char -> Maybe (Either Unit Unit)
fromChar c | isLower c = Left  <$> unit
           | otherwise = Right <$> unit
  where
    unit = packFinite . fromIntegral $ ((-) `on` ord) (toLower c) 'a'
```

## Helpers

As per [the
documentation](https://hackage.haskell.org/package/free-algebras-0.6.0.0/docs/Data-Algebra-Free.html#v:returnFree),
`returnFree` is an
[injective](https://en.wikipedia.org/wiki/Injective_function) map that
embeds generators into a [free
algebra](https://en.wikipedia.org/wiki/Free_algebra)
([`FreeAlgebra`](https://hackage.haskell.org/package/free-algebras-0.6.0.0/docs/Data-Algebra-Free.html#t:FreeAlgebra)).

``` sourceCode literate haskell
inject :: Char -> FG.FreeGroupL Unit
inject = foldMap (either returnFree (invert . returnFree)) . fromChar
```

``` sourceCode literate haskell
clean :: Unit -> FG.FreeGroupL Unit -> FG.FreeGroupL Unit
clean c = foldMapFree go
  where
    go :: Unit -> FG.FreeGroupL Unit
    go d | d == c    = mempty
         | otherwise = returnFree d
```

``` sourceCode literate haskell
order :: FG.FreeGroupL a -> Int
order = length . FG.toList
```

## Parts

### Part One

``` sourceCode literate haskell
partOne :: [Char] -> Int
partOne = order . foldMap inject
```

### Part Two

``` sourceCode literate haskell
partTwo :: [Char] -> Int
partTwo = minimum . cleanedPolymers . foldMap inject
  where
    cleanedPolymers :: FG.FreeGroupL Unit -> [Int]
    cleanedPolymers polymer = (order . flip clean polymer) <$> finites
```
