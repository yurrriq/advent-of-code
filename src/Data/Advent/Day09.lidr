---
pandoc-minted:
language: idris
---

= Explosives in Cyberspace

[Link](https://adventofcode.com/2016/day/9)

Wandering around a secure area, you come across a datalink port to a new part of
the network. After briefly scanning it for interesting files, you find one file
in particular that catches your attention. It's compressed with an experimental
format, but fortunately, the documentation for the format is nearby.

The format compresses a sequence of characters. Whitespace is ignored. To
indicate that some sequence should be repeated, a marker is added to the file,
like `(10x2)`. To decompress this marker, take the subsequent `10` characters
and repeat them `2` times. Then, continue reading the file *after* the repeated
data. The marker itself is not included in the decompressed output.

If parentheses or other characters appear within the data referenced by a
marker, that's okay - treat it like normal data, not a marker, and then resume
looking for markers after the decompressed section.

For example:

- `ADVENT` contains no markers and decompresses to itself with no changes,
  resulting in a decompressed length of `6`.
- `A(1x5)BC` repeats only the `B` a total of `5` times, becoming `ABBBBBC` for a
  decompressed length of `7`.
- `(3x3)XYZ1 becomes `XYZXYZXYZ` for a decompressed length of `9`.
- `A(2x2)BCD(2x2)EFG` doubles the `BC` and `EF`, becoming `ABCBCDEFEFG` for a
  decompressed length of `11`.
- `(6x1)(1x3)A` simply becomes `(1x3)A` - the `(1x3)` looks like a marker, but
  because it's within a data section of another marker, it is not treated any
  differently from the A that comes after it. It has a decompressed length of
  `6`.
- `X(8x2)(3x3)ABCY` becomes `X(3x3)ABC(3x3)ABCY` (for a decompressed length of
  `18`), because the decompressed data from the `(8x2)` marker (the `(3x3)ABC`)
  is skipped and not processed further.


== Module Declaration and Imports

> ||| Day 9: Explosives in Cyberspace
> module Data.Advent.Day09

Import the [generic `Day`
structure](https://github.com/yurrriq/advent-of-idris/blob/master/src/Data/Advent/Day.idr),
inspired by [Steve Purcell Haskell
solution](https://github.com/purcell/adventofcode2016).

> import public Data.Advent.Day


== Data Types

Ensure both the type and data constructors are
[exported](http://docs.idris-lang.org/en/latest/tutorial/modules.html#meaning-for-data-types)
for all the following data types.

> %access public export

TODO: describe this

> data Block = Seq Nat String (List Block)
>            | Str String


== Parsers

> %access private
>
> notSpace : Parser Char
> notSpace = satisfy (not . isSpace)

Ensure the type constructors of the following parsers are
[exported](http://docs.idris-lang.org/en/latest/tutorial/modules.html#meaning-for-data-types).

> %access export
>
> partial block : Parser Block
> block = go <|>| Str . pack <$> some letter <?>
>         "a repeated sequence or plain string"
>   where
>     go = do len  <- char '(' *> integer <* char 'x'
>             reps <- fromIntegerNat <$> integer <* char ')'
>             rest <- pack <$> ntimes len notSpace
>             case parse (some block <* eof) rest of
>                  Right subBlocks => pure $ Seq reps rest subBlocks
>                  Left err => fail err


== Part One

\begin{quote}
  What is the \textit{decompressed length} of the file (your puzzle input)?
  Don't count whitespace.
\end{quote}

> partOne : Block -> Nat
> partOne (Seq n str _) = n * length str
> partOne (Str str)     = length str
>
> %default partial
>
> namespace PartOne
>
>     ex : String -> Either String Nat
>     ex str = sum . map partOne <$> parse (some block <* newline) str
>
>     ex1 : Either String Nat
>     ex1 = ex "ADVENT"
>
>     ex2 : Either String Nat
>     ex2 = ex "A(1x5)BC"
>
>     ex3 : Either String Nat
>     ex3 = ex "(3x3)XYZ"
>
>     ex4 : Either String Nat
>     ex4 = ex "A(2x2)BCD(2x2)EFG"
>
>     ex5 : Either String Nat
>     ex5 = ex "(6x1)(1x3)A"
>
>     ex6 : Either String Nat
>     ex6 = ex "X(8x2)(3x3)ABCY"


== Main

> namespace Main
>
>     main : IO ()
>     main = runDay $ MkDay 9 (some block <* newline)
>            (pure . show . sum . map partOne)
>            (pure . const "Not yet implemented!")
