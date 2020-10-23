-- --------------------------------------------------------------- [ Day04.idr ]
-- Module      : Data.Advent.Day04
-- Description : My solution to the Day 4 puzzle of the 2016 Advent of Code.
-- Copyright   : Copyright (c) 2016, Eric Bailey
-- License     : MIT
-- Link        : http://adventofcode.com/2016/day/4
-- --------------------------------------------------------------------- [ EOH ]
||| Day 4: Security Through Obscurity
module Data.Advent.Day04

import public Data.SortedMap

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings
import public Lightyear.StringFile

%access export

-- NOTE: This is not awesome, but the maps here will be small...
private
inc : k -> SortedMap k Nat -> SortedMap k Nat
inc k m =
    case lookup k m of
         Nothing => insert k 1 m
         Just v  => insert k (S v) $ delete k m

-- ----------------------------------------------------------------- [ Parsers ]

encryptedName : Parser (List Char)
encryptedName = concat <$> some (some letter <* token "-")

sectorId : Parser Integer
sectorId = integer

checksum : Parser String
checksum = quoted' '[' ']'

room : Parser (List Char, Integer, String)
room = liftA2 MkPair encryptedName $ liftA2 MkPair sectorId checksum

-- ------------------------------------------------------------------- [ Logic ]

implementation [day04] Ord (Char, Nat) where
    compare (a,x) (b,y) = let o = compare y x in
                              if EQ /= o
                                 then o
                                 else compare a b

computeChecksum : List Char -> String
computeChecksum = pack . map Basics.fst .
                  take 5 . sort @{day04} . toList .
                  foldr inc empty

isReal : (List Char, Integer, String) -> Bool
isReal (cs, _, cksum) = cksum == computeChecksum cs

main' : Show a => Parser (List (List Char, Integer, String)) ->
        (List (List Char, Integer, String) -> a) -> IO ()
main' p f =
    either putStrLn (printLn . f)
           !(run $ parseFile (const show) (const id) p "input/day04.txt")

-- ---------------------------------------------------------------- [ Part One ]

namespace PartOne

    main : IO ()
    main = main' (some room) $ foldr go 0
      where
        go r@(_,sid,_) sum = if isReal r then sum + sid else sum

-- ---------------------------------------------------------------- [ Part Two ]

namespace PartTwo

    roomToMessage : (List Char, Integer, String) -> String
    roomToMessage (cs, sid, _) =
        let az = cycle ['a' .. 'z']
            n = toNat sid `mod` 26 in
            pack $ map (\c => index n (drop (toNat c `minus` 97) az)) cs

    main : IO ()
    main = main' (filter isReal <$> some room) $ \rs =>
           fromMaybe (-404) $ (\(_,sid,_) => sid) <$>
           find (("northpoleobjectstorage" ==) . roomToMessage) rs

-- -------------------------------------------------------------------- [ Main ]

namespace Main

    main : IO ()
    main = putStr "Part One: " *> PartOne.main *>
           putStr "Part Two: " *> PartTwo.main

-- --------------------------------------------------------------------- [ EOF ]
