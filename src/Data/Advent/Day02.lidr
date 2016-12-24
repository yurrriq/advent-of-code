---
pandoc-minted:
  language: idris
---

= Bathroom Security

[Link](https://adventofcode.com/2016/day/2)

You arrive at *Easter Bunny Headquarters* under cover of darkness. However, you
left in such a rush that you forgot to use the bathroom! Fancy office buildings
like this one usually have keypad locks on their bathrooms, so you search the
front desk for the code.

"In order to improve security," the document you find says, "bathroom codes will
no longer be written down. Instead, please memorize and follow the procedure
below to access the bathrooms."

The document goes on to explain that each button to be pressed can be found by
starting on the previous button and moving to adjacent buttons on the keypad:
`U` moves up, `D` moves down, `L` moves left, and `R` moves right. Each line of
instructions corresponds to one button, starting at the previous button (or, for
the first line, *the "5" button*); press whatever button you're on at the end of
each line. If a move doesn't lead to a button, ignore it.

You can't hold it much longer, so you decide to figure out the code as you walk
to the bathroom. You picture a keypad like this:

```text
1 2 3
4 5 6
7 8 9
```

Suppose your instructions are:

```text
ULL
RRDDD
LURDL
UUUUD
```

- You start at "5" and move up (to "2"), left (to "1"), and left (you can't, and
  stay on "1"), so the first button is `1`.
- Starting from the previous button ("1"), you move right twice (to "3") and
  then down three times (stopping at "9" after two moves and ignoring the
  third), ending up with `9`.
- Continuing from "9", you move left, up, right, down, and left, ending with
  `8`.
- Finally, you move up four times (stopping at "2"), then down once, ending with
  `5`.

So, in this example, the bathroom code is `1985`.

Your puzzle input is the instructions from the document you found at the front
desk.


== Module Declaration and Imports

> ||| Day 2: Bathroom Security
> module Data.Advent.Day02
>
> import public Data.Advent.Day
> import public Data.Ix
>
> import Data.Vect
>
> import public Lightyear
> import public Lightyear.Char
> import public Lightyear.Strings


== Data Types

> %access public export
>
> ||| Up, down, left or right.
> data Instruction = ||| Up
>                    U
>                  | ||| Down
>                    D
>                  | ||| Left
>                    L
>                  | ||| Right
>                    R
>
> ||| A single digit, i.e. a number strictly less than ten.
> Digit : Type
> Digit = Fin 10
>
> implementation Show Digit where
>   show = show . finToInteger
>
> implementation [showDigits] Show (List Digit) where
>   show = concatMap show
>
> ||| A pair of coordinates on the keypad, `(x, y)`.
> Coordinates : Type
> Coordinates = (Fin 3, Fin 3)

\newpage


== Parsers

> %access export
>
> up : Parser Instruction
> up = char 'U' *> pure U <?> "up"
>
> down : Parser Instruction
> down = char 'D' *> pure D <?> "down"
>
> left : Parser Instruction
> left = char 'L' *> pure L <?> "left"
>
> right : Parser Instruction
> right = char 'R' *> pure R <?> "right"
>
> instruction : Parser Instruction
> instruction = up <|> down <|> left <|> right <?> "up, down, left or right"
>
> partial instructions : Parser (List Instruction)
> instructions = some instruction <* (skip endOfLine <|> eof)


== Part One

\begin{quote}
  What is the bathroom code?
\end{quote}

> namespace PartOne
>
>   ||| A keypad like this:
>   |||
>   ||| ```
>   ||| 1 2 3
>   ||| 4 5 6
>   ||| 7 8 9
>   ||| ```
>   keypad : Vect 3 (Vect 3 Digit)
>   keypad = [ [1, 2, 3],
>              [4, 5, 6],
>              [7, 8, 9] ]
>
>   move : Coordinates -> Instruction -> Coordinates
>   move (x, y) U = (x, pred y)
>   move (x, y) D = (x, succ y)
>   move (x, y) L = (pred x, y)
>   move (x, y) R = (succ x, y)

\newpage

>   button : Coordinates -> List Instruction -> (Coordinates, Digit)
>   button loc@(x, y) [] = (loc, index x (index y keypad))
>   button loc (i :: is) = button (move loc i) is
>
> partial partOne : List (List Instruction) -> String
> partOne = show @{showDigits} . go ((1,1), [])
>   where
>     go : (Coordinates, List Digit) -> List (List Instruction) -> List Digit
>     go (_, ds) []            = reverse ds
>     go (loc, ds) (is :: iis) = let (loc', d) = PartOne.button loc is in
>                                    go (loc', d :: ds) iis
>
> namespace PartOne
>
>   ||| ```idris example
>   ||| example
>   ||| ```
>   partial example : String
>   example = fromEither $ partOne <$>
>             parse (some instructions) "ULL\nRRDDD\nLURDL\nUUUUD"


== Part Two

You finally arrive at the bathroom (it's a several minute walk from the lobby so
visitors can behold the many fancy conference rooms and water coolers on this
floor) and go to punch in the code. Much to your bladder's dismay, the keypad is
not at all like you imagined it. Instead, you are confronted with the result of
hundreds of man-hours of bathroom-keypad-design meetings:

```text
    1
  2 3 4
5 6 7 8 9
  A B C
    D
```

You still start at "5" and stop when you're at an edge, but given the same
instructions as above, the outcome is very different:

- You start at "5" and don't move at all (up and left are both edges), ending at
  `5`.
- Continuing from "5", you move right twice and down three times (through "6",
  "7", "B", "D", "D"), ending at `D`.
- Then, from "D", you move five more times (through "D", "B", "C", "C", "B"),
  ending at `B`.
- Finally, after five more moves, you end at `3`.

\newpage

So, given the actual keypad layout, the code would be `5DB3`.

\begin{quote}
  Using the same instructions in your puzzle input, what is the correct
  \textit{bathroom code}?
\end{quote}

> namespace PartTwo
>
>   keypad : Vect 5 (n ** Vect n Char)
>   keypad = [ (1 **           ['1'])
>            , (3 **      ['2', '3', '4'])
>            , (5 ** ['5', '6', '7', '8', '9'])
>            , (3 **      ['A', 'B', 'C'])
>            , (1 **           ['D'])
>            ]
>
>   -- NOTE: This will wrap at the bounds, which might be unexpected.
>   partial convert : (n : Nat) -> Fin m -> Fin n
>   convert (S j) fm {m} =
>       let delta = half $ if S j > m
>                             then S j `minus` m
>                             else m `minus` S j in
>           the (Fin (S j)) $ fromNat $ finToNat fm `f` delta
>     where
>       f : Nat -> Nat -> Nat
>       f = if S j > m then plus else minus
>       partial half : Nat -> Nat
>       half = flip div 2
>
>   canMoveVertically : (Fin (S k), Fin 5) -> Instruction -> Bool
>   canMoveVertically (x, y) i with ((finToNat x, finToNat y))
>     canMoveVertically (x, y) U | (col, row) =
>         case row of
>              Z                   => False
>              S Z                 => col == 1
>              S (S Z)             => inRange (1,3) col
>              _                   => True
>     canMoveVertically (x, y) D | (col, row) =
>         case row of
>              S (S Z)             => inRange (1,3) col
>              S (S (S Z))         => col == 1
>              S (S (S (S Z)))     => False
>              _                   => True
>     canMoveVertically _ _ | _ = True

\newpage

>   partial move : (Fin (S k), Fin 5) -> Instruction ->
>     ((n ** Fin n), Fin 5)
>   move (x, y) U = if canMoveVertically (x, y) U
>                      then let n = fst (index (pred y) keypad) in
>                               ((n ** convert n x), pred y)
>                      else ((_ ** x), y)
>   move (x, y) D = if canMoveVertically (x, y) D
>                      then let n = fst (index (succ y) keypad) in
>                               ((n ** convert n x), succ y)
>                      else ((_ ** x), y)
>   move (x, y) L = let n = fst (index y keypad) in
>                       ((n ** convert n (pred x)), y)
>   move (x, y) R = let n = fst (index y keypad) in
>                       ((n ** convert n (succ x)), y)
>
>   partial button : (Fin (S k), Fin 5) -> List Instruction ->
>     (((n ** Fin n), Fin 5), Char)
>   button loc@(x, y) [] =
>       let (n ** row) = index y PartTwo.keypad
>           xx = convert n x in
>           (((n ** xx), y), index xx row)
>   button loc (i :: is) =
>       let ((S _ ** x), y) = move loc i in
>           button (x, y) is
>
> partial partTwo : List (List Instruction) -> String
> partTwo = go (((5 ** 0),2), [])
>   where
>     partial go : (((n ** Fin n), Fin 5), List Char) ->
>       List (List Instruction) -> String
>     go (_, cs) []            = pack $ reverse cs
>     go (loc, cs) (is :: iis) =
>         let ((S k ** xx), y) = loc
>             (loc', c) = PartTwo.button (xx, y) {k=k} is in
>             go (loc', c :: cs) iis
>
> namespace PartTwo
>
>   ||| ```idris example
>   ||| PartTwo.example
>   ||| ```
>   partial example : String
>   example = fromEither $ partTwo <$>
>             parse (some instructions) "ULL\nRRDDD\nLURDL\nUUUUD"


== Main

> namespace Main
>
>   partial main : IO ()
>   main = runDay $ MkDay 2 (some instructions)
>          (pure . partOne)
>          (pure . partTwo)
