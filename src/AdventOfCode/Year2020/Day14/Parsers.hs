{-# LANGUAGE NoImplicitPrelude #-}

module AdventOfCode.Year2020.Day14.Parsers where

import AdventOfCode.Year2020.Day14.Types
import Relude
import Text.Trifecta (Parser, brackets, char, choice, natural, symbol, token)

instruction :: Parser Instruction
instruction = setMask <|> setValue

setMask :: Parser Instruction
setMask =
  fmap SetMask
    $ symbol "mask"
    *> symbol "="
    *> token (many maskBit)
    <&> reverse

maskBit :: Parser (Maybe Bool)
maskBit =
  choice
    [ Just True <$ char '1',
      Just False <$ char '0',
      Nothing <$ char 'X'
    ]

setValue :: Parser Instruction
setValue = do
  address <- symbol "mem" *> brackets (fromInteger <$> natural)
  value <- symbol "=" *> natural <&> fromInteger
  pure $ SetValue address value
