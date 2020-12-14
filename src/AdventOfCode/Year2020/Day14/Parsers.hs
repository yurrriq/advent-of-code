module AdventOfCode.Year2020.Day14.Parsers where

import AdventOfCode.Year2020.Day14.Types
import Control.Applicative ((<|>))
import Control.Monad (void)
import Text.Trifecta

instruction :: Parser Instruction
instruction = setMask <|> setValue

setMask :: Parser Instruction
setMask =
  do
    void $ symbol "mask"
    void $ symbol "="
    maskBits <- reverse <$> many maskBit
    void whiteSpace
    pure (SetMask maskBits)

maskBit :: Parser (Maybe Bool)
maskBit =
  choice
    [ Just True <$ char '1',
      Just False <$ char '0',
      Nothing <$ char 'X'
    ]

setValue :: Parser Instruction
setValue =
  do
    void $ symbol "mem"
    address <- between (symbol "[") (symbol "]") (fromInteger <$> natural)
    void $ symbol "="
    value <- fromInteger <$> natural
    pure $ SetValue address value
