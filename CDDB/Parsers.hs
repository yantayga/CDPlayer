module CDDB.Parsers where

import Text.Parsec

type Parser = Parsec String ()

nonQuotedStringParser :: Parser String
nonQuotedStringParser = many (noneOf "\"")

quotedStringParser :: Parser String
quotedStringParser = char '"' *> nonQuotedStringParser <* char '"'

data Test = Test String

testParser :: Parser Test
testParser = Test <$> nonQuotedStringParser
