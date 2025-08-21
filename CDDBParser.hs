module Toy where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec (optional, many)
import qualified Data.Text as T
import Data.Time
import Data.Maybe


test :: String
test = unlines [
   "rules:",
   "  rule: # rule comment ",
   "",
   "    comment: c 1",
   "    score: 4.15",
   "    match: S (NP VP)",
   "    primitives:",
   "      primitive 11",
   "      primitive 12",
   "    actions: a 1",
   "  rule:",
   "    match: S # comment",
   "    actions: a 2",
   "",
   "rules: list...",
   ""
  ]

type IParser a = IndentParser String () a

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
  runIndentParser aParser () source_name input

-- Parser for quoted strings
noncommentStringParser :: IParser String
noncommentStringParser = many (noneOf "\"\n#")

parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%d.%m.%Y"

dateParser :: IParser (Maybe UTCTime)
dateParser = do
    strDate <- noncommentStringParser
    spaces
    optional commentParser
    return $ parseBasicDate strDate

integerParser :: IParser Integer
integerParser = do
    sign <- option "" (string "-" <|> string "+")
    integerPart <- many1 digit
    return $ read (sign ++ integerPart)

doubleParser :: IParser Double
doubleParser = do
    sign <- option "" (string "-" <|> string "+")
    integerPart <- many1 digit
    fractionalPart <- option "" (char '.' >> many1 digit)
    scientificPart <- option "" scientific
    return $ read (sign ++ integerPart ++ ('.':fractionalPart))
      where
        scientific = do
            expChar <- oneOf "eE"
            expSign <- option '+' (char '-' <|> char '+')
            expDigits <- many1 digit
            return (expChar:expSign:expDigits)

-- Parser for comments (lines starting with #)
commentParser :: IParser String
commentParser = do
  char '#'
  content <- many (noneOf "\n")
  spaces
  return ""

commaSeparatedparser :: IParser a -> IParser [a]
commaSeparatedparser parser = sepBy parser (char ',')

type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

type Name = String
type Version = Integer
type Date = Maybe UTCTime

type Comment = String
type Score = Double
type Match = String
type Actions = String
data Rule = Rule Comment Score Match [Taxonomy] Actions deriving (Eq, Show)

data Rules = Rules [Rule] deriving (Eq, Show)

data Primitive = Primitive Name [Name] deriving (Eq, Show)

data Primitives = Primitives [Primitive] deriving (Eq, Show)

data CDDB = CDDB Name Version Date Primitives Rules deriving (Eq, Show)

fieldNameParser :: Name -> IParser String
fieldNameParser name = string name <* char ':' <* spaces <* optional commentParser

fieldDataParser :: Name -> IParser a -> IParser (String, a)
fieldDataParser name parser = do
    res <- string name
    char ':'
    spaces
    value <- parser
    spaces
    optional commentParser
    return (res, value)

fieldParser :: Name -> IParser a -> IParser (String, [a])
fieldParser name parser = withPos $ do
    term <- fieldNameParser name
    subs <- many $ indented *> parser
    return (term, subs)

indentedfieldDataParser :: Name -> IParser a -> IParser a
indentedfieldDataParser name parser = fmap snd $ indented *> fieldDataParser name parser

indentedFieldOptionalDataParser :: Name -> IParser a -> a -> IParser a
indentedFieldOptionalDataParser name parser defaultvalue = option defaultvalue $ fmap snd $ indented *> fieldDataParser name parser

indentedFieldParser:: Name -> IParser a -> IParser [a]
indentedFieldParser name parser = fmap snd $ indented *> fieldParser name parser

indentedFieldOptionalParser:: Name -> IParser a -> IParser [a]
indentedFieldOptionalParser name parser = option [] $ fmap snd $ indented *> fieldParser name parser

-- TODO: Parse 'search' field
-- TODO: Parse primiives
-- TODO: Parse 'context' field
-- TODO: Parse 'locals' field
-- TODO: Parse 'conditions' field
-- TODO: Parse tree
ruleParser :: Primitives -> IParser Rule
ruleParser primitives = withPos $ do
    _ <- fieldNameParser "rule"
    comment <- indentedFieldOptionalDataParser "comment" noncommentStringParser ""
    score <- indentedFieldOptionalDataParser "score" doubleParser 1.0
    match <- indentedfieldDataParser "match" noncommentStringParser
    primitives <- indentedFieldOptionalParser "primitives" pTaxonomy
    actions <- indentedFieldOptionalDataParser "actions" noncommentStringParser ""
    return $ Rule comment score match primitives actions

rulesParser :: IParser Rules
rulesParser = do
    (name, rules) <- fieldParser "rules" ruleParser
    return $ Rules rules

primitiveParser :: IParser Primitive
primitiveParser = withPos $ do
    _ <- fieldNameParser "primitive"
    name <- indentedfieldDataParser "name" noncommentStringParser
    fields <- indentedfieldDataParser "fields" $ commaSeparatedparser noncommentStringParser
    return $ Primitive name fields

primitivesParsers :: IParser Primitives
primitivesParsers = withPos $ do
    (name, primitives) <- fieldParser "primitives" primitiveParser
    return $ Primitives primitives

aCDDBParsers :: IParser CDDB
aCDDBParsers = do
    many commentParser
    name <- fmap snd $ fieldDataParser "name" noncommentStringParser
    version <- fmap snd $ fieldDataParser "version" integerParser
    date <- fmap snd $ fieldDataParser "date" dateParser
    primitives <- primitivesParsers
    rules <- rulesParser primitives
    return $ CDDB name version date primitives rules

parseCDDBFile :: FilePath -> IO (Either ParseError CDDB)
parseCDDBFile path = do
    fileContent <- readFile path
    let res = iParse aCDDBParsers path fileContent
    return res











-------------------
pTerm :: IParser String
pTerm = many1 alphaNum <* spaces

pTaxonomy :: IParser Taxonomy
pTaxonomy = withPos $ do
    term <- pTerm
    subs <- many $ indented *> pTaxonomy
    return $ Taxonomy term subs