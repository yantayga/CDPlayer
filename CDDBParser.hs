module Toy where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec (optional, many)
import qualified Data.Text as T

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
noncommentString :: IParser String
noncommentString = many (noneOf "\"\n#")

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

type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

type Name = String
type Comment = String
type Score = Double
type Match = String
type Actions = String
data Rule = Rule Name Comment Score Match [Taxonomy] Actions deriving (Eq, Show)

data Rules = Rules Name [Rule] deriving (Eq, Show)

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

rulesParser :: IParser Rules
rulesParser = do
    (name, rules) <- fieldParser "rules" ruleParser
    return $ Rules name rules

ruleParser :: IParser Rule
ruleParser = withPos $ do
    name <- fieldNameParser "rule"
    comment <- indentedFieldOptionalDataParser "comment" noncommentString ""
    score <- indentedFieldOptionalDataParser "score" doubleParser 1.0
    match <- indentedfieldDataParser "match" noncommentString
    primitives <- indentedFieldOptionalParser "primitives" pTaxonomy
    actions <- indentedFieldOptionalDataParser "actions" noncommentString ""
    return $ Rule name comment score match primitives actions

-------------------
pTerm :: IParser String
pTerm = many1 alphaNum <* spaces

pTaxonomy :: IParser Taxonomy
pTaxonomy = withPos $ do
    term <- pTerm
    subs <- many $ indented *> pTaxonomy
    return $ Taxonomy term subs