module Toy where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec (optional, many)
import qualified Data.Text as T

test :: String
test = unlines [
   "rules:",
   "  rule:",
   "",
   "    comment 1",
   "    score 1",
   "    primitives 1",
   "      primitive 1",
   "      primitive 2",
   "    actions 1",
   "  rule: with data",
   "    field 2",
   "    field 3", --this breaks parsing!
   "",
   "  rule:   ",
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

-- Parser for comments (lines starting with #)
commentParser :: IParser ()
commentParser = do
  char '#'
  content <- many (noneOf "\n")
  spaces
  return ()

type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

data Rule = Rule Term [Taxonomy] deriving (Eq, Show)

data Rules = Rules String [Rule] deriving (Eq, Show)

fieldNameParser :: String -> IParser String
fieldNameParser name = string name <* char ':' <* spaces

rulesParser :: IParser Rules
rulesParser = withPos $ do
    term <- fieldNameParser "rules"
    subs <- many $ indented *> ruleParser
    return $ Rules term subs

ruleParser :: IParser Rule
ruleParser = withPos $ do
    term <- fieldNameParser "rule"
    subs <- many $ indented *> pTaxonomy
    return $ Rule term subs

-------------------
pTerm :: IParser String
pTerm = many1 alphaNum <* spaces

pTaxonomy :: IParser Taxonomy
pTaxonomy = withPos $ do
    term <- pTerm
    subs <- many $ indented *> pTaxonomy
    return $ Taxonomy term subs