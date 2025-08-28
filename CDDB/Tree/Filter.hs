{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Tree.Filter where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.List (intercalate)
import qualified Data.Map as M

import Control.Monad (guard)
import Control.Applicative ((<|>))

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Tree.Syntax
import CDDB.Parser
import CDDB.JSON
import CDDB.Expression.Constants
import CDDB.Expression.Eval

data FilterExpressionT a = Asterisk a
    | FilterTag a TagId [FilterExpressionT a]
    | FilterWord a TagId String
    | FilterNot (FilterExpressionT a) -- No variable binding for Not
    | FilterOr a [FilterExpressionT a]
    deriving (Eq, Generic, Functor)

type FilterExpression = FilterExpressionT (Maybe VariableName)

instance ToJSON FilterExpression where
    toJSON t = toJSON $ show t

instance FromJSON FilterExpression where
   parseJSON = tryParseJSON

instance Show FilterExpression where
    show :: FilterExpression -> String
    show (Asterisk v) = showVariable v ++ "*"
    show (FilterTag v tagId ts) = showVariable v ++ tagId ++ "[" ++ intercalate ", " (map show ts) ++ "]"
    show (FilterWord v tagId w) = showVariable v ++ tagId ++ "(" ++ show w ++ ")"
    show (FilterNot f) = "~" ++ show f
    show (FilterOr v fs) = showVariable v ++ "{" ++ intercalate ", " (map show fs) ++ "}"

showVariable :: Maybe String -> String
showVariable Nothing = ""
showVariable (Just vn) = vn ++ ":"

instance Read FilterExpression where
    readPrec = choice [readAsterisk, readAsteriskWithVariable, readTag, readWord, readNot, readOr]
        where
            readAsterisk = do
                expectP (L.Symbol "*")
                return $ Asterisk Nothing
            readAsteriskWithVariable = do
                L.Ident vn <- lexP
                expectP (L.Symbol ":*")
                return $ Asterisk $ Just vn
            readTag = do
                v <- readVariable
                L.Ident n <- lexP
                l <- readListPrec
                return $ FilterTag v n l
            readWord = do
                v <- readVariable
                L.Ident n <- lexP
                expectP (L.Punc "(")
                L.String s <- lexP
                expectP (L.Punc ")")
                return $ FilterWord v n s
            readNot = do
                expectP (L.Punc "~")
                f <- readPrec :: ReadPrec FilterExpression
                return $ FilterNot f
            readOr = do
                v <- readVariable
                fs <- step $ cbList readPrec :: ReadPrec [FilterExpression]
                return $ FilterOr v fs

    readListPrec = readListPrecDefault

readVariable :: ReadPrec (Maybe String)
readVariable = readJust +++ return Nothing
    where
        readJust = do
            L.Ident vn <- lexP
            expectP (L.Symbol ":")
            return $ Just vn

-- Very inefficirent
-- TODO: look at Trie data type
-- TODO: add variable setting for * and {}
-- TODO: implement Not
-- TODO: implement Or
matchFilterExpr :: SyntacticTree -> FilterExpression -> Maybe VariableStates
matchFilterExpr t filterExpr = matchFilterExpr' t filterExpr 0 []

matchFilterExpr' :: SyntacticTree -> FilterExpression -> TreePos -> TreePath -> Maybe VariableStates
matchFilterExpr' _ (Asterisk _) _ _ = Just M.empty
matchFilterExpr' (Tag tagId ts) (FilterTag Nothing fid fs) pos path    = guard (tagId == fid) >> matchFilterExprs ts fs pos (pos:path)
matchFilterExpr' t@(Tag tagId ts) (FilterTag (Just vn) fid fs) pos path  =
    guard (tagId == fid) >> matchFilterExprs ts fs pos (pos:path) >>= \vs -> Just (addConstant vn pos path t vs)
matchFilterExpr' (Word tagId s _) (FilterWord Nothing fid fs) _ _        = guard (tagId == fid && s == fs) >> Just M.empty
matchFilterExpr' t@(Word tagId s _) (FilterWord (Just vn) fid fs) pos path = guard (tagId == fid && s == fs) >> Just (addConstant vn pos path t M.empty)
matchFilterExpr' _ (FilterNot _) _ _  = error "Filter expression for 'Not' is not implemented yet..."
matchFilterExpr' _ (FilterOr _ _) _ _ = error "Filter expression for 'Or' is not implemented yet..."
matchFilterExpr' _ _ _ _ = Nothing

addConstant :: VariableName -> TreePos -> TreePath -> SyntacticTree -> VariableStates -> VariableStates
addConstant vn n ns t = M.insert vn (CTreePart (tail $ reverse $ n:ns) t)

matchFilterExprs :: [SyntacticTree] -> [FilterExpression] -> TreePos -> TreePath -> Maybe VariableStates
matchFilterExprs [] [] _ _ = Just M.empty
matchFilterExprs _ [] _ _ = Nothing
matchFilterExprs [] ((Asterisk _): sfs) pos path = matchFilterExprs [] sfs pos path
matchFilterExprs [] _ _ _ = Nothing
matchFilterExprs ts@(_: sts) fs@((Asterisk _): sfs) pos path = matchFilterExprs ts sfs pos path <|> matchFilterExprs sts fs (pos + 1) path
matchFilterExprs (t: sts) (f: sfs) pos path = do
    vs1 <- matchFilterExpr' t f pos path
    vs2 <- matchFilterExprs sts sfs (pos + 1) path
    return $ M.unionWith const vs1 vs2
