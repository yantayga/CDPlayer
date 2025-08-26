{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
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
import CDDB.JSON

data FilterExpression = Asterisk
    | FilterTag (Maybe VariableName) TagId [FilterExpression]
    | FilterWord (Maybe VariableName) TagId String
    deriving (Eq, Generic)

instance ToJSON FilterExpression where
    toJSON t = toJSON $ show t

instance FromJSON FilterExpression where
   parseJSON = tryParseJSON

instance Show FilterExpression where
    show :: FilterExpression -> String
    show Asterisk = "*"
    show (FilterTag Nothing id ts) = id ++ "[" ++ (intercalate ", " $ map show ts) ++ "]"
    show (FilterTag (Just vn) id ts) = vn ++ ":" ++ id ++ "[" ++ (intercalate ", " $ map show ts) ++ "]"
    show (FilterWord Nothing id w) = id ++ "(" ++ show w ++ ")"
    show (FilterWord (Just vn) id w) = vn ++ ":" ++ id ++ "(" ++ show w ++ ")"

instance Read FilterExpression where
    readPrec = choice [readAsterisk, readTag, readTagWithVariable, readWord, readWordWithVariable]
        where
            readAsterisk = do
                L.Symbol "*" <- lexP
                return Asterisk
            readTag = do
                L.Ident n <- lexP
                l <- step readListPrec
                return $ FilterTag Nothing n l
            readTagWithVariable = do
                L.Ident vn <- lexP
                L.Symbol ":" <- lexP
                L.Ident n <- lexP
                l <- step readListPrec
                return $ FilterTag (Just vn) n l
            readWord = do
                L.Ident n <- lexP
                paren
                    ( do
                        L.String s <- lexP
                        return $ FilterWord Nothing n s
                    )
            readWordWithVariable = do
                L.Ident vn <- lexP
                L.Symbol ":" <- lexP
                L.Ident n <- lexP
                paren
                    ( do
                        L.String s <- lexP
                        return $ FilterWord (Just vn) n s
                    )
    readListPrec = readListPrecDefault

type VariablePaths = M.Map VariableName TreePath

matchFilterExpr :: SyntacticTree -> FilterExpression -> Maybe VariablePaths
matchFilterExpr t filterExpr = matchFilterExpr' t 0 filterExpr []

matchFilterExpr' :: SyntacticTree -> TreePos -> FilterExpression -> TreePath -> Maybe VariablePaths
matchFilterExpr' _ _ Asterisk _ = Just emptyVariablePaths
matchFilterExpr' t@(Tag id ts) pos (FilterTag Nothing fid fs) path = guard (id == fid) >> (matchFilterExprs ts pos fs $ pos:path)
matchFilterExpr' t@(Tag id ts) pos (FilterTag (Just vn) fid fs) path = guard (id == fid) >> (matchFilterExprs ts pos fs $ pos:path) >>= \vs -> Just $ M.insert vn (tail $ reverse $ pos:path) vs
matchFilterExpr' (Word id s) pos (FilterWord Nothing fid fs) _ = guard (id == fid && s == fs) >> Just emptyVariablePaths
matchFilterExpr' (Word id s) pos (FilterWord (Just vn) fid fs) path = guard (id == fid && s == fs) >> Just (M.singleton vn (tail $ reverse $ pos:path))
matchFilterExpr' _ _ _ _ = Nothing

matchFilterExprs :: [SyntacticTree] -> TreePos -> [FilterExpression] -> TreePath -> Maybe VariablePaths
matchFilterExprs [] _ [] _ = Just emptyVariablePaths
matchFilterExprs _ _ [] _ = Nothing
matchFilterExprs [] pos (Asterisk: sfs) path = matchFilterExprs [] pos sfs path
matchFilterExprs [] _ _ _ = Nothing
matchFilterExprs ts@(t: sts) pos fs@(Asterisk: sfs) path = matchFilterExprs ts pos sfs path <|> matchFilterExprs sts (pos + 1) fs path
matchFilterExprs (t: sts) pos (f: sfs) path = do
    vs1 <- matchFilterExpr' t pos f path
    vs2 <- matchFilterExprs sts (pos + 1) sfs path
    return $ M.unionWith const vs1 vs2

emptyVariablePaths :: VariablePaths
emptyVariablePaths = M.empty