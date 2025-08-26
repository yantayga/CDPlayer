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
matchFilterExpr t filterExpr = matchFilterExpr' t filterExpr 0 []

matchFilterExpr' :: SyntacticTree -> FilterExpression -> TreePos -> TreePath -> Maybe VariablePaths
matchFilterExpr' _ Asterisk _ _ = Just M.empty
matchFilterExpr' (Tag id ts) (FilterTag Nothing fid fs) pos path    = guard (id == fid) >> (matchFilterExprs ts fs pos $ pos:path)
matchFilterExpr' (Tag id ts) (FilterTag (Just vn) fid fs) pos path  = guard (id == fid) >> (matchFilterExprs ts fs pos $ pos:path) >>= \vs -> Just (addPath vn pos path vs)
matchFilterExpr' (Word id s) (FilterWord Nothing fid fs) pos _      = guard (id == fid && s == fs) >> Just M.empty
matchFilterExpr' (Word id s) (FilterWord (Just vn) fid fs) pos path = guard (id == fid && s == fs) >> Just (addPath vn pos path M.empty)
matchFilterExpr' _ _ _ _ = Nothing

addPath :: VariableName -> TreePos -> TreePath -> VariablePaths -> VariablePaths
addPath vn n ns vs = M.insert vn (tail $ reverse $ n:ns) vs

matchFilterExprs :: [SyntacticTree] -> [FilterExpression] -> TreePos -> TreePath -> Maybe VariablePaths
matchFilterExprs [] [] _ _ = Just M.empty
matchFilterExprs _ [] _ _ = Nothing
matchFilterExprs [] (Asterisk: sfs) pos path = matchFilterExprs [] sfs pos path
matchFilterExprs [] _ _ _ = Nothing
matchFilterExprs ts@(t: sts) fs@(Asterisk: sfs) pos path = matchFilterExprs ts sfs pos path <|> matchFilterExprs sts fs (pos + 1) path
matchFilterExprs (t: sts) (f: sfs) pos path = do
    vs1 <- matchFilterExpr' t f pos path
    vs2 <- matchFilterExprs sts sfs (pos + 1) path
    return $ M.unionWith const vs1 vs2
