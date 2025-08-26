{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Tree.Syntax where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON)
import Data.List (intercalate)
import Data.List.Extra ((!?))

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.JSON

type TagId = String

data SyntacticTree = Tag TagId [SyntacticTree]
    | Word TagId String
    deriving (Eq, Generic)

instance ToJSON SyntacticTree where
    toJSON t = toJSON $ show t

instance FromJSON SyntacticTree where
   parseJSON = tryParseJSON

instance Show SyntacticTree where
    show :: SyntacticTree -> String
    show (Tag id ts) = id ++ " [" ++ intercalate ", " (map show ts) ++ "]"
    show (Word id w) = id ++ "(" ++ show w ++ ")"

instance Read SyntacticTree where
    readPrec = choice [readTag, readWord]
        where
            readTag = do
                L.Ident n <- lexP
                l <- step readListPrec
                return $ Tag n l
            readWord = do
                L.Ident n <- lexP
                paren
                    ( do
                        L.String s <- lexP
                        return $ Word n s
                    )
    readListPrec = readListPrecDefault

-- Find node by path
findNode :: TreePath -> SyntacticTree -> Maybe SyntacticTree
findNode [n] (Tag _ ts) = ts !? n
findNode (n: ns) (Tag _ ts) = ts !? n >>= \t -> findNode ns t
findNode [] t = Just t
findNode _ t = Nothing

-- Rempve node by path
findAndRemoveNode :: TreePath -> SyntacticTree -> SyntacticTree
findAndRemoveNode [n] t@(Tag id ts) = if null bs then t else Tag id (as ++ tail bs)
    where
        (as, bs) = splitAt n ts
findAndRemoveNode (n: ns) t@(Tag id ts) = if null bs then t else Tag id (as ++ findAndRemoveNode ns (head bs): tail bs)
    where
        (as, bs) = splitAt n ts
findAndRemoveNode _ t = t
