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

import CDDB.JSON
import CDDB.Tree.WordProperty

type TagId = String
type TreePos = Int
type TreePath = [TreePos]

-- TODO: Rework according to X Bar -----
data XBTree = XBTree TagId XBTree XBTree
    | XBWord TagId String WordProperties
    
----------------------------------------

data SyntacticTree = Tag TagId [SyntacticTree]
    | Word TagId String WordProperties
    deriving (Eq, Generic)

instance ToJSON SyntacticTree where
    toJSON t = toJSON $ show t

instance FromJSON SyntacticTree where
   parseJSON = tryParseJSON

instance Show SyntacticTree where
    show :: SyntacticTree -> String
    show (Tag tid ts) = tid ++ " [" ++ intercalate ", " (map show ts) ++ "]"
    show (Word tid w desc) = tid ++ "(" ++ show w ++ " [" ++ intercalate ", " (map show desc) ++ "])"

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
                        ps <- readListPrec
                        return $ Word n s ps
                    )
    readListPrec = readListPrecDefault

-- Find node by path
findNode :: TreePath -> SyntacticTree -> Maybe SyntacticTree
findNode [n] (Tag _ ts) = ts !? n
findNode (n: ns) (Tag _ ts) = ts !? n >>= \t -> findNode ns t
findNode [] t = Just t
findNode _ _ = Nothing

-- Rempve node by path
findAndRemoveNode :: TreePath -> SyntacticTree -> SyntacticTree
findAndRemoveNode [n] t@(Tag tid ts) = if null bs then t else Tag tid (as ++ tail bs)
    where
        (as, bs) = splitAt n ts
findAndRemoveNode (n: ns) t@(Tag tid ts) = if null bs then t else Tag tid (as ++ findAndRemoveNode ns (head bs): tail bs)
    where
        (as, bs) = splitAt n ts
findAndRemoveNode _ t = t
