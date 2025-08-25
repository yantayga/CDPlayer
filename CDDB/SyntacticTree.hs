{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.SyntacticTree where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, toJSON, FromJSON, parseJSON, Value(..))
import Data.List (intercalate)
import Data.Text (unpack)
import Text.Read (readMaybe)
import Control.Applicative

--import Text.Parsec hiding(choice, (<|>))
import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Parsers

type VariableName = String
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
    show (Tag id ts) = id ++ " [" ++ (intercalate ", " $ map show ts) ++ "]"
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

-- Tree like S [*, NP, VP [VPP ["chase"]], *]
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

tryParseJSON (String s) = case (readMaybe $ unpack s) of
    Nothing -> empty
    Just t -> return t
tryParseJSON _ = empty

