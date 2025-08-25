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

-- Tree like "S" ["NP" ["DET" [Word "the"], "N" [Word "cat"]], "VP" [Word "chase"], "NP" ["DET" [Word "a"], "N" [Word "mouse"]]]
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
    readPrec = choice [readTag, readWord Word]
        where
            readTag = do
                L.Ident n <- lexP
                l <- step readListPrec
                return $ Tag n l
    readListPrec = readListPrecDefault

-- Tree like S [*, NP, VP [VPP ["chase"]], *]
data FilterExpression = Asterisk
    | FilterTag (Maybe VariableName) TagId [FilterExpression]
    | FilterWord TagId String
    deriving (Eq, Generic)

instance ToJSON FilterExpression where
    toJSON t = toJSON $ show t

instance FromJSON FilterExpression where
   parseJSON = tryParseJSON

instance Show    where
    show :: FilterExpression -> String
    show Asterisk = "*"
    show (FilterTag Nothing id ts) = id ++ "[" ++ (intercalate ", " $ map show ts) ++ "]"
    show (FilterTag (Just vn) id ts) = vn ++ ":" ++ id ++ "[" ++ (intercalate ", " $ map show ts) ++ "]"
    show (FilterWord id w) = id ++ "(" ++ show w ++ ")"

instance Read FilterExpression where
    readPrec = choice [readAsterisk, readTag, readTagWithVAriable, readWord FilterWord]
        where
            readAsterisk = do
                L.Symbol "*" <- lexP
                return Asterisk
            readTag = do
                L.Ident n <- lexP
                l <- step readListPrec
                return $ FilterTag Nothing n l
            readTagWithVAriable = do
                L.Ident vn <- lexP
                L.Symbol ":" <- lexP
                L.Ident n <- lexP
                l <- step readListPrec
                return $ FilterTag (Just vn) n l
    readListPrec = readListPrecDefault

tryParseJSON (String s) = case (readMaybe $ unpack s) of
    Nothing -> empty
    Just t -> return t
tryParseJSON _ = empty

readWord c = do
    L.Ident n <- lexP
    paren
        ( do
            L.String s <- lexP
            return $ c n s
        )