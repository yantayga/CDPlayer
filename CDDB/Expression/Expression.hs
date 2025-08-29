{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Expression where

import GHC.Generics
import GHC.Read
import Data.Aeson (ToJSON, FromJSON)

import Text.ParserCombinators.ReadPrec as R
import qualified Text.Read.Lex as L

import CDDB.Types
import CDDB.Expression.UnOps
import CDDB.Expression.BinOps
import CDDB.Expression.Constants

data Expression = Constant Constant
    | Variable VariableName
    | UnOp UnOp Expression
    | BinOp BinOp Expression Expression
    deriving (Eq, Generic, ToJSON, FromJSON)

instance Show Expression where
    show :: Expression -> String
    show (Constant c) = show c
    show (Variable name) = name
    show (UnOp op e) = show op ++ " " ++ show e
    show (BinOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"

instance Read Expression where -- TODO: Fix 1-1-1 parsing to (1-(1-1)) https://hackage.haskell.org/package/parsec-3.1.17.0/docs/Text-Parsec-Expr.html
    readPrec = readBinOp <++ readBinOpPrioritized <++ readUnOpDoubled <++ readUnOp <++ readFirstExpr
        where
            readFirstExpr = parens ( do
                    c <- readPrec :: ReadPrec Constant
                    return $ Constant c
                <++ do
                    L.Ident v <- lexP
                    return $ Variable v
                )
                <++ paren (readPrec :: ReadPrec Expression)
            readBinOp = parens ( do
                ce <- readFirstExpr
                op <- readPrec :: ReadPrec BinOp
                e <- readPrec :: ReadPrec Expression
                checkExpressionPriority op e $ return $ BinOp op ce e
                )
            readBinOpPrioritized = parens ( do
                ce1 <- readFirstExpr
                op1 <- readPrec :: ReadPrec BinOp
                ce2 <- readFirstExpr
                op2 <- readPrec :: ReadPrec BinOp
                e <- readPrec :: ReadPrec Expression
                return $ BinOp op2 (BinOp op1 ce1 ce2) e
                )
            readUnOp = parens ( do
                op <- readPrec :: ReadPrec UnOp
                ce <- readFirstExpr
                return $ UnOp op ce
                )
            readUnOpDoubled = parens ( do
                op <- readPrec :: ReadPrec UnOp
                e@(UnOp _ _) <- readPrec :: ReadPrec Expression
                return $ UnOp op e
                )

checkExpressionPriority :: BinOp -> Expression -> ReadPrec a -> ReadPrec a
checkExpressionPriority op (BinOp e _ _) f = if binOpPriority op <= binOpPriority e then f else pfail
checkExpressionPriority _ _ f = f
