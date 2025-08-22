{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expressions where

import GHC.Generics
import Data.Aeson hiding (Null, Error)
import Data.List
import Data.Maybe
import qualified Data.Map as M

import CDDB.Types

newtype VariableStates = VariableStates (M.Map VariableName Constant)

evaluateExpression :: VariableStates -> Expression -> Constant
evaluateExpression _ (Constant const) = const
evaluateExpression (VariableStates map) (Variable name) = fromMaybe Null $ M.lookup name map
evaluateExpression states (UnOp op expr) = evaluateUnOpExpression op (evaluateExpression states expr)
evaluateExpression states (BinOp op expr1 expr2) = evaluateBinOpExpression op (evaluateExpression states expr1) (evaluateExpression states expr2)

-- TODO: Make better error handling, not just return Error

evaluateUnOpExpression :: UnOp -> Constant -> Constant
evaluateUnOpExpression IsNull Null      = CBoolean True
evaluateUnOpExpression IsNull _         = CBoolean False
evaluateUnOpExpression IsNotNull Null   = CBoolean False
evaluateUnOpExpression IsNotNull _      = CBoolean True

evaluateUnOpExpression UnaryMinus (CBoolean v)  = CBoolean $ not v
evaluateUnOpExpression UnaryMinus (CInteger v)  = CInteger $ -v
evaluateUnOpExpression UnaryMinus (CDouble v)   = CDouble $ -v
evaluateUnOpExpression UnaryMinus _             = Error


evaluateBinOpExpression :: BinOp -> Constant -> Constant -> Constant
evaluateBinOpExpression _ Null _ = Null
evaluateBinOpExpression _ _ Null = Null

evaluateBinOpExpression Plus (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 || v2
evaluateBinOpExpression Plus (CInteger v1) (CInteger v2)  = CInteger $ v1 + v2
evaluateBinOpExpression Plus (CDouble v1) (CInteger v2)  = CDouble $ v1 + (fromIntegral v2)
evaluateBinOpExpression Plus (CInteger v1) (CDouble v2)  = CDouble $ (fromIntegral v1) + v2
evaluateBinOpExpression Plus (CDouble v1) (CDouble v2)  = CDouble $ v1 + v2
evaluateBinOpExpression Plus (CString v1) (CString v2)  = CString $ v1 ++ v2
evaluateBinOpExpression Plus _ _  = Error

evaluateBinOpExpression Minus (CInteger v1) (CInteger v2)  = CInteger $ v1 - v2
evaluateBinOpExpression Minus (CDouble v1) (CInteger v2)  = CDouble $ v1 - (fromIntegral v2)
evaluateBinOpExpression Minus (CInteger v1) (CDouble v2)  = CDouble $ (fromIntegral v1) - v2
evaluateBinOpExpression Minus (CDouble v1) (CDouble v2)  = CDouble $ v1 - v2
evaluateBinOpExpression Minus _ _  = Error

evaluateBinOpExpression Multiply (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 && v2
evaluateBinOpExpression Multiply (CInteger v1) (CInteger v2)  = CInteger $ v1 * v2
evaluateBinOpExpression Multiply (CDouble v1) (CInteger v2)  = CDouble $ v1 * (fromIntegral v2)
evaluateBinOpExpression Multiply (CInteger v1) (CDouble v2)  = CDouble $ (fromIntegral v1) * v2
evaluateBinOpExpression Multiply (CDouble v1) (CDouble v2)  = CDouble $ v1 * v2
evaluateBinOpExpression Multiply (CString v1) (CInteger v2)  = CString $ concat $ genericTake v2 $ repeat v1
evaluateBinOpExpression Multiply _ _  = Error

evaluateBinOpExpression Divide (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 && v2
evaluateBinOpExpression Divide (CInteger v1) (CInteger v2)  = CInteger $ v1 * v2
evaluateBinOpExpression Divide (CDouble v1) (CInteger v2)  = CDouble $ v1 * (fromIntegral v2)
evaluateBinOpExpression Divide (CInteger v1) (CDouble v2)  = CDouble $ (fromIntegral v1) * v2
evaluateBinOpExpression Divide (CDouble v1) (CDouble v2)  = CDouble $ v1 * v2
evaluateBinOpExpression Divide (CString v1) (CInteger v2)  = CString [v1 !! (fromIntegral v2)]
evaluateBinOpExpression Divide _ _  = Error

evaluateBinOpExpression IsA _ (CType v)  = CBoolean True
evaluateBinOpExpression IsA _ _  = Error

evaluateBinOpExpression IsNotA _ (CType v)  = CBoolean False
evaluateBinOpExpression IsNotA _ _  = Null

evaluateBinOpExpression Dot _ _  = Error