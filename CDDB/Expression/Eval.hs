{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Expression.Eval where

import Data.List
import Data.Maybe
import qualified Data.Map as M

import CDDB.Types
import CDDB.Expression.UnOps
import CDDB.Expression.BinOps
import CDDB.Expression.Constants
import CDDB.Expression.Expression
import CDDB.Expression.VariableDefs
import CDDB.Tree.Syntax

type VariableStates = M.Map VariableName Constant

emptyVariableStates :: VariableStates
emptyVariableStates = M.empty

addVariableDef :: VariableStates -> VariableDef -> VariableStates
addVariableDef states (VariableDef name expr) = M.insert name (evaluateExpression states expr) states

bindTreeVariables :: VariableStates -> SyntacticTree -> SyntacticTree
bindTreeVariables states t = evalRes t $ evaluateExpression states (Variable $ nodeName t)
    where
        evalRes _ (CTreePart _ tp) = tp
        evalRes (Tag name st) _ = Tag name $ map (bindTreeVariables states) st
        evalRes t' _ = t'
        nodeName (Tag name _) = name
        nodeName (Word name _ _) = name

evaluateExpression :: VariableStates -> Expression -> Constant
evaluateExpression _ (Constant c) = c
evaluateExpression states (Variable name) = fromMaybe Null $ M.lookup name states
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
evaluateUnOpExpression UnaryMinus c             = ErrorInEvaluation $ "Unary minus is not applicable to " ++ show c


evaluateBinOpExpression :: BinOp -> Constant -> Constant -> Constant
evaluateBinOpExpression _ Null _ = Null
evaluateBinOpExpression _ _ Null = Null

evaluateBinOpExpression Plus (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 || v2
evaluateBinOpExpression Plus (CInteger v1) (CInteger v2)  = CInteger $ v1 + v2
evaluateBinOpExpression Plus (CDouble v1) (CInteger v2)  = CDouble $ v1 + fromIntegral v2
evaluateBinOpExpression Plus (CInteger v1) (CDouble v2)  = CDouble $ fromIntegral v1 + v2
evaluateBinOpExpression Plus (CDouble v1) (CDouble v2)  = CDouble $ v1 + v2
evaluateBinOpExpression Plus (CString v1) (CString v2)  = CString $ v1 ++ v2
evaluateBinOpExpression Plus a b  = ErrorInEvaluation $ "Plus is not applicable to " ++ show a ++ "/" ++ show b

evaluateBinOpExpression Minus (CInteger v1) (CInteger v2)  = CInteger $ v1 - v2
evaluateBinOpExpression Minus (CDouble v1) (CInteger v2)  = CDouble $ v1 - fromIntegral v2
evaluateBinOpExpression Minus (CInteger v1) (CDouble v2)  = CDouble $ fromIntegral v1 - v2
evaluateBinOpExpression Minus (CDouble v1) (CDouble v2)  = CDouble $ v1 - v2
evaluateBinOpExpression Minus a b  = ErrorInEvaluation $ "Minus is not applicable to " ++ show a ++ "/" ++ show b

evaluateBinOpExpression Multiply (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 && v2
evaluateBinOpExpression Multiply (CInteger v1) (CInteger v2)  = CInteger $ v1 * v2
evaluateBinOpExpression Multiply (CDouble v1) (CInteger v2)  = CDouble $ v1 * fromIntegral v2
evaluateBinOpExpression Multiply (CInteger v1) (CDouble v2)  = CDouble $ fromIntegral v1 * v2
evaluateBinOpExpression Multiply (CDouble v1) (CDouble v2)  = CDouble $ v1 * v2
evaluateBinOpExpression Multiply (CString v1) (CInteger v2)  = CString $ concat $ genericTake v2 $ repeat v1
evaluateBinOpExpression Multiply a b  = ErrorInEvaluation $ "Multiply is not applicable to " ++ show a ++ "/" ++ show b

evaluateBinOpExpression Divide (CBoolean v1) (CBoolean v2)  = CBoolean $ v1 && v2
evaluateBinOpExpression Divide (CInteger v1) (CInteger v2)  = CInteger $ v1 * v2
evaluateBinOpExpression Divide (CDouble v1) (CInteger v2)  = CDouble $ v1 * fromIntegral v2
evaluateBinOpExpression Divide (CInteger v1) (CDouble v2)  = CDouble $ fromIntegral v1 * v2
evaluateBinOpExpression Divide (CDouble v1) (CDouble v2)  = CDouble $ v1 * v2
evaluateBinOpExpression Divide (CString v1) (CInteger v2)  = CString [v1 !! fromIntegral v2]
evaluateBinOpExpression Divide a b  = ErrorInEvaluation $ "Divide is not applicable to " ++ show a ++ "/" ++ show b

evaluateBinOpExpression IsA _ _  = ErrorInEvaluation "IsA operation is not implemented yet"
evaluateBinOpExpression IsNotA _ _  = ErrorInEvaluation "IsNotA operation is not implemented yet"
evaluateBinOpExpression Dot _ _  = ErrorInEvaluation "Dot operation is not implemented yet"