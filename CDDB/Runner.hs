{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CDDB.Runner where

import System.IO.Unsafe

import GHC.Generics
import Data.Aeson hiding (Null)
import qualified Data.Map as M
import Data.Maybe
import Data.Either.Extra
import Control.Monad

import CDDB.Types
import CDDB.Expressions
import CDDB.SyntacticTree

data Context = Context {
        variableStates :: VariableStates,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge
    }

applyTree :: SyntacticTree -> CDDB -> Either Context Context
applyTree t db = foldM evaluateRule emptyContext rules
    where
        rules = matchRules t db

emptyContext :: Context
emptyContext = Context {
        variableStates = VariableStates M.empty,
        accumulatedScore = 1.0,
        accumulatedKnowledge = []
    }

matchRules :: SyntacticTree -> CDDB -> [Rule]
matchRules t cddb = filter (matchRule t) $ rules cddb

matchRule :: SyntacticTree -> Rule -> Bool
matchRule t (Rule _ _ filter _ _ _) = matchFilter t filter

matchFilter :: SyntacticTree -> FilterExpression -> Bool
matchFilter _ Asterix = True
matchFilter (Tag id ts) (FilterTag fid fs) = id == fid && matchFilter' ts fs
matchFilter (Word s) (FilterWord fs) = s == fs
matchFilter _ _ = False

matchFilter' :: [SyntacticTree] -> [FilterExpression] -> Bool
matchFilter' [] [] = True
matchFilter' _ [] = False
matchFilter' [] (Asterix: sfs) = matchFilter' [] sfs
matchFilter' [] _ = False
matchFilter' ts@(t: sts) fs@(Asterix: sfs) = matchFilter' sts fs  -- maitched t with *, not consumed t, consumed *
                                          || matchFilter' ts sfs  -- maitched [] with *, consumed *
                                          -- || matchFilter' sts sfs -- maitched t with *, consumed t, not consumed *
matchFilter' (t: sts) (f: sfs) = matchFilter t f &&  matchFilter' sts sfs

evaluateRule :: Context -> Rule -> Either Context Context
evaluateRule ctx rule@(Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (ctx {variableStates = states', accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else Right ctx
    where
        states' = addLocals (variableStates ctx) locals
        applicable = checkConditions states' conditions

doActions :: Context -> Actions -> Either Context Context
doActions ctx actions = foldM doAction ctx actions

doAction :: Context -> Action -> Either Context Context
doAction ctx Stop = Left ctx
doAction ctx (AddFact p) = Right $ addFact ctx p
doAction ctx (Delete a) = undefined

addFact :: Context -> Primitive -> Context
addFact ctx p = ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) p: (accumulatedKnowledge ctx)}

addLocals :: VariableStates -> Locals -> VariableStates
addLocals states ls = foldl addVariableDef states ls

addVariableDef :: VariableStates -> VariableDef -> VariableStates
addVariableDef states@(VariableStates map) (VariableDef name expr) = VariableStates $ M.insert name value map
    where
        value = evaluateExpression states expr

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states exprs = all (== (CBoolean True)) $ map (evaluateExpression states) exprs

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name fieldVariables) = Fact name $ map (evaluateExpression states) fieldVariables



