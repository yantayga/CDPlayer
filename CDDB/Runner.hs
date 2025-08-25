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
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge
    }

applyTree :: SyntacticTree -> CDDB -> [Either Context Context]
applyTree t db = map (\(r, vs) -> (evaluateRule (newContext t vs) r)) rules
    where
        rules = matchRules t db

newContext :: SyntacticTree -> VariableStates -> Context
newContext t vs = Context {
        variableStates = vs,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = []
    }

matchRules :: SyntacticTree -> CDDB -> [(Rule, VariableStates)]
matchRules t cddb = map snd $ filter fst $ map (matchRule t) $ rules cddb

matchRule :: SyntacticTree -> Rule -> (Bool, (Rule, VariableStates))
matchRule t r@(Rule _ _ filterExpr _ _ _) = (\(a, b) -> (a, (r, b))) $ matchFilter t filterExpr

matchFilter :: SyntacticTree -> FilterExpression -> (Bool, VariableStates)
matchFilter _ Asterisk = (True, emptyVariableStates)
matchFilter (Tag id ts) (FilterTag fid fs) = if id == fid then matchFilter' ts fs else (False, emptyVariableStates) -- TODO: add variable id to defs!!!
matchFilter (Word id s) (FilterWord fid fs) = (id = fid && s == fs, emptyVariableStates)
matchFilter _ _ = (False, emptyVariableStates)

matchFilter' :: [SyntacticTree] -> [FilterExpression] -> (Bool, VariableStates)
matchFilter' [] [] = (True, emptyVariableStates)
matchFilter' _ [] = (False, emptyVariableStates)
matchFilter' [] (Asterisk: sfs) = matchFilter' [] sfs
matchFilter' [] _ = (False, emptyVariableStates)
matchFilter' ts@(t: sts) fs@(Asterisk: sfs) = if fst m1 then m1 else m2
        where
                m1 = matchFilter' sts fs
                m2 = matchFilter' ts sfs
matchFilter' (t: sts) (f: sfs) = if r1 && r2 then (True, M.unionWith const vs1 vs2) else (False, emptyVariableStates)
        where
                (r1, vs1) = matchFilter t f
                (r2, vs2) = matchFilter' sts sfs

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
doAction ctx (Delete as) = Right $ removeNodes ctx as

removeNodes :: Context -> [VariableName] -> Context
removeNodes = undefined

addFact :: Context -> Primitive -> Context
addFact ctx p = ctx {accumulatedKnowledge = evaluateFact (variableStates ctx) p: (accumulatedKnowledge ctx)}

addLocals :: VariableStates -> Locals -> VariableStates
addLocals states ls = foldl addVariableDef states ls

addVariableDef :: VariableStates -> VariableDef -> VariableStates
addVariableDef states (VariableDef name expr) = M.insert name value states
    where
        value = evaluateExpression states expr

checkConditions :: VariableStates -> Conditions -> Bool
checkConditions states exprs = all (== (CBoolean True)) $ map (evaluateExpression states) exprs

evaluateFact :: VariableStates -> Primitive -> Fact
evaluateFact states (Primitive name fieldVariables) = Fact name $ map (evaluateExpression states) fieldVariables



