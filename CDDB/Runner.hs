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
import Control.Applicative

import CDDB.Types
import CDDB.Expressions
import CDDB.SyntacticTree

data ContextState = Finished | NonFinished deriving (Eq, Ord)

data Context = Context {
        variableStates :: VariableStates,
        currentTree :: SyntacticTree,
        accumulatedScore :: Score,
        accumulatedKnowledge :: Knowledge,
        state :: ContextState
    }

applyTree :: CDDB -> SyntacticTree -> [Context]
applyTree db t = filter ((== Finished) . state) $ applyTreeWithContext db $ emptyContext t

applyTreeWithContext :: CDDB -> Context -> [Context]
applyTreeWithContext db ctx = finished ++ concatMap subApply nonfinished
    where
        (finished, nonfinished) = span ((== Finished) . state) $ applyTreeOnce db ctx
        subApply ctx = applyTreeWithContext db $ resetContext ctx

applyTreeOnce :: CDDB -> Context-> [Context]
applyTreeOnce db ctx = map evaluateRuleAt $ matchRules (currentTree ctx) db
    where
        evaluateRuleAt (r, vs) = evaluateRule ctx {variableStates = vs} r

resetContext :: Context -> Context
resetContext ctx = ctx {variableStates = emptyVariableStates, state = NonFinished}

emptyContext :: SyntacticTree -> Context
emptyContext t = Context {
        variableStates = emptyVariableStates,
        currentTree = t,
        accumulatedScore = 1.0,
        accumulatedKnowledge = [],
        state = NonFinished
    }

matchRules :: SyntacticTree -> CDDB -> [(Rule, VariableStates)]
matchRules t cddb = catMaybes $ map (matchRule t) $ rules cddb

matchRule :: SyntacticTree -> Rule -> Maybe (Rule, VariableStates)
matchRule t r@(Rule _ _ filterExpr _ _ _) = (r,) <$> matchFilter t filterExpr

matchFilter :: SyntacticTree -> FilterExpression -> Maybe VariableStates
matchFilter _ Asterisk = Just emptyVariableStates
matchFilter (Tag id ts) (FilterTag fid fs) = guard (id == fid) >> (matchFilter' ts fs)
matchFilter (Word id s) (FilterWord fid fs) = guard (id == fid && s == fs) >> Just emptyVariableStates
matchFilter _ _ = Nothing

matchFilter' :: [SyntacticTree] -> [FilterExpression] -> Maybe VariableStates
matchFilter' [] [] = Just emptyVariableStates
matchFilter' _ [] = Nothing
matchFilter' [] (Asterisk: sfs) = matchFilter' [] sfs
matchFilter' [] _ = Nothing
matchFilter' ts@(t: sts) fs@(Asterisk: sfs) = matchFilter' sts fs <|> matchFilter' ts sfs
matchFilter' (t: sts) (f: sfs) = do
    vs1 <- matchFilter t f
    vs2 <- matchFilter' sts sfs
    return $ M.unionWith const vs1 vs2

evaluateRule :: Context -> Rule -> Context
evaluateRule ctx rule@(Rule _ ruleScore _ locals conditions actions) =
    if applicable
        then doActions (ctx {variableStates = states', accumulatedScore = accumulatedScore ctx * ruleScore}) actions
        else ctx
    where
        states' = addLocals (variableStates ctx) locals
        applicable = checkConditions states' conditions

doActions :: Context -> Actions -> Context
doActions ctx actions = foldl doAction ctx actions

doAction :: Context -> Action -> Context
doAction ctx Stop = ctx {state = Finished}
doAction ctx (AddFact p) = addFact ctx p
doAction ctx (Delete as) = removeNodes ctx as

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



